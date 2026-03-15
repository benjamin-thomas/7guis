import { useState, useEffect, useReducer, useCallback, useRef, useSyncExternalStore } from 'react'
import { assertNever } from './utils'
import './timeTravelDebugger.css'

// --- Single-instance store ---

type HistoryEntry = {
  actionStr: string
  modelJson: string
  rawModel: unknown
}

type InstanceData = {
  previousModelJson: string
  currentModelJson: string
  history: HistoryEntry[]
  jumpToModel: (rawModel: unknown) => void
}

let instance: InstanceData | null = null
let listeners: (() => void)[] = []
let storeVersion = 0

const notifyListeners = () => {
  storeVersion = storeVersion + 1
  listeners.forEach(fn => fn())
}

const subscribe = (listener: () => void): (() => void) => {
  listeners = [...listeners, listener]
  return () => {
    listeners = listeners.filter(fn => fn !== listener)
  }
}

const getSnapshot = () => storeVersion

// --- Action buffer (module-level, shared by all consumers) ---

let pendingActions: string[] = []
let actionListeners: (() => void)[] = []

const subscribeActions = (listener: () => void): (() => void) => {
  actionListeners = [...actionListeners, listener]
  return () => {
    actionListeners = actionListeners.filter(fn => fn !== listener)
  }
}

export const reportAction = (action: string) => {
  pendingActions = [...pendingActions, action]
  actionListeners.forEach(fn => fn())
}

// --- Pause state (module-level, checked by consumers) ---

let paused = false
export const isPaused = () => paused

// --- Instance hook (called from consumers) ---

export const useInstance = <Model,>(model: Model, jumpToModel: (rawModel: unknown) => void) => {
  const [trigger, setTrigger] = useState(0)

  useEffect(() => {
    pendingActions = []
    const initJson = JSON.stringify(model, null, 2) ?? "{}"
    const data: InstanceData = {
      previousModelJson: initJson,
      currentModelJson: initJson,
      history: [
        {
          actionStr: '"@@INIT"',
          modelJson: initJson,
          rawModel: model,
        },
      ],
      jumpToModel,
    }
    instance = data
    notifyListeners()

    const unsubscribeActions = subscribeActions(() => setTrigger(n => n + 1))

    return () => {
      instance = null
      pendingActions = []
      unsubscribeActions()
      notifyListeners()
    }
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  useEffect(() => {
    if (instance === null) return

    const data = instance
    const modelJson = JSON.stringify(model, null, 2) ?? "{}"
    data.previousModelJson = data.currentModelJson
    data.currentModelJson = modelJson

    const actions = pendingActions
    if (actions.length > 0) {
      actions.forEach(actionStr => {
        const entry: HistoryEntry = {
          actionStr,
          modelJson,
          rawModel: model,
        }
        data.history = [...data.history, entry]
      })
      pendingActions = []
    }

    notifyListeners()
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [model, trigger])
}

// --- Overlay helpers ---

const actionTag = (actionStr: string): string => {
  if (actionStr.startsWith("{")) {
    try {
      const obj = JSON.parse(actionStr)
      if (typeof obj === "object" && obj !== null && "kind" in obj) {
        return obj.kind
      }
      return actionStr
    } catch {
      return actionStr
    }
  }
  return actionStr.replaceAll('"', '')
}

type DiffEntry = { path: string; oldVal: string; newVal: string }

const jsonDiff = (old: unknown, cur: unknown, path = ""): DiffEntry[] => {
  if (old === cur) return []

  if (
    typeof old === "object" && old !== null && !Array.isArray(old) &&
    typeof cur === "object" && cur !== null && !Array.isArray(cur)
  ) {
    const oldObj = old as Record<string, unknown>
    const curObj = cur as Record<string, unknown>
    const allKeys = [...new Set([...Object.keys(oldObj), ...Object.keys(curObj)])]
    return allKeys.flatMap(key => {
      const subPath = path === "" ? key : path + "." + key
      if (key in oldObj && key in curObj) {
        return jsonDiff(oldObj[key], curObj[key], subPath)
      } else if (key in oldObj) {
        return [{ path: subPath, oldVal: JSON.stringify(oldObj[key]), newVal: "-" }]
      } else {
        return [{ path: subPath, oldVal: "-", newVal: JSON.stringify(curObj[key]) }]
      }
    })
  }

  if (Array.isArray(old) && Array.isArray(cur)) {
    const maxLen = Math.max(old.length, cur.length)
    return Array.from({ length: maxLen }, (_, i) => i).flatMap(i => {
      const subPath = path === "" ? `[${i}]` : `${path}[${i}]`
      if (i < old.length && i < cur.length) {
        return jsonDiff(old[i], cur[i], subPath)
      } else if (i < old.length) {
        return [{ path: subPath, oldVal: JSON.stringify(old[i]), newVal: "(removed)" }]
      } else {
        return [{ path: subPath, oldVal: "(added)", newVal: JSON.stringify(cur[i]) }]
      }
    })
  }

  const label = path === "" ? "root" : path
  return [{ path: label, oldVal: JSON.stringify(old), newVal: JSON.stringify(cur) }]
}

const computeDiff = (prevJson: string, curJson: string): DiffEntry[] => {
  if (prevJson === curJson) return []
  try {
    return jsonDiff(JSON.parse(prevJson), JSON.parse(curJson))
  } catch {
    return []
  }
}

// --- Resize handle ---

const ResizeHandle = ({ onDrag }: { onDrag: (dy: number) => void }) => {
  const dragging = useRef(false)
  const cleanupRef = useRef(() => {})

  useEffect(() => {
    return () => cleanupRef.current()
  }, [])

  const onMouseDown = useCallback((e: React.MouseEvent) => {
    e.preventDefault()
    dragging.current = true

    const onMouseMove = (evt: MouseEvent) => {
      if (dragging.current) {
        onDrag(evt.movementY)
      }
    }

    let onMouseUp: (evt: MouseEvent) => void
    const cleanup = () => {
      dragging.current = false
      document.removeEventListener("mousemove", onMouseMove)
      document.removeEventListener("mouseup", onMouseUp)
      cleanupRef.current = () => {}
    }
    onMouseUp = () => cleanup()
    cleanupRef.current = cleanup

    document.addEventListener("mousemove", onMouseMove)
    document.addEventListener("mouseup", onMouseUp)
  }, [onDrag])

  return <div className="debug-resize-handle" onMouseDown={onMouseDown} />
}

// --- Overlay component ---

type OverlayState = {
  collapsed: boolean
  onLeft: boolean
  modelHeight: number
  diffHeight: number
  hideFilter: string
  collapseFilter: string
  selectedHistoryNum: number | null
}

type OverlayAction =
  | { kind: "Expand" }
  | { kind: "Collapse" }
  | { kind: "ToggleSide" }
  | { kind: "DragModelPanel"; dy: number }
  | { kind: "DragDiffPanel"; dy: number }
  | { kind: "SetHideFilter"; value: string }
  | { kind: "SetCollapseFilter"; value: string }
  | { kind: "SelectHistory"; num: number }
  | { kind: "DeselectHistory" }

const overlayInit: OverlayState = {
  collapsed: false,
  onLeft: false,
  modelHeight: 200,
  diffHeight: 80,
  hideFilter: "",
  collapseFilter: "",
  selectedHistoryNum: null,
}

const overlayReducer = (state: OverlayState, action: OverlayAction): OverlayState => {
  switch (action.kind) {
    case "Expand": return { ...state, collapsed: false }
    case "Collapse": return { ...state, collapsed: true }
    case "ToggleSide": return { ...state, onLeft: !state.onLeft }
    case "DragModelPanel": return { ...state, modelHeight: Math.max(40, state.modelHeight + action.dy) }
    case "DragDiffPanel": return { ...state, diffHeight: Math.max(30, state.diffHeight + action.dy) }
    case "SetHideFilter": return { ...state, hideFilter: action.value }
    case "SetCollapseFilter": return { ...state, collapseFilter: action.value }
    case "SelectHistory": return { ...state, selectedHistoryNum: action.num }
    case "DeselectHistory": return { ...state, selectedHistoryNum: null }
    default: return assertNever(action)
  }
}

export const Overlay = () => {
  useSyncExternalStore(subscribe, getSnapshot)
  const [state, dispatch] = useReducer(overlayReducer, overlayInit)

  const parseTags = (str: string) =>
    str.split(",").map(s => s.trim().toLowerCase()).filter(s => s !== "")

  const hiddenTags = parseTags(state.hideFilter)
  const collapseTags = parseTags(state.collapseFilter)

  if (instance === null) return null

  const data = instance

  if (state.collapsed) {
    return (
      <button
        className={"debug-toggle" + (state.onLeft ? " debug-toggle--left" : "")}
        onClick={() => dispatch({ kind: "Expand" })}
      >
        Debug
      </button>
    )
  }

  const indexedHistory = data.history.map((entry, idx) => [entry, idx + 1] as const)
  const filteredHistory = indexedHistory.filter(([entry]) => {
    const tag = actionTag(entry.actionStr).toLowerCase()
    return !hiddenTags.some(h => tag === h)
  })

  // Collapse consecutive runs of the same tag
  const displayHistory: [HistoryEntry, number, number, string][] = []
  for (const [entry, num] of filteredHistory) {
    const tag = actionTag(entry.actionStr)
    const last = displayHistory.at(-1)
    const shouldCollapse = (t: string) => collapseTags.some(c => c === t.toLowerCase())
    if (last && last[3] === tag && shouldCollapse(tag)) {
      displayHistory[displayHistory.length - 1] = [entry, num, last[2] + 1, tag]
    } else {
      displayHistory.push([entry, num, 1, tag])
    }
  }

  const isActionHidden = (entry: HistoryEntry) => {
    const tag = actionTag(entry.actionStr).toLowerCase()
    return hiddenTags.some(h => tag === h)
  }

  const diffHidden = state.selectedHistoryNum !== null
    ? (data.history[state.selectedHistoryNum - 1] ? isActionHidden(data.history[state.selectedHistoryNum - 1]) : false)
    : (data.history.at(-1) ? isActionHidden(data.history.at(-1)!) : false)

  const [diffs, diffLabel] = (() => {
    if (state.selectedHistoryNum !== null) {
      const num = state.selectedHistoryNum
      const prevJson = data.history[num - 2]?.modelJson ?? "{}"
      const curJson = data.history[num - 1]?.modelJson ?? "{}"
      return [computeDiff(prevJson, curJson), `Diff (step ${num})`] as const
    } else {
      return [computeDiff(data.previousModelJson, data.currentModelJson), "Diff (live)"] as const
    }
  })()

  const allTags = (() => {
    const seen = new Set<string>()
    return data.history.flatMap(entry => {
      const tag = actionTag(entry.actionStr)
      if (seen.has(tag)) return []
      seen.add(tag)
      return [tag]
    })
  })()

  const renderFilterInput = (
    label: string,
    value: string,
    onSetValue: (v: string) => void,
    excludeTags: string[],
  ) => {
    const currentInput = (() => {
      const parts = value.split(",")
      return (parts.at(-1) ?? "").trim().toLowerCase()
    })()

    const suggestions = allTags.filter(tag => {
      const lower = tag.toLowerCase()
      const alreadyUsed = excludeTags.some(h => h === lower)
      return !alreadyUsed && currentInput !== "" && lower.startsWith(currentInput)
    })

    const completeWith = (tag: string) => {
      const parts = value.split(",")
      parts.pop()
      const prefix = parts.length > 0 ? parts.join(", ") + ", " : ""
      onSetValue(prefix + tag)
    }

    return (
      <>
        <input
          className="debug-filter"
          type="text"
          placeholder={label + ": action names, ..."}
          value={value}
          onChange={e => onSetValue(e.target.value)}
          onKeyDown={e => {
            if (e.key === "Tab" && suggestions.length > 0) {
              e.preventDefault()
              completeWith(suggestions[0])
            }
          }}
        />
        {suggestions.length > 0 && (
          <div className="debug-suggestions">
            {suggestions.map((tag, idx) => (
              <button
                key={idx}
                className="debug-suggestion"
                onClick={() => completeWith(tag)}
              >
                {tag}
              </button>
            ))}
          </div>
        )}
      </>
    )
  }

  return (
    <>
      {paused && <div className="debug-app-lock" />}
      <div className={"debug-overlay" + (state.onLeft ? " debug-overlay--left" : "")}>
        <div className="debug-header">
          <span className="debug-title">Time Travel Debugger</span>
          <span className="debug-header-buttons">
            <button
              className={"debug-header-btn" + (paused ? " debug-header-btn--active" : "")}
              onClick={() => {
                paused = !paused
                notifyListeners()
              }}
            >
              {paused ? "Play" : "Pause"}
            </button>
            <button className="debug-header-btn" onClick={() => dispatch({ kind: "ToggleSide" })}>
              {state.onLeft ? "Right" : "Left"}
            </button>
            <button className="debug-header-btn" onClick={() => dispatch({ kind: "Collapse" })}>
              Min
            </button>
          </span>
        </div>
        <div className="debug-body">
          <div
            className="debug-section debug-section--panel"
            style={{ height: state.modelHeight + "px" }}
          >
            <div className="debug-section-title">Current Model</div>
            <pre className="debug-model">{data.currentModelJson}</pre>
          </div>
          <ResizeHandle onDrag={dy => dispatch({ kind: "DragModelPanel", dy })} />
          <div
            className="debug-section debug-section--panel"
            style={{ height: state.diffHeight + "px" }}
          >
            <div className="debug-section-title">{diffLabel}</div>
            {diffHidden ? (
              <div className="debug-diff-hidden">Hidden</div>
            ) : (
              <div className="debug-diff">
                {diffs.map((d, idx) => (
                  <div key={idx} className="debug-diff-entry">
                    <span className="debug-diff-path">{d.path}</span>
                    <span className="debug-diff-old">{d.oldVal}</span>
                    <span className="debug-diff-arrow"> -&gt; </span>
                    <span className="debug-diff-new">{d.newVal}</span>
                  </div>
                ))}
              </div>
            )}
          </div>
          <ResizeHandle onDrag={dy => dispatch({ kind: "DragDiffPanel", dy })} />
          <div className="debug-section debug-section--history">
            <div className="debug-section-header">
              <div className="debug-section-title">
                History ({displayHistory.length}/{indexedHistory.length})
              </div>
              <button
                className="debug-jump-btn"
                onClick={() => {
                  data.history = []
                  paused = false
                  notifyListeners()
                }}
              >
                Clear
              </button>
            </div>
            {renderFilterInput(
              "Hide",
              state.hideFilter,
              v => dispatch({ kind: "SetHideFilter", value: v }),
              hiddenTags,
            )}
            {renderFilterInput(
              "Collapse",
              state.collapseFilter,
              v => dispatch({ kind: "SetCollapseFilter", value: v }),
              collapseTags,
            )}
            <div className="debug-history">
              {[...displayHistory].reverse().map(([entry, num, count, _tag], idx) => {
                const isSelected = state.selectedHistoryNum === num
                return (
                  <div
                    key={idx}
                    className={isSelected
                      ? "debug-history-entry debug-history-entry--selected"
                      : "debug-history-entry"}
                    onClick={() => {
                      if (isSelected) {
                        paused = false
                        dispatch({ kind: "DeselectHistory" })
                      } else {
                        paused = true
                        dispatch({ kind: "SelectHistory", num })
                      }
                      data.jumpToModel(entry.rawModel)
                    }}
                  >
                    <span className="debug-entry-num">{num}</span>
                    <span className="debug-action-name">{actionTag(entry.actionStr)}</span>
                    {count > 1 && (
                      <span className="debug-entry-count">(x{count})</span>
                    )}
                  </div>
                )
              })}
            </div>
          </div>
        </div>
      </div>
    </>
  )
}
