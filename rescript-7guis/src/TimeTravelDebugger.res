%%raw("import './timeTravelDebugger.css'")

// --- Single-instance store ---

type historyEntry = {
  actionStr: string,
  modelJson: string,
  rawModel: JSON.t,
}

type instanceData = {
  mutable previousModelJson: string,
  mutable currentModelJson: string,
  mutable history: array<historyEntry>,
  jumpToModel: JSON.t => unit,
}

let instance: ref<option<instanceData>> = ref(None)
let listeners: ref<array<unit => unit>> = ref([])
let storeVersion: ref<int> = ref(0)

let notifyListeners = () => {
  storeVersion.contents = storeVersion.contents + 1
  listeners.contents->Array.forEach(fn => fn())
}

let subscribe = (listener: unit => unit): (unit => unit) => {
  listeners.contents = Array.concat(listeners.contents, [listener])
  () => {
    listeners.contents = listeners.contents->Array.filter(fn => fn !== listener)
  }
}

let getSnapshot = () => storeVersion.contents

// --- Action buffer (module-level, shared by all consumers) ---

let pendingActions: ref<array<string>> = ref([])
let actionListeners: ref<array<unit => unit>> = ref([])

let subscribeActions = (listener: unit => unit): (unit => unit) => {
  actionListeners.contents = Array.concat(actionListeners.contents, [listener])
  () => {
    actionListeners.contents = actionListeners.contents->Array.filter(fn => fn !== listener)
  }
}

let reportAction = (action: string) => {
  pendingActions.contents = Array.concat(pendingActions.contents, [action])
  actionListeners.contents->Array.forEach(fn => fn())
}

// --- Pause state (module-level, checked by consumers) ---

let paused: ref<bool> = ref(false)
let isPaused = () => paused.contents

// --- Instance hook (called from consumers) ---

let useInstance = (~model: 'model, ~jumpToModel: JSON.t => unit) => {
  let (trigger, setTrigger) = React.useState(() => 0)

  React.useEffect0(() => {
    pendingActions.contents = []
    let initJson = JSON.stringifyAny(Obj.magic(model), ~space=2)->Option.getOr("{}")
    let data: instanceData = {
      previousModelJson: initJson,
      currentModelJson: initJson,
      history: [
        {
          actionStr: "\"@@INIT\"",
          modelJson: initJson,
          rawModel: Obj.magic(model),
        },
      ],
      jumpToModel,
    }
    instance.contents = Some(data)
    notifyListeners()

    let unsubscribeActions = subscribeActions(() => setTrigger(n => n + 1))

    Some(
      () => {
        instance.contents = None
        pendingActions.contents = []
        unsubscribeActions()
        notifyListeners()
      },
    )
  })

  React.useEffect2(() => {
    switch instance.contents {
    | None => ()
    | Some(data) =>
      let modelJson = JSON.stringifyAny(Obj.magic(model), ~space=2)->Option.getOr("{}")
      data.previousModelJson = data.currentModelJson
      data.currentModelJson = modelJson

      let actions = pendingActions.contents
      if Array.length(actions) > 0 {
        actions->Array.forEach(actionStr => {
          let entry: historyEntry = {
            actionStr,
            modelJson,
            rawModel: Obj.magic(model),
          }
          data.history = Array.concat(data.history, [entry])
        })
        pendingActions.contents = []
      }

      notifyListeners()
    }
    None
  }, (model, trigger))
}

// --- Overlay helpers ---

let actionTag = (actionStr: string): string => {
  switch actionStr->String.startsWith("{") {
  | true =>
    switch JSON.parseOrThrow(actionStr)->JSON.Decode.object {
    | Some(obj) =>
      switch Dict.get(obj, "TAG") {
      | Some(tag) => JSON.Decode.string(tag)->Option.getOr(actionStr)
      | None => actionStr
      }
    | None => actionStr
    | exception _ => actionStr
    }
  | false => actionStr->String.replaceAll("\"", "")
  }
}

type diffEntry = {path: string, oldVal: string, newVal: string}

let rec jsonDiff = (~path="", old: JSON.t, cur: JSON.t): array<diffEntry> => {
  switch (old, cur) {
  | _ if old === cur => []
  | (JSON.Object(oldObj), JSON.Object(curObj)) =>
    let allKeys = Dict.keysToArray(oldObj)->Array.concat(Dict.keysToArray(curObj))
    let seen = Dict.make()
    allKeys
    ->Array.filterMap(key => {
      if Dict.get(seen, key)->Option.isSome {
        None
      } else {
        Dict.set(seen, key, true)
        let subPath = path === "" ? key : path ++ "." ++ key
        let entries = switch (Dict.get(oldObj, key), Dict.get(curObj, key)) {
        | (Some(a), Some(b)) => jsonDiff(~path=subPath, a, b)
        | (Some(a), None) => [{path: subPath, oldVal: JSON.stringify(a), newVal: "-"}]
        | (None, Some(b)) => [{path: subPath, oldVal: "-", newVal: JSON.stringify(b)}]
        | (None, None) => []
        }
        if Array.length(entries) > 0 {
          Some(entries)
        } else {
          None
        }
      }
    })
    ->Array.flat
  | (JSON.Array(oldArr), JSON.Array(curArr)) =>
    let maxLen = Math.Int.max(Array.length(oldArr), Array.length(curArr))
    Array.fromInitializer(~length=maxLen, i => i)->Array.flatMap(i => {
      let subPath =
        path === "" ? "[" ++ Int.toString(i) ++ "]" : path ++ "[" ++ Int.toString(i) ++ "]"
      switch (oldArr->Array.get(i), curArr->Array.get(i)) {
      | (Some(a), Some(b)) => jsonDiff(~path=subPath, a, b)
      | (Some(a), None) => [{path: subPath, oldVal: JSON.stringify(a), newVal: "(removed)"}]
      | (None, Some(b)) => [{path: subPath, oldVal: "(added)", newVal: JSON.stringify(b)}]
      | (None, None) => []
      }
    })
  | _ =>
    let label = if path === "" {
      "root"
    } else {
      path
    }
    [{path: label, oldVal: JSON.stringify(old), newVal: JSON.stringify(cur)}]
  }
}

let computeDiff = (prevJson: string, curJson: string): array<diffEntry> => {
  if prevJson === curJson {
    []
  } else {
    switch (JSON.parseOrThrow(prevJson), JSON.parseOrThrow(curJson)) {
    | (old, cur) => jsonDiff(old, cur)
    | exception _ => []
    }
  }
}

// --- Resize handle ---

@val external addMouseListener: (string, {..} => unit) => unit = "document.addEventListener"
@val external removeMouseListener: (string, {..} => unit) => unit = "document.removeEventListener"

module ResizeHandle = {
  @react.component
  let make = (~onDrag: int => unit) => {
    let dragging = React.useRef(false)
    let cleanupRef = React.useRef(() => ())

    React.useEffect0(() => {
      Some(() => cleanupRef.current())
    })

    let onMouseDown = React.useCallback1((e: JsxEvent.Mouse.t) => {
      JsxEvent.Mouse.preventDefault(e)
      dragging.current = true

      let onMouseMove = (evt: {..}) => {
        if dragging.current {
          onDrag(evt["movementY"])
        }
      }

      let onMouseUp = ref(_ => ())
      let cleanup = () => {
        dragging.current = false
        removeMouseListener("mousemove", onMouseMove)
        removeMouseListener("mouseup", onMouseUp.contents)
        cleanupRef.current = () => ()
      }
      onMouseUp.contents = _evt => cleanup()
      cleanupRef.current = cleanup

      addMouseListener("mousemove", onMouseMove)
      addMouseListener("mouseup", onMouseUp.contents)
    }, [onDrag])

    <div className="debug-resize-handle" onMouseDown />
  }
}

// --- Overlay component ---

type overlayState = {
  collapsed: bool,
  onLeft: bool,
  modelHeight: int,
  diffHeight: int,
  hideFilter: string,
  collapseFilter: string,
  selectedHistoryNum: option<int>,
}

type overlayAction =
  | Expand
  | Collapse
  | ToggleSide
  | DragModelPanel(int)
  | DragDiffPanel(int)
  | SetHideFilter(string)
  | SetCollapseFilter(string)
  | SelectHistory(int)
  | DeselectHistory

let overlayInit = {
  collapsed: false,
  onLeft: false,
  modelHeight: 200,
  diffHeight: 80,
  hideFilter: "",
  collapseFilter: "",
  selectedHistoryNum: None,
}

let overlayReducer = (state, action) =>
  switch action {
  | Expand => {...state, collapsed: false}
  | Collapse => {...state, collapsed: true}
  | ToggleSide => {...state, onLeft: !state.onLeft}
  | DragModelPanel(dy) => {...state, modelHeight: Math.Int.max(40, state.modelHeight + dy)}
  | DragDiffPanel(dy) => {...state, diffHeight: Math.Int.max(30, state.diffHeight + dy)}
  | SetHideFilter(v) => {...state, hideFilter: v}
  | SetCollapseFilter(v) => {...state, collapseFilter: v}
  | SelectHistory(num) => {...state, selectedHistoryNum: Some(num)}
  | DeselectHistory => {...state, selectedHistoryNum: None}
  }

module Overlay = {
  @react.component
  let make = () => {
    let _ = React.useSyncExternalStore(~subscribe, ~getSnapshot)
    let (state, dispatch) = React.useReducer(overlayReducer, overlayInit)

    let parseTags = str =>
      str
      ->String.split(",")
      ->Array.map(s => s->String.trim->String.toLowerCase)
      ->Array.filter(s => s !== "")

    let hiddenTags = parseTags(state.hideFilter)
    let collapseTags = parseTags(state.collapseFilter)

    switch instance.contents {
    | None => React.null
    | Some(data) =>
      if state.collapsed {
        <button
          className={"debug-toggle" ++ (state.onLeft ? " debug-toggle--left" : "")}
          onClick={_ => dispatch(Expand)}
        >
          {React.string("Debug")}
        </button>
      } else {
        let indexedHistory = data.history->Array.mapWithIndex((entry, idx) => (entry, idx + 1))
        let filteredHistory = indexedHistory->Array.filter(((entry, _)) => {
          let tag = actionTag(entry.actionStr)->String.toLowerCase
          !(hiddenTags->Array.some(h => tag === h))
        })

        // Collapse consecutive runs of the same tag
        let displayHistory = {
          let shouldCollapse = tag => collapseTags->Array.some(c => c === tag->String.toLowerCase)

          filteredHistory->Array.reduce([], (acc, (entry, num)) => {
            let tag = actionTag(entry.actionStr)
            switch acc->Array.at(-1) {
            | Some((_prevEntry, _prevNum, count, prevTag))
              if prevTag === tag && shouldCollapse(tag) =>
              // Replace the last element with updated entry/count
              let len = Array.length(acc)
              let _ =
                acc->Array.splice(~start=len - 1, ~remove=1, ~insert=[(entry, num, count + 1, tag)])
              acc
            | _ =>
              let _ = acc->Array.push((entry, num, 1, tag))
              acc
            }
          })
        }

        let isActionHidden = (entry: historyEntry) => {
          let tag = actionTag(entry.actionStr)->String.toLowerCase
          hiddenTags->Array.some(h => tag === h)
        }

        let diffHidden = switch state.selectedHistoryNum {
        | Some(num) =>
          data.history->Array.get(num - 1)->Option.map(isActionHidden)->Option.getOr(false)
        | None => data.history->Array.at(-1)->Option.map(isActionHidden)->Option.getOr(false)
        }

        let (diffs, diffLabel) = switch state.selectedHistoryNum {
        | Some(num) =>
          let prevJson =
            data.history
            ->Array.get(num - 2)
            ->Option.map(e => e.modelJson)
            ->Option.getOr("{}")
          let curJson =
            data.history
            ->Array.get(num - 1)
            ->Option.map(e => e.modelJson)
            ->Option.getOr("{}")
          (computeDiff(prevJson, curJson), "Diff (step " ++ Int.toString(num) ++ ")")
        | None => (computeDiff(data.previousModelJson, data.currentModelJson), "Diff (live)")
        }

        <>
          {if paused.contents {
            <div className="debug-app-lock" />
          } else {
            React.null
          }}
          <div className={"debug-overlay" ++ (state.onLeft ? " debug-overlay--left" : "")}>
            <div className="debug-header">
              <span className="debug-title"> {React.string("Time Travel Debugger")} </span>
              <span className="debug-header-buttons">
                <button
                  className={"debug-header-btn" ++ (
                    paused.contents ? " debug-header-btn--active" : ""
                  )}
                  onClick={_ => {
                    paused.contents = !paused.contents
                    notifyListeners()
                  }}
                >
                  {React.string(paused.contents ? "Play" : "Pause")}
                </button>
                <button className="debug-header-btn" onClick={_ => dispatch(ToggleSide)}>
                  {React.string(state.onLeft ? "Right" : "Left")}
                </button>
                <button className="debug-header-btn" onClick={_ => dispatch(Collapse)}>
                  {React.string("Min")}
                </button>
              </span>
            </div>
            <div className="debug-body">
              <div
                className="debug-section debug-section--panel"
                style={height: Int.toString(state.modelHeight) ++ "px"}
              >
                <div className="debug-section-title"> {React.string("Current Model")} </div>
                <pre className="debug-model"> {React.string(data.currentModelJson)} </pre>
              </div>
              <ResizeHandle onDrag={dy => dispatch(DragModelPanel(dy))} />
              <div
                className="debug-section debug-section--panel"
                style={height: Int.toString(state.diffHeight) ++ "px"}
              >
                <div className="debug-section-title"> {React.string(diffLabel)} </div>
                {if diffHidden {
                  <div className="debug-diff-hidden"> {React.string("Hidden")} </div>
                } else {
                  <div className="debug-diff">
                    {React.array(
                      diffs->Array.mapWithIndex((d, idx) => {
                        <div key={Int.toString(idx)} className="debug-diff-entry">
                          <span className="debug-diff-path"> {React.string(d.path)} </span>
                          <span className="debug-diff-old"> {React.string(d.oldVal)} </span>
                          <span className="debug-diff-arrow"> {React.string(" -> ")} </span>
                          <span className="debug-diff-new"> {React.string(d.newVal)} </span>
                        </div>
                      }),
                    )}
                  </div>
                }}
              </div>
              <ResizeHandle onDrag={dy => dispatch(DragDiffPanel(dy))} />
              <div className="debug-section debug-section--history">
                <div className="debug-section-header">
                  <div className="debug-section-title">
                    {React.string(
                      "History (" ++
                      Int.toString(Array.length(displayHistory)) ++
                      "/" ++
                      Int.toString(Array.length(indexedHistory)) ++ ")",
                    )}
                  </div>
                  <button
                    className="debug-jump-btn"
                    onClick={_ => {
                      data.history = []
                      paused.contents = false
                      notifyListeners()
                    }}
                  >
                    {React.string("Clear")}
                  </button>
                </div>
                {
                  let allTags = {
                    let seen = Dict.make()
                    data.history->Array.filterMap(entry => {
                      let tag = actionTag(entry.actionStr)
                      if Dict.get(seen, tag)->Option.isSome {
                        None
                      } else {
                        Dict.set(seen, tag, true)
                        Some(tag)
                      }
                    })
                  }

                  let renderFilterInput = (~label, ~value, ~onSetValue, ~excludeTags) => {
                    let currentInput = {
                      let parts = value->String.split(",")
                      parts->Array.at(-1)->Option.getOr("")->String.trim->String.toLowerCase
                    }

                    let suggestions = allTags->Array.filter(tag => {
                      let lower = tag->String.toLowerCase
                      let alreadyUsed = excludeTags->Array.some(h => h === lower)
                      !alreadyUsed && currentInput !== "" && lower->String.startsWith(currentInput)
                    })

                    let completeWith = tag => {
                      let parts = value->String.split(",")
                      let _ = parts->Array.pop
                      let prefix = if Array.length(parts) > 0 {
                        parts->Array.join(", ") ++ ", "
                      } else {
                        ""
                      }
                      onSetValue(prefix ++ tag)
                    }

                    <>
                      <input
                        className="debug-filter"
                        type_="text"
                        placeholder={label ++ ": action names, ..."}
                        value
                        onChange={e => {
                          let v = ReactEvent.Form.target(e)["value"]
                          onSetValue(v)
                        }}
                        onKeyDown={e => {
                          if ReactEvent.Keyboard.key(e) === "Tab" && Array.length(suggestions) > 0 {
                            ReactEvent.Keyboard.preventDefault(e)
                            switch suggestions->Array.get(0) {
                            | Some(tag) => completeWith(tag)
                            | None => ()
                            }
                          }
                        }}
                      />
                      {if Array.length(suggestions) > 0 {
                        <div className="debug-suggestions">
                          {React.array(
                            suggestions->Array.mapWithIndex((tag, idx) => {
                              <button
                                key={Int.toString(idx)}
                                className="debug-suggestion"
                                onClick={_ => completeWith(tag)}
                              >
                                {React.string(tag)}
                              </button>
                            }),
                          )}
                        </div>
                      } else {
                        React.null
                      }}
                    </>
                  }

                  <>
                    {renderFilterInput(
                      ~label="Hide",
                      ~value=state.hideFilter,
                      ~onSetValue=v => dispatch(SetHideFilter(v)),
                      ~excludeTags=hiddenTags,
                    )}
                    {renderFilterInput(
                      ~label="Collapse",
                      ~value=state.collapseFilter,
                      ~onSetValue=v => dispatch(SetCollapseFilter(v)),
                      ~excludeTags=collapseTags,
                    )}
                  </>
                }
                <div className="debug-history">
                  {React.array(
                    displayHistory
                    ->Array.toReversed
                    ->Array.mapWithIndex(((entry, num, count, _tag), idx) => {
                      let isSelected = state.selectedHistoryNum == Some(num)
                      <div
                        key={Int.toString(idx)}
                        className={isSelected
                          ? "debug-history-entry debug-history-entry--selected"
                          : "debug-history-entry"}
                        onClick={_ => {
                          if isSelected {
                            paused.contents = false
                            dispatch(DeselectHistory)
                          } else {
                            paused.contents = true
                            dispatch(SelectHistory(num))
                          }
                          data.jumpToModel(entry.rawModel)
                        }}
                      >
                        <span className="debug-entry-num"> {React.string(Int.toString(num))} </span>
                        <span className="debug-action-name">
                          {React.string(actionTag(entry.actionStr))}
                        </span>
                        {if count > 1 {
                          <span className="debug-entry-count">
                            {React.string("(x" ++ Int.toString(count) ++ ")")}
                          </span>
                        } else {
                          React.null
                        }}
                      </div>
                    }),
                  )}
                </div>
              </div>
            </div>
          </div>
        </>
      }
    }
  }
}
