%%raw("import './timeTravelDebugger.css'")

let isDebugEnabled: bool = %raw("import.meta.env.VITE_DEBUG === '1'")

// --- Registry infrastructure ---

let nextInstanceId: ref<int> = ref(0)

type historyEntry = {
  actionStr: string,
  modelJson: string,
  rawModel: JSON.t,
}

type instanceData = {
  id: int,
  mutable previousModelJson: string,
  mutable currentModelJson: string,
  mutable history: array<historyEntry>,
  jumpToModel: JSON.t => unit,
}

let registry: ref<Dict.t<instanceData>> = ref(Dict.make())
let listeners: ref<array<unit => unit>> = ref([])

let notifyListeners = () =>
  listeners.contents->Array.forEach(fn => fn())

let subscribe = (listener: unit => unit): (unit => unit) => {
  listeners.contents = Array.concat(listeners.contents, [listener])
  () => {
    listeners.contents = listeners.contents->Array.filter(fn => fn !== listener)
  }
}

let getSnapshot = (): Dict.t<instanceData> =>
  registry.contents

let registerInstance = (data: instanceData): unit => {
  let newDict = Dict.fromArray(
    Array.concat(Dict.toArray(registry.contents), [(Int.toString(data.id), data)]),
  )
  registry.contents = newDict
  notifyListeners()
}

let unregisterInstance = (id: int): unit => {
  let newDict = Dict.make()
  registry.contents
  ->Dict.toArray
  ->Array.forEach(((key, value)) => {
    if key !== Int.toString(id) {
      Dict.set(newDict, key, value)
    }
  })
  registry.contents = newDict
  notifyListeners()
}

// --- Observer hook (called from Effect.useReducer consumers) ---

let useObserver = (~model: 'model, ~pendingActionRef: React.ref<option<string>>, ~jumpToModel: JSON.t => unit) => {
  let instanceId = React.useMemo0(() => {
    let id = nextInstanceId.contents
    nextInstanceId.contents = nextInstanceId.contents + 1
    id
  })

  React.useEffect0(() => {
    let initJson = JSON.stringifyAny(Obj.magic(model), ~space=2)->Option.getOr("{}")
    let data: instanceData = {
      id: instanceId,
      previousModelJson: initJson,
      currentModelJson: initJson,
      history: [],
      jumpToModel,
    }
    registerInstance(data)
    Some(() => unregisterInstance(instanceId))
  })

  React.useEffect1(() => {
    let key = Int.toString(instanceId)
    switch Dict.get(registry.contents, key) {
    | None => ()
    | Some(data) =>
      let modelJson = JSON.stringifyAny(Obj.magic(model), ~space=2)->Option.getOr("{}")
      data.previousModelJson = data.currentModelJson
      data.currentModelJson = modelJson

      switch pendingActionRef.current {
      | Some(actionStr) =>
        let entry: historyEntry = {
          actionStr,
          modelJson,
          rawModel: Obj.magic(model),
        }
        data.history = Array.concat(data.history, [entry])
        pendingActionRef.current = None
      | None => ()
      }

      notifyListeners()
    }
    None
  }, [model])
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
  | false =>
    actionStr->String.replaceAll("\"", "")
  }
}

type diffEntry = {path: string, oldVal: string, newVal: string}

let rec jsonDiff = (~path="", old: JSON.t, cur: JSON.t): array<diffEntry> => {
  switch (old, cur) {
  | _ if old === cur => []
  | (JSON.Object(oldObj), JSON.Object(curObj)) =>
    let allKeys = Dict.keysToArray(oldObj)->Array.concat(Dict.keysToArray(curObj))
    let seen = Dict.make()
    allKeys->Array.filterMap(key => {
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
  | _ =>
    let label = if path === "" { "root" } else { path }
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

// --- Overlay component ---

module Overlay = {
  @react.component
  let make = () => {
    if !isDebugEnabled {
      React.null
    } else {
      let (_, forceUpdate) = React.useState(() => 0)

      React.useEffect0(() => {
        let unsubscribe = subscribe(() => forceUpdate(n => n + 1))
        Some(unsubscribe)
      })

      let snapshot = getSnapshot()

      let (selectedId, setSelectedId) = React.useState(() => None)
      let (collapsed, setCollapsed) = React.useState(() => false)
      let (filter, setFilter) = React.useState(() => "")

      let instances = Dict.toArray(snapshot)

      React.useEffect1(() => {
        switch (selectedId, instances->Array.at(0)) {
        | (None, Some((key, _))) => setSelectedId(_ => Some(key))
        | _ => ()
        }
        None
      }, [instances])

      let hiddenTags =
        filter
        ->String.split(",")
        ->Array.map(s => s->String.trim->String.toLowerCase)
        ->Array.filter(s => s !== "")

      if collapsed {
        <button className="debug-toggle" onClick={_ => setCollapsed(_ => false)}>
          {React.string("Debug")}
        </button>
      } else {
        <div className="debug-overlay">
          <div className="debug-header">
            <span className="debug-title"> {React.string("Time Travel Debugger")} </span>
            <button className="debug-close" onClick={_ => setCollapsed(_ => true)}>
              {React.string("_")}
            </button>
          </div>
          {if Array.length(instances) > 1 {
            <div className="debug-tabs">
              {React.array(
                instances->Array.map(((key, data)) => {
                  let isActive = selectedId == Some(key)
                  <button
                    key
                    className={isActive ? "debug-tab debug-tab--active" : "debug-tab"}
                    onClick={_ => setSelectedId(_ => Some(key))}>
                    {React.string("Instance " ++ Int.toString(data.id))}
                  </button>
                }),
              )}
            </div>
          } else {
            React.null
          }}
          {switch selectedId->Option.flatMap(id => Dict.get(snapshot, id)) {
          | None =>
            <div className="debug-body"> {React.string("No instances registered")} </div>
          | Some(data) =>
            let filteredHistory =
              data.history->Array.filter(entry => {
                let tag = actionTag(entry.actionStr)->String.toLowerCase
                !(hiddenTags->Array.some(h => tag === h))
              })

            let diffs = computeDiff(data.previousModelJson, data.currentModelJson)

            <div className="debug-body">
              <div className="debug-section">
                <div className="debug-section-title"> {React.string("Current Model")} </div>
                <pre className="debug-model"> {React.string(data.currentModelJson)} </pre>
              </div>
              {if Array.length(diffs) > 0 {
                <div className="debug-section">
                  <div className="debug-section-title"> {React.string("Diff")} </div>
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
                </div>
              } else {
                React.null
              }}
              <div className="debug-section debug-section--history">
                <div className="debug-section-header">
                  <div className="debug-section-title">
                    {React.string(
                      "History (" ++
                      Int.toString(Array.length(filteredHistory)) ++
                      "/" ++
                      Int.toString(Array.length(data.history)) ++
                      ")",
                    )}
                  </div>
                  <button
                    className="debug-jump-btn"
                    onClick={_ => {
                      data.history = []
                      notifyListeners()
                    }}>
                    {React.string("Clear")}
                  </button>
                </div>
                <input
                  className="debug-filter"
                  type_="text"
                  placeholder="Hide: Ticked, DurationChanged, ..."
                  value={filter}
                  onChange={e => {
                    let v = ReactEvent.Form.target(e)["value"]
                    setFilter(_ => v)
                  }}
                />
                <div className="debug-history">
                  {React.array(
                    filteredHistory
                    ->Array.mapWithIndex((entry, originalIdx) => (entry, originalIdx + 1))
                    ->Array.toReversed
                    ->Array.mapWithIndex(((entry, num), idx) => {
                      <div
                        key={Int.toString(idx)}
                        className="debug-history-entry"
                        onClick={_ => data.jumpToModel(entry.rawModel)}>
                        <span className="debug-entry-num">
                          {React.string(Int.toString(num))}
                        </span>
                        <span className="debug-action-name">
                          {React.string(actionTag(entry.actionStr))}
                        </span>
                      </div>
                    }),
                  )}
                </div>
              </div>
            </div>
          }}
        </div>
      }
    }
  }
}
