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

let notifyListeners = () =>
  listeners.contents->Array.forEach(fn => fn())

let subscribe = (listener: unit => unit): (unit => unit) => {
  listeners.contents = Array.concat(listeners.contents, [listener])
  () => {
    listeners.contents = listeners.contents->Array.filter(fn => fn !== listener)
  }
}

// --- Observer hook (called from Effect.useReducer consumers) ---

let useObserver = (~model: 'model, ~pendingActionsRef: React.ref<array<string>>, ~flushCounter: int, ~jumpToModel: JSON.t => unit) => {
  React.useEffect0(() => {
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
    Some(() => {
      instance.contents = None
      notifyListeners()
    })
  })

  React.useEffect2(() => {
    switch instance.contents {
    | None => ()
    | Some(data) =>
      let modelJson = JSON.stringifyAny(Obj.magic(model), ~space=2)->Option.getOr("{}")
      data.previousModelJson = data.currentModelJson
      data.currentModelJson = modelJson

      let actions = pendingActionsRef.current
      if Array.length(actions) > 0 {
        actions->Array.forEach(actionStr => {
          let entry: historyEntry = {
            actionStr,
            modelJson,
            rawModel: Obj.magic(model),
          }
          data.history = Array.concat(data.history, [entry])
        })
        pendingActionsRef.current = []
      }

      notifyListeners()
    }
    None
  }, (model, flushCounter))
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
  | (JSON.Array(oldArr), JSON.Array(curArr)) =>
    let maxLen = Math.Int.max(Array.length(oldArr), Array.length(curArr))
    Array.fromInitializer(~length=maxLen, i => i)
    ->Array.flatMap(i => {
      let subPath =
        path === ""
          ? "[" ++ Int.toString(i) ++ "]"
          : path ++ "[" ++ Int.toString(i) ++ "]"
      switch (oldArr->Array.get(i), curArr->Array.get(i)) {
      | (Some(a), Some(b)) => jsonDiff(~path=subPath, a, b)
      | (Some(a), None) => [{path: subPath, oldVal: JSON.stringify(a), newVal: "(removed)"}]
      | (None, Some(b)) => [{path: subPath, oldVal: "(added)", newVal: JSON.stringify(b)}]
      | (None, None) => []
      }
    })
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
    let (_, forceUpdate) = React.useState(() => 0)

    React.useEffect0(() => {
      let unsubscribe = subscribe(() => forceUpdate(n => n + 1))
      Some(unsubscribe)
    })

    let (collapsed, setCollapsed) = React.useState(() => false)
    let (filter, setFilter) = React.useState(() => "")
    let (selectedHistoryNum, setSelectedHistoryNum) = React.useState(() => None)

    let hiddenTags =
      filter
      ->String.split(",")
      ->Array.map(s => s->String.trim->String.toLowerCase)
      ->Array.filter(s => s !== "")

    switch instance.contents {
    | None => React.null
    | Some(data) =>
      if collapsed {
          <button className="debug-toggle" onClick={_ => setCollapsed(_ => false)}>
            {React.string("Debug")}
          </button>
        } else {
          let indexedHistory =
            data.history->Array.mapWithIndex((entry, idx) => (entry, idx + 1))
          let filteredHistory =
            indexedHistory->Array.filter(((entry, _)) => {
              let tag = actionTag(entry.actionStr)->String.toLowerCase
              !(hiddenTags->Array.some(h => tag === h))
            })

          let (diffs, diffLabel) = switch selectedHistoryNum {
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
          | None => (
              computeDiff(data.previousModelJson, data.currentModelJson),
              "Diff (live)",
            )
          }

          <div className="debug-overlay">
            <div className="debug-header">
              <span className="debug-title"> {React.string("Time Travel Debugger")} </span>
              <button className="debug-close" onClick={_ => setCollapsed(_ => true)}>
                {React.string("_")}
              </button>
            </div>
            <div className="debug-body">
              <div className="debug-section">
                <div className="debug-section-title"> {React.string("Current Model")} </div>
                <pre className="debug-model"> {React.string(data.currentModelJson)} </pre>
              </div>
              {if Array.length(diffs) > 0 {
                <div className="debug-section">
                  <div className="debug-section-title"> {React.string(diffLabel)} </div>
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
                      Int.toString(Array.length(indexedHistory)) ++
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
                    ->Array.toReversed
                    ->Array.mapWithIndex(((entry, num), idx) => {
                      let isSelected = selectedHistoryNum == Some(num)
                      <div
                        key={Int.toString(idx)}
                        className={isSelected
                          ? "debug-history-entry debug-history-entry--selected"
                          : "debug-history-entry"}
                        onClick={_ => {
                          if isSelected {
                            setSelectedHistoryNum(_ => None)
                          } else {
                            setSelectedHistoryNum(_ => Some(num))
                          }
                          data.jumpToModel(entry.rawModel)
                        }}>
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
          </div>
      }
    }
  }
}
