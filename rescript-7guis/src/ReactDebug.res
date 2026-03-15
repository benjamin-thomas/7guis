type debugMsg<'msg> =
  | AppMsg('msg)
  | DebugSetModel(JSON.t)

let useReducer = (reducer, init) => {
  let isMounted = React.useRef(true)
  React.useEffect0(() => {
    isMounted.current = true
    Some(
      () => {
        isMounted.current = false
      },
    )
  })

  let (model, rawDispatch) = React.useReducer((model, msg) => {
    switch msg {
    | AppMsg(action) => reducer(model, action)
    | DebugSetModel(raw) => Obj.magic(raw)
    }
  }, init)

  let jumpToModel = raw => rawDispatch(DebugSetModel(raw))
  TimeTravelDebugger.useInstance(~model, ~jumpToModel)

  let dispatch = React.useCallback1(action => {
    if isMounted.current {
      TimeTravelDebugger.reportAction(JSON.stringifyAny(Obj.magic(action))->Option.getOr("?"))
      rawDispatch(AppMsg(action))
    }
  }, [rawDispatch])

  (model, dispatch)
}
