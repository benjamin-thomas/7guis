type debugMsg<'msg> =
  | AppMsg('msg)
  | DebugSetModel(JSON.t)

let useReducer = (init, update, runEffect) => {
  let wrappedUpdate = (model, msg) => {
    switch msg {
    | AppMsg(msg) =>
      let (newModel, effects) = update(model, msg)
      (newModel, effects->Array.map(e => Some(e)))
    | DebugSetModel(rawModel) => (Obj.magic(rawModel), [])
    }
  }

  let pendingActionsRef = React.useRef([])
  let (flushCounter, setFlushCounter) = React.useState(() => 0)

  let wrappedRunEffect = (dispatch, wrappedEffect) => {
    switch wrappedEffect {
    | Some(effect) =>
      runEffect(msg => {
        pendingActionsRef.current = Array.concat(
          pendingActionsRef.current,
          [JSON.stringifyAny(Obj.magic(msg))->Option.getOr("?")],
        )
        setFlushCounter(n => n + 1)
        dispatch(AppMsg(msg))
      }, effect)
    | None => ()
    }
  }

  let (model, wrappedDispatch) = Effect.useReducer(init, wrappedUpdate, wrappedRunEffect)

  let jumpToModel = rawModel => wrappedDispatch(DebugSetModel(rawModel))

  TimeTravelDebugger.useObserver(~model, ~pendingActionsRef, ~flushCounter, ~jumpToModel)

  let userDispatch = React.useCallback1(msg => {
    pendingActionsRef.current = Array.concat(
      pendingActionsRef.current,
      [JSON.stringifyAny(Obj.magic(msg))->Option.getOr("?")],
    )
    setFlushCounter(n => n + 1)
    wrappedDispatch(AppMsg(msg))
  }, [wrappedDispatch])

  (model, userDispatch)
}
