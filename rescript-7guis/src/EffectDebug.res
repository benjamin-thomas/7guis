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

  let pendingActionRef = React.useRef(None)

  let wrappedRunEffect = (dispatch, wrappedEffect) => {
    switch wrappedEffect {
    | Some(effect) =>
      runEffect(msg => {
        pendingActionRef.current = Some(
          JSON.stringifyAny(Obj.magic(msg))->Option.getOr("?"),
        )
        dispatch(AppMsg(msg))
      }, effect)
    | None => ()
    }
  }

  let (model, wrappedDispatch) = Effect.useReducer(init, wrappedUpdate, wrappedRunEffect)

  let jumpToModel = rawModel => wrappedDispatch(DebugSetModel(rawModel))

  TimeTravelDebugger.useObserver(~model, ~pendingActionRef, ~jumpToModel)

  let userDispatch = React.useCallback1(msg => {
    pendingActionRef.current = Some(
      JSON.stringifyAny(Obj.magic(msg))->Option.getOr("?"),
    )
    wrappedDispatch(AppMsg(msg))
  }, [wrappedDispatch])

  (model, userDispatch)
}
