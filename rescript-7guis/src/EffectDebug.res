type debugMsg<'msg> =
  | AppMsg('msg, int)
  | DebugSetModel(JSON.t)

let useReducer = (init, update, runEffect) => {
  let isMounted = React.useRef(true)
  let mountEpochRef = React.useRef(0)
  React.useEffect0(() => {
    mountEpochRef.current = mountEpochRef.current + 1
    let mountedEpoch = mountEpochRef.current
    isMounted.current = true
    Some(
      () => {
        if mountEpochRef.current === mountedEpoch {
          mountEpochRef.current = mountEpochRef.current + 1
        }
        isMounted.current = false
      },
    )
  })

  let wrappedUpdate = (model, msg) => {
    switch msg {
    | AppMsg(msg, msgEpoch) =>
      if msgEpoch === mountEpochRef.current {
        let (newModel, effects) = update(model, msg)
        (newModel, effects->Array.map(e => Some((e, msgEpoch))))
      } else {
        (model, [])
      }
    | DebugSetModel(rawModel) => (Obj.magic(rawModel), [])
    }
  }

  let wrappedRunEffect = (dispatch, wrappedEffect) => {
    switch wrappedEffect {
    | Some((effect, effectEpoch)) => runEffect(msg => {
        if (
          isMounted.current &&
          mountEpochRef.current === effectEpoch &&
          !TimeTravelDebugger.isPaused()
        ) {
          TimeTravelDebugger.reportAction(JSON.stringifyAny(Obj.magic(msg))->Option.getOr("?"))
          dispatch(AppMsg(msg, effectEpoch))
        }
      }, effect)
    | None => ()
    }
  }

  let (model, wrappedDispatch) = Effect.useReducer(init, wrappedUpdate, wrappedRunEffect)

  let jumpToModel = rawModel => wrappedDispatch(DebugSetModel(rawModel))

  TimeTravelDebugger.useInstance(~model, ~jumpToModel)

  let userDispatch = React.useCallback1(msg => {
    if isMounted.current {
      let epoch = mountEpochRef.current
      TimeTravelDebugger.reportAction(JSON.stringifyAny(Obj.magic(msg))->Option.getOr("?"))
      wrappedDispatch(AppMsg(msg, epoch))
    }
  }, [wrappedDispatch])

  (model, userDispatch)
}
