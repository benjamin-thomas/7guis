type internalMsg<'msg> =
  | UserMsg('msg)
  | ClearProcessedEffects(int)

type state<'model, 'effect> = {
  model: 'model,
  effectsQueue: array<'effect>,
}

let useReducer = (init, update, runEffect) => {
  let (state, dispatch) = React.useReducer((state, internalMsg) => {
    switch internalMsg {
    | UserMsg(msg) =>
      let (newModel, effects) = update(state.model, msg)
      if effects->Array.length > 0 {
        {model: newModel, effectsQueue: Array.concat(state.effectsQueue, effects)}
      } else {
        {model: newModel, effectsQueue: state.effectsQueue}
      }
    | ClearProcessedEffects(n) => {
        ...state,
        effectsQueue: state.effectsQueue->Array.slice(~start=n),
      }
    }
  }, {model: init, effectsQueue: []})

  let effectsRunCount = React.useRef(0)

  React.useEffect1(() => {
    let queueLength = state.effectsQueue->Array.length
    if queueLength > effectsRunCount.current {
      let newEffects =
        state.effectsQueue->Array.slice(~start=effectsRunCount.current, ~end=queueLength)

      effectsRunCount.current = queueLength
      newEffects->Array.forEach(effect => runEffect(msg => dispatch(UserMsg(msg)), effect))

      effectsRunCount.current = 0
      dispatch(ClearProcessedEffects(queueLength))
    }
    None
  }, [state.effectsQueue])

  let userDispatch = React.useCallback1(msg => dispatch(UserMsg(msg)), [dispatch])

  (state.model, userDispatch)
}
