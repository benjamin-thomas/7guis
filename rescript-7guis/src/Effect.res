type state<'model, 'effect> = {
  model: 'model,
  effectsQueue: array<'effect>,
}

let useReducer = (init, update, runEffect) => {
  let (state, dispatch) = React.useReducer((state, msg) => {
    let (newModel, effects) = update(state.model, msg)
    {
      model: newModel,
      effectsQueue: Array.concat(state.effectsQueue, effects),
    }
  }, {model: init, effectsQueue: []})

  let effectsRunCount = React.useRef(0)

  React.useEffect1(() => {
    let queueLength = state.effectsQueue->Array.length
    if queueLength > effectsRunCount.current {
      let newEffects =
        state.effectsQueue->Array.slice(~start=effectsRunCount.current, ~end=queueLength)
      
      effectsRunCount.current = queueLength
      newEffects->Array.forEach(effect => runEffect(dispatch, effect))
    }
    None
  }, [state.effectsQueue])

  (state.model, dispatch)
}
