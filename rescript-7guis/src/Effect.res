let useReducer = (init, update, runEffect) => {
  let effectsRef = React.useRef([])
  let (effectCount, setEffectCount) = React.useState(() => 0)

  let (model, dispatch) = React.useReducer((model, msg) => {
    let (newModel, effects) = update(model, msg)
    if effects->Array.length > 0 {
      effectsRef.current = Array.concat(effectsRef.current, effects)
      setEffectCount(n => n + 1)
    }
    newModel
  }, init)

  React.useEffect1(() => {
    let effects = effectsRef.current
    effectsRef.current = []
    effects->Array.forEach(effect => runEffect(dispatch, effect))
    None
  }, [effectCount])

  (model, dispatch)
}
