import { useReducer as reactUseReducer, useEffect, useRef, useCallback } from 'react'
import { assertNever } from './utils'

type InternalMsg<Msg> =
  | { kind: "UserMsg"; msg: Msg }
  | { kind: "ClearProcessedEffects"; n: number }

const UserMsg = <Msg,>(msg: Msg): InternalMsg<Msg> => ({ kind: "UserMsg", msg })
const ClearProcessedEffects = <Msg,>(n: number): InternalMsg<Msg> => ({ kind: "ClearProcessedEffects", n })

type State<Model, Eff> = {
  model: Model
  effectsQueue: Eff[]
}

export const useReducer = <Model, Msg, Eff>(
  init: Model,
  update: (model: Model, msg: Msg) => [Model, Eff[]],
  runEffect: (dispatch: (msg: Msg) => void, effect: Eff) => void,
): [Model, (msg: Msg) => void] => {
  const [state, dispatch] = reactUseReducer(
    (state: State<Model, Eff>, internalMsg: InternalMsg<Msg>): State<Model, Eff> => {
      switch (internalMsg.kind) {
        case "UserMsg": {
          const [newModel, effects] = update(state.model, internalMsg.msg)
          if (effects.length > 0) {
            return { model: newModel, effectsQueue: [...state.effectsQueue, ...effects] }
          } else {
            return { model: newModel, effectsQueue: state.effectsQueue }
          }
        }
        case "ClearProcessedEffects":
          return { ...state, effectsQueue: state.effectsQueue.slice(internalMsg.n) }
        default:
          return assertNever(internalMsg)
      }
    },
    { model: init, effectsQueue: [] as Eff[] },
  )

  const effectsRunCount = useRef(0)

  useEffect(() => {
    const queueLength = state.effectsQueue.length
    if (queueLength > effectsRunCount.current) {
      const newEffects = state.effectsQueue.slice(effectsRunCount.current, queueLength)

      effectsRunCount.current = queueLength
      newEffects.forEach(effect => runEffect(msg => dispatch(UserMsg(msg)), effect))

      effectsRunCount.current = 0
      dispatch(ClearProcessedEffects(queueLength))
    }
  }, [state.effectsQueue, runEffect])

  const userDispatch = useCallback(
    (msg: Msg) => dispatch(UserMsg(msg)),
    [dispatch],
  )

  return [state.model, userDispatch]
}
