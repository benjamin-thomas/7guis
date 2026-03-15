import { useRef, useEffect, useCallback } from 'react'
import { assertNever } from './utils'
import * as Effect from './Effect'
import * as TimeTravelDebugger from './TimeTravelDebugger'

type DebugMsg<Msg> =
  | { kind: "AppMsg"; msg: Msg; epoch: number }
  | { kind: "DebugSetModel"; rawModel: unknown }

const AppMsg = <Msg,>(msg: Msg, epoch: number): DebugMsg<Msg> => ({ kind: "AppMsg", msg, epoch })
const DebugSetModel = <Msg,>(rawModel: unknown): DebugMsg<Msg> => ({ kind: "DebugSetModel", rawModel })

type WrappedEffect<Eff> =
  | { kind: "Some"; effect: Eff; epoch: number }
  | { kind: "None" }

const Some = <Eff,>(effect: Eff, epoch: number): WrappedEffect<Eff> => ({ kind: "Some", effect, epoch })

export const useReducer = <Model, Msg, Eff>(
  init: Model,
  update: (model: Model, msg: Msg) => [Model, Eff[]],
  runEffect: (dispatch: (msg: Msg) => void, effect: Eff) => void,
): [Model, (msg: Msg) => void] => {
  const isMounted = useRef(true)
  const mountEpochRef = useRef(0)

  useEffect(() => {
    mountEpochRef.current = mountEpochRef.current + 1
    const mountedEpoch = mountEpochRef.current
    isMounted.current = true
    return () => {
      if (mountEpochRef.current === mountedEpoch) {
        mountEpochRef.current = mountEpochRef.current + 1
      }
      isMounted.current = false
    }
  }, [])

  const wrappedUpdate = (model: Model, msg: DebugMsg<Msg>): [Model, WrappedEffect<Eff>[]] => {
    switch (msg.kind) {
      case "AppMsg": {
        if (msg.epoch === mountEpochRef.current) {
          const [newModel, effects] = update(model, msg.msg)
          return [newModel, effects.map(e => Some(e, msg.epoch))]
        } else {
          return [model, []]
        }
      }
      case "DebugSetModel":
        return [msg.rawModel as Model, []]
      default:
        return assertNever(msg)
    }
  }

  const wrappedRunEffect = (dispatch: (msg: DebugMsg<Msg>) => void, wrappedEffect: WrappedEffect<Eff>) => {
    switch (wrappedEffect.kind) {
      case "Some":
        runEffect(msg => {
          if (
            isMounted.current &&
            mountEpochRef.current === wrappedEffect.epoch &&
            !TimeTravelDebugger.isPaused()
          ) {
            TimeTravelDebugger.reportAction(JSON.stringify(msg) ?? "?")
            dispatch(AppMsg(msg, wrappedEffect.epoch))
          }
        }, wrappedEffect.effect)
        break
      case "None":
        break
      default:
        assertNever(wrappedEffect)
    }
  }

  const [model, wrappedDispatch] = Effect.useReducer(init, wrappedUpdate, wrappedRunEffect)

  const jumpToModel = (rawModel: unknown) => wrappedDispatch(DebugSetModel(rawModel))

  TimeTravelDebugger.useInstance(model, jumpToModel)

  const userDispatch = useCallback((msg: Msg) => {
    if (isMounted.current) {
      const epoch = mountEpochRef.current
      TimeTravelDebugger.reportAction(JSON.stringify(msg) ?? "?")
      wrappedDispatch(AppMsg(msg, epoch))
    }
  }, [wrappedDispatch])

  return [model, userDispatch]
}
