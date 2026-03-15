import { useReducer as reactUseReducer, useRef, useEffect, useCallback } from 'react'
import * as TimeTravelDebugger from './TimeTravelDebugger'

type DebugMsg<Msg> =
  | { kind: "AppMsg"; msg: Msg }
  | { kind: "DebugSetModel"; rawModel: unknown }

export const useReducer = <Model, Msg>(
  reducer: (model: Model, msg: Msg) => Model,
  init: Model,
): [Model, (msg: Msg) => void] => {
  const isMounted = useRef(true)

  useEffect(() => {
    isMounted.current = true
    return () => {
      isMounted.current = false
    }
  }, [])

  const [model, rawDispatch] = reactUseReducer(
    (model: Model, msg: DebugMsg<Msg>): Model => {
      switch (msg.kind) {
        case "AppMsg":
          return reducer(model, msg.msg)
        case "DebugSetModel":
          return msg.rawModel as Model
      }
    },
    init,
  )

  const jumpToModel = (rawModel: unknown) => rawDispatch({ kind: "DebugSetModel", rawModel })
  TimeTravelDebugger.useInstance(model, jumpToModel)

  const dispatch = useCallback((msg: Msg) => {
    if (isMounted.current) {
      TimeTravelDebugger.reportAction(JSON.stringify(msg) ?? "?")
      rawDispatch({ kind: "AppMsg", msg })
    }
  }, [rawDispatch])

  return [model, dispatch]
}
