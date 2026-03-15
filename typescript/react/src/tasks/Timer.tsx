import { useRef } from 'react'
import { assertNever } from '../utils'
import * as EffectDebug from '../EffectDebug'
import './timer.css'

// --- ADTs ---

type TimerState =
  | { kind: "Stopped" }
  | { kind: "Running"; elapsedMs: number }

const Stopped: TimerState = { kind: "Stopped" }
const Running = (elapsedMs: number): TimerState => ({ kind: "Running", elapsedMs })

type ServerResult = { kind: "Ok" } | { kind: "Error"; error: string }
const Ok: ServerResult = { kind: "Ok" }
const ServerError = (error: string): ServerResult => ({ kind: "Error", error })

type ServerStatus =
  | { kind: "Idle" }
  | { kind: "Saved" }
  | { kind: "Failed"; error: string }

const Idle: ServerStatus = { kind: "Idle" }
const Saved: ServerStatus = { kind: "Saved" }
const Failed = (error: string): ServerStatus => ({ kind: "Failed", error })

type Model = {
  timerState: TimerState
  durationMs: number
  serverStatus: ServerStatus
}

type Msg =
  | { kind: "Ticked"; deltaMs: number }
  | { kind: "DurationChanged"; ms: number }
  | { kind: "StartBtnClicked" }
  | { kind: "StopBtnClicked" }
  | { kind: "GotServerNotification"; result: ServerResult }
  | { kind: "DismissNotification" }

const Ticked = (deltaMs: number): Msg => ({ kind: "Ticked", deltaMs })
const DurationChanged = (ms: number): Msg => ({ kind: "DurationChanged", ms })
const StartBtnClicked: Msg = { kind: "StartBtnClicked" }
const StopBtnClicked: Msg = { kind: "StopBtnClicked" }
const GotServerNotification = (result: ServerResult): Msg => ({ kind: "GotServerNotification", result })
const DismissNotification: Msg = { kind: "DismissNotification" }

type Effect =
  | { kind: "StartTimer" }
  | { kind: "StopTimer" }
  | { kind: "NotifyServer"; elapsedMs: number }
  | { kind: "DismissNotificationAfter"; ms: number }
  | { kind: "ConsoleLog"; message: string }

const StartTimer: Effect = { kind: "StartTimer" }
const StopTimer: Effect = { kind: "StopTimer" }
const NotifyServer = (elapsedMs: number): Effect => ({ kind: "NotifyServer", elapsedMs })
const DismissNotificationAfter = (ms: number): Effect => ({ kind: "DismissNotificationAfter", ms })
const ConsoleLog = (message: string): Effect => ({ kind: "ConsoleLog", message })

// --- Init / Update ---

const init: Model = {
  timerState: Stopped,
  durationMs: 15000,
  serverStatus: Idle,
}

const update = (model: Model, msg: Msg): [Model, Effect[]] => {
  switch (msg.kind) {
    case "Ticked": {
      switch (model.timerState.kind) {
        case "Running": {
          const newElapsedMs = model.timerState.elapsedMs + msg.deltaMs
          if (newElapsedMs >= model.durationMs) {
            return [
              { ...model, timerState: Stopped },
              [StopTimer, NotifyServer(newElapsedMs)],
            ]
          } else {
            return [
              { ...model, timerState: Running(newElapsedMs) },
              [],
            ]
          }
        }
        case "Stopped":
          return [model, []]
        default:
          return assertNever(model.timerState)
      }
    }
    case "DurationChanged":
      return [{ ...model, durationMs: msg.ms }, []]
    case "StartBtnClicked":
      return [
        { ...model, timerState: Running(0) },
        [StartTimer, ConsoleLog("Timer started")],
      ]
    case "StopBtnClicked":
      return [
        { ...model, timerState: Stopped },
        [StopTimer, ConsoleLog("Timer stopped")],
      ]
    case "GotServerNotification": {
      switch (msg.result.kind) {
        case "Ok":
          return [
            { ...model, serverStatus: Saved },
            [DismissNotificationAfter(1000)],
          ]
        case "Error":
          return [
            { ...model, serverStatus: Failed(msg.result.error) },
            [],
          ]
        default:
          return assertNever(msg.result)
      }
    }
    case "DismissNotification":
      return [{ ...model, serverStatus: Idle }, []]
    default:
      return assertNever(msg)
  }
}

// --- Component ---

export const Timer = () => {
  const intervalIdRef = useRef<ReturnType<typeof setInterval> | null>(null)

  const runEffect = (dispatch: (msg: Msg) => void, effect: Effect) => {
    switch (effect.kind) {
      case "StartTimer": {
        let prevTime = Date.now()
        const id = setInterval(() => {
          const now = Date.now()
          dispatch(Ticked(now - prevTime))
          prevTime = now
        }, 100)
        intervalIdRef.current = id
        break
      }
      case "StopTimer":
        if (intervalIdRef.current !== null) {
          clearInterval(intervalIdRef.current)
          intervalIdRef.current = null
        }
        break
      case "NotifyServer":
        new Promise<void>(resolve => setTimeout(resolve, 500))
          .then(() => dispatch(GotServerNotification(Ok)))
          .catch(() => dispatch(GotServerNotification(ServerError("Server error"))))
        break
      case "DismissNotificationAfter":
        setTimeout(() => dispatch(DismissNotification), effect.ms)
        break
      case "ConsoleLog":
        console.log("[Timer]", effect.message)
        break
      default:
        assertNever(effect)
    }
  }

  const [model, dispatch] = EffectDebug.useReducer(init, update, runEffect)

  const elapsedMs = model.timerState.kind === "Running" ? model.timerState.elapsedMs : 0
  const durationMs = model.durationMs
  const cappedElapsedMs = Math.min(elapsedMs, durationMs)
  const progressRatio = cappedElapsedMs / durationMs
  const elapsedSecondsStr = (cappedElapsedMs / 1000).toFixed(1)

  const serverToast = (() => {
    switch (model.serverStatus.kind) {
      case "Idle": return null
      case "Saved": return <div className="timer-toast">Server notified!</div>
      case "Failed": return <div className="timer-toast timer-toast--error">{model.serverStatus.error}</div>
      default: return assertNever(model.serverStatus)
    }
  })()

  return (
    <div className="task-container">
      <h1 className="task-title">Timer</h1>
      <div className="card timer">
        <div className="timer-row">
          <label className="timer-label">Elapsed Time:</label>
          <progress className="timer-progress" value={progressRatio} max={1} />
        </div>
        <div className="timer-row">
          <span className="timer-elapsed">{elapsedSecondsStr}s</span>
        </div>
        <div className="timer-row">
          <label className="timer-label">Duration:</label>
          <input
            type="range"
            className="timer-slider"
            min={1000}
            max={30000}
            value={model.durationMs}
            onChange={e => {
              const ms = parseInt(e.target.value, 10)
              if (!isNaN(ms)) dispatch(DurationChanged(ms))
            }}
          />
        </div>
        {model.timerState.kind === "Stopped" ? (
          <button className="button" onClick={() => dispatch(StartBtnClicked)}>
            Start
          </button>
        ) : (
          <button className="button" onClick={() => dispatch(StopBtnClicked)}>
            Stop
          </button>
        )}
      </div>
      {serverToast}
    </div>
  )
}
