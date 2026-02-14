%%raw("import './timer.css'")

module State = {
  type loadedTime = {startedAt: float, now: float}

  type timeState =
    | Loading
    | Loaded(loadedTime)

  type t = {
    time: timeState,
    durationMs: int,
  }

  let init: t = {time: Loading, durationMs: 15000}

  module Derived = {
    let elapsedMs = (~startedAt: float, ~now: float) => now -. startedAt
  }
}

type action =
  | Initialized({now: float})
  | Tick({now: float})
  | DurationChanged(int)
  | ResetBtnClicked

let reducerLoading = (state: State.t, action: action) =>
  switch action {
  | Initialized({now}) => {...state, time: Loaded({startedAt: now, now})}
  | _ => state
  }

let reducerLoaded = (state: State.t, time: State.loadedTime, action: action) =>
  switch action {
  | Initialized(_) => state
  | Tick({now}) => {...state, time: Loaded({...time, now})}
  | DurationChanged(ms) => {...state, durationMs: ms}
  | ResetBtnClicked => {...state, time: Loaded({...time, startedAt: time.now})}
  }

let reducer = (state: State.t, action: action) =>
  switch state.time {
  | Loading => reducerLoading(state, action)
  | Loaded(time) => reducerLoaded(state, time, action)
  }

module Debug = {
  // VITE_DEBUG=1 npm run dev
  let isEnabled: bool = %raw("import.meta.env.VITE_DEBUG === '1'")

  @react.component
  let make = (~state: State.t) => {
    !isEnabled
      ? React.null
      : <pre style={{fontSize: "20px", lineHeight: "1.5"}}>
          {React.string({
            let (startedAtStr, nowStr, elapsedMsStr) = switch state.time {
            | Loading => ("Loading...", "Loading...", "Waiting...")
            | Loaded({startedAt, now}) => (
                Float.toString(startedAt),
                Float.toString(now),
                State.Derived.elapsedMs(~startedAt, ~now)->Float.toFixed(~digits=1),
              )
            }

            `
STATE
=====
startedAt  : ${startedAtStr}
now        : ${nowStr}
durationMs : ${state.durationMs->Int.toString}

DERIVED
=======
elapsedMs : ${elapsedMsStr}
    `
          })}
        </pre>
  }
}

@react.component
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, State.init)

  React.useEffect0(() => {
    let now = Date.now()
    dispatch(Initialized({now: now}))

    let tickId = setInterval(() => {
      dispatch(Tick({now: Date.now()}))
    }, 100)
    Some(() => clearInterval(tickId))
  })

  let timerContent = switch state.time {
  | Loading =>
    <div className="card timer">
      <span> {React.string("Loading...")} </span>
    </div>
  | Loaded(time) =>
    let elapsedMs = State.Derived.elapsedMs(~startedAt=time.startedAt, ~now=time.now)
    let durationMs = state.durationMs->Int.toFloat
    let cappedElapsedMs = Math.min(elapsedMs, durationMs)
    let progressRatio = cappedElapsedMs /. durationMs
    let elapsedSecondsStr = (cappedElapsedMs /. 1000.0)->Float.toFixed(~digits=1)

    <div className="card timer">
      <div className="timer-row">
        <label className="timer-label"> {React.string("Elapsed Time:")} </label>
        <progress className="timer-progress" value={Float.toString(progressRatio)} max="1.0" />
      </div>
      <div className="timer-row">
        <span className="timer-elapsed"> {React.string(elapsedSecondsStr ++ "s")} </span>
      </div>
      <div className="timer-row">
        <label className="timer-label"> {React.string("Duration:")} </label>
        <input
          type_="range"
          className="timer-slider"
          min="1000"
          max="30000"
          value={Int.toString(state.durationMs)}
          onChange={e => {
            let durationMs: int =
              ReactEvent.Form.target(e)["value"]
              ->Option.flatMap(v => Int.fromString(v))
              ->Option.getOrThrow(~message="ABNORMAL: bad range input value")
            dispatch(DurationChanged(durationMs))
          }}
        />
      </div>
      <button className="button" onClick={_ => dispatch(ResetBtnClicked)}>
        {React.string("Reset")}
      </button>
    </div>
  }

  <>
    <Debug state />
    <div className="task-container">
      <h1 className="task-title"> {React.string("Timer")} </h1>
      {timerContent}
    </div>
  </>
}
