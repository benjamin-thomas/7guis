%%raw("import './timer.css'")

module State = {
  type time = {startedAt: float, now: float}

  type timeStatus =
    | Initializing
    | Ready(time)

  type t = {
    time: timeStatus,
    durationMs: int,
  }

  let init: t = {time: Initializing, durationMs: 15000}

  module Derived = {
    let elapsedMs = (~startedAt: float, ~now: float) => now -. startedAt
  }
}

type action =
  | Ticked(float)
  | DurationChanged(int)
  | ResetBtnClicked

let reducer = (state: State.t, action: action) => {
  switch action {
  | Ticked(now) =>
    switch state.time {
    | Initializing => {...state, time: Ready({startedAt: now, now})}
    | Ready(time) => {...state, time: Ready({...time, now})}
    }
  | DurationChanged(ms) => {...state, durationMs: ms}
  | ResetBtnClicked =>
    switch state.time {
    | Ready(time) => {...state, time: Ready({...time, startedAt: time.now})}
    | Initializing => panic("This should never happen!")
    }
  }
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
            | Initializing => ("Loading...", "Loading...", "Initializing...")
            | Ready({startedAt, now}) => (
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
    dispatch(Ticked(Date.now()))
    let tickId = setInterval(() => {
      dispatch(Ticked(Date.now()))
    }, 100)
    Some(() => clearInterval(tickId))
  })

  let loadingElem =
    <div className="card timer">
      <span> {React.string("Loading...")} </span>
    </div>

  let loadedElem = (time: State.time) => {
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
      {switch state.time {
      | Initializing => loadingElem
      | Ready(time) => loadedElem(time)
      }}
    </div>
  </>
}
