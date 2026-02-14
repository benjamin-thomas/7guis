%%raw("import './timer.css'")

module Layout = {
  @react.component
  let make = (~children) =>
    <div className="task-container">
      <h1 className="task-title"> {React.string("Timer")} </h1>
      <div className="card timer"> children </div>
    </div>
}

module TimerReady = {
  type state = {
    startedAt: float,
    now: float,
    durationMs: int,
  }

  let init = (~now) => {startedAt: now, now, durationMs: 15000}

  type action =
    | Ticked(float)
    | DurationChanged(int)
    | ResetBtnClicked

  let reducer = (state, action) =>
    switch action {
    | Ticked(now) => {...state, now}
    | DurationChanged(ms) => {...state, durationMs: ms}
    | ResetBtnClicked => {...state, startedAt: state.now}
    }

  module Derived = {
    let elapsedMs = (~startedAt: float, ~now: float) => now -. startedAt
  }

  module Debug = {
    // VITE_DEBUG=1 npm run dev
    let isEnabled: bool = %raw("import.meta.env.VITE_DEBUG === '1'")

    @react.component
    let make = (~state: state) => {
      !isEnabled
        ? React.null
        : <pre style={{fontSize: "20px", lineHeight: "1.5"}}>
            {React.string({
              let startedAtStr = Float.toString(state.startedAt)
              let nowStr = Float.toString(state.now)
              let elapsedMsStr =
                Derived.elapsedMs(~startedAt=state.startedAt, ~now=state.now)->Float.toFixed(
                  ~digits=1,
                )

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
  let make = (~now: float) => {
    let (state, dispatch) = React.useReducer(reducer, init(~now))

    React.useEffect0(() => {
      let tickId = setInterval(() => {
        dispatch(Ticked(Date.now()))
      }, 100)
      Some(() => clearInterval(tickId))
    })

    let elapsedMs = Derived.elapsedMs(~startedAt=state.startedAt, ~now=state.now)
    let durationMs = state.durationMs->Int.toFloat
    let cappedElapsedMs = Math.min(elapsedMs, durationMs)
    let progressRatio = cappedElapsedMs /. durationMs
    let elapsedSecondsStr = (cappedElapsedMs /. 1000.0)->Float.toFixed(~digits=1)

    <>
      <Debug state />
      <Layout>
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
      </Layout>
    </>
  }
}

module TimerLoading = {
  @react.component
  let make = () => {
    <Layout>
      <span> {React.string("Loading...")} </span>
    </Layout>
  }
}

@react.component
let make = () => {
  let (now, setNow) = React.useState(() => None)

  React.useEffect0(() => {
    setNow(_ => Some(Date.now()))
    None
  })

  switch now {
  | None => <TimerLoading />
  | Some(now) => <TimerReady now />
  }
}
