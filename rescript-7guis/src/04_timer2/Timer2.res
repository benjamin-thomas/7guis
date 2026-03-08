%%raw("import './timer2.css'")

type timerState =
  | Stopped
  | Running({elapsedMs: float})

type serverStatus = Idle | Saved | Failed(string)

type model = {
  timerState: timerState,
  durationMs: int,
  serverStatus: serverStatus,
}

type msg =
  | Ticked(float)
  | DurationChanged(int)
  | StartBtnClicked
  | StopBtnClicked
  | GotServerNotification(result<unit, string>)
  | DismissNotification

type effect =
  | StartTimer
  | StopTimer
  | NotifyServer(float)
  | DismissNotificationAfter(int)
  | ConsoleLog(string)

let init: model = {
  timerState: Stopped,
  durationMs: 15000,
  serverStatus: Idle,
}

let update = (model, msg) => {
  switch msg {
  | Ticked(deltaMs) =>
    switch model.timerState {
    | Running({elapsedMs}) => {
        let newElapsedMs = elapsedMs + deltaMs
        if newElapsedMs >= model.durationMs->Int.toFloat {
          ({...model, timerState: Stopped}, [StopTimer, NotifyServer(newElapsedMs)])
        } else {
          ({...model, timerState: Running({elapsedMs: newElapsedMs})}, [])
        }
      }
    | Stopped => (model, [])
    }
  | DurationChanged(ms) => ({...model, durationMs: ms}, [])
  | StartBtnClicked => (
      {...model, timerState: Running({elapsedMs: 0.0})},
      [StartTimer, ConsoleLog("Timer started")],
    )
  | StopBtnClicked => ({...model, timerState: Stopped}, [StopTimer, ConsoleLog("Timer stopped")])
  | GotServerNotification(Ok()) => (
      {...model, serverStatus: Saved},
      [DismissNotificationAfter(1000)],
    )
  | GotServerNotification(Error(err)) => ({...model, serverStatus: Failed(err)}, [])
  | DismissNotification => ({...model, serverStatus: Idle}, [])
  }
}

@react.component
let make = () => {
  let intervalIdRef = React.useRef(None)

  let runEffect = (dispatch, effect) =>
    switch effect {
    | StartTimer => {
        let prevTimeRef = ref(Date.now())
        let id = setInterval(() => {
          let now = Date.now()
          dispatch(Ticked(now - prevTimeRef.contents))
          prevTimeRef.contents = now
        }, 100)
        intervalIdRef.current = Some(id)
      }
    | StopTimer => {
        intervalIdRef.current->Option.forEach(id => clearInterval(id))
        intervalIdRef.current = None
      }
    | NotifyServer(_elapsedMs) =>
      Promise.make((resolve, _reject) => {
        setTimeout(resolve, 500)->ignore
      })
      ->Promise.thenResolve(() => {
        dispatch(GotServerNotification(Ok()))
      })
      ->Promise.catch(_err => {
        dispatch(GotServerNotification(Error("Server error")))
        Promise.resolve()
      })
      ->ignore
    | DismissNotificationAfter(ms) => setTimeout(() => dispatch(DismissNotification), ms)->ignore
    | ConsoleLog(msg) => Console.log2("[Timer2]", msg)
    }

  let (model, dispatch) = Effect.useReducer(init, update, runEffect)

  let elapsedMs = switch model.timerState {
  | Running({elapsedMs}) => elapsedMs
  | Stopped => 0.0
  }
  let durationMs = model.durationMs->Int.toFloat
  let cappedElapsedMs = Math.min(elapsedMs, durationMs)
  let progressRatio = cappedElapsedMs / durationMs
  let elapsedSecondsStr = (cappedElapsedMs / 1000.0)->Float.toFixed(~digits=1)

  <div className="task-container">
    <h1 className="task-title"> {React.string("Timer 2")} </h1>
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
          value={Int.toString(model.durationMs)}
          onChange={e => {
            let ms =
              ReactEvent.Form.target(e)["value"]
              ->Option.flatMap(v => Int.fromString(v))
              ->Option.getOrThrow(~message="bad range input value")
            dispatch(DurationChanged(ms))
          }}
        />
      </div>
      {switch model.timerState {
      | Stopped =>
        <button className="button" onClick={_ => dispatch(StartBtnClicked)}>
          {React.string("Start")}
        </button>
      | Running(_) =>
        <button className="button" onClick={_ => dispatch(StopBtnClicked)}>
          {React.string("Stop")}
        </button>
      }}
    </div>
    {switch model.serverStatus {
    | Idle => React.null
    | Saved => <div className="timer-toast"> {React.string("Server notified!")} </div>
    | Failed(err) => <div className="timer-toast timer-toast--error"> {React.string(err)} </div>
    }}
  </div>
}
