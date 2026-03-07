// NOTE: NotifyServer est un effet async fictif. Il simule un appel HTTP pour
// démontrer le pattern "effet qui produit un message en retour" (comme Cmd en Elm).

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
    | Stopped => (model, []) // peut arriver entre dispatch(Stop) et clearInterval
    }
  | DurationChanged(ms) => ({...model, durationMs: ms}, [])
  | StartBtnClicked => ({...model, timerState: Running({elapsedMs: 0.0})}, [StartTimer])
  | StopBtnClicked => ({...model, timerState: Stopped}, [StopTimer])
  | GotServerNotification(Ok()) => ({...model, serverStatus: Saved}, [DismissNotificationAfter(1000)])
  | GotServerNotification(Error(err)) => ({...model, serverStatus: Failed(err)}, [])
  | DismissNotification => ({...model, serverStatus: Idle}, [])
  }
}

// --- Interpréteur d'effets (side effects réels) ---

let handleEffect = (effect, dispatch, intervalIdRef: React.ref<option<intervalId>>) =>
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
      setTimeout(() => resolve(), 500)->ignore
    })
    ->Promise.thenResolve(_ => dispatch(GotServerNotification(Ok())))
    ->Promise.catch(_err => {
      dispatch(GotServerNotification(Error("Server error")))
      Promise.resolve()
    })
    ->ignore
  | DismissNotificationAfter(ms) =>
    setTimeout(() => dispatch(DismissNotification), ms)->ignore
  }

// --- Composant React ---

%%raw("import './timer2.css'")

@react.component
let make = () => {
  let intervalIdRef = React.useRef(None)
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
    effects->Array.forEach(effect => handleEffect(effect, dispatch, intervalIdRef))
    None
  }, [effectCount])

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
      {switch model.serverStatus {
      | Idle => React.null
      | Saved => <div className="timer-toast"> {React.string("Server notified!")} </div>
      | Failed(err) => <div className="timer-toast timer-toast--error"> {React.string(err)} </div>
      }}
    </div>
  </div>
}
