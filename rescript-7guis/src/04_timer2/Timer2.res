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
  | GotServerResponse(result<unit, string>)

type effect =
  | StartTimer
  | StopTimer
  | NotifyServer(float)

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
    | Stopped => panic("Ticked while Stopped: interval should have been cleared")
    }
  | DurationChanged(ms) => ({...model, durationMs: ms}, [])
  | StartBtnClicked => ({...model, timerState: Running({elapsedMs: 0.0})}, [StartTimer])
  | StopBtnClicked => ({...model, timerState: Stopped}, [StopTimer])
  | GotServerResponse(Ok()) => ({...model, serverStatus: Saved}, [])
  | GotServerResponse(Error(err)) => ({...model, serverStatus: Failed(err)}, [])
  }
}
