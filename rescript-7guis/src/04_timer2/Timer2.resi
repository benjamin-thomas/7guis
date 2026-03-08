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

let update: (model, msg) => (model, array<effect>)

@react.component
let make: unit => React.element
