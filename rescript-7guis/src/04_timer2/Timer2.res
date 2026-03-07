type timerState =
  | Stopped
  | Running({elapsedMs: float})

type model = {
  timerState: timerState,
  durationMs: int,
}

type msg =
  | StartBtnClicked

type effect =
  | StartTimer

let update = (model, msg) => {
  switch msg {
  | StartBtnClicked => ({...model, timerState: Running({elapsedMs: 0.0})}, [StartTimer])
  }
}
