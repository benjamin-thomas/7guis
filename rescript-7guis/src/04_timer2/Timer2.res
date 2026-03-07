type timerState =
  | Stopped
  | Running({elapsedMs: float})

type model = {
  timerState: timerState,
  durationMs: int,
}

type msg =
  | Ticked(float)
  | StartBtnClicked

type effect =
  | StartTimer

let update = (model, msg) => {
  switch msg {
  | Ticked(deltaMs) =>
    switch model.timerState {
    | Running({elapsedMs}) => (
        {...model, timerState: Running({elapsedMs: elapsedMs + deltaMs})},
        [],
      )
    | Stopped => panic("Ticked while Stopped: interval should have been cleared")
    }
  | StartBtnClicked => ({...model, timerState: Running({elapsedMs: 0.0})}, [StartTimer])
  }
}
