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
  | StopTimer

let update = (model, msg) => {
  switch msg {
  | Ticked(deltaMs) =>
    switch model.timerState {
    | Running({elapsedMs}) => {
        let newElapsedMs = elapsedMs + deltaMs
        if newElapsedMs >= model.durationMs->Int.toFloat {
          ({...model, timerState: Stopped}, [StopTimer])
        } else {
          ({...model, timerState: Running({elapsedMs: newElapsedMs})}, [])
        }
      }
    | Stopped => panic("Ticked while Stopped: interval should have been cleared")
    }
  | StartBtnClicked => ({...model, timerState: Running({elapsedMs: 0.0})}, [StartTimer])
  }
}
