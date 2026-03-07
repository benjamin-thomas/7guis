open Vitest

describe("Timer2 state machine", () => {
  test("start button: Stopped => Running, produces StartTimer", t => {
    // Arrange
    let model: Timer2.model = {timerState: Stopped, durationMs: 3000}

    // Act
    let (newModel, effects) = Timer2.update(model, Timer2.StartBtnClicked)

    // Assert
    t->expect(newModel.timerState)->Expect.toEqual(Running({elapsedMs: 0.0}))
    t->expect(effects)->Expect.toEqual([Timer2.StartTimer])
  })
})
