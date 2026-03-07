open Vitest

describe("Timer2 state machine", () => {
  test("tick while running: accumulates elapsedMs, no effect", t => {
    // Arrange
    let model: Timer2.model = {
      timerState: Running({elapsedMs: 500.0}),
      durationMs: 3000,
    }

    // Act
    let (newModel, effects) = Timer2.update(model, Timer2.Ticked(100.0))

    // Assert
    t->expect(newModel.timerState)->Expect.toEqual(Running({elapsedMs: 600.0}))
    t->expect(effects)->Expect.toEqual([])
  })

  test("tick reaching duration: Running => Stopped, produces StopTimer", t => {
    // Arrange
    let model: Timer2.model = {
      timerState: Running({elapsedMs: 2900.0}),
      durationMs: 3000,
    }

    // Act
    let (newModel, effects) = Timer2.update(model, Timer2.Ticked(200.0))

    // Assert
    t->expect(newModel.timerState)->Expect.toEqual(Stopped)
    t->expect(effects)->Expect.toEqual([Timer2.StopTimer])
  })

  test("stop button while running: Running => Stopped, produces StopTimer", t => {
    // Arrange
    let model: Timer2.model = {
      timerState: Running({elapsedMs: 1500.0}),
      durationMs: 3000,
    }

    // Act
    let (newModel, effects) = Timer2.update(model, Timer2.StopBtnClicked)

    // Assert
    t->expect(newModel.timerState)->Expect.toEqual(Stopped)
    t->expect(effects)->Expect.toEqual([Timer2.StopTimer])
  })

  test("duration changed: updates durationMs, no effect", t => {
    // Arrange
    let model: Timer2.model = {timerState: Stopped, durationMs: 3000}

    // Act
    let (newModel, effects) = Timer2.update(model, Timer2.DurationChanged(5000))

    // Assert
    t->expect(newModel.durationMs)->Expect.toBe(5000)
    t->expect(effects)->Expect.toEqual([])
  })

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
