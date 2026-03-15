open Vitest

describe("Timer2 state machine", () => {
  test("tick while running: accumulates elapsedMs, no effect", t => {
    // Arrange
    let model: Timer2.model = {
      timerState: Running({elapsedMs: 500.0}),
      durationMs: 3000,
      serverStatus: Idle,
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
      serverStatus: Idle,
    }

    // Act
    let (newModel, effects) = Timer2.update(model, Timer2.Ticked(200.0))

    // Assert
    t->expect(newModel.timerState)->Expect.toEqual(Stopped)
    t->expect(effects)->Expect.toEqual([Timer2.StopTimer, Timer2.NotifyServer(3100.0)])
  })

  test("stop button while running: Running => Stopped, produces StopTimer", t => {
    // Arrange
    let model: Timer2.model = {
      timerState: Running({elapsedMs: 1500.0}),
      durationMs: 3000,
      serverStatus: Idle,
    }

    // Act
    let (newModel, effects) = Timer2.update(model, Timer2.StopBtnClicked)

    // Assert
    t->expect(newModel.timerState)->Expect.toEqual(Stopped)
    t->expect(effects)->Expect.toEqual([Timer2.StopTimer, Timer2.ConsoleLog("Timer stopped")])
  })

  test("duration changed: updates durationMs, no effect", t => {
    // Arrange
    let model: Timer2.model = {timerState: Stopped, durationMs: 3000, serverStatus: Idle}

    // Act
    let (newModel, effects) = Timer2.update(model, Timer2.DurationChanged(5000))

    // Assert
    t->expect(newModel.durationMs)->Expect.toBe(5000)
    t->expect(effects)->Expect.toEqual([])
  })

  test("server response ok: status becomes Saved", t => {
    // Arrange
    let model: Timer2.model = {timerState: Stopped, durationMs: 3000, serverStatus: Idle}

    // Act
    let (newModel, effects) = Timer2.update(model, Timer2.GotServerNotification(Ok()))

    // Assert
    t->expect(newModel.serverStatus)->Expect.toEqual(Timer2.Saved)
    t->expect(effects)->Expect.toEqual([Timer2.DismissNotificationAfter(1000)])
  })

  test("dismiss notification: Saved => Idle", t => {
    // Arrange
    let model: Timer2.model = {timerState: Stopped, durationMs: 3000, serverStatus: Saved}

    // Act
    let (newModel, effects) = Timer2.update(model, Timer2.DismissNotification)

    // Assert
    t->expect(newModel.serverStatus)->Expect.toEqual(Timer2.Idle)
    t->expect(effects)->Expect.toEqual([])
  })

  test("server response error: status becomes Failed", t => {
    // Arrange
    let model: Timer2.model = {timerState: Stopped, durationMs: 3000, serverStatus: Idle}

    // Act
    let (newModel, effects) = Timer2.update(
      model,
      Timer2.GotServerNotification(Error("Network error")),
    )

    // Assert
    t->expect(newModel.serverStatus)->Expect.toEqual(Timer2.Failed("Network error"))
    t->expect(effects)->Expect.toEqual([])
  })

  test("start button: Stopped => Running, produces StartTimer", t => {
    // Arrange
    let model: Timer2.model = {timerState: Stopped, durationMs: 3000, serverStatus: Idle}

    // Act
    let (newModel, effects) = Timer2.update(model, Timer2.StartBtnClicked)

    // Assert
    t->expect(newModel.timerState)->Expect.toEqual(Running({elapsedMs: 0.0}))
    t->expect(effects)->Expect.toEqual([Timer2.StartTimer, Timer2.ConsoleLog("Timer started")])
  })

  test("full start-tick-stop sequence", t => {
    let (finalModel, allEffects) = [
      Timer2.StartBtnClicked,
      Ticked(100.0),
      Ticked(100.0),
      StopBtnClicked,
    ]->Array.reduce(
      (Timer2.init, []),
      ((model, effects), msg) => {
        let (newModel, newEffects) = Timer2.update(model, msg)
        (newModel, Array.concat(effects, newEffects))
      },
    )

    t->expect(finalModel.timerState)->Expect.toEqual(Stopped)
    t
    ->expect(allEffects)
    ->Expect.toEqual([
      Timer2.StartTimer,
      ConsoleLog("Timer started"),
      StopTimer,
      ConsoleLog("Timer stopped"),
    ])
  })

  test("timer auto-stops and notifies server when duration reached", t => {
    let model = {...Timer2.init, durationMs: 300}

    let (finalModel, allEffects) = [
      Timer2.StartBtnClicked,
      Ticked(100.0),
      Ticked(100.0),
      Ticked(200.0),
    ]->Array.reduce(
      (model, []),
      ((model, effects), msg) => {
        let (newModel, newEffects) = Timer2.update(model, msg)
        (newModel, Array.concat(effects, newEffects))
      },
    )

    t->expect(finalModel.timerState)->Expect.toEqual(Stopped)
    t
    ->expect(allEffects)
    ->Expect.toEqual([
      Timer2.StartTimer,
      ConsoleLog("Timer started"),
      StopTimer,
      NotifyServer(400.0),
    ])
  })
})
