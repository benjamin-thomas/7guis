-- | Ported from rescript-7guis `test/04_timer2/Timer2Test.res`.
-- |
-- | Notice what is NOT imported: Flame, the DOM, any test renderer, any mock.
-- | These tests drive the pure `update` and assert on the exact
-- | `(model, [Instr])` it returns. The effects are DATA, so asserting that an
-- | action "fires a StopTimer and NotifyServer" is a plain value equality.
-- | That is the whole power of the decoupled-effects technique.
module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Timer (Instr(..), Model, Msg(..), ServerStatus(..), TimerState(..), init, update)

-- | Run a sequence of messages from a starting model, accumulating all effects.
run :: Model -> Array Msg -> Tuple Model (Array Instr)
run start msgs =
  foldl step (Tuple start []) msgs
  where
  step (Tuple model effects) msg =
    let
      Tuple model' newEffects = update model msg
    in
      Tuple model' (effects <> newEffects)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Timer2 state machine (effects as data)" do

    it "tick while running: accumulates elapsedMs, no effect" do
      let
        model = { timerState: Running { elapsedMs: 500.0 }, durationMs: 3000, serverStatus: Idle }
      update model (Ticked 100.0)
        `shouldEqual` Tuple (model { timerState = Running { elapsedMs: 600.0 } }) []

    it "tick reaching duration: Running => Stopped, produces StopTimer + NotifyServer" do
      let
        model = { timerState: Running { elapsedMs: 2900.0 }, durationMs: 3000, serverStatus: Idle }
      update model (Ticked 200.0)
        `shouldEqual` Tuple (model { timerState = Stopped }) [ StopTimer, NotifyServer 3100.0 ]

    it "stop button while running: Running => Stopped, produces StopTimer + log" do
      let
        model = { timerState: Running { elapsedMs: 1500.0 }, durationMs: 3000, serverStatus: Idle }
      update model StopBtnClicked
        `shouldEqual` Tuple (model { timerState = Stopped }) [ StopTimer, ConsoleLog "Timer stopped" ]

    it "duration changed: updates durationMs, no effect" do
      let model = { timerState: Stopped, durationMs: 3000, serverStatus: Idle }
      update model (DurationChanged 5000)
        `shouldEqual` Tuple (model { durationMs = 5000 }) []

    it "server response ok: status becomes Saved, schedules dismissal" do
      let model = { timerState: Stopped, durationMs: 3000, serverStatus: Idle }
      update model (GotServerNotification (Right unit))
        `shouldEqual` Tuple (model { serverStatus = Saved }) [ DismissNotificationAfter 1000 ]

    it "dismiss notification: Saved => Idle" do
      let model = { timerState: Stopped, durationMs: 3000, serverStatus: Saved }
      update model DismissNotification
        `shouldEqual` Tuple (model { serverStatus = Idle }) []

    it "server response error: status becomes Failed" do
      let model = { timerState: Stopped, durationMs: 3000, serverStatus: Idle }
      update model (GotServerNotification (Left "Network error"))
        `shouldEqual` Tuple (model { serverStatus = Failed "Network error" }) []

    it "start button: Stopped => Running, produces StartTimer + log" do
      let model = { timerState: Stopped, durationMs: 3000, serverStatus: Idle }
      update model StartBtnClicked
        `shouldEqual` Tuple (model { timerState = Running { elapsedMs: 0.0 } })
          [ StartTimer, ConsoleLog "Timer started" ]

    it "full start-tick-stop sequence" do
      run init [ StartBtnClicked, Ticked 100.0, Ticked 100.0, StopBtnClicked ]
        `shouldEqual` Tuple (init { timerState = Stopped })
          [ StartTimer, ConsoleLog "Timer started", StopTimer, ConsoleLog "Timer stopped" ]

    it "timer auto-stops and notifies server when duration reached" do
      let start = init { durationMs = 300 }
      run start [ StartBtnClicked, Ticked 100.0, Ticked 100.0, Ticked 200.0 ]
        `shouldEqual` Tuple (start { timerState = Stopped })
          [ StartTimer, ConsoleLog "Timer started", StopTimer, NotifyServer 400.0 ]
