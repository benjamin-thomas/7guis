-- | The whole Timer, MVU-style in one module: types, the pure `update`
-- | (effects as `Instr` data), the `interpret`er that runs those effects, and
-- | the `view`. Ported from the ReScript 7guis "Timer2" example.
-- |
-- | The point of the demo: `update` is PURE and returns effects as plain DATA
-- | (`Instr`), so it can be unit-tested by asserting on the exact `(model,
-- | [Instr])` it returns — no mocks, no DOM. `interpret` is the only place real
-- | side effects happen.
-- |
-- | (Why `Instr`? `Effect` is the natural name — this is Elm's "Effect
-- | pattern" — but it clashes with PureScript's `Effect` monad. And `Cmd`
-- | (Elm's name) means the *opaque*, runtime-managed effect: the opposite of
-- | this inspectable data. An `Instr` is just a description of something
-- | for the interpreter to carry out.)
module Timer
  ( TimerState(..)
  , ServerStatus(..)
  , Model
  , Msg(..)
  , Instr(..)
  , init
  , update
  , interpret
  , subscriptions
  , view
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (min) as Number
import Data.Number.Format (fixed, toStringWith)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Subscription (onCustomEvent')
import Flame.Types (Subscription)
import Web.Event.Event (EventType(..))

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

data TimerState
  = Stopped
  | Running { elapsedMs :: Number }

derive instance Eq TimerState
instance Show TimerState where
  show Stopped = "Stopped"
  show (Running r) = "Running { elapsedMs: " <> show r.elapsedMs <> " }"

data ServerStatus
  = Idle
  | Saved
  | Failed String

derive instance Eq ServerStatus
instance Show ServerStatus where
  show Idle = "Idle"
  show Saved = "Saved"
  show (Failed e) = "Failed " <> show e

type Model =
  { timerState :: TimerState
  , durationMs :: Int
  , serverStatus :: ServerStatus
  }

init :: Model
init =
  { timerState: Stopped
  , durationMs: 15000
  , serverStatus: Idle
  }

--------------------------------------------------------------------------------
-- Messages & effects-as-data
--------------------------------------------------------------------------------

data Msg
  = Ticked Number
  | DurationChanged Int
  | StartBtnClicked
  | StopBtnClicked
  | GotServerNotification (Either String Unit)
  | DismissNotification

derive instance Eq Msg
instance Show Msg where
  show (Ticked n) = "Ticked " <> show n
  show (DurationChanged ms) = "DurationChanged " <> show ms
  show StartBtnClicked = "StartBtnClicked"
  show StopBtnClicked = "StopBtnClicked"
  show (GotServerNotification r) = "GotServerNotification " <> show r
  show DismissNotification = "DismissNotification"

-- | An `Instr` is an **application effect represented as data** — an
-- | "instruction" describing a side effect, later carried out by the
-- | `interpret`er. Each constructor *describes* an effect but performs none
-- | itself; that's exactly what keeps `update` pure and testable.
data Instr
  = StartTimer
  | StopTimer
  | NotifyServer Number
  | DismissNotificationAfter Int
  | ConsoleLog String

derive instance Eq Instr
instance Show Instr where
  show StartTimer = "StartTimer"
  show StopTimer = "StopTimer"
  show (NotifyServer n) = "NotifyServer " <> show n
  show (DismissNotificationAfter ms) = "DismissNotificationAfter " <> show ms
  show (ConsoleLog s) = "ConsoleLog " <> show s

--------------------------------------------------------------------------------
-- Pure update: Model -> Msg -> (Model, [Instr])
--------------------------------------------------------------------------------

update :: Model -> Msg -> Tuple Model (Array Instr)
update model = case _ of
  Ticked deltaMs ->
    case model.timerState of
      Running { elapsedMs } ->
        let
          newElapsedMs = elapsedMs + deltaMs
        in
          if newElapsedMs >= toNumber model.durationMs then
            Tuple (model { timerState = Stopped })
              [ StopTimer, NotifyServer newElapsedMs ]
          else
            Tuple (model { timerState = Running { elapsedMs: newElapsedMs } }) []
      Stopped -> Tuple model []

  DurationChanged ms -> Tuple (model { durationMs = ms }) []

  StartBtnClicked ->
    Tuple (model { timerState = Running { elapsedMs: 0.0 } })
      [ StartTimer, ConsoleLog "Timer started" ]

  StopBtnClicked ->
    Tuple (model { timerState = Stopped })
      [ StopTimer, ConsoleLog "Timer stopped" ]

  GotServerNotification (Right _) ->
    Tuple (model { serverStatus = Saved })
      [ DismissNotificationAfter 1000 ]

  GotServerNotification (Left err) ->
    Tuple (model { serverStatus = Failed err }) []

  DismissNotification -> Tuple (model { serverStatus = Idle }) []

--------------------------------------------------------------------------------
-- Interpreter: the only place real side effects happen
--------------------------------------------------------------------------------

foreign import startTickingImpl :: Effect Unit
foreign import stopTickingImpl :: Effect Unit

-- | Every Instr maps to at most ONE message, so the signature is the honest
-- | `Instr -> Aff (Maybe Msg)`. The recurring tick is NOT here — it's a
-- | subscription (see `subscriptions`); `StartTimer`/`StopTimer` only switch
-- | the underlying interval source on and off.
interpret :: Instr -> Aff (Maybe Msg)
interpret = case _ of
  StartTimer -> do
    liftEffect startTickingImpl
    pure Nothing

  StopTimer -> do
    liftEffect stopTickingImpl
    pure Nothing

  -- Fake a server round-trip: wait, then report success.
  NotifyServer _elapsedMs -> do
    delay (Milliseconds 500.0)
    pure (Just (GotServerNotification (Right unit)))

  DismissNotificationAfter ms -> do
    delay (Milliseconds (toNumber ms))
    pure (Just DismissNotification)

  ConsoleLog msg -> do
    log ("[Timer2] " <> msg)
    pure Nothing

--------------------------------------------------------------------------------
-- Subscriptions: long-lived message sources (the recurring tick)
--------------------------------------------------------------------------------

-- | The tick is a *subscription*, not a command — a recurring source of
-- | messages, which is what subscriptions are for. `StartTimer` switches on an
-- | interval that fires payload-free "tick" events; this subscription turns
-- | each into a fixed `Ticked 100.0`. While stopped, no events fire, so
-- | nothing arrives — and the pure `update` ignores `Ticked` when `Stopped`
-- | anyway. (Uses only Flame's public `onCustomEvent'`; the trade-off for
-- | staying public is a fixed step rather than a measured wall-clock delta.)
subscriptions :: Array (Subscription Msg)
subscriptions = [ onCustomEvent' (EventType "tick") (Ticked 100.0) ]

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

view :: Model -> Html Msg
view model =
  HE.div [ HA.class' "task-container" ]
    [ HE.h1 [ HA.class' "task-title" ]
        [ HE.text "Flame Timer"
        , HE.span [ HA.class' "task-subtitle" ] [ HE.text "With effects as data" ]
        ]
    , HE.div [ HA.class' "card timer" ]
        [ HE.div [ HA.class' "timer-row" ]
            [ HE.label [ HA.class' "timer-label" ] [ HE.text "Elapsed Time:" ]
            , HE.progress
                [ HA.class' "timer-progress"
                , HA.value (show progressRatio)
                , HA.max "1.0"
                ]
                []
            ]
        , HE.div [ HA.class' "timer-row" ]
            [ HE.span [ HA.class' "timer-elapsed" ] [ HE.text (elapsedSecondsStr <> "s") ] ]
        , HE.div [ HA.class' "timer-row" ]
            [ HE.label [ HA.class' "timer-label" ] [ HE.text "Duration:" ]
            , HE.input
                [ HA.type' "range"
                , HA.class' "timer-slider"
                , HA.min "1000"
                , HA.max "30000"
                , HA.value (show model.durationMs)
                , HA.onInput onDurationInput
                ]
            ]
        , case model.timerState of
            Stopped ->
              HE.button [ HA.class' "button", HA.onClick StartBtnClicked ] [ HE.text "Start" ]
            Running _ ->
              HE.button [ HA.class' "button", HA.onClick StopBtnClicked ] [ HE.text "Stop" ]
        ]
    , case model.serverStatus of
        Idle -> HE.text ""
        Saved -> HE.div [ HA.class' "timer-toast" ] [ HE.text "Server notified!" ]
        Failed err -> HE.div [ HA.class' "timer-toast timer-toast--error" ] [ HE.text err ]
    ]
  where
  elapsedMs = case model.timerState of
    Running { elapsedMs: e } -> e
    Stopped -> 0.0

  durationMs = toNumber model.durationMs
  cappedElapsedMs = Number.min elapsedMs durationMs
  progressRatio = cappedElapsedMs / durationMs
  elapsedSecondsStr = toStringWith (fixed 1) (cappedElapsedMs / 1000.0)

  onDurationInput :: String -> Msg
  onDurationInput v = DurationChanged (fromMaybe model.durationMs (fromString v))
