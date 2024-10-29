module Main (main) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.DateTime.Instant (Instant, diff)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Now (now)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  now <- H.liftEffect now
  body <- HA.awaitBody
  runUI component now body

type Input = Instant
type State =
  { started :: Instant
  , duration :: Number
  , elapsed :: Milliseconds
  }

data Action
  = Initialize
  | Tick
  | Reset
  | DurationChanged String

derive instance Generic Action _

instance Show Action where
  show = genericShow

component :: forall query output m. MonadAff m => H.Component query Instant output m
component =
  H.mkComponent
    { initialState
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    , render
    }
  where
  initialState :: Instant -> State
  initialState now =
    { started: now
    , elapsed: Milliseconds 0.0
    , duration: 3.0
    }

  logAction :: { activate :: Boolean } -> Action -> H.HalogenM State Action () output m Unit
  logAction { activate } =
    if not activate then
      const $ pure unit
    else
      case _ of
        Tick ->
          -- log "Tick(1)"
          pure unit
        action -> do
          state <- H.get
          log $ "action" <> " : " <> show action <> " : " <> "state" <> " : " <> show state

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction action = do
    logAction { activate: false } action
    case action of
      Initialize -> do
        { emitter, listener } <- H.liftEffect HS.create
        void $ H.subscribe emitter
        void $ H.liftAff $ Aff.forkAff $ forever do
          Aff.delay $ Milliseconds 100.0
          -- log "Tick(0)"
          H.liftEffect $ HS.notify listener Tick
        pure unit
      Tick -> do
        now <- H.liftEffect now
        H.modify_ \state ->
          let
            elapsed = diff now state.started
          in
            state { elapsed = min (Milliseconds $ state.duration * 1000.0) elapsed }

      Reset -> do
        now <- H.liftEffect now
        H.modify_ _ { started = now }

      DurationChanged (str :: String) ->
        case fromString str of
          Nothing -> pure unit
          Just duration ->
            H.modify_ _ { duration = duration }

  render :: State -> H.ComponentHTML Action () m
  render state =
    let
      (Milliseconds elapsed) = state.elapsed
    in
      HH.div_
        [ -- HH.code_ [ HH.text $ show state ]
          HH.h1_ [ HH.text "Timer" ]
        , HH.div_
            [ HH.label_ [ HH.text "Elapsed time:" ]
            , HH.meter
                [ HP.min 0.0
                , HP.max $ 1000.0 * state.duration
                , HP.value elapsed
                , HP.style "width: 200px"
                ]
                []
            , HH.p_ [ HH.text $ show elapsed ]
            ]
        , HH.div_
            [ HH.label_ [ HH.text "Duration:" ]
            , HH.input
                [ HP.type_ $ HP.InputRange
                , HP.min 0.0
                , HP.max 10.0
                , HP.value $ show state.duration
                , HE.onValueInput DurationChanged
                , HP.style "width: 200px"
                ]
            , HH.span_ [ HH.text $ show state.duration ]

            ]

        , HH.button [ HE.onClick \_ -> Reset ] [ HH.text "Reset" ]
        ]