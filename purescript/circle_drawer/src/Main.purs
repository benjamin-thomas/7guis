module Main (main) where

import Prelude

import Data.Int (decimal, toNumber)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Number (sqrt)
import Data.Number as Number
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Halogen.VDom.Driver (runUI)
import MouseExtra as MouseExtra
import Partial.Unsafe (unsafePartial)
import Web.Event.Event as WE
import Web.UIEvent.MouseEvent (MouseEvent)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type Circle =
  { x :: Number
  , y :: Number
  , r :: Number
  }

type CircleClickData =
  { circleId :: Int
  , circle :: Circle
  }

type State =
  { mouseX :: Int
  , mouseY :: Int
  , circles :: Map Int Circle
  , showAdjustDialog :: Maybe CircleClickData
  , defaultCircleRadius :: Number
  }

data Action
  = MouseMoved MouseEvent
  | DrawingAreaClicked
  | MouseRightBtnClicked CircleClickData WE.Event
  | CircleRadiusChanged { circleId :: Int } String
  | MouseReleased

onContextMenu :: forall r i. (WE.Event -> i) -> HP.IProp r i
onContextMenu = HE.handler (WE.EventType "contextmenu")

distFromCenter :: Tuple Number Number -> Tuple Number Number -> Number
distFromCenter (Tuple x1 y1) (Tuple x2 y2) =
  let
    dx = x2 - x1
    dy = y2 - y1
  in
    sqrt $ dx * dx + dy * dy

component :: forall query output m. MonadEffect m => H.Component query Unit output m
component =
  H.mkComponent
    { initialState
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    , render
    }
  where
  initialState :: Unit -> State
  initialState _ =
    let
      defaultCircleRadius = 20.0
    in
      { mouseX: 0
      , mouseY: 0
      , circles:
          Map.fromFoldable
            [ 1 /\ { x: 20.0, y: 20.0, r: defaultCircleRadius }
            , 2 /\ { x: 380.0, y: 380.0, r: defaultCircleRadius }
            ]
      , showAdjustDialog: Nothing
      , defaultCircleRadius
      }

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    MouseMoved evt ->
      H.modify_ \state -> state
        { mouseX = MouseExtra.offsetX evt
        , mouseY = MouseExtra.offsetY evt
        }

    DrawingAreaClicked ->
      H.modify_ \state ->
        let
          nextId :: Int
          nextId = fromMaybe 1 $ Map.findMax state.circles <#> _.key <#> ((+) 1)

          newCircles :: Map Int Circle
          newCircles = state.circles # Map.insert nextId
            { x: toNumber state.mouseX
            , y: toNumber state.mouseY
            , r: state.defaultCircleRadius
            }
        in
          state
            { circles = newCircles
            }

    MouseRightBtnClicked circleData evt -> do
      H.liftEffect $ WE.preventDefault evt
      H.modify_ _ { showAdjustDialog = Just circleData }

    CircleRadiusChanged { circleId } str ->
      H.modify_ \state ->
        case state.circles # Map.lookup circleId of
          Nothing -> state
          Just circle ->
            let
              r = unsafePartial fromJust $ Number.fromString str
              newCircle = circle { r = r }
            in
              state
                { circles = state.circles # Map.insert circleId newCircle
                }

    MouseReleased ->
      H.modify_ _ { showAdjustDialog = Nothing }

  render :: State -> H.ComponentHTML Action () m
  render state =
    let
      circleColor circle =
        let
          dist = distFromCenter (circle.x /\ circle.y) (toNumber state.mouseX /\ toNumber state.mouseY)
        in
          SA.Named $ if dist < circle.r then "lightgrey" else "white"

      mkCircle (circleId /\ circle) =
        SE.circle
          [ SA.fill $ circleColor circle
          , SA.stroke (SA.Named "black")
          , SA.cx circle.x
          , SA.cy circle.y
          , SA.r circle.r
          , onContextMenu $ MouseRightBtnClicked { circleId, circle }
          ]
    in
      HH.div [ HP.id "container" ]
        [ HH.h1_ [ HH.text "Circle drawer" ]
        , HH.div [ HP.id "buttons" ]
            [ HH.button_ [ HH.text "Undo" ]
            , HH.button_ [ HH.text "Redo" ]
            ]

        , SE.svg
            [ HE.onMouseMove MouseMoved
            , HE.onClick $ const DrawingAreaClicked
            , HP.style "background:white"
            ]
            (map mkCircle $ Map.toUnfoldable state.circles)

        , case state.showAdjustDialog of
            Nothing -> HH.text ""
            Just ({ circleId, circle }) ->
              let
                conv :: Number -> String
                conv n = Int.fromNumber n <#> Int.toStringAs decimal # fromMaybe "?"
                coords = "(" <> conv circle.x <> ", " <> conv circle.y <> ")"
              in
                HH.div [ HP.id "adjust-dialog" ]
                  [ HH.div_
                      [ HH.text $ "Adjust diameter of circle at " <> coords <> "."
                      ]
                  , HH.input
                      [ HP.type_ HP.InputRange
                      , HE.onValueInput $ CircleRadiusChanged { circleId }
                      , HE.onMouseUp $ const MouseReleased
                      , HP.min 5.0
                      , HP.max 100.0
                      , HP.value
                          $ Int.toStringAs decimal
                          $ unsafePartial fromJust
                          $ Int.fromNumber circle.r

                      ]
                  ]

        , HH.code_ [ HH.text $ show state ]
        ]