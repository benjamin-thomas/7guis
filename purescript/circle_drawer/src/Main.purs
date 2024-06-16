module Main (main) where

import Prelude

import Data.Array (snoc)
import Data.Int (toNumber)
import Data.Number (sqrt)
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
import Web.Event.Event as WE
import Web.UIEvent.MouseEvent (MouseEvent)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type Circle =
  { cx :: Number
  , cy :: Number
  , r :: Number
  }

type State =
  { mouseX :: Int
  , mouseY :: Int
  , circles :: Array Circle
  , showAdjustDialog :: Boolean
  , defaultCircleRadius :: Number
  }

data Action
  = MouseMoved MouseEvent
  | DrawingAreaClicked
  | MouseRightBtnClicked WE.Event

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
          [ { cx: 20.0, cy: 20.0, r: defaultCircleRadius }
          , { cx: 380.0, cy: 380.0, r: defaultCircleRadius }
          ]
      , showAdjustDialog: false
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
        state
          { circles = snoc state.circles
              { cx: toNumber state.mouseX
              , cy: toNumber state.mouseY
              , r: state.defaultCircleRadius
              }
          , showAdjustDialog = false
          }

    MouseRightBtnClicked evt -> do
      H.liftEffect $ WE.preventDefault evt
      H.modify_ _ { showAdjustDialog = true }

  render :: State -> H.ComponentHTML Action () m
  render state =
    let
      circleColor (x /\ y /\ r) =
        let
          dist = distFromCenter (x /\ y) (toNumber state.mouseX /\ toNumber state.mouseY)
        in
          SA.Named $ if dist < r then "lightgrey" else "white"

      mkCircle { cx, cy, r } =
        SE.circle
          [ SA.fill $ circleColor $ cx /\ cy /\ r
          , SA.stroke (SA.Named "black")
          , SA.cx cx
          , SA.cy cy
          , SA.r r
          , onContextMenu $ MouseRightBtnClicked
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
            (map mkCircle state.circles)

        , if state.showAdjustDialog then
            HH.div [ HP.id "adjust-dialog" ]
              [ HH.div_ [ HH.text "Adjust diameter of circle at (x, y)." ]
              , HH.input
                  [ HP.type_ HP.InputRange
                  , HP.min 0.0
                  , HP.max 100.0
                  , HP.value $ show state.defaultCircleRadius
                  ]
              ]
          else
            HH.text ""

        , HH.code_ [ HH.text $ show state ]
        ]