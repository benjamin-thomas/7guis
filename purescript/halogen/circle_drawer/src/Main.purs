module Main (main) where

import Prelude

import Data.Int (decimal, toNumber)
import Data.Int as Int
import Data.List (List, head, tail, (:))
import Data.List as List
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
  , history ::
      { past :: List (Map Int Circle)
      , current :: Map Int Circle
      , future :: List (Map Int Circle)
      }
  , showAdjustDialog :: Maybe CircleClickData
  , defaultCircleRadius :: Number
  }

data Action
  = MouseMoved MouseEvent
  | DrawingAreaClicked
  | MouseRightBtnClicked CircleClickData WE.Event
  | CircleRadiusChanged { circleId :: Int } String
  | MouseReleased
  | UndoBtnClicked
  | RedoBtnClicked

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
      , history:
          { past: List.Nil
          , current: Map.empty
          , future: List.Nil
          }
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
          nextId = fromMaybe 1 $ Map.findMax state.history.current <#> _.key <#> ((+) 1)

          newCircles :: Map Int Circle
          newCircles = state.history.current # Map.insert nextId
            { x: toNumber state.mouseX
            , y: toNumber state.mouseY
            , r: state.defaultCircleRadius
            }
        in
          state
            { history
                { past = state.history.current : state.history.past
                , current = newCircles
                }
            }

    MouseRightBtnClicked circleData evt -> do
      H.liftEffect $ WE.preventDefault evt
      H.modify_ \state -> state
        { showAdjustDialog = Just circleData
        , history { past = state.history.current : state.history.past }
        }

    CircleRadiusChanged { circleId } str ->
      H.modify_ \state ->
        case state.history.current # Map.lookup circleId of
          Nothing -> state
          Just circle ->
            let
              r = unsafePartial fromJust $ Number.fromString str
              newCircle = circle { r = r }

              current :: Map Int Circle
              current = state.history.current # Map.insert circleId newCircle
            in

              state { history { current = current } }

    MouseReleased ->
      H.modify_ \state -> state
        { showAdjustDialog = Nothing
        }

    UndoBtnClicked ->
      H.modify_ \state ->
        let
          past :: List (Map Int Circle)
          past = fromMaybe List.Nil $ tail state.history.past

          current :: Map Int Circle
          current = fromMaybe state.history.current $ head state.history.past

          future :: List (Map Int Circle)
          future = state.history.current : state.history.future
        in
          state
            { history
                { past = past
                , current = current
                , future = future
                }
            }

    RedoBtnClicked ->
      H.modify_ \state ->
        let
          past :: List (Map Int Circle)
          past = state.history.current : state.history.past

          current :: Map Int Circle
          current = fromMaybe state.history.current $ head state.history.future

          future :: List (Map Int Circle)
          future = fromMaybe List.Nil $ tail state.history.future
        in
          state
            { history
                { past = past
                , current = current
                , future = future
                }
            }

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
        , HH.p_ [ HH.text "Right click to change the diameter" ]
        , HH.div [ HP.id "buttons" ]
            [ HH.button [ HE.onClick $ const UndoBtnClicked ] [ HH.text "Undo" ]
            , HH.button [ HE.onClick $ const RedoBtnClicked ] [ HH.text "Redo" ]
            ]

        , SE.svg
            [ HE.onMouseMove MouseMoved
            , HE.onClick $ const DrawingAreaClicked
            , HP.style "background:white"
            ]
            (map mkCircle $ Map.toUnfoldable state.history.current)

        , case state.showAdjustDialog of
            Nothing -> HH.text ""
            Just ({ circleId, circle }) ->
              let
                conv :: Number -> String
                conv n = Int.fromNumber n <#> Int.toStringAs decimal # fromMaybe "?"
                coords = "(" <> conv circle.x <> ", " <> conv circle.y <> ")"
              in
                HH.div [ HP.id "adjust-dialog", HP.style $ "top:" <> conv circle.y <> "px" ]
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
        ]