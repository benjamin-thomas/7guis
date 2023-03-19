module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as D
import Svg exposing (Svg, circle, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, width)



{-
   elm-live ./src/Main.elm -- --debug
-}


type alias MousePos =
    { x : Int
    , y : Int
    }


type alias Circle =
    { cx : Int
    , cy : Int
    , r : Int
    , stroke : String
    , fill : String
    }


type alias Model =
    { mousePos : MousePos, circles : List Circle }


init : Model
init =
    { mousePos = { x = 0, y = 0 }
    , circles =
        [ { cx = 50, cy = 50, r = 40, stroke = "black", fill = "transparent" }
        ]
    }


type Msg
    = MouseMove MousePos
    | AddCircle


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseMove pos ->
            { model | mousePos = pos }

        AddCircle ->
            let
                newCircle : Circle
                newCircle =
                    { cx = model.mousePos.x
                    , cy = model.mousePos.y
                    , r = 40
                    , stroke = "black"
                    , fill = "transparent"
                    }
            in
            { model | circles = newCircle :: model.circles }


decodeMousePos : D.Decoder MousePos
decodeMousePos =
    D.map2 MousePos
        (D.at [ "offsetX" ] D.int)
        (D.at [ "offsetY" ] D.int)


view : Model -> Html Msg
view model =
    let
        viewCircle : Circle -> Svg msg
        viewCircle c =
            circle
                [ cx (String.fromInt c.cx)
                , cy (String.fromInt c.cy)
                , r (String.fromInt c.r)
                , stroke c.stroke
                , fill c.fill
                ]
                []
    in
    H.div
        []
        [ H.h1 [] [ H.text "Circle Drawer" ]
        , H.pre [ A.style "white-space" "pre-wrap" ] [ H.text (Debug.toString model.mousePos) ]
        , H.div [ A.style "text-align" "center" ]
            [ svg
                [ width "400px"
                , height "400px"
                , A.style "border" "1px solid red"
                , E.on "mousemove" (D.map MouseMove decodeMousePos)
                , E.onClick AddCircle
                ]
                (List.map viewCircle model.circles)
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
