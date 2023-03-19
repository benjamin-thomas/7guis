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
    { mousePos : MousePos, circles : List Circle, interSect : Bool }


init : Model
init =
    { mousePos = { x = 0, y = 0 }
    , circles =
        [ { cx = 50, cy = 50, r = 40, stroke = "black", fill = "transparent" }
        ]
    , interSect = False
    }


type Msg
    = MouseMove MousePos
    | SelectOrAddCircle


isIntersect : MousePos -> Circle -> Bool
isIntersect pos c =
    let
        dx =
            c.cx - pos.x |> toFloat

        dy =
            c.cy - pos.y |> toFloat

        dist =
            sqrt (dx * dx + dy * dy)
    in
    dist < toFloat c.r


findCircle : MousePos -> List Circle -> { before : List Circle, found : Maybe Circle, after : List Circle }
findCircle pos lst =
    -- NOTE: I don't handle overlapping circles very well (the drawing order may not be preserved)
    let
        inner pos_ lst_ acc =
            case lst_ of
                [] ->
                    { before = acc, found = Nothing, after = [] }

                x :: xs ->
                    if isIntersect pos_ x then
                        { before = acc, found = Just x, after = xs }

                    else
                        inner pos_ xs (x :: acc)
    in
    inner pos lst []


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseMove pos ->
            let
                dx =
                    50 - pos.x |> toFloat

                dy =
                    50 - pos.y |> toFloat

                dist =
                    sqrt (dx * dx + dy * dy)
            in
            { model | mousePos = pos, interSect = dist < toFloat 40 }

        SelectOrAddCircle ->
            let
                pos =
                    model.mousePos

                search =
                    findCircle pos model.circles

                newModel =
                    case search.found of
                        Nothing ->
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

                        Just circle ->
                            let
                                newCircle =
                                    { circle | fill = "orange" }
                            in
                            { model | circles = search.before ++ newCircle :: search.after }
            in
            newModel


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
        , H.pre [ A.style "white-space" "pre-wrap" ]
            [ H.text <|
                Debug.toString model.mousePos
                    ++ " "
                    ++ Debug.toString model.interSect
            ]
        , H.pre [ A.style "white-space" "pre-wrap" ]
            [ H.text <| String.join "\n" <| List.map (\c -> Debug.toString c) model.circles ]
        , H.div [ A.style "text-align" "center" ]
            [ svg
                [ width "400px"
                , height "400px"
                , A.style "border" "1px solid red"
                , E.on "mousemove" (D.map MouseMove decodeMousePos)
                , E.onClick SelectOrAddCircle
                ]
                (List.map viewCircle model.circles)
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
