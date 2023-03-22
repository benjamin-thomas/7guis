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
    , absoluteX : Int
    , absoluteY : Int
    }


type alias Circle =
    { cx : Int
    , cy : Int
    , r : Int
    , stroke : String
    }


type alias Model =
    { mousePos : MousePos, circles : List Circle, selected : Maybe ( Circle, MousePos ), isMenuOpen : Bool }


init : Model
init =
    { mousePos = { x = 0, y = 0, absoluteX = 0, absoluteY = 0 }
    , circles =
        [ { cx = 50, cy = 50, r = 40, stroke = "black" }
        ]

    -- , selected = Just ( { cx = 50, cy = 50, r = 40, stroke = "black" }, { clientX = 797, clientY = 180, x = 45, y = 43 } )
    -- , isMenuOpen = True
    , selected = Nothing
    , isMenuOpen = False
    }


type Msg
    = MouseMove MousePos
    | SelectOrAddCircle
    | MouseRightClicked
    | RadiusChanged String


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
            { model | mousePos = pos }

        SelectOrAddCircle ->
            let
                pos =
                    model.mousePos

                search =
                    findCircle pos model.circles

                withSelectedOrWithNewCircle =
                    case search.found of
                        Nothing ->
                            let
                                newCircle : Circle
                                newCircle =
                                    { cx = model.mousePos.x
                                    , cy = model.mousePos.y
                                    , r = 40
                                    , stroke = "black"
                                    }
                            in
                            { model
                                | circles = newCircle :: model.circles
                                , selected = Nothing
                            }

                        Just newCircle ->
                            { model
                                | circles = search.before ++ newCircle :: search.after
                                , selected = Just ( newCircle, model.mousePos )
                            }
            in
            if model.isMenuOpen then
                { model | isMenuOpen = False }

            else
                withSelectedOrWithNewCircle

        MouseRightClicked ->
            { model | isMenuOpen = True }

        RadiusChanged s ->
            case ( model.isMenuOpen, model.selected ) of
                ( True, Just ( circle, pos ) ) ->
                    let
                        newCircle =
                            s
                                |> String.toInt
                                |> Maybe.map (\n -> { circle | r = n })
                                |> Maybe.withDefault circle

                        newCircles =
                            model.circles
                                |> List.map
                                    (\c ->
                                        if c == circle then
                                            newCircle

                                        else
                                            c
                                    )
                    in
                    { model | selected = Just ( newCircle, pos ), circles = newCircles }

                _ ->
                    model


decodeMousePos : D.Decoder MousePos
decodeMousePos =
    D.map4 MousePos
        (D.at [ "offsetX" ] D.int)
        (D.at [ "offsetY" ] D.int)
        (D.at [ "clientX" ] D.int)
        (D.at [ "clientY" ] D.int)


mouseRightClickDecoder : D.Decoder Msg
mouseRightClickDecoder =
    D.field "button" D.int
        |> D.andThen
            (\n ->
                if n == 2 then
                    D.succeed MouseRightClicked

                else
                    D.fail <| "Other: " ++ String.fromInt n
            )


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


showMenu : Bool -> Maybe ( Circle, MousePos ) -> Html Msg
showMenu isMenuOpen selected =
    case ( isMenuOpen, selected ) of
        ( True, Just ( circle, circlePos ) ) ->
            H.span
                [ A.style "position" "absolute"
                , A.style "top" (String.fromInt (circlePos.absoluteY + 80) ++ "px")
                , A.style "left" (String.fromInt circlePos.absoluteX ++ "px")
                , A.style "background" "red"
                , A.style "padding" "10px 30px"
                ]
                [ H.div
                    [ A.style "display" "flex"
                    , A.style "flex-direction" "column"
                    ]
                    [ H.label []
                        [ H.text <|
                            "Adjust diameter of circle at ("
                                ++ String.fromInt circle.cx
                                ++ ", "
                                ++ String.fromInt circle.cy
                                ++ ")"
                        ]
                    , H.input
                        [ A.type_ "range"
                        , A.min "10"
                        , A.max "100"
                        , A.value (String.fromInt circle.r)
                        , E.onInput RadiusChanged
                        ]
                        []
                    ]
                ]

        _ ->
            H.text ""


view : Model -> Html Msg
view model =
    let
        selectedCircle =
            model.selected |> Maybe.map Tuple.first

        viewCircle : Circle -> Svg Msg
        viewCircle c =
            circle
                [ cx (String.fromInt c.cx)
                , cy (String.fromInt c.cy)
                , r (String.fromInt c.r)
                , stroke c.stroke
                , fill
                    (if selectedCircle == Just c then
                        "orange"

                     else
                        "transparent"
                    )
                , E.preventDefaultOn "contextmenu" (D.map alwaysPreventDefault mouseRightClickDecoder)
                ]
                []
    in
    H.div
        []
        [ H.h1
            []
            [ H.text "Circle Drawer" ]
        , H.pre [ A.style "white-space" "pre-wrap" ]
            [ H.text <|
                Debug.toString model.mousePos
                    ++ " "
                    ++ Debug.toString model.selected
                    ++ " "
                    ++ Debug.toString model.isMenuOpen
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

                -- Disables doubleclick behaviour on Edge (a search menu pops up)
                , A.style "user-select" "none"
                ]
                (List.map viewCircle model.circles)
            , showMenu model.isMenuOpen model.selected
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
