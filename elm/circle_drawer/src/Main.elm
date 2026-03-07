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


type alias State =
    { mousePos : MousePos
    , circles : List Circle
    , selected : Maybe ( Circle, MousePos )
    , isMenuOpen : Bool
    }


type alias Model =
    { curr : State, prev : List State, next : List State }


init : Model
init =
    { curr =
        { mousePos = { x = 0, y = 0, absoluteX = 0, absoluteY = 0 }
        , circles = []
        , selected = Nothing
        , isMenuOpen = False

        -- , circles = [ { cx = 239, cy = 205, r = 40, stroke = "black" } ]
        -- , selected = Just ( { cx = 239, cy = 205, r = 40, stroke = "black" }, { absoluteX = 995, absoluteY = 347, x = 235, y = 217 } )
        -- , isMenuOpen = True
        }
    , prev = []
    , next = []
    }


type Msg
    = MouseMove MousePos
    | SelectOrAddCircle
    | MouseRightClicked
    | RadiusChanged String
    | CloseMenuAndUnselect
    | Undo
    | Redo


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
    let
        curr =
            model.curr
    in
    case msg of
        MouseMove pos ->
            { model | curr = { curr | mousePos = pos } }

        SelectOrAddCircle ->
            let
                pos =
                    curr.mousePos

                search =
                    findCircle pos curr.circles

                withSelectedOrWithNewCircle =
                    case search.found of
                        Nothing ->
                            let
                                newCircle : Circle
                                newCircle =
                                    { cx = curr.mousePos.x
                                    , cy = curr.mousePos.y
                                    , r = 40
                                    , stroke = "#888"
                                    }
                            in
                            { curr
                                | circles = newCircle :: curr.circles
                                , selected = Nothing
                            }

                        Just foundCircle ->
                            { curr
                                | circles = search.before ++ foundCircle :: search.after
                                , selected = Just ( foundCircle, curr.mousePos )
                                , isMenuOpen = False
                            }
            in
            { model
                | curr = withSelectedOrWithNewCircle
                , prev = { curr | isMenuOpen = False } :: model.prev
            }

        MouseRightClicked ->
            { model | curr = { curr | isMenuOpen = True } }

        RadiusChanged s ->
            case ( curr.isMenuOpen, curr.selected ) of
                ( True, Just ( circle, pos ) ) ->
                    let
                        newCircle =
                            s
                                |> String.toInt
                                |> Maybe.map (\n -> { circle | r = n })
                                |> Maybe.withDefault circle

                        newCircles =
                            curr.circles
                                |> List.map
                                    (\c ->
                                        if c == circle then
                                            newCircle

                                        else
                                            c
                                    )
                    in
                    { model
                        | curr =
                            { curr
                                | selected = Just ( newCircle, pos )
                                , circles = newCircles
                            }
                    }

                _ ->
                    model

        CloseMenuAndUnselect ->
            { model | curr = { curr | isMenuOpen = False, selected = Nothing } }

        Undo ->
            case model.prev of
                [] ->
                    model

                prevState :: prevHistory ->
                    { model | curr = prevState, prev = prevHistory, next = model.curr :: model.next }

        Redo ->
            case model.next of
                [] ->
                    model

                nextState :: nextHistory ->
                    { model | curr = nextState, next = nextHistory, prev = model.curr :: model.prev }


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


onMouseUp : msg -> H.Attribute msg
onMouseUp msg =
    E.on "mouseup" (D.succeed msg)


showMenu : Bool -> Maybe ( Circle, MousePos ) -> Html Msg
showMenu isMenuOpen selected =
    case ( isMenuOpen, selected ) of
        ( True, Just ( circle, circlePos ) ) ->
            H.span
                [ A.class "circle-drawer-popup"
                , A.style "position" "absolute"
                , A.style "top" (String.fromInt (circlePos.absoluteY + 80) ++ "px")
                , A.style "left" (String.fromInt circlePos.absoluteX ++ "px")
                ]
                [ H.button [ A.class "circle-drawer-popup-close", E.onClick CloseMenuAndUnselect ] [ H.text "✕" ]
                , H.div [ A.class "circle-drawer-popup-label" ]
                    [ H.text "Adjust diameter" ]
                , H.input
                    [ A.type_ "range"
                    , A.class "circle-drawer-slider"
                    , A.min "10"
                    , A.max "100"
                    , A.value (String.fromInt circle.r)
                    , E.onInput RadiusChanged
                    , onMouseUp CloseMenuAndUnselect
                    ]
                    []
                ]

        _ ->
            H.text ""


view : Model -> Html Msg
view model =
    let
        state =
            model.curr

        selectedCircle =
            state.selected |> Maybe.map Tuple.first

        viewCircle : Circle -> Svg Msg
        viewCircle c =
            circle
                [ cx (String.fromInt c.cx)
                , cy (String.fromInt c.cy)
                , r (String.fromInt c.r)
                , stroke "#222"
                , fill
                    (if selectedCircle == Just c then
                        "#666"

                     else
                        "#555"
                    )
                , E.preventDefaultOn "contextmenu" (D.map alwaysPreventDefault mouseRightClickDecoder)
                ]
                []
    in
    H.div [ A.class "task-container" ]
        [ H.h1 [] [ H.text "Circle Drawer" ]
        , H.div [ A.class "card circle-drawer" ]
            [ H.div [ A.class "circle-drawer-toolbar" ]
                [ H.button [ E.onClick Undo ] [ H.text "Undo" ]
                , H.button [ E.onClick Redo ] [ H.text "Redo" ]
                ]
            , H.div [ A.class "circle-drawer-svg-container" ]
                [ svg
                    [ width "100%"
                    , height "100%"
                    , A.style "user-select" "none"
                    , E.on "mousemove" (D.map MouseMove decodeMousePos)
                    , E.onClick SelectOrAddCircle
                    ]
                    (List.map viewCircle state.circles)
                ]
            , showMenu state.isMenuOpen state.selected
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
