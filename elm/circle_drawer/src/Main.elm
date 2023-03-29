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
    | Save
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
                                    , stroke = "black"
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

                        -- , prev = { curr | isMenuOpen = False } :: model.prev
                    }

                _ ->
                    model

        CloseMenuAndUnselect ->
            { model | curr = { curr | isMenuOpen = False, selected = Nothing } }

        Save ->
            model

        -- let
        --     curr_ : State
        --     curr_ =
        --         model.curr
        --     curr__ =
        --         { curr_ | isMenuOpen = False }
        --     _ =
        --         Debug.log "cur_" curr_
        -- in
        -- { model | prev = curr__ :: model.prev }
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



-- onChange : (String -> msg) -> H.Attribute msg
-- onChange tagger =
--     E.on "change" (D.map tagger E.targetValue)


onMouseUp : msg -> H.Attribute msg
onMouseUp msg =
    E.on "mouseup" (D.succeed msg)


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
                        , onMouseUp CloseMenuAndUnselect
                        , E.onBlur Save
                        ]
                        []
                    ]
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
                Debug.toString state.mousePos
                    ++ " "
                    ++ Debug.toString state.selected
                    ++ " "
                    ++ Debug.toString state.isMenuOpen
            ]
        , H.div [ A.style "text-align" "center" ]
            [ H.div []
                [ H.button [ E.onClick Undo ] [ H.text "Undo" ]
                , H.button [ E.onClick Redo ] [ H.text "Redo" ]
                ]
            , svg
                [ width "400px"
                , height "400px"
                , A.style "border" "1px solid red"
                , E.on "mousemove" (D.map MouseMove decodeMousePos)
                , E.onClick SelectOrAddCircle

                -- Disables doubleclick behaviour on Edge (a search menu pops up)
                , A.style "user-select" "none"
                ]
                (List.map viewCircle state.circles)
            , showMenu state.isMenuOpen state.selected
            ]
        , H.pre [ A.style "white-space" "pre-wrap" ]
            [ H.text <| "CIRCLES:\n -" ++ (String.join "\n -" <| List.map (\c -> Debug.toString c) state.circles) ]
        , H.pre [ A.style "white-space" "pre-wrap" ]
            [ H.text <| "PREV:\n -" ++ (String.join "\n -" <| List.map (\c -> Debug.toString c) model.prev)
            ]
        , H.pre [ A.style "white-space" "pre-wrap" ]
            [ H.text <| "NEXT\n -" ++ (String.join "\n -" <| List.map (\c -> Debug.toString c) model.next)
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
