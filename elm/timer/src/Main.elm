module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attrs exposing (class, type_)
import Html.Events exposing (onClick, onInput)
import Task
import Time



-- MODEL


type ReachTime
    = ReachTime Time.Posix


type Elapsed
    = Elapsed Int


type PrevTime
    = PrevTime Time.Posix


type State
    = Stopped
    | Running Elapsed PrevTime
    | Reached ReachTime


type alias Model =
    { curr : Time.Posix, duration : Int, state : State }



-- UPDATE


type Msg
    = Tick Time.Posix
    | ChangedDuration String
    | StartTimer
    | StopTimer


init : () -> ( Model, Cmd Msg )
init _ =
    ( { curr = Time.millisToPosix 0, duration = 3000, state = Stopped }
    , Task.perform Tick Time.now
    )



-- UPDATE


posixDelta : Time.Posix -> Time.Posix -> Int
posixDelta b a =
    let
        toMS =
            Time.posixToMillis
    in
    toMS b - toMS a


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- Think of a state machine:
    --   CURR_STATE + EVENT => NEW_STATE
    case ( model.state, msg ) of
        ( state, Tick t ) ->
            case state of
                Running (Elapsed elapsed) (PrevTime t0) ->
                    let
                        delta =
                            posixDelta t t0

                        newElapsed =
                            elapsed + delta
                    in
                    if newElapsed < model.duration then
                        ( { model | curr = t, state = Running (Elapsed newElapsed) (PrevTime model.curr) }, Cmd.none )

                    else
                        ( { model | curr = t, state = Reached (ReachTime t) }, Cmd.none )

                _ ->
                    ( { model | curr = t }, Cmd.none )

        ( Stopped, StartTimer ) ->
            ( { model | state = Running (Elapsed 0) (PrevTime model.curr) }, Cmd.none )

        ( Reached (ReachTime _), ChangedDuration s ) ->
            let
                newDuration : Int
                newDuration =
                    String.toInt s |> Maybe.withDefault 0

                newModel =
                    { model | duration = newDuration, state = Running (Elapsed newDuration) (PrevTime model.curr) }
            in
            ( newModel, Cmd.none )

        ( _, ChangedDuration s ) ->
            ( { model | duration = String.toInt s |> Maybe.withDefault 0 }, Cmd.none )

        ( _, StopTimer ) ->
            ( { model | state = Stopped }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )



-- VIEW


durationDiv : String -> Html Msg
durationDiv durationStr =
    div [ class "timer-row" ]
        [ label [ class "timer-label" ]
            [ text <| "Duration: " ++ durationStr ++ "ms" ]
        , input
            [ type_ "range"
            , class "timer-slider"
            , Attrs.value durationStr
            , Attrs.min "0"
            , Attrs.max "10000"
            , Attrs.step "500"
            , onInput ChangedDuration
            ]
            []
        ]


elapsedTimeDiv : State -> String -> Html msg
elapsedTimeDiv state durationStr =
    div [ class "timer-row" ]
        [ label [ class "timer-label" ] [ text "Elapsed Time:" ]
        , node "progress"
            [ class "timer-progress"
            , Attrs.value
                (case state of
                    Running (Elapsed elapsed) _ ->
                        String.fromInt elapsed

                    Reached _ ->
                        durationStr

                    _ ->
                        "0"
                )
            , Attrs.max durationStr
            ]
            []
        ]


buttonDiv : State -> Html Msg
buttonDiv state =
    case state of
        Stopped ->
            button [ onClick StartTimer ] [ text "START" ]

        Running _ _ ->
            button [ onClick StopTimer ] [ text "CANCEL" ]

        Reached _ ->
            button [ onClick StopTimer ] [ text "RESET" ]


runningSecondsDiv : Model -> Html msg
runningSecondsDiv model =
    div [ class "timer-elapsed" ]
        [ case model.state of
            Running (Elapsed e) _ ->
                text <| "Running: " ++ String.fromInt e ++ "ms"

            Reached _ ->
                text <| "Completed: " ++ String.fromInt model.duration ++ "ms"

            _ ->
                text "\u{00A0}"
        ]


view : Model -> Html Msg
view model =
    let
        durationStr =
            String.fromInt model.duration
    in
    div [ class "task-container" ]
        [ h1 [] [ text "Timer" ]
        , div [ class "card timer" ]
            [ elapsedTimeDiv model.state durationStr
            , runningSecondsDiv model
            , durationDiv durationStr
            , buttonDiv model.state
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 Tick



-- BOOTSTRAP


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }