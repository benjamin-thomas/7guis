module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attrs exposing (style, type_)
import Html.Events exposing (onClick, onInput)
import Task
import Time



-- MODEL


type alias Model =
    { time : Time.Posix, duration : Int, started : Maybe Time.Posix }



-- UPDATE


type Msg
    = Tick Time.Posix
    | ChangedDuration String
    | StartTimer
    | StopTimer


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = Time.millisToPosix 0
      , duration = 3000
      , started = Nothing
      }
    , Task.perform Tick Time.now
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        ChangedDuration s ->
            let
                newModel =
                    String.toInt s
                        |> Maybe.map (\n -> { model | duration = n })
                        |> Maybe.withDefault model
            in
            ( newModel, Cmd.none )

        StartTimer ->
            ( { model | started = Just model.time }, Cmd.none )

        StopTimer ->
            let
                forceTimeUpdate =
                    -- Bug if STOP after duration passed otherwise
                    Task.perform Tick Time.now
            in
            ( { model | started = Nothing }, forceTimeUpdate )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.started of
        Nothing ->
            Sub.none

        Just t ->
            let
                diff =
                    Time.posixToMillis model.time - Time.posixToMillis t
            in
            if diff < model.duration then
                Time.every 0 Tick

            else
                Sub.none



-- VIEW


elapsed : Time.Posix -> Maybe Time.Posix -> Int
elapsed curr started =
    case started of
        Nothing ->
            0

        Just start ->
            Time.posixToMillis curr - Time.posixToMillis start


view : Model -> Html Msg
view model =
    let
        elapsedMs =
            String.fromInt <| elapsed model.time model.started

        durationStr =
            model.duration |> String.fromInt
    in
    div []
        [ div []
            [ text <| "Curr: " ++ String.fromInt (Time.posixToMillis model.time)
            ]
        , div []
            [ input
                [ type_ "range"
                , Attrs.value (model.duration |> String.fromInt)
                , Attrs.min "0"
                , Attrs.max "10000"
                , Attrs.step "500"
                , onInput ChangedDuration
                , style "width" "200px"
                ]
                []
            , span [] [ text durationStr ]
            ]
        , div []
            [ meter
                [ Attrs.value elapsedMs
                , Attrs.min "0"
                , Attrs.max durationStr
                , style "width" "200px"
                ]
                []
            ]
        , div []
            [ button [ onClick StopTimer ] [ text "STOP" ]
            , button [ onClick StartTimer ] [ text "START" ]
            ]
        , p [] [ text (Debug.toString model) ]
        , p [] [ text <| elapsedMs ]
        ]



-- BOOTSTRAP


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
