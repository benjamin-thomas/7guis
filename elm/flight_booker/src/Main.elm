module Main exposing (main)

import Browser
import Date exposing (Date, fromCalendarDate, toIsoString)
import Html exposing (Html, button, div, input, option, select, text)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onInput)
import Time exposing (Month(..))


type Departure
    = Departure Date


type Return
    = Return Date


type Model
    = OneWay (Maybe Departure)
    | RoundTrip (Maybe Departure) (Maybe Return)


init : Model
init =
    OneWay (Just <| Departure (fromCalendarDate 2023 Mar 1))


type Msg
    = FlightTypeChanged String
    | DepartureChanged String
    | ReturnChanged String


departureToReturn : Departure -> Return
departureToReturn (Departure d) =
    Return d


update : Msg -> Model -> Model
update msg model =
    case msg of
        FlightTypeChanged _ ->
            case model of
                OneWay md ->
                    RoundTrip md (Maybe.map departureToReturn md)

                RoundTrip md _ ->
                    OneWay md

        DepartureChanged s ->
            case model of
                OneWay _ ->
                    OneWay (Date.fromIsoString s |> Result.toMaybe |> Maybe.map Departure)

                RoundTrip _ mr ->
                    RoundTrip (Date.fromIsoString s |> Result.toMaybe |> Maybe.map Departure) mr

        ReturnChanged s ->
            case model of
                RoundTrip md _ ->
                    RoundTrip md (Date.fromIsoString s |> Result.toMaybe |> Maybe.map Return)

                OneWay _ ->
                    -- FIXME: satisfy the compiler, impossible state!
                    init


txtOneWay : String
txtOneWay =
    "one-way flight"


txtReturn : String
txtReturn =
    "return flight"


isSelected : String -> Model -> Bool
isSelected txt model =
    case model of
        OneWay _ ->
            txt == txtOneWay

        RoundTrip _ _ ->
            txt == txtReturn


flightOption : String -> Model -> Html Msg
flightOption txt model =
    option [ selected (isSelected txt model) ] [ text txt ]



-- I move away frome the requirements slightly here.
-- Returning 2 inputs for one way seems kludgy to me.


departureToIsoString : Departure -> String
departureToIsoString (Departure d) =
    toIsoString d


returnToIsoString : Return -> String
returnToIsoString (Return r) =
    toIsoString r


viewDateInputs : Model -> Html Msg
viewDateInputs model =
    case model of
        OneWay md ->
            let
                v =
                    md |> Maybe.map departureToIsoString |> Maybe.withDefault ""
            in
            div [] [ input [ value v, onInput DepartureChanged ] [] ]

        RoundTrip md mr ->
            let
                departure =
                    md |> Maybe.map departureToIsoString |> Maybe.withDefault ""

                return =
                    mr |> Maybe.map returnToIsoString |> Maybe.withDefault ""
            in
            div []
                [ div [] [ input [ value departure ] [] ]
                , div [] [ input [ value return ] [] ]
                ]


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ select [ onInput FlightTypeChanged ]
                [ flightOption txtOneWay model
                , flightOption txtReturn model
                ]
            ]
        , viewDateInputs model
        , div [] [ button [] [ text "Book" ] ]
        ]



-- BOOTSTRAP


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
