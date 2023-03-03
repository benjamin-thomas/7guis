module Main exposing (main)

import Browser
import Date exposing (Date, fromCalendarDate, fromIsoString, toIsoString)
import Html exposing (Html, button, div, input, option, select, text)
import Html.Attributes exposing (selected, style, value)
import Html.Events exposing (onInput)
import Time exposing (Month(..))


type Departure
    = Departure Date


type Return
    = Return Date


type Model
    = OneWay (Result String Departure)
    | RoundTrip (Result String Departure) (Result String Return)


init : Model
init =
    OneWay (Ok <| Departure (fromCalendarDate 2023 Mar 1))


type Msg
    = FlightTypeChanged String
    | DepartureChanged String
    | ReturnChanged String


departureToReturn : Departure -> Return
departureToReturn (Departure d) =
    Return d


errWith : String -> Result String Date
errWith s =
    fromIsoString s |> Result.mapError (always s)


update : Msg -> Model -> Model
update msg model =
    case msg of
        FlightTypeChanged _ ->
            case model of
                OneWay md ->
                    RoundTrip md (Result.map departureToReturn md)

                RoundTrip md _ ->
                    OneWay md

        DepartureChanged s ->
            case model of
                OneWay _ ->
                    OneWay (errWith s |> Result.map Departure)

                RoundTrip _ mr ->
                    RoundTrip (errWith s |> Result.map Departure) mr

        ReturnChanged s ->
            case model of
                RoundTrip md _ ->
                    RoundTrip md (errWith s |> Result.map Return)

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


departureValue : Result String Departure -> String
departureValue md =
    case md of
        Err s ->
            s

        Ok (Departure d) ->
            toIsoString d


returnValue : Result String Return -> String
returnValue md =
    case md of
        Err s ->
            s

        Ok (Return d) ->
            toIsoString d


errStyle : Result err val -> Html.Attribute msg
errStyle e =
    case e of
        Err _ ->
            style "background-color" "red"

        Ok _ ->
            style "" ""


viewDateInputs : Model -> Html Msg
viewDateInputs model =
    case model of
        -- I move away frome the requirements slightly here.
        -- Returning 2 inputs for one way seems kludgy to me.
        OneWay dep ->
            div [] [ input [ errStyle dep, value (departureValue dep), onInput DepartureChanged ] [] ]

        RoundTrip dep ret ->
            div []
                [ div [] [ input [ errStyle dep, value (departureValue dep), onInput DepartureChanged ] [] ]
                , div [] [ input [ errStyle ret, value (returnValue ret), onInput ReturnChanged ] [] ]
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
