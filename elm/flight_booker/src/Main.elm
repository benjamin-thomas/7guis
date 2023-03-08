module Main exposing (..)

import Browser
import Date exposing (Date, fromIsoString)
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, input, option, select, span, text)
import Html.Attributes exposing (disabled, selected, style, value)
import Html.Events exposing (onClick, onInput)
import Time exposing (Month(..))



-- MODEL


type Flight
    = OneWay Date
    | Return Date Date


type FlightType
    = OneWay_
    | Return_


flightTypes : Dict Int ( FlightType, String )
flightTypes =
    Dict.fromList
        [ ( 0, ( OneWay_, "one-way flight" ) )
        , ( 1, ( Return_, "return flight" ) )
        ]


type alias Form =
    { flightType : FlightType
    , departureDate : String
    , returnDate : String
    }


type alias FormErrors =
    { departureDate : String
    , returnDate : String
    , returnIsBeforeDeparture : String
    }


type Model
    = UserEdit Form
    | ConfirmBooking Flight



-- VALIDATION


validate : Form -> Result FormErrors Flight
validate form =
    case form.flightType of
        OneWay_ ->
            case fromIsoString form.departureDate of
                Ok dt ->
                    Ok (OneWay dt)

                Err _ ->
                    Err (FormErrors "invalid dep" "" "")

        Return_ ->
            let
                depRes : Result String Date
                depRes =
                    Result.mapError (\_ -> "invalid dep") (fromIsoString form.departureDate)

                retRes : Result String Date
                retRes =
                    Result.mapError (\_ -> "invalid ret") (fromIsoString form.returnDate)
            in
            case ( depRes, retRes ) of
                ( Ok dep, Ok ret ) ->
                    if Date.compare ret dep == LT then
                        Err (FormErrors "" "" "Return cannot happen before departure!")

                    else
                        Ok (Return dep ret)

                ( _, _ ) ->
                    let
                        left x =
                            case x of
                                Ok _ ->
                                    ""

                                Err s ->
                                    s
                    in
                    Err (FormErrors (left depRes) (left retRes) "")



-- UPDATE


init : Model
init =
    UserEdit
        { flightType = OneWay_
        , departureDate = "2023-03-08"
        , returnDate = ""
        }


type Msg
    = FlightTypeChanged String
    | DepartureChanged String
    | ReturnChanged String
    | BookClicked


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( FlightTypeChanged s, UserEdit form ) ->
            let
                getFlightType n =
                    Dict.get n flightTypes
                        |> Maybe.map Tuple.first

                mft =
                    String.toInt s
                        |> Maybe.andThen getFlightType
            in
            case mft of
                Nothing ->
                    model

                Just ft ->
                    let
                        rd =
                            if form.returnDate == "" then
                                form.departureDate

                            else
                                form.returnDate
                    in
                    UserEdit <|
                        { form | flightType = ft, returnDate = rd }

        ( DepartureChanged s, UserEdit form ) ->
            UserEdit <|
                { form | departureDate = s }

        ( ReturnChanged s, UserEdit form ) ->
            UserEdit <|
                { form | returnDate = s }

        ( BookClicked, UserEdit form ) ->
            case validate form of
                Err _ ->
                    UserEdit form

                Ok flight ->
                    ConfirmBooking flight

        ( _, _ ) ->
            -- Impossible states
            model



-- VIEW


isSelected : Int -> FlightType -> Bool
isSelected id ft =
    case Dict.get id flightTypes of
        Nothing ->
            False

        Just ( ft_, _ ) ->
            ft == ft_


flightOption : Int -> String -> Form -> Html Msg
flightOption id txt form =
    option
        [ selected (isSelected id form.flightType)
        , value (String.fromInt id)
        ]
        [ text txt ]


viewDateInputs : Form -> Result FormErrors Flight -> Html Msg
viewDateInputs form validation =
    let
        showErr field =
            span []
                [ case validation of
                    Err fe ->
                        text (field fe)

                    Ok _ ->
                        text ""
                ]

        errStyle e =
            case validation of
                Err fe ->
                    if String.length (e fe) > 0 then
                        style "background-color" "red"

                    else
                        style "" ""

                Ok _ ->
                    style "" ""
    in
    case form.flightType of
        -- I move away from the requirements slightly here.
        -- Returning 2 inputs for one way seems kludgy to me.
        OneWay_ ->
            div []
                [ input [ errStyle .departureDate, value form.departureDate, onInput DepartureChanged ] []
                , showErr .departureDate
                ]

        Return_ ->
            div []
                [ div []
                    [ input [ errStyle .departureDate, value form.departureDate, onInput DepartureChanged ] []
                    , showErr .departureDate
                    ]
                , div []
                    [ input [ errStyle .returnDate, value form.returnDate, onInput ReturnChanged ] []
                    , showErr .returnDate
                    ]
                , div [ errStyle .returnIsBeforeDeparture ]
                    [ showErr .returnIsBeforeDeparture
                    ]
                ]


isErr : Result error value -> Bool
isErr validation =
    case validation of
        Err _ ->
            True

        Ok _ ->
            False


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Flight Booker" ]
        , div []
            (case model of
                UserEdit form ->
                    let
                        validation =
                            validate form
                    in
                    [ select [ onInput FlightTypeChanged ]
                        (Dict.toList flightTypes
                            |> List.map
                                (\( id, ( _, str ) ) ->
                                    flightOption id str form
                                )
                        )
                    , viewDateInputs form validation
                    , div [] [ button [ disabled (isErr validation), onClick BookClicked ] [ text "Book" ] ]
                    ]

                ConfirmBooking flight ->
                    [ case flight of
                        OneWay dep ->
                            text <| "You have booked a one-way flight on " ++ Date.toIsoString dep ++ "!"

                        Return dep ret ->
                            text <|
                                "You have booked a return flight: "
                                    ++ Date.toIsoString dep
                                    ++ "⇔"
                                    ++ Date.toIsoString ret
                                    ++ "!"
                    ]
            )
        ]



-- BOOTSTRAP


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
