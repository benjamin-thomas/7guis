module Main exposing (..)

import Browser
import Date exposing (Date, fromCalendarDate, fromIsoString, toIsoString)
import Html exposing (Html, button, div, h1, input, option, select, text)
import Html.Attributes exposing (disabled, selected, style, value)
import Html.Events exposing (onClick, onInput)
import Time exposing (Month(..))


type Departure
    = Departure Date


type Return
    = Return Date


type DepartureErr
    = InvalidDepartureDate String


type ReturnErr
    = InvalidReturnDate String
    | ReturnIsBeforeDeparture String


type Model
    = UserEdit UserEdit
    | ConfirmBooking String


type UserEdit
    = OneWay (Result DepartureErr Departure)
    | RoundTrip (Result DepartureErr Departure) (Result ReturnErr Return)


init : Model
init =
    UserEdit <| OneWay <| Ok <| Departure <| fromCalendarDate 2023 Mar 1


type Msg
    = FlightTypeChanged String
    | DepartureChanged String
    | ReturnChanged String
    | BookClicked


departureToReturn : Departure -> Return
departureToReturn (Departure d) =
    Return d


departureToReturnErr : DepartureErr -> ReturnErr
departureToReturnErr err =
    case err of
        InvalidDepartureDate d ->
            InvalidReturnDate d


validateDepartureString : String -> Result DepartureErr Departure
validateDepartureString s =
    fromIsoString s
        |> Result.mapError (always (InvalidDepartureDate s))
        |> Result.map Departure


validateReturnString : String -> Result ReturnErr Return
validateReturnString s =
    fromIsoString s
        |> Result.mapError (always (InvalidReturnDate s))
        |> Result.map Return


validateNotBefore :
    ( Result DepartureErr Departure, Result ReturnErr Return )
    -> ( Result DepartureErr Departure, Result ReturnErr Return )
validateNotBefore ( dep, ret ) =
    case ( dep, ret ) of
        ( Ok (Departure d), Ok (Return r) ) ->
            if Date.compare r d == LT then
                -- Bad user input, return cannot happen before departure!
                let
                    badRet =
                        Err (ReturnIsBeforeDeparture (toIsoString r))
                in
                ( dep, badRet )

            else
                ( dep, ret )

        _ ->
            ( dep, ret )


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( FlightTypeChanged _, UserEdit edit ) ->
            case edit of
                OneWay dep ->
                    let
                        ret =
                            dep |> Result.map departureToReturn |> Result.mapError departureToReturnErr
                    in
                    UserEdit <| RoundTrip dep ret

                RoundTrip dep _ ->
                    UserEdit <| OneWay dep

        ( DepartureChanged s, UserEdit edit ) ->
            case edit of
                OneWay _ ->
                    UserEdit <| OneWay (validateDepartureString s)

                RoundTrip _ ret ->
                    let
                        dep : Result DepartureErr Departure
                        dep =
                            validateDepartureString s

                        ( vd, vr ) =
                            validateNotBefore ( dep, ret )
                    in
                    UserEdit <| RoundTrip vd vr

        ( ReturnChanged s, UserEdit edit ) ->
            case edit of
                RoundTrip dep _ ->
                    let
                        ret : Result ReturnErr Return
                        ret =
                            validateReturnString s

                        ( vd, vr ) =
                            validateNotBefore ( dep, ret )
                    in
                    UserEdit <| RoundTrip vd vr

                OneWay _ ->
                    -- FIXME: Impossible state / satisfy the compiler. The relevant field is not shown in the user interface.
                    model

        ( BookClicked, UserEdit edit ) ->
            case edit of
                OneWay dep ->
                    case dep of
                        Ok (Departure d) ->
                            ConfirmBooking <| "You have booked a one-way flight on " ++ Date.toIsoString d ++ "!"

                        Err _ ->
                            -- FIXME: Impossible state / satisfy the compiler. The button cannot be clicked if they are errors.
                            model

                RoundTrip dep ret ->
                    case ( dep, ret ) of
                        ( Ok (Departure d), Ok (Return r) ) ->
                            ConfirmBooking <|
                                "You have booked a return flight: "
                                    ++ Date.toIsoString d
                                    ++ "⇔"
                                    ++ Date.toIsoString r
                                    ++ "!"

                        _ ->
                            -- FIXME: Impossible state / satisfy the compiler. The button cannot be clicked if they are errors.
                            model

        ( _, _ ) ->
            -- FIXME: Impossible state / satisfy the compiler. This branch should never trigger.
            model


txtOneWay : String
txtOneWay =
    "one-way flight"


txtReturn : String
txtReturn =
    "return flight"


isSelected : String -> UserEdit -> Bool
isSelected txt edit =
    case edit of
        OneWay _ ->
            txt == txtOneWay

        RoundTrip _ _ ->
            txt == txtReturn


flightOption : String -> UserEdit -> Html Msg
flightOption txt edit =
    option [ selected (isSelected txt edit) ] [ text txt ]


departureValue : Result DepartureErr Departure -> String
departureValue dep =
    case dep of
        Err (InvalidDepartureDate s) ->
            s

        Ok (Departure d) ->
            toIsoString d


returnValue : Result ReturnErr Return -> String
returnValue ret =
    case ret of
        Err (InvalidReturnDate s) ->
            s

        Err (ReturnIsBeforeDeparture s) ->
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


invalidDepartureWarning : Result DepartureErr value -> Html msg
invalidDepartureWarning dep =
    case dep of
        Ok _ ->
            text ""

        Err (InvalidDepartureDate _) ->
            text "The departure date is not valid!"


invalidReturnWarning : Result ReturnErr value -> Html msg
invalidReturnWarning ret =
    case ret of
        Ok _ ->
            text ""

        Err (InvalidReturnDate _) ->
            text "The return date is not valid!"

        Err (ReturnIsBeforeDeparture _) ->
            text "Return cannot happen before departure!"


viewDateInputs : UserEdit -> Html Msg
viewDateInputs edit =
    case edit of
        -- I move away from the requirements slightly here.
        -- Returning 2 inputs for one way seems kludgy to me.
        OneWay dep ->
            div []
                [ input [ errStyle dep, value (departureValue dep), onInput DepartureChanged ] []
                , invalidDepartureWarning dep
                ]

        RoundTrip dep ret ->
            div []
                [ div []
                    [ input [ errStyle dep, value (departureValue dep), onInput DepartureChanged ] []
                    , invalidDepartureWarning dep
                    ]
                , div []
                    [ input [ errStyle ret, value (returnValue ret), onInput ReturnChanged ] []
                    , invalidReturnWarning ret
                    ]
                ]


anyErrors : UserEdit -> Bool
anyErrors edit =
    case edit of
        OneWay (Err _) ->
            True

        RoundTrip (Err _) _ ->
            True

        RoundTrip _ (Err _) ->
            True

        OneWay (Ok _) ->
            False

        RoundTrip (Ok _) (Ok _) ->
            False


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Flight Booker" ]
        , div []
            (case model of
                UserEdit edit ->
                    [ select [ onInput FlightTypeChanged ]
                        [ flightOption txtOneWay edit
                        , flightOption txtReturn edit
                        ]
                    , viewDateInputs edit
                    , div [] [ button [ disabled (anyErrors edit), onClick BookClicked ] [ text "Book" ] ]
                    ]

                ConfirmBooking str ->
                    [ text str ]
            )
        ]



-- BOOTSTRAP


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
