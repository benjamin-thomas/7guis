module Main exposing (..)

import Browser
import Date exposing (Date, fromCalendarDate, fromIsoString, toIsoString)
import Html exposing (Html, button, div, h1, input, option, select, text)
import Html.Attributes exposing (disabled, selected, style, value)
import Html.Events exposing (onClick, onInput)
import Time exposing (Month(..))



{-
   BUG/FIXME:
       - make departure invalid
       - make return invalid
       - make return valid
       - make departure valid
       - IsAfterReturn state is not detected

   POSSIBLE SOLUTION:
       Handle the form change as a whole, not per input
-}


type Departure
    = Departure Date


type Return
    = Return Date


type DepartureErr
    = InvalidDepartureDate String


type ReturnErr
    = InvalidReturnDate String
    | IsAfterReturn String


type Model
    = OneWay (Result DepartureErr Departure)
    | RoundTrip (Result DepartureErr Departure) (Result ReturnErr Return)


init : Model
init =
    OneWay (Ok <| Departure (fromCalendarDate 2023 Mar 1))


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


errWith : String -> Result DepartureErr Date
errWith s =
    fromIsoString s |> Result.mapError (always (InvalidDepartureDate s))


update : Msg -> Model -> Model
update msg model =
    case msg of
        FlightTypeChanged _ ->
            case model of
                OneWay md ->
                    RoundTrip md (md |> Result.map departureToReturn |> Result.mapError departureToReturnErr)

                RoundTrip md _ ->
                    OneWay md

        DepartureChanged s ->
            case model of
                OneWay _ ->
                    OneWay (errWith s |> Result.map Departure)

                RoundTrip _ ret ->
                    RoundTrip (errWith s |> Result.map Departure) ret

        ReturnChanged s ->
            case model of
                RoundTrip dep _ ->
                    let
                        res : Result ReturnErr Date
                        res =
                            let
                                err =
                                    errWith s
                            in
                            case ( err, dep ) of
                                ( Ok r, Ok (Departure d) ) ->
                                    if Date.compare r d == LT then
                                        -- Bad user input, return cannot happen before departure!
                                        Err (IsAfterReturn s)

                                    else
                                        err |> Result.mapError departureToReturnErr

                                _ ->
                                    err |> Result.mapError departureToReturnErr

                        ret : Result ReturnErr Return
                        ret =
                            res |> Result.map Return
                    in
                    RoundTrip dep ret

                OneWay _ ->
                    -- FIXME: satisfy the compiler, impossible state!
                    init

        BookClicked ->
            Debug.todo "BookClicked"


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


departureValue : Result DepartureErr Departure -> String
departureValue md =
    case md of
        Err (InvalidDepartureDate s) ->
            s

        Ok (Departure d) ->
            toIsoString d


returnValue : Result ReturnErr Return -> String
returnValue md =
    case md of
        Err (InvalidReturnDate s) ->
            s

        Err (IsAfterReturn s) ->
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

        Err (IsAfterReturn _) ->
            text "Return cannot happen before departure!"


viewDateInputs : Model -> Html Msg
viewDateInputs model =
    case model of
        -- I move away frome the requirements slightly here.
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


anyErrors : Model -> Bool
anyErrors model =
    case model of
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
            [ select [ onInput FlightTypeChanged ]
                [ flightOption txtOneWay model
                , flightOption txtReturn model
                ]
            ]
        , viewDateInputs model
        , div [] [ button [ disabled (anyErrors model), onClick BookClicked ] [ text "Book" ] ]
        ]



-- BOOTSTRAP


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
