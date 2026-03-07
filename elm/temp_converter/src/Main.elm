module Main exposing (main)

import Browser
import Conversions.Celsius as Celsius
import Conversions.Fahrenheit as Fahrenheit
import Conversions.Units exposing (Celsius(..), Fahrenheit(..))
import Html exposing (Html, div, h1, hr, input, label, pre, span, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput)



-- MODEL


type Validation a
    = Valid a
    | Invalid a


type alias Unvalidate a =
    { klass : String, value : a }


unvalidate : Validation a -> Unvalidate a
unvalidate validation =
    case validation of
        Valid v ->
            { klass = "", value = v }

        Invalid v ->
            { klass = "error", value = v }


type alias Model =
    { celsius : Validation String
    , fahrenheit : Validation String
    }


init : Model
init =
    update (FahrenheitChanged "100")
        { celsius = Invalid ""
        , fahrenheit = Invalid ""
        }



-- UPDATE


type Msg
    = CelsiusChanged String
    | FahrenheitChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        CelsiusChanged cStr ->
            case String.toFloat cStr of
                Nothing ->
                    { model | celsius = Invalid cStr }

                Just vc ->
                    { celsius = Valid cStr
                    , fahrenheit = Valid <| Fahrenheit.toString <| Celsius.toFahrenheit (MkCelsius vc)
                    }

        FahrenheitChanged fStr ->
            case String.toFloat fStr of
                Nothing ->
                    { model
                        | fahrenheit = Invalid fStr
                    }

                Just vf ->
                    { fahrenheit = Valid fStr
                    , celsius = Valid <| Celsius.toString <| Fahrenheit.toCelsius (MkFahrenheit vf)
                    }



-- VIEW


inputClass : String -> Html.Attribute msg
inputClass klass =
    if klass == "error" then
        class "input input--invalid"

    else
        class "input"


temperature : String -> (String -> msg) -> Unvalidate String -> Html msg
temperature txt msg unvalid =
    span []
        [ input
            [ value unvalid.value
            , inputClass unvalid.klass
            , onInput msg
            ]
            []
        , span [ class "temp-label" ] [ text txt ]
        ]


view : Model -> Html Msg
view model =
    let
        unvalidCelsius =
            unvalidate model.celsius

        unvalidFahrenheit =
            unvalidate model.fahrenheit
    in
    div [ class "task-container" ]
        [ h1 [] [ text "Temperature Converter" ]
        , div [ class "card temp-converter" ]
            [ temperature "Celsius" CelsiusChanged unvalidCelsius
            , span [ class "temp-equals" ] [ text "=" ]
            , temperature "Fahrenheit" FahrenheitChanged unvalidFahrenheit
            ]
        , case ( model.celsius, model.fahrenheit ) of
            ( Valid cStr, Valid fStr ) ->
                div [ class "error-section error-section--hidden" ] [ text "\u{00A0}" ]

            _ ->
                div [ class "error-section" ] [ text "Cannot compute due to bad data!" ]
        ]



-- BOOTSTRAP


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
