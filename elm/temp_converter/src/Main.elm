module Main exposing (main)

import Browser
import Conversions.Celsius as Celsius
import Conversions.Fahrenheit as Fahrenheit
import Conversions.Units exposing (Celsius(..), Fahrenheit(..))
import Html exposing (Html, div, h1, hr, input, label, pre, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)



-- MODEL


type Validation a
    = Valid a
    | Invalid a


type alias Unvalidate a =
    { klass : String, value : a }


unValidate : Validation a -> Unvalidate a
unValidate validation =
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
    { celsius = Valid "0"
    , fahrenheit = Valid "32"
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
                    { celsius = Invalid cStr
                    , fahrenheit = model.fahrenheit
                    }

                Just vc ->
                    { celsius = Valid cStr
                    , fahrenheit = Valid <| Fahrenheit.toString <| Celsius.toFahrenheit (MkCelsius vc)
                    }

        FahrenheitChanged fStr ->
            case String.toFloat fStr of
                Nothing ->
                    { celsius = model.celsius
                    , fahrenheit = Invalid fStr
                    }

                Just vf ->
                    { celsius = Valid <| Celsius.toString <| Fahrenheit.toCelsius (MkFahrenheit vf)
                    , fahrenheit = Valid fStr
                    }



-- VIEW


background : String -> Html.Attribute msg
background klass =
    if klass == "error" then
        style "background-color" "red"

    else
        style "" ""


temperatureInput : String -> (String -> msg) -> Unvalidate String -> Html msg
temperatureInput txt msg unvalid =
    div []
        [ label [ style "margin-right" "10px" ] [ text txt ]
        , input
            [ value unvalid.value
            , background unvalid.klass
            , onInput msg
            ]
            []
        ]


view : Model -> Html Msg
view model =
    let
        unvalidCelsius =
            unValidate model.celsius

        unvalidFahrenheit =
            unValidate model.fahrenheit
    in
    div [ style "margin-left" "20px", style "zoom" "1.3" ]
        [ pre [] [ text <| Debug.toString model ]
        , h1 [] [ text "Temp converter (Elm)" ]
        , div [ style "display" "flex", style "gap" "30px" ]
            [ temperatureInput "Celsius" CelsiusChanged unvalidCelsius
            , temperatureInput "Fahrenheit" FahrenheitChanged unvalidFahrenheit
            ]
        , hr [ style "margin" "30px 0" ] []
        , case ( model.celsius, model.fahrenheit ) of
            ( Valid cStr, Valid fStr ) ->
                div [] [ text <| cStr ++ "C° = " ++ fStr ++ "F°" ]

            _ ->
                div [ style "color" "red" ] [ text "Cannot compute due to bad data!" ]
        ]



-- BOOTSTRAP


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
