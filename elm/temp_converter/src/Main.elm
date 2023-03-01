module Main exposing (main)

import Browser
import Html exposing (Html, div, input, label, span, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)



-- UTILS


toCelsius : Float -> Float
toCelsius f =
    (f - 32) * 5 / 9


toFahrenheit : Float -> Float
toFahrenheit c =
    c * 9 / 5 + 32


maybeConvert : (Float -> Float) -> String -> Maybe String
maybeConvert f str =
    String.toFloat str |> Maybe.map (f >> String.fromFloat)



-- MODEL


type Celsius
    = Celsius String


type Fahrenheit
    = Fahrenheit String


type Model
    = NoUserInput
    | Converted Celsius Fahrenheit
    | BadInputCelsius Celsius
    | BadInputFahrenheit Fahrenheit


init : Model
init =
    NoUserInput



-- UPDATE


type Msg
    = CelsiusChanged String
    | FahrenheitChanged String


update : Msg -> Model -> Model
update msg _ =
    case msg of
        CelsiusChanged celsius ->
            case maybeConvert toFahrenheit celsius of
                Nothing ->
                    BadInputCelsius (Celsius celsius)

                Just fahrenheit ->
                    Converted (Celsius celsius) (Fahrenheit fahrenheit)

        FahrenheitChanged fahrenheit ->
            case maybeConvert toCelsius fahrenheit of
                Nothing ->
                    BadInputFahrenheit (Fahrenheit fahrenheit)

                Just celsius ->
                    Converted (Celsius celsius) (Fahrenheit fahrenheit)



-- VIEW


valueFor : Model -> { celsius : String, fahrenheit : String }
valueFor model =
    case model of
        NoUserInput ->
            { celsius = "", fahrenheit = "" }

        Converted (Celsius celsius) (Fahrenheit fahrenheit) ->
            { celsius = celsius, fahrenheit = fahrenheit }

        BadInputCelsius (Celsius celsius) ->
            { celsius = celsius, fahrenheit = "" }

        BadInputFahrenheit (Fahrenheit fahrenheit) ->
            { celsius = "", fahrenheit = fahrenheit }


showInvalid : Model -> Html msg
showInvalid model =
    case model of
        BadInputCelsius _ ->
            text "Celsius: bad input!"

        BadInputFahrenheit _ ->
            text "Fahrenheit: bad input!"

        NoUserInput ->
            text ""

        Converted _ _ ->
            text ""


view : Model -> Html Msg
view model =
    div []
        [ span []
            [ input [ value (valueFor model).celsius, onInput CelsiusChanged ] []
            , label [] [ text "Celsius" ]
            ]
        , span [] [ text " = " ]
        , span []
            [ input [ value (valueFor model).fahrenheit, onInput FahrenheitChanged ] []
            , label [] [ text "Fahrenheit" ]
            ]
        , div []
            [ showInvalid model
            ]
        ]



-- BOOTSTRAP


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
