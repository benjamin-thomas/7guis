module Main exposing (main)

import Browser
import Html exposing (Html, div, input, label, p, span, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)



-- UTILS


toCelsius : Float -> Float
toCelsius f =
    (f - 32) * 5 / 9


toFahrenheit : Float -> Float
toFahrenheit c =
    c * 9 / 5 + 32


toNothingIfBlank : String -> Maybe String
toNothingIfBlank s =
    if String.trim s == "" then
        Nothing

    else
        Just s


maybeConvert : (Float -> Float) -> String -> Maybe String
maybeConvert f str =
    Maybe.map (f >> String.fromFloat) (String.toFloat str)



-- MODEL


type alias Model =
    { celsius : Maybe String, fahrenheit : Maybe String }


init : Model
init =
    { celsius = Nothing, fahrenheit = Nothing }



-- UPDATE


type Msg
    = CelsiusChanged String
    | FahrenheitChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        CelsiusChanged s ->
            { model | celsius = toNothingIfBlank s, fahrenheit = maybeConvert toFahrenheit s }

        FahrenheitChanged s ->
            { model | fahrenheit = toNothingIfBlank s, celsius = maybeConvert toCelsius s }



-- VIEW


warnOnInvalidInput : String -> Maybe String -> Html msg
warnOnInvalidInput name ms =
    case Maybe.andThen toNothingIfBlank ms of
        Nothing ->
            text ""

        Just s ->
            case String.toFloat s of
                Nothing ->
                    p [] [ text ("Invalid " ++ name ++ " input") ]

                Just _ ->
                    text ""


view : Model -> Html Msg
view model =
    div []
        [ span []
            [ input [ value (Maybe.withDefault "" model.celsius), onInput CelsiusChanged ] []
            , label [] [ text "Celsius" ]
            ]
        , span [] [ text " = " ]
        , span []
            [ input [ value (Maybe.withDefault "" model.fahrenheit), onInput FahrenheitChanged ] []
            , label [] [ text "Fahrenheit" ]
            ]
        , div []
            [ warnOnInvalidInput "celsius" model.celsius
            , warnOnInvalidInput "fahrenheit" model.fahrenheit
            ]
        ]



-- BOOTSTRAP


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
