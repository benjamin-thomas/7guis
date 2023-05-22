module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, input, label, pre, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)
import Round



-- UTILS


toCelsius : Fahrenheit -> Celsius
toCelsius (Fahrenheit f) =
    Celsius <| (f - 32) * 5 / 9


toFahrenheit : Celsius -> Fahrenheit
toFahrenheit (Celsius c) =
    Fahrenheit (c * 9 / 5 + 32)


celsiusFromString : String -> Maybe Celsius
celsiusFromString s =
    s |> String.toFloat |> Maybe.map Celsius


fahreheitFromString : String -> Maybe Fahrenheit
fahreheitFromString s =
    s |> String.toFloat |> Maybe.map Fahrenheit


fromFloatPrecision2 : Float -> String
fromFloatPrecision2 =
    -- String.fromFloat gives undesirable results (i.e. 3.3333333...)
    Round.round 2


stringFromFahrenheit : Fahrenheit -> String
stringFromFahrenheit (Fahrenheit val) =
    fromFloatPrecision2 val


stringFromCelsius : Celsius -> String
stringFromCelsius (Celsius val) =
    fromFloatPrecision2 val



-- MODEL


type Celsius
    = Celsius Float


type Fahrenheit
    = Fahrenheit Float


type alias Model =
    { celsius : { input : Maybe String, conv : Maybe Celsius }
    , fahrenheit : { input : Maybe String, conv : Maybe Fahrenheit }
    }


init : Model
init =
    { celsius = { input = Nothing, conv = Nothing }
    , fahrenheit = { input = Nothing, conv = Nothing }
    }



-- UPDATE


type Msg
    = CelsiusChanged String
    | FahrenheitChanged String


nonEmpty : String -> Maybe String
nonEmpty s =
    case s of
        "" ->
            Nothing

        x ->
            Just x


update : Msg -> Model -> Model
update msg model =
    case msg of
        CelsiusChanged str ->
            let
                celsiusInp =
                    nonEmpty str

                celsiusConv =
                    celsiusInp |> Maybe.andThen celsiusFromString

                fahrenheitConv =
                    celsiusConv |> Maybe.map toFahrenheit

                fahrenheitInp =
                    fahrenheitConv |> Maybe.map stringFromFahrenheit
            in
            { model
                | celsius = { input = celsiusInp, conv = celsiusConv }
                , fahrenheit = { input = fahrenheitInp, conv = fahrenheitConv }
            }

        FahrenheitChanged str ->
            let
                fahrenheitInp =
                    nonEmpty str

                fahrenheitConv =
                    fahrenheitInp |> Maybe.andThen fahreheitFromString

                celsiusConv =
                    fahrenheitConv |> Maybe.map toCelsius

                celsiusInp =
                    celsiusConv |> Maybe.map stringFromCelsius
            in
            { model
                | celsius = { input = celsiusInp, conv = celsiusConv }
                , fahrenheit = { input = fahrenheitInp, conv = fahrenheitConv }
            }



-- VIEW


showInvalid : String -> { input : Maybe input, conv : Maybe converted } -> Html msg
showInvalid errMsg { input, conv } =
    if input /= Nothing && conv == Nothing then
        div [] [ text errMsg ]

    else
        text ""


view : Model -> Html Msg
view model =
    div [ style "margin-left" "20px" ]
        [ pre [] [ text <| Debug.toString model ]
        , h1 [] [ text "Temp converter (Elm)" ]
        , div [ style "display" "flex" ]
            [ div []
                [ input
                    [ value (model.celsius.input |> Maybe.withDefault "")
                    , style "background-color"
                        (if model.celsius.input /= Nothing && model.celsius.conv == Nothing then
                            "red"

                         else
                            ""
                        )
                    , onInput CelsiusChanged
                    ]
                    []
                , label [] [ text "Celsius" ]
                , showInvalid "Bad celsius" model.celsius
                ]
            , div []
                [ input
                    [ value (model.fahrenheit.input |> Maybe.withDefault "")
                    , style "background-color"
                        (if model.fahrenheit.input /= Nothing && model.fahrenheit.conv == Nothing then
                            "red"

                         else
                            ""
                        )
                    , onInput FahrenheitChanged
                    ]
                    []
                , label [] [ text "Fahrenheit" ]
                , showInvalid "Bad fahrenheit" model.fahrenheit
                ]
            , div []
                []
            ]
        ]



-- BOOTSTRAP


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
