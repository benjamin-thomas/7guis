module Main exposing (main)

import Browser
import Conversions.Celsius
import Conversions.Fahrenheit
import Conversions.Units exposing (Celsius, Fahrenheit)
import Html exposing (Html, div, h1, input, label, pre, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)



-- MODEL


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
                    celsiusInp |> Maybe.andThen Conversions.Celsius.fromString

                fahrenheitConv =
                    celsiusConv |> Maybe.map Conversions.Celsius.toFahrenheit

                fahrenheitInp =
                    fahrenheitConv |> Maybe.map Conversions.Fahrenheit.toString
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
                    fahrenheitInp |> Maybe.andThen Conversions.Fahrenheit.fromString

                celsiusConv =
                    fahrenheitConv |> Maybe.map Conversions.Fahrenheit.toCelsius

                celsiusInp =
                    celsiusConv |> Maybe.map Conversions.Celsius.toString
            in
            { model
                | celsius = { input = celsiusInp, conv = celsiusConv }
                , fahrenheit = { input = fahrenheitInp, conv = fahrenheitConv }
            }



-- VIEW


isInvalid : { input : Maybe input, conv : Maybe converted } -> Bool
isInvalid { input, conv } =
    input /= Nothing && conv == Nothing


background : { input : Maybe input, conv : Maybe converted } -> Html.Attribute msg
background result =
    if isInvalid result then
        style "background-color" "red"

    else
        style "" ""


showInvalid : String -> { input : Maybe input, conv : Maybe converted } -> Html msg
showInvalid errMsg result =
    if isInvalid result then
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
                    , background model.celsius
                    , onInput CelsiusChanged
                    ]
                    []
                , label [] [ text "Celsius" ]
                , showInvalid "Bad celsius" model.celsius
                ]
            , div []
                [ input
                    [ value (model.fahrenheit.input |> Maybe.withDefault "")
                    , background model.fahrenheit
                    , onInput CelsiusChanged
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
