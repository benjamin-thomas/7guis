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


background : String -> Html.Attribute msg
background klass =
    -- I would normally just insert class name
    if klass == "error" then
        style "background-color" "red"

    else
        style "" ""


temperature : String -> (String -> msg) -> Unvalidate String -> Html msg
temperature txt msg unvalid =
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
            unvalidate model.celsius

        unvalidFahrenheit =
            unvalidate model.fahrenheit
    in
    div [ style "margin-left" "20px", style "zoom" "1.3" ]
        [ pre [] [ text <| Debug.toString model ]
        , h1 [] [ text "Temp converter (Elm)" ]
        , div [ style "display" "flex", style "gap" "30px" ]
            [ temperature "Celsius" CelsiusChanged unvalidCelsius
            , temperature "Fahrenheit" FahrenheitChanged unvalidFahrenheit
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
