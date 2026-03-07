module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, span, text)
import Html.Attributes exposing (class, readonly, value)
import Html.Events exposing (onClick)


type alias Model =
    Int


init : Model
init =
    0


view : Model -> Html Msg
view model =
    div [ class "task-container" ]
        [ h1 [] [ text "Counter" ]
        , div [ class "card counter-controls" ]
            [ input [ class "input counter-display", value (String.fromInt model), readonly True ] []
            , button [ onClick Inc ] [ text "Count" ]
            ]
        ]


type Msg
    = Inc


update : Msg -> Model -> Model
update msg model =
    case msg of
        Inc ->
            model + 1


main : Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }
