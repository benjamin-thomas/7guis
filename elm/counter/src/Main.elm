module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, span, text)
import Html.Attributes exposing (disabled, readonly, style, value)
import Html.Events exposing (onClick)


type alias Model =
    Int


init : Model
init =
    0


cssDisableSelection : List (Html.Attribute Msg)
cssDisableSelection =
    [ style "user-select" "none"
    , style "pointer-events" "none"
    ]


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ h1 []
                [ text "Counter example"
                ]

            -- Extra div required to prevent click events bubbling up to the h1
            , div []
                [ span
                    cssDisableSelection
                    [ input [ value (String.fromInt model), readonly True, disabled True ] []
                    ]
                , button [ onClick Inc ] [ text "count" ]
                ]
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
