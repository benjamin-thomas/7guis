module Main exposing (main)

{-
   Run with:

     elm-live --host=0.0.0.0 src/Main.elm
-}

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
        [ h1 [] [ text "Counter example" ]
        , span
            cssDisableSelection
            [ input [ value (String.fromInt model), readonly True, disabled True ] []
            ]
        , button [ onClick Inc ] [ text "count" ]
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
