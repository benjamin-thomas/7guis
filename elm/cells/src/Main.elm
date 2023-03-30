module Main exposing (..)

import Browser
import Html as H
import Html.Attributes as A



-- elm-live ./src/Main.elm -- --debug


type alias Model =
    ()


init : Model
init =
    ()


type Msg
    = NOOP


update : Msg -> Model -> Model
update msg model =
    case msg of
        NOOP ->
            model


view : Model -> H.Html msg
view _ =
    H.div []
        [ H.table []
            [ H.thead []
                [ H.tr []
                    [ H.th [] []
                    , H.th [] [ H.text "A" ]
                    , H.th [] [ H.text "B" ]
                    , H.th [] [ H.text "C" ]
                    , H.th [] [ H.text "D" ]
                    ]
                ]
            , H.tbody []
                [ H.tr []
                    [ H.td [] [ H.text "1" ]
                    , H.td [ A.contenteditable True ] [ H.text "A val" ]
                    , H.td [ A.contenteditable True ] [ H.text "B val" ]
                    , H.td [ A.contenteditable True ] [ H.text "C val" ]
                    , H.td [ A.contenteditable True ] [ H.text "D val" ]
                    ]
                , H.tr []
                    [ H.td [] [ H.text "2" ]
                    , H.td [ A.contenteditable True ] [ H.text "A val" ]
                    , H.td [ A.contenteditable True ] [ H.text "B val" ]
                    , H.td [ A.contenteditable True ] [ H.text "C val" ]
                    , H.td [ A.contenteditable True ] [ H.text "D val" ]
                    ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
