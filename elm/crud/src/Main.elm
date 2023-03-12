module Main exposing (..)

import Html as H exposing (Html)
import Html.Attributes as A



{-
   elm-live --host=0.0.0.0 --start-page=index2.html src/Main.elm -- --output=build/main.js  --debug
   echo main.css | entr touch src/Main.elm
-}


type alias Person =
    { firstName : String, lastName : String }


fakeList : List Person
fakeList =
    [ { firstName = "Hans", lastName = "Emil" }
    , { firstName = "Max", lastName = "Mustermann" }
    , { firstName = "Roman", lastName = "Tisch" }
    ]


personOption : { a | lastName : String, firstName : String } -> Html msg
personOption p =
    H.option [] [ H.text <| String.join ", " [ p.lastName, p.firstName ] ]


main : Html msg
main =
    H.div [ A.class "container" ]
        [ H.div [ A.id "widget" ]
            [ H.div []
                [ H.div []
                    [ H.label [] [ H.text "Filter prefix:" ]
                    , H.input [ A.id "filter" ] []
                    ]
                , H.div [ A.class "listbox spacer" ]
                    [ H.select [ A.size (List.length fakeList) ] <|
                        List.map personOption fakeList
                    ]
                ]
            , H.div
                [ A.id "new-person" ]
                [ H.div [ A.class "spacer" ]
                    [ H.label [] [ H.text "Name:" ]
                    , H.input [ A.value "John" ] []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Surname:" ]
                    , H.input [ A.value "Doe" ] []
                    ]
                ]
            ]
        ]
