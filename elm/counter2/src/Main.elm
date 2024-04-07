port module Main exposing (main)

import Html exposing (text)
import Json.Encode as E
import Web
import Web.Dom


port toJs : E.Value -> Cmd event


port fromJs : (E.Value -> event) -> Sub event


type alias Model =
    Int


type Msg
    = Inc
    | Dec


update : Msg -> Model -> Model
update msg model =
    case msg of
        Inc ->
            model + 1

        Dec ->
            max 0 (model - 1)


view : Model -> Web.Dom.Node Msg
view model =
    let
        div =
            Web.Dom.element "div"

        span =
            Web.Dom.element "span"

        h1 =
            Web.Dom.element "h1"

        text =
            Web.Dom.text

        btn : future -> String -> Web.Dom.Node future
        btn msg text_ =
            Web.Dom.element "button"
                [ Web.Dom.listenTo "click" |> Web.Dom.modifierFutureMap (\_ -> msg) ]
                [ text text_ ]

        padding =
            Web.Dom.style "padding" "0px 10px"
    in
    div []
        [ h1 []
            [ Web.Dom.text "Counter example"
            ]
        , div []
            [ span []
                [ btn Dec "DEC"
                , span [ padding ] [ text <| String.fromInt model ]
                , btn Inc "INC"
                ]
            ]
        ]


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


interface : Model -> Web.Interface Model
interface model =
    [ Web.Dom.render (view model) ]
        |> Web.interfaceBatch
        |> Web.interfaceFutureMap
            (flip update model)


main : Web.Program Model
main =
    Web.program
        { initialState = 0
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }
