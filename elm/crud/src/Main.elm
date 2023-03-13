module Main exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as D exposing (Decoder)



{-
   elm-live --host=0.0.0.0 --start-page=index2.html src/Main.elm -- --output=build/main.js  --debug
   echo main.css | entr touch src/Main.elm
   json-server --watch db.json


   http GET localhost:3000/users
   http GET localhost:3000/users firstName==Hans
   http GET localhost:3000/users firstName_like==m
-}


type alias Person =
    { id : Int, firstName : String, lastName : String }


type alias UserSelection =
    Maybe Person


type Model
    = Loading
    | Loaded (List Person) UserSelection
    | Errored String


type Msg
    = GotUsers (Result Http.Error (List Person))
    | SelectedPerson String
    | ClickedCreate
    | ClickedUpdate
    | ClickedDelete


personDecoder : Decoder Person
personDecoder =
    D.map3 Person
        (D.field "id" D.int)
        (D.field "firstName" D.string)
        (D.field "lastName" D.string)


fetchUsers : Cmd Msg
fetchUsers =
    Http.get
        { url = "http://192.168.52.100:3000/users"
        , expect = Http.expectJson GotUsers (D.list personDecoder)
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, fetchUsers )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUsers (Ok users) ->
            ( Loaded users Nothing, Cmd.none )

        GotUsers (Err error) ->
            let
                newModel =
                    case error of
                        Http.BadUrl _ ->
                            Errored "BadUrl, programmer error"

                        Http.Timeout ->
                            Errored "The API did not respond"

                        Http.NetworkError ->
                            Errored "Check your network connection"

                        Http.BadStatus status ->
                            Errored ("The server respond with an error:" ++ String.fromInt status)

                        Http.BadBody reason ->
                            Errored ("I could not decode: " ++ reason)
            in
            ( newModel, Cmd.none )

        SelectedPerson s ->
            case model of
                Loaded people _ ->
                    let
                        filterById n =
                            List.filter (\p -> p.id == n) people

                        maybePerson : Maybe Person
                        maybePerson =
                            String.toInt s
                                |> Maybe.map filterById
                                |> Maybe.withDefault []
                                |> List.head
                    in
                    ( Loaded people maybePerson, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClickedCreate ->
            ( model, Cmd.none )

        ClickedUpdate ->
            ( model, Cmd.none )

        ClickedDelete ->
            ( model, Cmd.none )


personOption : Person -> Html msg
personOption p =
    H.option [ A.value <| String.fromInt p.id ] [ H.text <| String.join ", " [ p.lastName, p.firstName ] ]


viewPeople : List Person -> UserSelection -> Html Msg
viewPeople people maybeSelected =
    let
        valueFor f =
            maybeSelected |> Maybe.map f |> Maybe.withDefault ""
    in
    H.div [ A.class "container" ]
        [ H.div [ A.id "widget" ]
            [ H.div [ A.class "lr" ]
                [ H.div []
                    [ H.div []
                        [ H.label [] [ H.text "Filter prefix:" ]
                        , H.input [ A.id "filter" ] []
                        ]
                    , H.div [ A.class "listbox spacer" ]
                        [ H.select [ E.onInput SelectedPerson, A.size (List.length people) ] <|
                            List.map personOption people
                        ]
                    ]
                , H.div
                    [ A.id "new-person" ]
                    [ H.div [ A.class "spacer" ]
                        [ H.label [] [ H.text "Name:" ]
                        , H.input [ A.value <| valueFor .firstName ] []
                        ]
                    , H.div []
                        [ H.label [] [ H.text "Surname:" ]
                        , H.input [ A.value <| valueFor .lastName ] []
                        ]
                    ]
                ]
            , H.div [ A.class "buttons" ]
                [ H.button [ E.onClick ClickedCreate ] [ H.text "Create" ]
                , H.button [ E.onClick ClickedUpdate ] [ H.text "Update" ]
                , H.button [ E.onClick ClickedDelete ] [ H.text "Delete" ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            H.text "Loading..."

        Loaded people maybeSelected ->
            viewPeople people maybeSelected

        Errored errMsg ->
            H.text errMsg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }
