module Main exposing (..)

import Browser
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode



{-
   elm-live --host=0.0.0.0 --start-page=index2.html src/Main.elm -- --output=build/main.js  --debug
   echo main.css | entr touch src/Main.elm
   json-server --host=$(hostname -I | awk '{print $1}') --watch db.json


   http GET localhost:3000/users
   http GET localhost:3000/users firstName==Hans
   http GET localhost:3000/users firstName_like==m
-}


type alias Person =
    { id : Int, firstName : String, lastName : String }


type alias UserSelection =
    Maybe Person


type alias Form =
    { firstName : String
    , lastName : String
    }


personToForm : Person -> Form
personToForm p =
    { firstName = p.firstName, lastName = p.lastName }


type Model
    = Loading
    | Errored String
    | Loaded (List Person) UserSelection Form


type Msg
    = GotUsers (Result Http.Error (List Person))
    | SelectedPerson String
    | ClickedOnCreate
    | ClickedOnUpdate
    | ClickedOnDelete
    | NOOP
    | UserCreated (Result Http.Error ())
    | UserUpdated (Result Http.Error ())
    | UserDeleted (Result Http.Error ())
    | ChangedFirstName String
    | ChangedLastName String


personDecoder : Decoder Person
personDecoder =
    D.map3 Person
        (D.field "id" D.int)
        (D.field "firstName" D.string)
        (D.field "lastName" D.string)


getUsers : Cmd Msg
getUsers =
    Http.get
        { url = "http://192.168.52.100:3000/users"
        , expect = Http.expectJson GotUsers (D.list personDecoder)
        }


buildBody : Form -> Http.Body
buildBody f =
    Http.jsonBody <|
        Json.Encode.object
            [ ( "firstName", Json.Encode.string f.firstName )
            , ( "lastName", Json.Encode.string f.lastName )
            ]


postUsers : Form -> Cmd Msg
postUsers f =
    Http.post
        { url = "http://192.168.52.100:3000/users"
        , body = buildBody f
        , expect = Http.expectWhatever UserCreated
        }


deleteUser : Int -> Cmd Msg
deleteUser id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://192.168.52.100:3000/users/" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever UserUpdated
        , timeout = Nothing
        , tracker = Nothing
        }


putUser : Int -> Form -> Cmd Msg
putUser id form =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "http://192.168.52.100:3000/users/" ++ String.fromInt id
        , body = buildBody form
        , expect = Http.expectWhatever UserDeleted
        , timeout = Nothing
        , tracker = Nothing
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getUsers )


initForm : Maybe Person -> { firstName : String, lastName : String }
initForm selected =
    selected
        |> Maybe.map personToForm
        |> Maybe.withDefault { firstName = "", lastName = "" }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Loading, GotUsers (Ok users) ) ->
            ( Loaded users Nothing (initForm Nothing), Cmd.none )

        ( Loading, GotUsers (Err error) ) ->
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

        ( Loaded people _ _, SelectedPerson s ) ->
            let
                filterById n =
                    List.filter (\p -> p.id == n) people

                maybePerson : Maybe Person
                maybePerson =
                    String.toInt s
                        |> Maybe.map filterById
                        |> Maybe.andThen List.head
            in
            ( Loaded people maybePerson (initForm maybePerson), Cmd.none )

        ( Loaded _ _ f, ClickedOnCreate ) ->
            if f.firstName /= "" && f.lastName /= "" then
                ( model, postUsers f )

            else
                ( model, Cmd.none )

        ( Loaded _ _ _, UserCreated res ) ->
            case res of
                Ok () ->
                    ( Loading, getUsers )

                Err _ ->
                    Debug.todo "display error message"

        ( Loaded _ _ _, UserUpdated res ) ->
            case res of
                Ok () ->
                    ( Loading, getUsers )

                Err _ ->
                    Debug.todo "display error message"

        ( Loaded _ _ _, UserDeleted res ) ->
            case res of
                Ok () ->
                    ( Loading, getUsers )

                Err _ ->
                    Debug.todo "display error message"

        ( Loaded people maybeSelected form, ChangedFirstName s ) ->
            ( Loaded people maybeSelected { form | firstName = s }, Cmd.none )

        ( Loaded people maybeSelected form, ChangedLastName s ) ->
            ( Loaded people maybeSelected { form | lastName = s }, Cmd.none )

        ( Loaded _ maybeSelected form, ClickedOnUpdate ) ->
            case maybeSelected of
                Nothing ->
                    ( model, Cmd.none )

                Just person ->
                    ( model, putUser person.id form )

        ( Loaded _ maybeSelected _, ClickedOnDelete ) ->
            case maybeSelected of
                Nothing ->
                    ( model, Cmd.none )

                Just person ->
                    ( model, deleteUser person.id )

        ( Loaded _ _ _, NOOP ) ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


personOption : Person -> Html msg
personOption p =
    H.option
        [ A.value (String.fromInt p.id) ]
        [ H.text <|
            String.join ", " [ p.lastName, p.firstName ]
        ]


onClickStop : a -> Attribute a
onClickStop msg =
    E.custom "click"
        (D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )


type InputDataSource
    = UserSelect UserSelection
    | UserEdit Form


viewPeople : List Person -> UserSelection -> Form -> Html Msg
viewPeople people _ form =
    let
        left : Html Msg
        left =
            H.div []
                [ H.div []
                    [ H.label [] [ H.text "Filter prefix:" ]
                    , H.input [ A.id "filter" ] []
                    ]
                , H.div [ A.class "listbox spacer", onClickStop NOOP ]
                    [ H.select
                        [ A.size (List.length people)
                        , E.onInput SelectedPerson
                        , onClickStop NOOP
                        ]
                        (List.map personOption people)
                    ]
                ]

        right : Html Msg
        right =
            H.div
                [ A.id "new-person" ]
                [ H.div [ A.class "spacer" ]
                    [ H.label [] [ H.text "Name:" ]
                    , H.input [ A.value form.firstName, E.onInput ChangedFirstName ] []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Surname:" ]
                    , H.input [ A.value form.lastName, E.onInput ChangedLastName ] []
                    ]
                ]
    in
    H.div [ A.class "container" ]
        [ H.div [ A.id "widget" ]
            [ H.div [ A.class "lr" ] [ left, right ]
            , H.div [ A.class "buttons" ]
                [ H.button [ onClickStop ClickedOnCreate ] [ H.text "Create" ]
                , H.button [ onClickStop ClickedOnUpdate ] [ H.text "Update" ]
                , H.button [ onClickStop ClickedOnDelete ] [ H.text "Delete" ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    H.div []
        [ case model of
            Loading ->
                H.text "Loading..."

            Loaded people maybeSelected f ->
                viewPeople people maybeSelected f

            Errored errMsg ->
                H.text errMsg
        , H.pre [] [ H.text (Debug.toString model) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
