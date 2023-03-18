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
   json-server --watch db.json
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
    | Loaded (List Person) UserSelection
    | Editing (List Person) Form


type Msg
    = GotUsers (Result Http.Error (List Person))
    | SelectedPerson String
    | EnterEditMode
    | ClickedOnCreate
    | ClickedOnUpdate
    | ClickedOnDelete
    | ClickedOutside
    | NOOP
    | UserPosted (Result Http.Error ())
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
        , expect = Http.expectWhatever UserPosted
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getUsers )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Loading, GotUsers (Ok users) ) ->
            ( Loaded users Nothing, Cmd.none )

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

        ( Loaded people _, SelectedPerson s ) ->
            let
                filterById n =
                    List.filter (\p -> p.id == n) people

                maybePerson : Maybe Person
                maybePerson =
                    String.toInt s
                        |> Maybe.map filterById
                        |> Maybe.andThen List.head
            in
            ( Loaded people maybePerson, Cmd.none )

        ( Loaded people _, ClickedOutside ) ->
            ( Loaded people Nothing, Cmd.none )

        ( Loaded people selected, EnterEditMode ) ->
            let
                initForm =
                    selected
                        |> Maybe.map personToForm
                        |> Maybe.withDefault { firstName = "", lastName = "" }
            in
            ( Editing people initForm, Cmd.none )

        ( Loaded _ _, ClickedOnCreate ) ->
            ( model, Cmd.none )

        {-
           FIXME: Editing should be:

               Editing (List Person) [maybe selected, need the personID] Form
        -}
        ( Editing _ f, ClickedOnUpdate ) ->
            if f.firstName /= "" && f.lastName /= "" then
                ( model, postUsers f )

            else
                ( model, Cmd.none )

        ( Editing _ _, UserPosted res ) ->
            case res of
                Err _ ->
                    Debug.todo "display error message"

                Ok () ->
                    ( Loading, getUsers )

        ( Editing people form, ChangedFirstName s ) ->
            ( Editing people { form | firstName = s }, Cmd.none )

        ( Editing people form, ChangedLastName s ) ->
            ( Editing people { form | lastName = s }, Cmd.none )

        ( Loaded _ _, ClickedOnDelete ) ->
            ( model, Cmd.none )

        ( Loaded _ _, NOOP ) ->
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


viewPeople : List Person -> InputDataSource -> Bool -> Html Msg
viewPeople people inputDataSource disableSelectMenu =
    let
        valueFor f =
            case inputDataSource of
                UserSelect maybeSelected ->
                    maybeSelected |> Maybe.map (personToForm >> f) |> Maybe.withDefault ""

                UserEdit form ->
                    f form

        left : Html Msg
        left =
            H.div []
                [ H.div []
                    [ H.label [] [ H.text "Filter prefix:" ]
                    , H.input [ A.id "filter" ] []
                    ]
                , H.div [ A.class "listbox spacer" ]
                    [ H.select
                        [ A.size (List.length people)
                        , A.disabled disableSelectMenu
                        , E.onInput SelectedPerson
                        , onClickStop NOOP
                        ]
                        (List.map personOption people)
                    ]
                ]

        right : Html Msg
        right =
            H.div
                [ A.id "new-person", E.onClick EnterEditMode ]
                [ H.div [ A.class "spacer" ]
                    [ H.label [] [ H.text "Name:" ]
                    , H.input [ A.value <| valueFor .firstName, E.onInput ChangedFirstName ] []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Surname:" ]
                    , H.input [ A.value <| valueFor .lastName, E.onInput ChangedLastName ] []
                    ]
                ]
    in
    H.div [ A.class "container" ]
        [ H.div [ A.id "widget", E.onClick ClickedOutside ]
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

            Loaded people maybeSelected ->
                viewPeople people (UserSelect maybeSelected) False

            Editing people form ->
                viewPeople people (UserEdit form) True

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
