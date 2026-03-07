module Main
  ( main
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), stripPrefix, toLower)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Data = Loaded (Map Int Person)

derive instance Generic Data _

instance Show Data where
  show = genericShow

type Form =
  { term :: String
  , firstName :: String
  , lastName :: String
  , selectedPersonId :: Maybe Int
  }

type State =
  { data :: Data
  , form :: Form
  }

data Action
  = TermChanged String
  | FirstNameChanged String
  | LastNameChanged String
  | PersonSelected Int
  | CreateBtnClicked
  | UpdateBtnClicked
  | DeleteBtnClicked

derive instance Generic Action _

instance Show Action where
  show = genericShow

type Person =
  { firstName :: String
  , lastName :: String
  }

filterDb :: String -> Map Int Person -> Map Int Person
filterDb term = M.filter (\person -> startsWith term person.lastName)
  where
  startsWith :: String -> String -> Boolean
  startsWith pat src = isJust $ stripPrefix (Pattern $ toLower pat) (toLower src)

component :: forall query output m. MonadEffect m => H.Component query Unit output m
component =
  H.mkComponent
    { initialState
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    , render
    }
  where
  initialState :: Unit -> State
  initialState _ =
    let
      secondPerson = Tuple 2 { firstName: "Jane", lastName: "Doe" }

      initialDb :: Map Int Person
      initialDb = M.fromFoldable
        [ Tuple 1 { firstName: "John", lastName: "Doe" }
        , secondPerson
        , Tuple 3 { firstName: "Mark", lastName: "Twain" }
        ]
    in
      { data: Loaded initialDb
      , form:
          { term: ""
          , firstName: (snd secondPerson).firstName
          , lastName: (snd secondPerson).lastName
          , selectedPersonId: (Just $ fst secondPerson)
          }
      }

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction action = do
    log $ "Action: " <> show action
    case action of
      TermChanged str ->
        H.modify_ _ { form { term = str } }

      FirstNameChanged str ->
        H.modify_ _ { form { firstName = str } }

      LastNameChanged str ->
        H.modify_ _ { form { lastName = str } }

      PersonSelected personId -> H.modify_ \state ->
        case state.data of
          Loaded db ->
            case M.lookup personId db of
              Nothing ->
                state
                  { form
                      { selectedPersonId = Nothing
                      , firstName = ""
                      , lastName = ""
                      }
                  }
              Just person ->
                state
                  { form
                      { selectedPersonId = Just personId
                      , firstName = person.firstName
                      , lastName = person.lastName
                      }
                  }

      CreateBtnClicked ->
        H.modify_ \state ->
          case state.data of
            Loaded db ->
              let
                maxId :: Maybe Int
                maxId = _.key <$> M.findMax db

                newId :: Int
                newId = fromMaybe 1 $ ((+) 1) <$> maxId

                newDb :: Map Int Person
                newDb = db # M.insert newId
                  { firstName: state.form.firstName
                  , lastName: state.form.lastName
                  }
              in
                state
                  { data = Loaded newDb
                  , form { selectedPersonId = Just $ newId }
                  }

      UpdateBtnClicked ->
        H.modify_ \state ->
          case state.form.selectedPersonId of
            Nothing -> state
            Just selectedPersonId ->
              case state.data of
                Loaded db ->
                  let
                    newDb :: Map Int Person
                    newDb =
                      db # M.insert
                        selectedPersonId
                        { firstName: state.form.firstName
                        , lastName: state.form.lastName
                        }

                  in
                    state { data = Loaded newDb }

      DeleteBtnClicked -> do
        H.modify_ \state ->
          case state.form.selectedPersonId of
            Nothing -> state
            Just selectedPersonId ->
              case state.data of
                Loaded db ->
                  let
                    newDb :: Map Int Person
                    newDb =
                      db # M.delete selectedPersonId

                    firstPerson :: Maybe { key :: Int, value :: Person }
                    firstPerson = M.findMin (filterDb state.form.term newDb)

                  in
                    case firstPerson of
                      Nothing ->
                        state
                          { data = Loaded newDb
                          , form
                              { selectedPersonId = Nothing
                              , firstName = ""
                              , lastName = ""
                              }
                          }
                      Just { key, value } ->
                        state
                          { data = Loaded newDb
                          , form
                              { selectedPersonId = Just key
                              , firstName = value.firstName
                              , lastName = value.lastName
                              }

                          }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ HP.classes [ H.ClassName "task-container" ] ]
      [ HH.h1_ [ HH.text "CRUD" ]
      , case state.data of
          Loaded db ->
            renderLoaded (filterDb state.form.term db) state.form
      ]

  renderLoaded :: Map Int Person -> Form -> H.ComponentHTML Action () m
  renderLoaded db form =
    HH.div [ HP.classes [ H.ClassName "card", H.ClassName "crud" ] ]
      [ HH.div [ HP.classes [ H.ClassName "crud-columns" ] ]
          [ HH.div [ HP.classes [ H.ClassName "crud-col-left" ] ]
              [ HH.div [ HP.classes [ H.ClassName "crud-field" ] ]
                  [ HH.label [ HP.classes [ H.ClassName "crud-label" ] ] [ HH.text "Filter prefix:" ]
                  , HH.input
                      [ HP.classes [ H.ClassName "crud-input" ]
                      , HP.value form.term
                      , HE.onValueInput TermChanged
                      ]
                  ]
              , HH.div
                  [ HP.classes [ H.ClassName "crud-listbox" ]
                  , HP.tabIndex 0
                  , HP.attr (HH.AttrName "autofocus") ""
                  ]
                  ( map
                      ( \(Tuple personId person) ->
                          HH.div
                            [ HP.classes $
                                [ H.ClassName "crud-listbox-item" ] <>
                                  ( if Just personId == form.selectedPersonId then
                                      [ H.ClassName "crud-listbox-item--selected" ]
                                    else
                                      []
                                  )
                            , HE.onClick \_ -> PersonSelected personId
                            ]
                            [ HH.text $ person.lastName <> ", " <> person.firstName
                            ]
                      )
                      (M.toUnfoldable db)
                  )
              ]

          , HH.div [ HP.classes [ H.ClassName "crud-col-right" ] ]
              [ HH.div [ HP.classes [ H.ClassName "crud-field" ] ]
                  [ HH.label [ HP.classes [ H.ClassName "crud-label" ] ] [ HH.text "Name:" ]
                  , HH.input
                      [ HP.classes [ H.ClassName "crud-input" ]
                      , HP.value form.firstName
                      , HE.onValueInput FirstNameChanged
                      ]
                  ]
              , HH.div [ HP.classes [ H.ClassName "crud-field" ] ]
                  [ HH.label [ HP.classes [ H.ClassName "crud-label" ] ] [ HH.text "Surname:" ]
                  , HH.input
                      [ HP.classes [ H.ClassName "crud-input" ]
                      , HP.value form.lastName
                      , HE.onValueInput LastNameChanged
                      ]
                  ]
              ]
          ]
      , HH.div [ HP.classes [ H.ClassName "crud-buttons" ] ]
          [ HH.button [ HE.onClick (const CreateBtnClicked) ] [ HH.text "Create" ]
          , HH.button [ HE.onClick (const UpdateBtnClicked) ] [ HH.text "Update" ]
          , HH.button [ HE.onClick (const DeleteBtnClicked) ] [ HH.text "Delete" ]
          ]
      ]
