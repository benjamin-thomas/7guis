module Main (main) where

import Prelude

import Data.Array (deleteAt, length, modifyAt, snoc, (!!))
import Data.Foldable (maximumBy)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (IProp, prop)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Data = Loaded (Array Person)

derive instance Generic Data _

instance Show Data where
  show = genericShow

type Form =
  { term :: String
  , firstName :: String
  , lastName :: String
  , selectedPersonIdx :: Int
  }

type State =
  { data :: Data
  , form :: Form
  }

data Action
  = Initialize
  | TermChanged String
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
  { id :: Int
  , firstName :: String
  , lastName :: String
  }

size :: forall r i. Int -> IProp (size :: Int | r) i
size = prop (H.PropName "size")

component :: forall query output m. MonadEffect m => H.Component query Unit output m
component =
  H.mkComponent
    { initialState
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    , render
    }
  where
  initialState :: Unit -> State
  initialState _ =
    let
      initialDb :: Array Person
      initialDb =
        [ { id: 1
          , firstName: "John"
          , lastName: "Doe"
          }
        , { id: 2
          , firstName: "Jane"
          , lastName: "Doe"
          }
        , { id: 3
          , firstName: "Bob"
          , lastName: "Smith"
          }
        , { id: 4
          , firstName: "Alice"
          , lastName: "Smith"
          }
        ]
    in
      { data: Loaded initialDb
      , form:
          { term: ""
          , firstName: ""
          , lastName: ""
          , selectedPersonIdx: 1
          }
      }

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction action = do
    log $ "Action: " <> show action
    case action of
      Initialize ->
        H.get >>= \state ->
          handleAction $ PersonSelected state.form.selectedPersonIdx

      TermChanged str ->
        H.modify_ _ { form { term = str } }

      FirstNameChanged str ->
        H.modify_ _ { form { firstName = str } }

      LastNameChanged str ->
        H.modify_ _ { form { lastName = str } }

      PersonSelected i ->
        H.modify_
          \state ->
            case state.data of
              Loaded db ->
                case db !! i of
                  Nothing ->
                    state { form { selectedPersonIdx = i } }
                  Just person ->
                    state
                      { form
                          { selectedPersonIdx = i
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
                maxId = _.id <$> maximumBy (\a b -> compare (a.id) (b.id)) db

                newId :: Int
                newId = fromMaybe 1 $ ((+) 1) <$> maxId

                newDb :: Array Person
                newDb = snoc db
                  { id: newId
                  , firstName: state.form.firstName
                  , lastName: state.form.lastName
                  }
              in
                state { data = Loaded newDb, form { selectedPersonIdx = newId - 1 } }

      UpdateBtnClicked ->
        H.modify_ \state ->
          case state.data of
            Loaded db ->
              let
                newDb :: Array Person
                newDb =
                  fromMaybe db $
                    modifyAt
                      (state.form.selectedPersonIdx)
                      ( _
                          { firstName = state.form.firstName
                          , lastName = state.form.lastName
                          }
                      )
                      db
              in
                state { data = Loaded newDb }

      DeleteBtnClicked -> do
        H.modify_ \state ->
          case state.data of
            Loaded db ->
              let
                newDb :: Array Person
                newDb =
                  fromMaybe db $
                    deleteAt (state.form.selectedPersonIdx) db

              in
                state { data = Loaded newDb }

        state <- H.get
        case state.data of
          Loaded db ->
            handleAction
              $ PersonSelected
              $ min
                  (max 0 $ length db - 1)
                  state.form.selectedPersonIdx

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.h1_ [ HH.text "CRUD example" ]
      , case state.data of
          Loaded db ->
            renderLoaded db state.form
      , HH.div [ HP.style "margin-top:40px" ] [ HH.code_ [ HH.text $ show state ] ]
      ]

  renderLoaded :: Array Person -> Form -> H.ComponentHTML Action () m
  renderLoaded db form =
    HH.div_
      [ HH.div [ HP.id "form" ]
          [ HH.div_
              [ HH.div [ HP.classes [ H.ClassName "left" ] ]
                  [ HH.div [ HP.id "prefix-section" ]
                      [ HH.label_ [ HH.text "Filter prefix:" ]
                      , HH.input
                          [ HP.value form.term
                          -- , HE.onValueChange TermChanged -- changes on blur
                          , HE.onValueInput TermChanged
                          ]
                      ]
                  , HH.div [ HP.id "people" ]
                      [ HH.select
                          [ size 8
                          , HE.onSelectedIndexChange PersonSelected
                          , HP.selectedIndex form.selectedPersonIdx
                          , HP.autofocus true
                          ]
                          ( map
                              ( \person -> HH.option_
                                  [ HH.text $ person.firstName <> " " <> person.lastName
                                  ]
                              )
                              db
                          )
                      ]
                  ]

              , HH.div [ HP.classes [ H.ClassName "right" ] ]
                  [ HH.div_
                      [ HH.label_ [ HH.text "Name:" ]
                      , HH.input
                          [ HP.value form.firstName
                          , HE.onValueInput FirstNameChanged
                          ]
                      ]
                  , HH.div_
                      [ HH.label_ [ HH.text "Surname:" ]
                      , HH.input
                          [ HP.value form.lastName
                          , HE.onValueInput LastNameChanged
                          ]
                      ]

                  ]
              ]

          , HH.div [ HP.id "buttons" ]
              [ HH.button [ HE.onClick (const CreateBtnClicked) ] [ HH.text "Create" ]
              , HH.button [ HE.onClick (const UpdateBtnClicked) ] [ HH.text "Update" ]
              , HH.button [ HE.onClick (const DeleteBtnClicked) ] [ HH.text "Delete" ]
              ]
          ]
      ]