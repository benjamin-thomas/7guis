module Main (main) where

import Prelude

import Data.Array ((!!))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen (PropName(..))
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

type State =
  { term :: String
  , firstName :: String
  , lastName :: String
  , selectedPersonIdx :: Int
  }

data Action
  = TermChanged String
  | FirstNameChanged String
  | LastNameChanged String
  | PersonSelected Int

derive instance Generic Action _

instance Show Action where
  show = genericShow

type Person =
  { id :: Int
  , firstName :: String
  , lastName :: String
  }

db :: Array Person
db =
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
  ]

size :: forall r i. Int -> IProp (size :: Int | r) i
size = prop (PropName "size")

component :: forall query output m. MonadEffect m => H.Component query Unit output m
component =
  H.mkComponent
    { initialState
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    , render
    }
  where
  initialState :: Unit -> State
  initialState _ =
    { term: ""
    , firstName: ""
    , lastName: ""
    , selectedPersonIdx: 1
    }

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction action = do
    log $ "Action: " <> show action
    case action of
      TermChanged str -> H.modify_ _ { term = str }
      FirstNameChanged str -> H.modify_ _ { firstName = str }
      LastNameChanged str -> H.modify_ _ { lastName = str }
      PersonSelected i ->
        H.modify_ \state ->
          case db !! i of
            Nothing ->
              state { selectedPersonIdx = i }
            Just person ->
              state
                { selectedPersonIdx = i
                , firstName = person.firstName
                , lastName = person.lastName
                }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.code_ [ HH.text $ show state ]
      , HH.h1_ [ HH.text "CRUD example" ]
      , HH.div [ HP.id "form" ]
          [ HH.div_
              [ HH.div [ HP.classes [ H.ClassName "left" ] ]
                  [ HH.div [ HP.id "prefix-section" ]
                      [ HH.label_ [ HH.text "Filter prefix:" ]
                      , HH.input
                          [ HP.value state.term
                          -- , HE.onValueChange TermChanged -- changes on blur
                          , HE.onValueInput TermChanged
                          ]
                      ]
                  , HH.div [ HP.id "people" ]
                      [ HH.select
                          [ size 8
                          , HE.onSelectedIndexChange PersonSelected
                          , HP.selectedIndex state.selectedPersonIdx
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
                          [ HP.value state.firstName
                          , HE.onValueInput FirstNameChanged
                          ]
                      ]
                  , HH.div_
                      [ HH.label_ [ HH.text "Surname:" ]
                      , HH.input
                          [ HP.value state.lastName
                          , HE.onValueInput LastNameChanged
                          ]
                      ]

                  ]
              ]

          , HH.div [ HP.id "buttons" ]
              [ HH.button [] [ HH.text "Create" ]
              , HH.button [] [ HH.text "Update" ]
              , HH.button [] [ HH.text "Delete" ]
              ]
          ]
      ]