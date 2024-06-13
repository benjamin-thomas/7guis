module Main (main) where

import Prelude

import Effect (Effect)
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
  }

data Action
  = TermChanged String
  | FirstNameChanged String
  | LastNameChanged String

size :: forall r i. Int -> IProp (size :: Int | r) i
size = prop (PropName "size")

component :: forall query output m. H.Component query Unit output m
component =
  H.mkComponent
    { initialState
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    , render
    }
  where
  initialState :: Unit -> State
  initialState _ = { term: "", firstName: "", lastName: "" }

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    TermChanged str -> H.modify_ _ { term = str }
    FirstNameChanged str -> H.modify_ _ { firstName = str }
    LastNameChanged str -> H.modify_ _ { lastName = str }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.h1_ [ HH.text "CRUD example" ]
      , HH.div [ HP.id "form" ]
          [ HH.div_
              [ HH.div [ HP.classes [ H.ClassName "left" ] ]
                  [ HH.div [ HP.id "prefix-section" ]
                      [ HH.label_ [ HH.text "Filter prefix:" ]
                      , HH.input
                          [ HP.value state.term
                          ]
                      ]
                  , HH.div [ HP.id "people" ]
                      [ HH.select
                          [ size 8
                          ]
                          [ HH.option_ [ HH.text "John Doe" ]
                          , HH.option_ [ HH.text "Jane Doe" ]
                          , HH.option_ [ HH.text "Bob Smith" ]
                          ]
                      ]
                  ]

              , HH.div [ HP.classes [ H.ClassName "right" ] ]
                  [ HH.div_
                      [ HH.label_ [ HH.text "Name:" ]
                      , HH.input
                          [ HP.value state.firstName
                          ]
                      ]
                  , HH.div_
                      [ HH.label_ [ HH.text "Surname:" ]
                      , HH.input
                          [ HP.value state.lastName
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