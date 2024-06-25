module Main (main) where

import Prelude

import Effect (Effect)
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

type State = { counter :: Int }

data Action
  = Inc
  | Dec

component :: forall query output m. H.Component query Unit output m
component =
  H.mkComponent
    { initialState
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    , render
    }
  where
  initialState :: Unit -> State
  initialState _ = { counter: 0 }

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Inc -> H.modify_ \state -> state { counter = state.counter + 1 }
    Dec -> H.modify_ \state -> state { counter = state.counter - 1 }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ HP.id "app" ]
      [ HH.h1_ [ HH.text "Counter example" ]
      , HH.div [ HP.class_ $ HH.ClassName "control" ]
          [ HH.input
              [ HP.value $ show state.counter
              , HP.disabled true
              ]
          , HH.button [ HE.onClick \_ -> Inc ] [ HH.text "count" ]
          ]
      ]