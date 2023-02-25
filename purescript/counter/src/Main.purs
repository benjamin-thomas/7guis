module Main where

import Prelude

import Effect (Effect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval, modify_)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (button, div_, h1_, input, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (value, disabled)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

data Action = Inc | Dec

-- Or use an "open" type variable
-- type Input = Unit

type State = Int

{-

- `query` represents a way that parent components can communicate with this component.
- `input` represents the input our component accepts.
    In our case, the component doesn't accept any input, so we'll leave this variable open.
- `output` represents a way that this component can communicate with its parent component
- `m`, represents the monad that can be used to run effects in the component.
    Our component doesn't run any effects, so we'll leave this variable open

 -}
component :: forall query input output m. Component query input output m
component =
  mkComponent
    { initialState
    , eval: mkEval $ defaultEval { handleAction = handleAction }
    , render
    }
  where
  initialState :: forall input'. input' -> State
  initialState _ = 0

  {-
  The type () means our component has no child components.
  We could also leave it open as a type variable because we aren't using it.

  Since our counter has no child components we'll use () to describe them, and because it doesn't
  communicate with a parent or perform effects we'll leave the output and m type variables open.
   -}
  handleAction :: forall output' m'. Action -> HalogenM State Action () output' m' Unit
  handleAction = case _ of
    Inc -> modify_ \state -> state + 1
    Dec -> modify_ \state -> state - 1

  render :: forall m'. State -> ComponentHTML Action () m'
  render state =
    div_
      [ h1_ [ text "Counter example" ]
      , input [ value $ show state, disabled true ]
      , button [ onClick \_ -> Inc ] [ text "count" ]
      ]