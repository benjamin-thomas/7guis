module TempConverter (component) where

import Prelude

import Control.Monad.State (get, put)
import Data.Either (Either(..))
import Data.Formatter.Parser.Number (parseNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Parsing as P
import Parsing.String as PS

type State =
  { celsius :: String
  , fahrenheit :: String
  , error :: Maybe String
  }

data Action
  = CelsiusChanged String
  | FahrenheitChanged String

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }

  where

  initialState :: input -> State
  initialState = const
    { celsius: ""
    , fahrenheit: ""
    , error: Nothing
    }

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    CelsiusChanged str -> do
      {- Just demonstrating using the "raw" state monad
         Using `H.modify_` is more idiomatic (see below)
     -}
      old <- get
      let
        new = case convert str toFahrenheit of
          Left _ -> old
            { fahrenheit = ""
            , error = Just "Celsius: bad input"
            }
          Right num -> old
            { fahrenheit = show num
            , error = Nothing
            }
      put new
    FahrenheitChanged str -> H.modify_ \state ->
      case convert str toCelsius of
        Left _ -> state
          { celsius = ""
          , error = Just "Fahrenheit: bad input"
          }
        Right num -> state
          { celsius = show num
          , error = Nothing
          }

  render :: State -> H.ComponentHTML Action () m
  render state =
    let
      mkInput { id, label } value action =
        HH.span_
          [ HH.input
              [ HP.id id
              , HP.autocomplete HP.AutocompleteOff
              , HE.onValueInput \s -> action s
              , HP.value value
              ]
          , HH.label
              [ HP.for id ]
              [ HH.text label ]
          ]
    in
      HH.div_
        [ HH.h1_
            [ HH.text "Temperature Converter" ]
        , HH.div_
            [ mkInput { id: "celsius", label: "Celsius" } state.celsius CelsiusChanged
            , mkInput { id: "fahrenheit", label: "Fahrenheit" } state.fahrenheit FahrenheitChanged
            ]

        , HH.p_
            [ HH.text $ fromMaybe "" state.error ]
        ]

convert :: String -> (Number -> Number) -> Either P.ParseError Number
convert s f =
  map f $ P.runParser s (parseNumber <* PS.eof)

toFahrenheit :: Number -> Number
toFahrenheit n =
  n * 9.0 / 5.0 + 32.0

toCelsius :: Number -> Number
toCelsius n =
  (n - 32.0) * 5.0 / 9.0