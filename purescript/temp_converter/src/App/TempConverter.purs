module App.TempConverter
  ( Action(..)
  , State
  , component
  , handleAction
  , render
  ) where

import Prelude

import Control.Monad.State (get, put)
import Data.Either (Either(..))
import Data.Formatter.Parser.Number (parseNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Parsing (ParseError, runParser)
import Parsing.String (eof)

type State =
  { celsius :: Maybe String
  , fahrenheit :: Maybe String
  , error :: Either String Unit
  }

data Action
  = CelsiusChanged String
  | FahrenheitChanged String

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }

  where
  initialState = const { celsius: Nothing, fahrenheit: Nothing, error: Right unit }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  let
    input { id, label } mValue action =
      HH.span_
        [ HH.input
            [ HP.id id
            , HP.autocomplete HP.AutocompleteOff
            , HE.onValueInput \s -> action s
            , HP.value $ fromMaybe "" mValue
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
          [ input { id: "celsius", label: "Celsius" } state.celsius CelsiusChanged
          , input { id: "fahrenheit", label: "Fahrenheit" } state.fahrenheit FahrenheitChanged
          ]

      -- , HH.pre_ [ HH.text $ "C:" <> show state.celsius <> " F:" <> show state.fahrenheit ]
      , HH.p_
          [ HH.text $ showErr state.error ]
      ]

showErr ∷ Either String Unit → String
showErr (Left s) = s
showErr (Right _) = ""

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  CelsiusChanged s -> do
    old <- get
    let
      new = case convert s toFahrenheit of
        Left _ -> old { fahrenheit = Nothing, error = Left "Celsius: bad input" }
        Right n -> old { fahrenheit = Just (show n), error = Right unit }
    put new
  FahrenheitChanged s -> H.modify_ \st ->
    case convert s toCelsius of
      Left _ -> st { celsius = Nothing, error = Left "Fahrenheit: bad input" }
      Right n -> st { celsius = Just (show n), error = Right unit }

convert :: String -> (Number -> Number) -> Either ParseError Number
convert s f =
  map f $ runParser s (parseNumber <* eof)

toFahrenheit :: Number -> Number
toFahrenheit n =
  n * 9.0 / 5.0 + 32.0

toCelsius :: Number -> Number
toCelsius n =
  (n - 32.0) * 5.0 / 9.0