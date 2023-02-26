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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { celsius :: Maybe String, fahrenheit :: Maybe String, error :: Either String Unit }

data Action = CelsiusChanged String | FahrenheitChanged String

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { celsius: Nothing, fahrenheit: Nothing, error: Right unit }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  let
    celsius = "celsius"
    fahrenheit = "fahrenheit"
    input id label mValue action =
      HH.span_
        [ HH.input
            [ HP.id id
            , HP.autocomplete HP.AutocompleteOff
            , HE.onValueInput \s -> action s
            , HP.value $ fromMaybe "" mValue
            ]
        , HH.label [ HP.for id ] [ HH.text label ]
        ]
  in
    HH.div_
      [ HH.h1_ [ HH.text "Temperature Converter" ]
      , HH.div_
          [ input celsius "Celsius" state.celsius CelsiusChanged
          , input fahrenheit "Fahrenheit" state.fahrenheit FahrenheitChanged
          ]

      -- , HH.pre_ [ HH.text $ "C:" <> show state.celsius <> " F:" <> show state.fahrenheit ]
      , HH.p_ [ HH.text $ showErr state.error ]
      ]

showErr ∷ Either String Unit → String
showErr (Left s) = s
showErr (Right _) = ""

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  CelsiusChanged s -> do
    old <- get
    let
      (Tuple error fahrenheit) = convert s toFahrenheit "Celsius: bad input"
    let new = old { celsius = Just s, fahrenheit = fahrenheit, error = error }
    put new
  FahrenheitChanged s -> H.modify_ \st ->
    let
      (Tuple error celsius) = convert s toCelsius "Fahrenheit: bad input"
    in
      st { fahrenheit = Just s, celsius = celsius, error = error }

convert :: String -> (Number -> Number) -> String -> Tuple (Either String Unit) (Maybe String)
convert s f errMsg =
  let
    -- FIXME: JavaScript's parseFloat function is called under the hood.
    -- So as a result, "1x" gets parsed to 1.0, yuck
    maybeNum = Number.fromString s :: Maybe Number
    mConv = maybeNum >>= \n -> Just $ show $ f n
    error = toEither maybeNum errMsg
  in
    (Tuple error mConv)

toFahrenheit :: Number -> Number
toFahrenheit n =
  n * 9.0 / 5.0 + 32.0

toCelsius :: Number -> Number
toCelsius n =
  (n - 32.0) * 5.0 / 9.0

toEither :: Maybe Number -> String -> Either String Unit
toEither x msg = case x of
  -- Left is the failure case
  -- Right is Success
  Just _ -> Right unit
  Nothing -> Left msg

