module Main (main) where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (className, onChange, value) as P
import Concur.React.Props (unsafeTargetValue)
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Formatter.Number (formatNumber)
import Data.Formatter.Parser.Number (parseNumber)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.String.Utils as SU
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Parsing (ParserT, runParser)
import Parsing.String (char, eof)

celsiusToFarhenheit :: Number -> Number
celsiusToFarhenheit c = c * 9.0 / 5.0 + 32.0

farhenheitToCelsius :: Number -> Number
farhenheitToCelsius f = (f - 32.0) * 5.0 / 9.0

fmtNumber :: Number -> String
fmtNumber n =
  case formatNumber "0.000" n of
    Left _ -> "ERR!!" -- should never happen since the format is hardcoded
    Right str -> str

numberFromString ∷ String → Maybe Number
numberFromString str =
  let
    parseNegNumber :: ParserT String Identity Number
    parseNegNumber = (char '-' *> parseNumber) <#> negate

    parseVal :: ParserT String Identity Number
    parseVal =
      parseNegNumber <|> parseNumber
  in
    case runParser sanitized (parseVal <* eof) of
      Left _ -> Nothing
      Right n -> Just n
  where
  sanitized = if SU.endsWith "." str then str <> "0" else str

data Changed
  = Celsius String
  | Fahrenheit String

instance Show Changed where
  show = case _ of
    Celsius str -> "Celsius: " <> str
    Fahrenheit str -> "Fahrenheit: " <> str

input :: String -> (String -> Changed) -> Unvalidate String -> Widget HTML Changed
input label tag { value, klass } =
  D.div [ P.className "form-input" ]
    [ D.label' [ D.text $ label <> ":" ]
    , D.input
        [ tag <$> unsafeTargetValue <$> P.onChange
        , P.value value
        , P.className klass
        ]
    ]

data Validation a = Valid a | Invalid a

type Unvalidate a = { klass :: String, value :: a }

instance Show a => Show (Validation a) where
  show = case _ of
    Valid x -> "Valid: " <> show x
    Invalid x -> "Invalid: " <> show x

type TempConverterInput =
  { celsius :: Validation String
  , fahrenheit :: Validation String
  }

unValidate :: forall a. Validation a -> Unvalidate a
unValidate validation =
  case validation of
    Valid str -> { klass: "", value: str }
    Invalid str -> { klass: "error", value: str }

tempConverter :: forall a. TempConverterInput -> Widget HTML a
tempConverter { celsius, fahrenheit } = do
  liftEffect $ log $ "Rendering: " <> show { celsius, fahrenheit }
  let uc = unValidate celsius
  let uf = unValidate fahrenheit
  let
    recap :: forall b. Widget HTML b
    recap = case (celsius /\ fahrenheit) of
      Valid c /\ Valid f ->
        D.p' [ D.text $ c <> " C° = " <> f <> " F°" ]
      _ -> D.p [ P.className "error" ] [ D.text "Cannot compute due to bad data!" ]

  changed <- D.div'
    [ D.h1' [ D.text "Temp Converter" ]
    , D.div'
        [ D.div [ P.className "form" ]
            [ input "Celsius" Celsius uc
            , input "Farhenheit" Fahrenheit uf
            ]
        , D.hr'
        , recap
        ]
    ]
  liftEffect $ log $ "Changed: " <> show changed

  case changed of
    Celsius str ->
      tempConverter $
        case numberFromString str of
          Nothing ->
            { celsius: Invalid str
            , fahrenheit
            }
          Just n ->
            { celsius: Valid str
            , fahrenheit: Valid $ fmtNumber (celsiusToFarhenheit n)
            }

    Fahrenheit str ->
      tempConverter $
        case numberFromString str of
          Nothing ->
            { celsius
            , fahrenheit: Invalid str
            }
          Just n ->
            { celsius: Valid $ fmtNumber (farhenheitToCelsius n)
            , fahrenheit: Valid str
            }

main :: Effect Unit
main = do
  let initF = 100.0
  log "Temp converter: booting up..."
  runWidgetInDom "app" $ tempConverter
    { celsius: Valid $ fmtNumber (farhenheitToCelsius initF)
    , fahrenheit: Valid $ show initF
    }

