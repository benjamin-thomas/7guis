{-

npm install -g pscid vite

Terminal 1:
  pscid --censor-codes UnusedName,ShadowedName,UnusedImport,UnusedExplicitImport,UnusedDeclaration

Terminal 2:
  vite dev

 -}
module Main (main) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Formatter.Number (formatNumber)
import Data.Formatter.Parser.Number (parseNumber)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Utils as SU
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Console (log)
import FRP.Poll (Poll)
import Parsing (ParserT, runParser)
import Parsing.String (char, eof)

fahrenheitToCelsius :: Number → Number
fahrenheitToCelsius f = (f - 32.0) * 5.0 / 9.0

celsiusToFahrenheit :: Number → Number
celsiusToFahrenheit c = (c * 9.0 / 5.0) + 32.0

inputNut
  :: { label :: String
     , autofocus :: Boolean
     , mbVal :: Poll (Maybe Number)
     , readVal :: Poll String
     , setVal :: String → Effect Unit
     }
  → Nut
inputNut { label, autofocus, mbVal, readVal, setVal } = D.div [ DA.klass_ "form-input" ]
  [ D.label [] [ text_ label ]
  , D.input
      ( catMaybes
          [ Just $ DA.value readVal
          , Just $ DL.valueOn_ DL.input setVal
          , Just $ DA.klass $ mbVal <#> maybe "error" mempty
          , if autofocus then
              Just $ DA.autofocus_ ""
            else
              Nothing
          ]
      )
      []
  ]

debugNut :: { mbCelsius :: Poll (Maybe Number), mbFahrenheit :: Poll (Maybe Number) } → Nut
debugNut { mbCelsius, mbFahrenheit } = D.div [ DA.klass_ "debug" ]
  [ D.hr [] []
  , D.span_ [ text_ "ConvC=" ]
  , D.span [ DA.klass $ mbCelsius <#> maybe "error" show ]
      [ text $ mbCelsius <#> maybe "ERR!" show
      ]
  , D.br [] []
  , D.span_ [ text_ " ConvF=" ]
  , D.span [ DA.klass $ mbFahrenheit <#> maybe "error" show ]
      [ text $ mbFahrenheit <#> maybe "ERR!" show
      ]
  , D.hr [] []
  ]

conversionNut :: { converted :: Poll (Maybe { c :: Number, f :: Number }) } → Nut
conversionNut { converted } = D.p [ DA.klass $ converted <#> maybe "error" show ]
  [ text $ converted <#> case _ of
      Nothing -> "Cannot compute due to bad data!"
      Just { c, f } ->
        show c <> "°C = " <> show f <> "°F"
  ]

fmtNumber :: Number -> String
fmtNumber n =
  case formatNumber "0.000" n of
    Left _ -> "ERR!!" -- should never happen since the format is hardcoded
    Right str -> str

numberFromString ∷ String → Maybe Number
numberFromString str =
  -- Weird, the parser fails on leading minus sign so I must handle the negation manually
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

app :: Nut
app =
  Deku.do
    setCelsius /\ celsius <- useState'
    setFahrenheit /\ fahrenheit <- useState'

    let
      mbCelsius :: Poll (Maybe Number)
      mbCelsius = numberFromString <$> celsius

      setCelsius' :: String → Effect Unit
      setCelsius' str =
        setCelsius str *>
          for_
            (numberFromString str)
            (setFahrenheit <<< fmtNumber <<< celsiusToFahrenheit)

      mbFahrenheit :: Poll (Maybe Number)
      mbFahrenheit = fahrenheit <#> numberFromString

      setFahrenheit' :: String → Effect Unit
      setFahrenheit' str =
        setFahrenheit str *>
          for_
            (numberFromString str)
            (setCelsius <<< fmtNumber <<< fahrenheitToCelsius)

      converted :: Poll (Maybe { c :: Number, f :: Number })
      converted =
        make
          <$> mbCelsius
          <*> mbFahrenheit
        where
        make a b = case a /\ b of
          Just c /\ Just f -> Just { c, f }
          _ -> Nothing

    D.div []
      [ D.h1__ "Temp Converter"
      , D.div [ DA.klass_ "form" ]
          [ inputNut
              { label: "Celsius"
              , autofocus: false
              , mbVal: mbCelsius
              , readVal: celsius
              , setVal: setCelsius'
              }
          , inputNut
              { label: "Fahrenheit"
              , autofocus: true
              , mbVal: mbFahrenheit
              , readVal: fahrenheit
              , setVal: setFahrenheit'
              }
          ]
      , debugNut { mbCelsius, mbFahrenheit }
      , conversionNut { converted }
      ]

main :: Effect Unit
main = do
  log "Booting up..."
  -- log $ "Parsing test" <> show (numberFromString "-01.234")
  void $ runInBody app

