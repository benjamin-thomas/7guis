{-

npm install -g pscid vite

Terminal 1:
  pscid --censor-codes UnusedName,ShadowedName,UnusedImport,UnusedExplicitImport,UnusedDeclaration

Terminal 2:
  vite dev


---

TODO: Use floats, but I don't use Number.fromString since it delegates to crappy JS parseFloat. Rather, use a parser.

 -}
module Main (main) where

import Prelude

import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Poll (Poll)

fahrenheitToCelsius :: Int → Int
fahrenheitToCelsius f = (f - 32) * 5 / 9

celsiusToFahrenheit :: Int → Int
celsiusToFahrenheit c = (c * 9 / 5) + 32

inputNut
  :: { label :: String
     , mbVal :: Poll (Maybe Int)
     , readVal :: Poll String
     , setVal :: String → Effect Unit
     }
  → Nut
inputNut { label, mbVal, readVal, setVal } = D.div [ DA.klass_ "form-input" ]
  [ D.label [] [ text_ label ]
  , D.input
      [ DA.value readVal
      , DL.valueOn_ DL.input setVal
      , DA.klass $ mbVal <#> maybe "error" mempty
      ]
      []
  ]

debugNut :: { mbCelsius :: Poll (Maybe Int), mbFahrenheit :: Poll (Maybe Int) } → Nut
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

conversionNut :: { converted :: Poll (Maybe { c :: Int, f :: Int }) } → Nut
conversionNut { converted } = D.p [ DA.klass $ converted <#> maybe "error" show ]
  [ text $ converted <#> case _ of
      Nothing -> "Cannot compute due to bad data!"
      Just { c, f } ->
        show c <> "°C = " <> show f <> "°F"
  ]

app :: Nut
app =
  Deku.do
    setCelsius /\ celsius <- useState "0"
    setFahrenheit /\ fahrenheit <- useState "32"

    let
      mbCelsius :: Poll (Maybe Int)
      mbCelsius = Int.fromString <$> celsius

      setCelsius' :: String → Effect Unit
      setCelsius' str =
        setCelsius str *>
          for_
            (Int.fromString str)
            (\c -> setFahrenheit $ show $ celsiusToFahrenheit c)

      mbFahrenheit :: Poll (Maybe Int)
      mbFahrenheit = fahrenheit <#> Int.fromString

      setFahrenheit' :: String → Effect Unit
      setFahrenheit' str =
        setFahrenheit str *>
          for_
            (Int.fromString str)
            (\f -> setCelsius $ show $ fahrenheitToCelsius f)

      converted :: Poll (Maybe { c :: Int, f :: Int })
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
              , mbVal: mbCelsius
              , readVal: celsius
              , setVal: setCelsius'
              }
          , inputNut
              { label: "Fahrenheit"
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
  void $ runInBody app

