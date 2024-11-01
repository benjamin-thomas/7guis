{-

npm install -g pscid vite

Terminal 1:
  pscid --censor-codes UnusedName,ShadowedName,UnusedImport,UnusedExplicitImport,UnusedDeclaration

Terminal 2:
  vite dev

---

whenInt ∷ (Int → Effect Unit) → String → Effect Unit
whenInt setValue = \c -> do
  log $ "new value: " <> c
  case fromString c of
    Nothing -> pure unit
    Just n -> setValue n

whenInt ∷ (Int → Effect Unit) → String → Effect Unit
whenInt setValue = \c -> do
  log $ "new value: " <> c
  for_ (Int.fromString c) setValue


`traverse_ and `for_` are `mapM` and `forM` in Haskell
whenInt ∷ (Int → Effect Unit) → String → Effect Unit
whenInt setValue =
  traverse_ setValue <<< Int.fromString

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

data LastChanged = Celsius | Fahrenheit

instance Show LastChanged where
  show = case _ of
    Celsius -> "Celsius"
    Fahrenheit -> "Fahrenheit"

fahrenheitToCelsius :: Int → Int
fahrenheitToCelsius f = (f - 32) * 5 / 9

celsiusToFahrenheit :: Int → Int
celsiusToFahrenheit c = (c * 9 / 5) + 32

app :: Nut
app =
  Deku.do
    setCelsius /\ celsius <- useState "0"
    setFahrenheit /\ fahrenheit <- useState "32"
    setLastChanged /\ lastChanged <- useState Celsius

    let
      mbCelsius :: Poll (Maybe Int)
      mbCelsius = Int.fromString <$> celsius

      setCelsiusWithChange :: String → Effect Unit
      setCelsiusWithChange str =
        setLastChanged Celsius
          *> setCelsius str
          *> for_
            (Int.fromString str)
            (\c -> setFahrenheit $ show $ celsiusToFahrenheit c)

      mbFahrenheit :: Poll (Maybe Int)
      mbFahrenheit = fahrenheit <#> Int.fromString

      setFahrenheitWithChange :: String → Effect Unit
      setFahrenheitWithChange str =
        setLastChanged Fahrenheit
          *> setFahrenheit str
          *> for_
            (Int.fromString str)
            (\f -> setCelsius $ show $ fahrenheitToCelsius f)

      converted :: Poll (Maybe { c :: Int, f :: Int })
      converted = toCheck <#> case _ of
        { lc, c: Just c, f: Just f } ->
          case lc of
            Celsius -> Just
              { c
              , f: celsiusToFahrenheit c
              }
            Fahrenheit -> Just
              { c: fahrenheitToCelsius f
              , f
              }
        _ -> Nothing
        where

        toCheck :: Poll { lc :: LastChanged, c :: Maybe Int, f :: Maybe Int }
        toCheck =
          { lc: _, c: _, f: _ }
            <$> lastChanged
            <*> mbCelsius
            <*> mbFahrenheit

    D.div []
      [ D.h1__ "Temp Converter"

      , D.div [ DA.klass_ "form" ]
          [ D.div [ DA.klass_ "form-input" ]
              [ D.label [] [ text_ "Celsius" ]
              , D.input
                  [ DA.value celsius
                  , DL.valueOn_ DL.input setCelsiusWithChange

                  , DA.klass $ mbCelsius <#> maybe "error" mempty
                  ]
                  []
              ]

          , D.div [ DA.klass_ "form-input" ]
              [ D.label [] [ text_ "Fahrenheit" ]
              , D.input
                  [ DA.value fahrenheit
                  , DL.valueOn_ DL.input setFahrenheitWithChange
                  , DA.klass $ mbFahrenheit <#> maybe "error" mempty
                  ]
                  []
              ]
          ]
      , D.div [ DA.klass_ "debug" ]
          [ D.hr [] []
          , D.p_ [ text_ "Last changed: ", text $ lastChanged <#> show ]
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
      , D.p [ DA.klass $ converted <#> maybe "error" show ]
          [ text $ converted <#> case _ of
              Nothing -> "Cannot compute due to bad data!"
              Just { c, f } ->
                show c <> "°C = " <> show f <> "°F"

          ]

      ]

main :: Effect Unit
main = do
  void $ runInBody app

