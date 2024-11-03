module Conversions.Celsius exposing (toFahrenheit, toString)

import Conversions.Units exposing (Celsius(..), Fahrenheit(..))
import Round


toFahrenheit : Celsius -> Fahrenheit
toFahrenheit (MkCelsius c) =
    MkFahrenheit (c * 9 / 5 + 32)


toString : Celsius -> String
toString (MkCelsius val) =
    Round.round 2 val
