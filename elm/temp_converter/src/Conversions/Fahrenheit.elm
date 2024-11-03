module Conversions.Fahrenheit exposing (toCelsius, toString)

import Conversions.Units exposing (Celsius(..), Fahrenheit(..))
import Round


toCelsius : Fahrenheit -> Celsius
toCelsius (MkFahrenheit f) =
    MkCelsius <| (f - 32) * 5 / 9


toString : Fahrenheit -> String
toString (MkFahrenheit val) =
    Round.round 2 val
