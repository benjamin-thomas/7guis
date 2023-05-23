module Conversions.Fahrenheit exposing (fromString, toCelsius, toString)

import Conversions.Units exposing (Celsius(..), Fahrenheit(..))
import Round


toCelsius : Fahrenheit -> Celsius
toCelsius (Fahrenheit f) =
    Celsius <| (f - 32) * 5 / 9


fromString : String -> Maybe Fahrenheit
fromString s =
    s |> String.toFloat |> Maybe.map Fahrenheit


toString : Fahrenheit -> String
toString (Fahrenheit val) =
    Round.round 2 val
