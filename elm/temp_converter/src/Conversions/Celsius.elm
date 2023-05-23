module Conversions.Celsius exposing (fromString, toFahrenheit, toString)

import Conversions.Units exposing (Celsius(..), Fahrenheit(..))
import Round


toFahrenheit : Celsius -> Fahrenheit
toFahrenheit (Celsius c) =
    Fahrenheit (c * 9 / 5 + 32)


fromString : String -> Maybe Celsius
fromString s =
    s |> String.toFloat |> Maybe.map Celsius


toString : Celsius -> String
toString (Celsius val) =
    Round.round 2 val
