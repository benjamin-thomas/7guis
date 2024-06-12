module DateParser
  ( ParseError'(..)
  , month
  , parse
  , parse'
  , parser
  ) where

import Prelude

import Data.Array (replicate)
import Data.Date (Date, Day, Month, Year, exactDate)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Int (fromString)
import Data.Maybe (fromJust, maybe)
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import Parsing (ParseError(..), Parser, Position(..), fail, runParser)
import Parsing.Combinators ((<?>), (<|>))
import Parsing.String (char, eof)
import Partial.Unsafe (unsafePartial)

digit :: Parser String Char
digit =
  parser_ <?> "digit"
  where
  parser_ =
    identity
      <$> char '0'
      <|> char '1'
      <|> char '2'
      <|> char '3'
      <|> char '4'
      <|> char '5'
      <|> char '6'
      <|> char '7'
      <|> char '8'
      <|> char '9'

year :: Parser String Year
year =
  sequence (replicate 4 digit)
    <#> fromCharArray
    <#> fromString
    >>= maybe (fail "Impossible") pure
    <#> toEnum
    >>= maybe (fail "Not a valid year") pure

month :: Parser String Month
month =
  sequence (replicate 2 digit)
    <#> fromCharArray
    <#> fromString
    >>= maybe (fail "Impossible") pure
    <#> toEnum
    >>= maybe (fail "Not a valid month") pure

{- Playing with different styles below
-}

-- day :: Parser String Day
-- day =
--   sequence (replicate 2 digit)
--     <#> fromCharArray
--     <#> fromString
--     >>= maybe (fail "Impossible") pure
--     <#> toEnum
--     >>= maybe (fail "Not a valid day") pure

-- day :: Parser String Day
-- day =
--   sequence (replicate 2 digit)
--     <#> fromCharArray
--     <#> (unsafePartial $ fromJust <<< fromString)
--     <#> toEnum
--     >>= maybe (fail "Not a valid day") pure

day :: Parser String Day
day =
  maybe (fail "Not a valid day") pure =<<
    toEnum
      <$> (unsafePartial $ fromJust <<< fromString)
      <$> fromCharArray
      <$> sequence (replicate 2 digit)

parser :: Parser String Date
parser =
  maybe (fail "Invalid date") pure =<<
    mkDate
      <$> (day <* dot)
      <*> (month <* dot)
      <*> year
  where
  dot = char '.'
  mkDate d m y = exactDate y m d

-- parse :: String -> Either ParseError Date
parse ∷ String → Either ParseError Date
parse str = runParser str parser

data ParseError' = Full ParseError | Incomplete

-- instance Eq ParseError' where
--   eq (Full a) (Full b) = eq a b
--   eq (Incomplete) (Incomplete) = true
--   eq _ _ = false
derive instance Eq ParseError'

instance Show ParseError' where
  show (Full e) = "Full " <> show e
  show (Incomplete) = "Incomplete"

{-| This version of `parse` returns `Unit` instead of a `ParseError` if the
    input hasn't provided the full input yet.
-}
parse' ∷ { str :: String, expectedLength :: Int } → Either ParseError' Date
parse' { str, expectedLength } =
  let
    check :: Position -> Boolean
    check (Position { index }) = (String.length str /= expectedLength) && index == (String.length str)
  in
    case runParser str (parser <* eof) of
      Right date -> Right date
      Left (ParseError (msg :: String) (pos :: Position)) ->
        if check pos then
          Left Incomplete
        else
          Left $ Full (ParseError msg pos)

