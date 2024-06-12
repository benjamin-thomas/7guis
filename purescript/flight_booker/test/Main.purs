module Test.Main
  ( main
  ) where

import Prelude

import Control.Monad.Free (Free)
import Data.Date (Date, Month(..), exactDate)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import DateParser (ParseError'(..))
import DateParser as DateParser
import Effect (Effect)
import Parsing (ParseError(..), Position(..), runParser)
import Parsing.String (eof)
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  dateParserSuite

-- unsafeMkDate :: Int -> Month -> Int -> Date
-- unsafeMkDate year month day =
--   unsafePartial $ fromJust $
--     canonicalDate <$> toEnum year <@> month <*> toEnum day

-- unsafeMkDate :: Int -> Int -> Int -> Date
-- unsafeMkDate year month day =
--   unsafePartial
--     $ fromJust
--     $ fromJust
--     $ exactDate
--         <$> toEnum year
--         <*> toEnum month
--         <*> toEnum day

unsafeMkDate :: Int -> Month -> Int -> Date
unsafeMkDate year month day =
  unsafePartial $ fromJust <<< fromJust $
    exactDate
      <$> toEnum year
      <@> month
      <*> toEnum day

dateParserSuite :: Free TestF Unit
dateParserSuite = suite "dateParser" do
  test "valid input" do
    Assert.equal
      (Right $ unsafeMkDate 2024 June 10)
      (runParser "10.06.2024" (DateParser.parser <* eof))

  test "one char too many" do
    Assert.equal
      (Left $ ParseError "Expected EOF" $ Position { column: 11, index: 10, line: 1 })
      (runParser "10.06.2024x" (DateParser.parser <* eof))

  test "bad start" do
    Assert.equal
      (Left $ ParseError "Expected digit" $ Position { column: 1, index: 0, line: 1 })
      (runParser "x10.06.2024" DateParser.parser)

  test "missing dot" do
    Assert.equal
      (Left $ ParseError "Expected '.'" $ Position { column: 3, index: 2, line: 1 })
      (runParser "1006.2024" DateParser.parser)

  test "bad month" do
    Assert.equal
      (Left $ ParseError "Not a valid month" $ Position { column: 6, index: 5, line: 1 })
      (runParser "10.99.2024" DateParser.parser)

  test "incomplete input OK" do
    Assert.equal
      (Left Incomplete)
      (DateParser.parse' { str: "10.", expectedLength: 10 })

  test "incomplete input NOT OK" do
    Assert.equal
      (Left $ Full $ ParseError "Expected '.'" $ Position { column: 3, index: 2, line: 1 })
      (DateParser.parse' { str: "10x", expectedLength: 10 })

  test "incomplete: should reject bad day" do
    Assert.equal
      (Left $ Full $ ParseError "Not a valid day" $ Position { column: 3, index: 2, line: 1 })
      (DateParser.parse' { str: "99x", expectedLength: 10 })

  test "incomplete: should accept good day" do
    Assert.equal
      (Left Incomplete)
      (DateParser.parse' { str: "31.", expectedLength: 10 })

  test "incomplete: after should accept good day" do
    Assert.equal
      (Left $ Full $ ParseError "Expected '.'" $ Position { column: 3, index: 2, line: 1 })
      (DateParser.parse' { str: "31x", expectedLength: 10 })

-- FIXME: I can't differentiate the 2 cases below.
-- FIXME: they both have the same position, but I'd like to consider one of them OK, and the other NOT OK
--    - 99 should return (Full "Not a valid day")
--    - 31 should return Incomplete
--
-- test "incomplete: bad day should fail at 2nd char, not 3rd" do
--   Assert.equal
--     (Left $ Full $ ParseError "Not a valid day" $ Position { column: 3, index: 2, line: 1 })
--     (DateParser.parse' "99")

-- test "incomplete: should accept day" do
--   Assert.equal
--     (Left $ Full $ ParseError "Expected '.'" $ Position { column: 3, index: 2, line: 1 })
--     (DateParser.parse' "31")

