{-# LANGUAGE TemplateHaskell #-}

module Css
  ( counterCss
  , tempConverterCss
  ) where

import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

counterCss :: Text
counterCss =
  decodeUtf8 $
    $(embedFile "counter.css")

--
tempConverterCss :: Text
tempConverterCss =
  decodeUtf8 $
    $(embedFile "temp-converter.css")