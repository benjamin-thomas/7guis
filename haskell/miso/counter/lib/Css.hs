{-# LANGUAGE TemplateHaskell #-}

module Css (counterCss) where

import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

counterCss :: Text
counterCss =
  decodeUtf8 $
    $(embedFile "counter.css")