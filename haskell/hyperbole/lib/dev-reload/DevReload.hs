{-# LANGUAGE TemplateHaskell #-}

module DevReload (devReloadPageJs) where

import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

devReloadPageJs :: Text
devReloadPageJs =
    decodeUtf8 $
        $(embedFile "devReloadPage.js")