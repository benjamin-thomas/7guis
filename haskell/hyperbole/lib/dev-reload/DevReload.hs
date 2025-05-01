{-# LANGUAGE TemplateHaskell #-}

module DevReload (devReloadPageJs) where

import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Language.Haskell.TH.Syntax (loc_filename, location)
import System.FilePath (takeDirectory, (</>))

devReloadPageJs :: Text
devReloadPageJs =
    decodeUtf8 $
        $( do
            loc <- location
            let dir = takeDirectory (loc_filename loc)
            embedFile (dir </> "devReloadPage.js")
         )