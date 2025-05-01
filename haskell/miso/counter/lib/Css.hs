{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Css where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.TH (Exp (LitE), Lit (StringL), Q, runIO)
import Language.Haskell.TH.Syntax
  ( Exp (LitE)
  , Lit (StringL)
  , Q
  , Quasi (qAddDependentFile)
  , runIO
  )

readCssFile :: FilePath -> Q Exp
readCssFile fp = do
  qAddDependentFile fp -- Ensures recompilation when CSS file changes
  cssText <- runIO $ T.readFile fp
  [|$(return $ LitE $ StringL $ T.unpack cssText)|]