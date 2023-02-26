module Main where

import Prelude

import App.TempConverter as TempConverter
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI TempConverter.component unit body
