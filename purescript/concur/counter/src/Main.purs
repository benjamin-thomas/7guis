module Main (main) where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import LocalStorage as LocalStorage

counter :: forall a. Int -> Widget HTML a
counter n = do
  liftEffect $ log $ "Rendering counter: " <> show n
  liftEffect $ LocalStorage.set "counter" n
  D.div [ P._id "counter" ]
    [ D.h1 [ P.style { color: "orange" } ] [ D.text "Counter example" ]
    , D.input [ P.value $ show n, P.disabled true ]
    , D.button [ unit <$ P.onClick ] [ D.text "count" ]
    ]
  counter (n + 1)

main :: Effect Unit
main = do
  (savedCounterNullable :: Nullable String) <- liftEffect $ LocalStorage.get "counter"

  let
    initValue :: Int
    initValue = fromMaybe 0 (Int.fromString =<< Nullable.toMaybe savedCounterNullable)

  runWidgetInDom "app" $ counter initValue

