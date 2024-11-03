module Main (main) where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)

counter :: forall a. Int -> Widget HTML a
counter n = do
  liftEffect $ log $ "Rendering counter: " <> show n
  D.div [ P._id "counter" ]
    [ D.h1' [ D.text "Counter example" ]
    , D.input [ P.value $ show n, P.disabled true ]
    , D.button [ unit <$ P.onClick ] [ D.text "count" ]
    ]
  counter (n + 1)

main :: Effect Unit
main = do
  log "Counter: booting up..."
  runWidgetInDom "app" $ counter 0

