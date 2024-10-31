module Main (main) where

import Prelude

import Data.Foldable (traverse_)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Console (log)
import Web.DOM.Element (setClassName)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Location (search)
import Web.HTML.Window (document, location)

{-

npm install -g pscid vite

Terminal 1:
  pscid

Terminal 2:
  vite dev

---

NOTE

traversable is like fmap, but with an effect

> import Data.Maybe
> import Data.Functor
> map show (Just 1)
(Just "1")

> import Effect.Console
> import Data.Foldable
> traverse_ (log <<< show) (Just 1)
1

--- PureScript

> map ((*) 2) (Just 1)
(Just 2)

> traverse_ (log <<< show <<< ((*) 2)) (Just 1)
2


--- Haskell

Prelude> fmap (*2) (Just 1)
Just 2
Prelude> traverse (print . (*2)) (Just 1)
2
Just ()

 -}

setBodyClass :: String -> Effect Unit
setBodyClass klass =
  window >>= document >>= body >>=
    traverse_ (toElement >>> setClassName klass)

app :: Boolean -> Nut
app showExtra =
  Deku.do
    setCounter /\ counter <- useState 0

    let
      btn txt f = D.button
        [ DL.runOn DL.click f <$> counter ]
        [ text_ txt ]

    D.div [ DA.klass_ "app" ]
      [ D.h1__ "Counter example"
      , D.span_
          [ D.input
              [ DA.disabled_ "true"
              , DA.value $ show <$> counter
              ]
              []
          , btn "count" (setCounter <<< (_ + 1))
          ]
      , if not showExtra then
          mempty
        else
          D.div []
            [ D.hr [] []
            , btn "-" (setCounter <<< max 0 <<< (_ - 1))
            , text $ show <$> counter
            , btn "+" (setCounter <<< min 5 <<< (_ + 1))

            ]
      ]

main :: Effect Unit
main = do
  setBodyClass "container"
  window >>= location >>= search >>= \s -> do
    log $ "search: " <> s
    if (s == "?extra=1") then
      void $ runInBody (app true)
    else
      void $ runInBody (app false)

