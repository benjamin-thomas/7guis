module Main where

import Prelude

import Data.Foldable (traverse_)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_, text)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Web.DOM.Element (setClassName)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

{-

Terminal 1:
  npx pscid

Terminal 2:
  npx vite dev

 -}

main :: Effect Unit
main = do
  setBodyClass "container"
  void $ runInBody app
  where

  {-

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

  app :: Nut
  app =
    Deku.do
      setCounter /\ counter <- useState 0

      let
        btn txt f = D.button
          [ DL.runOn DL.click f <$> counter ]
          [ text_ txt ]

      D.div [ DA.klass_ "app" ]
        [ btn "-" (setCounter <<< max 0 <<< (_ - 1))
        , text $ show <$> counter
        , btn "+" (setCounter <<< min 5 <<< (_ + 1))
        ]

