module LocalStorage (get, set) where

import Prelude

import Data.Nullable (Nullable)
import Effect (Effect)

foreign import get :: forall a. String -> Effect (Nullable a)
foreign import set :: forall a. String -> a -> Effect Unit

