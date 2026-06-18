module Main where

import Prelude

import Data.Bifunctor (rmap)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)
import Flame.Application as FA
import Timer as Timer
import Web.DOM.ParentNode (QuerySelector(..))

-- | The generic glue of the effects-as-data pattern: turn a PURE update
-- | (effects as `instr` data) plus an `interpret`er into the effectful update
-- | Flame expects.
-- |
-- | `update` already produced the new model; `rmap (map interpret)` just runs
-- | the interpreter over the instructions slot of its result. Equivalent to:
-- |
-- |     let Tuple model' instrs = update model msg
-- |     in  Tuple model' (map interpret instrs)
mkUpdate
  :: forall model msg instr
   . (instr -> Aff (Maybe msg))
  -> (model -> msg -> Tuple model (Array instr))
  -> (model -> msg -> Tuple model (Array (Aff (Maybe msg))))
mkUpdate interpret update model msg = rmap (map interpret) (update model msg)

main :: Effect Unit
main =
  FA.mount_ (QuerySelector "#app")
    { model: Timer.init
    , update: mkUpdate Timer.interpret Timer.update
    , view: Timer.view
    , subscribe: Timer.subscriptions
    }
