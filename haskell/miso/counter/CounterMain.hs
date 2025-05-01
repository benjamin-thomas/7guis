{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
----------------------------------------------------------------------------
-- Exhaustiveness checks
{-# OPTIONS_GHC -Wall -Werror #-}

----------------------------------------------------------------------------
module Main where

----------------------------------------------------------------------------

import Control.Monad.State (MonadState (get), modify)
import Miso

import Miso.Lens (Lens, lens, (+=), (^.))
import Miso.String (ToMisoString (toMisoString), ms)

----------------------------------------------------------------------------

import Css qualified

----------------------------------------------------------------------------

-- | Application model state
newtype Model = Model
  { _counter :: Int
  }
  deriving (Show, Eq)

----------------------------------------------------------------------------
counter :: Lens Model Int
counter = lens _counter $ \record field -> record{_counter = field}

----------------------------------------------------------------------------

-- | Sum type for App events
data Action
  = Inc
  | Dec
  | Dbl
  | Halve
  | SayHelloWorld
  deriving (Show, Eq)

----------------------------------------------------------------------------

{- | Entry point for a miso application
main :: IO ()
main = run (startApp app)
-}
main :: IO ()
main =
  run $
    startApp
      app
        { events = defaultEvents <> keyboardEvents
        , -- , initialAction = Just FocusOnInput
          styles =
            [ Style (toMisoString Css.counterCss)
            ]
        }

----------------------------------------------------------------------------

-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------

-- | `defaultApp` takes as arguments the initial model, update function, view function
app :: App Model Action
app = defaultApp init' update' view'

----------------------------------------------------------------------------

-- | Empty application state
init' :: Model
init' = Model 0

----------------------------------------------------------------------------

-- | Updates model, optionally introduces side effects
update' :: Action -> Effect Model Action
update' = \case
  Inc -> counter += 1
  Dec -> do
    model@Model{..} <- get
    noEff
      model
        { _counter = _counter - 1
        }
  Dbl -> do
    io $ consoleLog "Doubling..."
    model <- get
    noEff
      model
        { _counter = (_counter model) * 2
        }
  Halve ->
    modify $ \model ->
      model
        { _counter = _counter model `div` 2
        }
  SayHelloWorld -> io $ do
    consoleLog "Hello World"
    alert "Hello World"

----------------------------------------------------------------------------

-- | Constructs a virtual DOM from a model
view' :: Model -> View Action
view' model =
  div_
    [class_ "app-container"]
    [ h1_ [] [text "Counter"]
    , div_
        [class_ "button-container"]
        [ button_ [class_ "counter-button", onClick Dec] [text "-"]
        , span_ [class_ "counter-display"] [text $ ms (model ^. counter)]
        , button_ [class_ "counter-button", onClick Inc] [text "+"]
        , button_ [class_ "counter-button", onClick Dbl] [text "x2"]
        , button_ [class_ "counter-button", onClick Halve] [text "/2"]
        , button_ [class_ "alert-button", onClick SayHelloWorld] [text "Alert Hello World!"]
        ]
    ]

----------------------------------------------------------------------------
