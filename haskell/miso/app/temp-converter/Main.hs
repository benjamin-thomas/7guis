{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Main (main) where

import Control.Monad.State
import Data.Text qualified as T
import Language.Javascript.JSaddle
import Miso
import Miso.String
    ( Text
    , ToMisoString (toMisoString)
    , toMisoString
    )
import Text.Printf (printf)
import Text.Read (readMaybe)

data Field
    = Celsius
    | Fahrenheit
    deriving (Eq, Show)

data Validation a
    = NotSet
    | Valid a
    | Invalid a
    deriving (Eq, Show)

-- sideEffectTest :: Effect model action
sideEffectTest :: JSM ()
sideEffectTest = setLocalStorage "hello" ("world" :: Text)

-- This is not working when I compile with GHC (vs GHCJS, or WASM??)
#ifdef GHCJS_NEW
foreign import javascript unsafe "Date.now()"
    getNow2 :: JSM Double
#else
getNow2 :: JSM Double
-- below can **not** work with Miso. At best, I can define an IO Double here, not a JSM Double
-- getNow2 = round . (*1000) <$> getPOSIXTime
getNow2 = pure 0
#endif

-- This works when compiling with GHC (see the jsaddle project)
getNow3 :: JSM Int
getNow3 = fromJSValUnchecked =<< (jsg ("Date" :: Text) # ("now" :: Text) $ ())

unvalidate :: (Monoid a) => Validation a -> a
unvalidate (Valid a) = a
unvalidate (Invalid a) = a
unvalidate NotSet = mempty

isInvalid :: Validation a -> Bool
isInvalid (Invalid _) = True
isInvalid _ = False

data Model = Model
    { celsius :: Validation Text
    , fahrenheit :: Validation Text
    , editing :: Field
    , now1 :: Double
    , now2 :: Double
    , now3 :: Int
    }
    deriving (Eq, Show)

init' :: Field -> Double -> Model
init' field t =
    Model
        { celsius = NotSet
        , fahrenheit = NotSet
        , editing = field
        , now1 = t
        , now2 = 0
        , now3 = 0
        }

data Msg
    = Focus Field
    | CelsiusChanged Text
    | FahrenheitChanged Text
    | GotTime1 Double
    | GotTime2 Double
    | GotTime3 Int

instance ToMisoString Field where
    toMisoString Celsius = "celsius"
    toMisoString Fahrenheit = "fahrenheit"

fmt :: Float -> Text
fmt x = T.pack $ printf "%0.2f" x

update' :: Msg -> Effect Model Msg
update' = \case
    GotTime1 t -> modify $ \m -> m{now1 = t}
    GotTime2 t -> modify $ \m -> m{now2 = t}
    GotTime3 t -> modify $ \m -> m{now3 = t}
    Focus field -> do
        io $ focus (toMisoString field)
        modify $ \m -> m{editing = field}

        -- Keeping for ref: various syntax to do the same thing
        -- NOTE: to schedule an Effect, I need to send the latest model with the schedule request.
        -- m <- get
        -- m <# do
        --     t <- now
        --     pure $ GotTime t

        -- get >>= \model' ->
        --     model' <# (GotTime <$> now)

        -- get >>= ((GotTime <$> now) #>)

        io sideEffectTest
        flip
            batchEff
            [ GotTime1 <$> now
            , GotTime2 <$> getNow2
            , GotTime3 <$> getNow3
            ]
            =<< get
    CelsiusChanged txt ->
        modify $ \oldModel ->
            let newModel = oldModel{editing = Celsius}
             in if T.null txt
                    then newModel{celsius = NotSet}
                    else case readMaybe (T.unpack txt) of
                        Nothing ->
                            newModel
                                { celsius = Invalid txt
                                }
                        Just val' ->
                            newModel
                                { celsius = Valid txt
                                , fahrenheit = Valid . fmt $ celsiusToFahrenheit val'
                                }
    FahrenheitChanged txt ->
        modify $ \oldModel ->
            let newModel = oldModel{editing = Fahrenheit}
             in if T.null txt
                    then newModel{fahrenheit = NotSet}
                    else case readMaybe (T.unpack txt) of
                        Nothing ->
                            newModel
                                { fahrenheit = Invalid txt
                                }
                        Just val' ->
                            newModel
                                { fahrenheit = Valid txt
                                , celsius = Valid . fmt $ fahrenheitToCelsius val'
                                }

celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = c * 9 / 5 + 32

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (f - 32) * 5 / 9

tempInput :: Field -> Validation Text -> (Text -> msg) -> View msg
tempInput field validation onInput' =
    input_
        [ id_ $ toMisoString field
        , onInput onInput'
        , autocomplete_ False
        , classList_
            [ ("input", True)
            , ("error", isInvalid validation)
            ]
        , value_ $ unvalidate validation
        ]

view' :: Model -> View Msg
view' model' =
    div_
        [class_ "app-container"]
        [ h1_ [] [text "Temperature Converter"]
        , div_
            [class_ "button-container"]
            [ div_
                [class_ "input-group"]
                [button_ [onClick $ Focus Celsius] [text "Focus Celsius"]]
            , div_ [class_ "equals-container"] []
            , div_
                [class_ "input-group"]
                [button_ [onClick $ Focus Fahrenheit] [text "Focus Fahrenheit"]]
            ]
        , div_
            [class_ "converter-container"]
            [ div_
                [class_ "input-group"]
                [ tempInput Celsius (celsius model') CelsiusChanged
                , span_ [class_ "unit-label"] [text "Celsius"]
                ]
            , div_
                [class_ "equals-container"]
                [span_ [] [text "="]]
            , div_
                [class_ "input-group"]
                [ tempInput Fahrenheit (fahrenheit model') FahrenheitChanged
                , span_ [class_ "unit-label"] [text "Fahrenheit"]
                ]
            ]
        , case (celsius model', fahrenheit model') of
            (NotSet, NotSet) -> text ""
            (Valid c, Valid f) ->
                p_
                    [class_ ""] -- If I don't set the class empty here specifically, then the prior "error" state may persist in this node
                    [ text c
                    , text "C = "
                    , text f
                    , text "F"
                    ]
            _ -> p_ [class_ "error"] [text "Cannot compute temperature due to bad data"]
        , pre_ [styleInline_ "margin-top:30px"] [text $ toMisoString $ show model']
        ]

app :: String -> Double -> App Model Msg
app css t =
    let toFocus = Celsius
     in (defaultApp (init' toFocus t) update' view')
            { initialAction = Just (Focus toFocus)
            , styles =
                [ Style (toMisoString css)
                ]
            }

main :: IO ()
main = do
    css <- readFile "temp-converter.css"
    run $ do
        t <- now
        startApp (app css t)