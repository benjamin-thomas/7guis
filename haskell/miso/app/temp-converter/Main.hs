{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad.State
import Data.Text qualified as T
import Miso
import Miso.String (Text, ToMisoString (toMisoString), toMisoString)
import Text.Read (readMaybe)

data Field
    = Celsius
    | Fahrenheit
    deriving (Eq, Show)

data Validation a
    = Valid a
    | Invalid a
    deriving (Eq, Show)

unvalidate :: Validation a -> a
unvalidate (Valid a) = a
unvalidate (Invalid a) = a

isInvalid :: Validation a -> Bool
isInvalid (Invalid _) = True
isInvalid _ = False

data Model = Model
    { celsius :: Validation Text
    , fahrenheit :: Validation Text
    , editing :: Field
    }
    deriving (Eq, Show)

init' :: Field -> Model
init' field =
    Model
        { celsius = Valid ""
        , fahrenheit = Valid ""
        , editing = field
        }

data Msg
    = Focus Field
    | CelsiusChanged Text
    | FahrenheitChanged Text

instance ToMisoString Field where
    toMisoString Celsius = "celsius"
    toMisoString Fahrenheit = "fahrenheit"

update' :: Msg -> Effect Model Msg
update' = \case
    Focus field -> do
        io $ focus (toMisoString field)
        modify $ \m -> m{editing = field}
    CelsiusChanged txt ->
        modify $ \oldModel ->
            let newModel = oldModel{editing = Celsius}
             in case readMaybe (T.unpack txt) of
                    Nothing ->
                        newModel
                            { celsius = Invalid txt
                            }
                    Just val ->
                        newModel
                            { celsius = Valid txt
                            , fahrenheit = Valid . tshow $ celsiusToFahrenheit val
                            }
    FahrenheitChanged txt ->
        modify $ \oldModel ->
            let newModel = oldModel{editing = Fahrenheit}
             in case readMaybe (T.unpack txt) of
                    Nothing ->
                        newModel
                            { fahrenheit = Invalid txt
                            }
                    Just val ->
                        newModel
                            { fahrenheit = Valid txt
                            , celsius = Valid . tshow $ fahrenheitToCelsius val
                            }

celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = c * 9 / 5 + 32

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (f - 32) * 5 / 9

tshow :: (Show a) => a -> Text
tshow = T.pack . show

tempInput :: Field -> Validation Text -> (Text -> msg) -> View msg
tempInput field validation onInput' =
    input_
        [ id_ $ toMisoString field
        , onInput onInput'
        , classList_
            [ ("input", True)
            , ("error", isInvalid validation)
            ]
        , value_ $ unvalidate validation
        ]

view' :: Model -> View Msg
view' model' =
    div_
        []
        [ h1_ [] [text "Temperature Converter"]
        , pre_ [] [text $ toMisoString $ show model']
        , button_ [onClick $ Focus Celsius] [text "Focus1"]
        , button_ [onClick $ Focus Fahrenheit] [text "Focus2"]
        , div_
            []
            [ tempInput Celsius (celsius model') CelsiusChanged
            , text "Celsius"
            , text " = "
            , tempInput Fahrenheit (fahrenheit model') FahrenheitChanged
            , text "Fahrenheit"
            ]
        ]

app :: String -> App Model Msg
app css =
    let toFocus = Celsius
     in (defaultApp (init' toFocus) update' view')
            { initialAction = Just (Focus toFocus)
            , styles =
                [ Style (toMisoString css)
                ]
            }

main :: IO ()
main = do
    css <- readFile "temp-converter.css"
    run $ startApp (app css)