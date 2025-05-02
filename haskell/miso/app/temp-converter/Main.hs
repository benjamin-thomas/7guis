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
    }
    deriving (Eq, Show)

init' :: Field -> Model
init' field =
    Model
        { celsius = NotSet
        , fahrenheit = NotSet
        , editing = field
        }

data Msg
    = Focus Field
    | CelsiusChanged Text
    | FahrenheitChanged Text

instance ToMisoString Field where
    toMisoString Celsius = "celsius"
    toMisoString Fahrenheit = "fahrenheit"

fmt :: Float -> Text
fmt x = T.pack $ printf "%0.2f" x

update' :: Msg -> Effect Model Msg
update' = \case
    Focus field -> do
        io $ focus (toMisoString field)
        modify $ \m -> m{editing = field}
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
                        Just val ->
                            newModel
                                { celsius = Valid txt
                                , fahrenheit = Valid . fmt $ celsiusToFahrenheit val
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
                        Just val ->
                            newModel
                                { fahrenheit = Valid txt
                                , celsius = Valid . fmt $ fahrenheitToCelsius val
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
            [ button_ [onClick $ Focus Celsius] [text "Focus Celsius"]
            , button_ [onClick $ Focus Fahrenheit] [text "Focus Fahrenheit"]
            ]
        , div_
            [class_ "converter-container"]
            [ tempInput Celsius (celsius model') CelsiusChanged
            , text "Celsius"
            , text " = "
            , tempInput Fahrenheit (fahrenheit model') FahrenheitChanged
            , text "Fahrenheit"
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