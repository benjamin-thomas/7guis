module Main where

import Data.ByteString.Lazy qualified as BSL
import Data.String.Interpolate (i)
import Data.Text (Text)

import Data.FileEmbed (embedFile)
import Data.Text.Encoding qualified as TE

import Network.Wai.Middleware.Static

import Control.Monad (when)
import Data.Bool (bool)
import Data.Text qualified as T
import Text.Printf (printf)
import Text.Read (readMaybe)
import Web.Hyperbole hiding (input, label)
import Prelude hiding (div, span)

{-
ghcid -c 'cabal repl tempConverter' -T :main --warnings --reload=./assets/css/temp-converter.css
-}

main :: IO ()
main = do
    let port = 4321 :: Int
    let staticMiddleware = staticPolicy (addBase "assets")
    run port $ staticMiddleware app

reloadJs :: Text
reloadJs =
    TE.decodeUtf8
        $(embedFile "reload.js")

document :: Text -> BSL.ByteString -> BSL.ByteString
document title cnt =
    [i|<html>
      <head>
        <title>#{title}</title>
        <script type="text/javascript">#{scriptEmbed}</script>

        <link href="/css/temp-converter.css"
              rel="stylesheet">

        <script>
          #{reloadJs}
        </script>

      </head>
      <body>#{cnt}</body>
  </html>|]

app :: Application
app = do
    liveApp
        (document "Temp Converter")
        (runPage page)

page ::
    ( Hyperbole :> es
    ) =>
    Eff es (Page '[MainView])
page = do
    let init' =
            MkModel
                { shouldAutofocus = True
                , celsius = ""
                , fahrenheit = ""
                , err = ""
                }
    pure $ col id $ do
        hyper MkMainView $ mainView init'

data MainView = MkMainView
    deriving
        ( Show
        , Read
        , ViewId
        )

instance HyperView MainView es where
    data Action MainView
        = CelsiusChanged Model Text
        | FahrenheitChanged Model Text
        deriving
            ( Show
            , Read
            , ViewAction
            )

    update action = do
        -- Construct the new Model separately to prevent cursor stealing on fahrenheit change.
        -- We want to autofocus once, one initial page load.
        let newModel =
                case action of
                    CelsiusChanged old txt -> do
                        let
                            res = conv celsiusToFahrenheit txt
                            (converted, err) =
                                case res of
                                    Nothing -> ("", "Bad Celsius")
                                    Just v -> (v, "")
                        old
                            { celsius = T.strip txt
                            , fahrenheit = converted
                            , err = err
                            }
                    FahrenheitChanged old txt -> do
                        let res = conv fahrenheitToCelsius txt
                            (converted, err) =
                                case res of
                                    Nothing -> ("", "Bad Fahrenheit")
                                    Just v -> (v, "")
                        old
                            { fahrenheit = T.strip txt
                            , celsius = converted
                            , err = err
                            }
        pure $
            mainView $
                newModel{shouldAutofocus = False}

data Model = MkModel
    { shouldAutofocus :: Bool
    , celsius :: Text
    , fahrenheit :: Text
    , err :: Text
    }
    deriving (Show, Read)

isDev :: Bool
isDev = False

mainView :: Model -> View MainView ()
mainView model = do
    h1 id "Temp Converter"

    when isDev $
        tag "pre" id $
            text $
                T.pack $
                    show model

    div id $ do
        let
            onChange :: (Model -> Text -> Action MainView) -> Mod MainView
            onChange action =
                let debounce = 250
                 in onInput (action model) debounce

            inp :: (Model -> Text -> Action MainView) -> Text -> View MainView ()
            inp action val =
                input
                    ( onChange action
                        . value val
                        . bool id autofocus (model.shouldAutofocus)
                    )

        span id $ do
            inp CelsiusChanged model.celsius

            label id "Celsius"
        span id "="
        span id $ do
            inp FahrenheitChanged model.fahrenheit
            label id "Fahrenheit"

    div (att "style" "margin-top: 5px") $ do
        text model.err

h1 :: Mod c -> View c () -> View c ()
h1 = tag "h1"

div :: Mod c -> View c () -> View c ()
div = tag "div"

span :: Mod c -> View c () -> View c ()
span = tag "span"

input :: Mod c -> View c ()
input m = tag "input" m ""

label :: Mod c -> View c () -> View c ()
label = tag "label"

disabled :: Mod c
disabled = att "disabled" ""

celsiusToFahrenheit :: Double -> Double
celsiusToFahrenheit c =
    c * 1.8 + 32

fahrenheitToCelsius :: Double -> Double
fahrenheitToCelsius f =
    (f - 32) / 1.8

conv :: (Double -> Double) -> Text -> Maybe Text
conv f txt =
    let
        toText :: Double -> Text
        toText = T.pack . printf "%.2f"
     in
        if T.null (T.strip txt)
            then
                Just ""
            else
                fmap
                    (toText . f)
                    (readMaybe $ T.unpack txt)
