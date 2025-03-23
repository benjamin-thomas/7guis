module Main where

import Data.ByteString.Lazy qualified as BSL
import Data.String.Interpolate (i)
import Data.Text (Text)

import Data.FileEmbed (embedFile)
import Data.Text.Encoding qualified as TE

import Network.Wai.Middleware.Static

import Data.Bool (bool)
import Data.Text qualified as T
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
                        old
                            { celsius = txt
                            , fahrenheit = unsafeConv celsiusToFahrenheit txt
                            }
                    FahrenheitChanged old txt -> do
                        old
                            { fahrenheit = txt
                            , celsius = unsafeConv fahrenheitToCelsius txt
                            }
        pure $
            mainView $
                newModel{shouldAutofocus = False}

data Editing = Celsius | Fahrenheit
    deriving
        ( Show
        , Read
        , Eq
        )

data Model = MkModel
    { shouldAutofocus :: Bool
    , celsius :: Text
    , fahrenheit :: Text
    }
    deriving (Show, Read)

mainView :: Model -> View MainView ()
mainView model = do
    h1 id "Temp Converter"

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

        span id $ do
            input
                ( onChange CelsiusChanged
                    . value model.celsius
                    . bool id autofocus (model.shouldAutofocus)
                )

            label id "Celsius"
        span id "="
        span id $ do
            input
                ( onChange FahrenheitChanged
                    . value model.fahrenheit
                )
            label id "Fahrenheit"

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

unsafeConv :: (Double -> Double) -> Text -> Text
unsafeConv f txt =
    T.pack $
        show $
            f (read $ T.unpack txt)