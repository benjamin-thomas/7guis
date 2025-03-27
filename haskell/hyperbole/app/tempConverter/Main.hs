module Main (main) where

import Data.ByteString.Lazy qualified as BSL
import Data.String.Interpolate (i)
import Data.Text (Text)

import Network.Wai.Middleware.Static

import Control.Monad (when)

-- import Data.Aeson

import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.Bool (bool)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Effectful.Dispatch.Dynamic
import System.Environment (lookupEnv)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Web.Hyperbole hiding (input, label)
import Web.View.Style (extClass)
import Prelude hiding (div, span)

import DevReload (devReloadPageJs)

{-
ghcid -c 'cabal repl tempConverter' -T :main --warnings --reload=./assets/css/temp-converter.css
APP_ENV=dev ghcid -c 'cabal repl tempConverter' -T :main --warnings --reload=./assets/css/temp-converter.css --reload=./reload.js
-}

main :: IO ()
main = do
    let port = 4321 :: Int
    let staticMiddleware = staticPolicy (addBase "assets")
    appEnv <-
        fmap
            (bool Prod Dev . (Just "dev" ==))
            (lookupEnv "APP_ENV")
    let appConfig = MkAppConfig{appEnv = appEnv}
    putStrLn $ "== Booting up with: " <> show appConfig
    run port $ staticMiddleware $ app appConfig

document :: Text -> BSL.ByteString -> BSL.ByteString
document title cnt =
    [i|<html>
      <head>
        <title>#{title}</title>
        <script type="text/javascript">#{scriptEmbed}</script>

        <link href="/css/temp-converter.css"
              rel="stylesheet">

        <script>
          #{devReloadPageJs}
        </script>

      </head>
      <body>#{cnt}</body>
  </html>|]

app :: AppConfig -> Application
app appConfig = do
    liveApp
        (document "Temp Converter")
        (runConfig appConfig $ runPage page)

data AppConfig = MkAppConfig
    { appEnv :: AppEnv
    }
    deriving (Show)

data AppEnv
    = Dev
    | Prod
    deriving (Show)

data Config :: Effect where
    GetConfig :: Config m AppConfig

getConfig :: (Config :> es) => Eff es AppConfig
getConfig = send $ GetConfig

type instance DispatchOf Config = 'Dynamic

runConfig :: AppConfig -> Eff (Config : es) a -> Eff es a
runConfig appConfig = interpret $ \_ -> \case
    GetConfig -> pure appConfig

page ::
    ( Hyperbole :> es
    , Config :> es
    ) =>
    Eff es (Page '[MainView])
page = do
    let init' =
            MkModel
                { shouldAutofocus = True
                , celsius = ""
                , fahrenheit = ""
                , errFields = mempty
                }
    viewed <- mainView init'
    pure $ col id $ do
        hyper MkMainView viewed

data MainView = MkMainView
    deriving
        ( Show
        , Read
        , Generic
        , ViewId
        )

instance (Config :> es) => HyperView MainView es where
    data Action MainView
        = CelsiusChanged Model Text
        | FahrenheitChanged Model Text
        deriving
            ( Show
            , Read
            , Generic
            , ViewAction
            )

    update action = do
        -- Construct the new Model separately to prevent cursor stealing on fahrenheit change.
        -- We want to autofocus once, one initial page load.
        let newModel =
                let
                    updateErrFields :: (Ord k) => v -> k -> Map k v -> Map k v
                    updateErrFields v =
                        M.alter $
                            \_ -> Just v
                 in
                    case action of
                        CelsiusChanged old txt -> do
                            let
                                res = conv celsiusToFahrenheit txt
                                (converted, hasErr) =
                                    case res of
                                        Nothing -> ("", True)
                                        Just v -> (v, False)
                            old
                                { celsius = T.strip txt
                                , fahrenheit = converted
                                , errFields =
                                    updateErrFields hasErr CelsiusField
                                        . updateErrFields False FahrenheitField
                                        $ old.errFields
                                }
                        FahrenheitChanged old txt -> do
                            let res = conv fahrenheitToCelsius txt
                                (converted, hasErr) =
                                    case res of
                                        Nothing -> ("", True)
                                        Just v -> (v, False)
                            old
                                { fahrenheit = T.strip txt
                                , celsius = converted
                                , errFields =
                                    updateErrFields hasErr FahrenheitField
                                        . updateErrFields False CelsiusField
                                        $ old.errFields
                                }

        mainView $
            newModel{shouldAutofocus = False}

data FormField = CelsiusField | FahrenheitField
    deriving
        ( Show
        , Read
        , Eq
        , Ord
        , Generic
        , ToJSON
        , FromJSON
        , ToJSONKey
        , FromJSONKey
        )

data Model = MkModel
    { shouldAutofocus :: Bool
    , celsius :: Text
    , fahrenheit :: Text
    , errFields :: Map FormField Bool
    }
    deriving
        ( Show
        , Read
        , Generic
        , ToJSON
        , FromJSON
        )

isDev :: AppEnv -> Bool
isDev = \case
    Dev -> True
    Prod -> False

mainView :: (Config :> es) => Model -> Eff es (View MainView ())
mainView model = do
    cfg <- getConfig
    pure $ do
        h1 id "Temp Converter"

        when (isDev cfg.appEnv) $
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

                inp :: (Model -> Text -> Action MainView) -> Text -> Bool -> View MainView ()
                inp action val hasError' =
                    input
                        ( onChange action
                            . value val
                            . bool id autofocus (model.shouldAutofocus)
                            . bool id (extClass "error") hasError'
                        )

                hasError key =
                    case M.lookup key model.errFields of
                        Just True -> True
                        _ -> False

            span id $ do
                inp CelsiusChanged model.celsius (hasError CelsiusField)
                label id "Celsius"

            span id "="

            span id $ do
                inp FahrenheitChanged model.fahrenheit (hasError FahrenheitField)
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
