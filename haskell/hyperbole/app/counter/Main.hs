module Main (main) where

import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import DevReload (devReloadPageJs)
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Effectful.Dispatch.Dynamic (interpret, send)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import System.Environment qualified as SE
import Text.Read (readMaybe)
import Web.Hyperbole
    ( Application
    , DefaultParam (..)
    , Eff
    , FromParam
    , Generic
    , HyperView (Action, update)
    , Hyperbole
    , Mod
    , Page
    , Session (sessionKey)
    , ToParam
    , View
    , ViewAction
    , ViewId
    , att
    , button
    , col
    , hyper
    , liveApp
    , modifySession_
    , run
    , runPage
    , scriptEmbed
    , session
    , tag
    , value
    , type (:>)
    )
import Prelude hiding (div)

{-
ghcid -c 'cabal repl counter' -T :main --warnings --reload=./assets/css/counter.css
-}

main :: IO ()
main = do
    mStr <- SE.lookupEnv "PORT"
    let port = fromMaybe 4321 $ readMaybe =<< mStr
    let staticMiddleware = staticPolicy (addBase "assets")
    run port $ staticMiddleware app

document :: Text -> BSL.ByteString -> BSL.ByteString
document title cnt =
    [i|
<!DOCTYPE html>
<html>
    <head>
    <title>#{title}</title>
    <script type="text/javascript">#{scriptEmbed}</script>

    <link href="/css/counter.css"
            rel="stylesheet">

    <script>
        #{devReloadPageJs}
    </script>

    </head>
    <body>#{cnt}</body>
</html>
|]

app :: Application
app = do
    liveApp
        (document "Counter")
        (runCookieSession $ runPage page)

page ::
    ( Hyperbole :> es
    , CounterEff :> es
    ) =>
    Eff es (Page '[CounterView])
page = do
    n <- load
    pure $ col id $ do
        hyper MkCounterView $ counterView n

newtype Counter = MkCounter Int
    deriving newtype
        ( Show
        , Read
        , ToParam
        , FromParam
        )

instance Session Counter where
    sessionKey = "counter"

instance DefaultParam Counter where
    defaultParam = MkCounter 0

data CounterEff :: Effect where
    Load :: CounterEff m Int
    Save :: Int -> CounterEff m ()

type instance DispatchOf CounterEff = 'Dynamic

load :: (CounterEff :> es) => Eff es Int
load = send Load

save :: (CounterEff :> es) => Int -> Eff es ()
save = send . Save

runCookieSession ::
    forall es a.
    (Hyperbole :> es) =>
    Eff (CounterEff : es) a ->
    Eff es a
runCookieSession = interpret $ \_ -> \case
    Load -> do
        MkCounter n <- session
        pure n
    Save n -> do
        modifySession_ $
            \_oldCounter -> MkCounter n

data CounterView = MkCounterView
    deriving
        ( Show
        , Read
        , Generic
        , ViewId
        )

instance (CounterEff :> es) => HyperView CounterView es where
    data Action CounterView
        = Inc Int
        deriving
            ( Show
            , Read
            , Generic
            , ViewAction
            )

    update action = do
        case action of
            Inc curr -> do
                let n = curr + 1
                save n
                pure $ counterView n

counterView :: Int -> View CounterView ()
counterView cnt = do
    h1 id "Counter"

    div id $ do
        input
            ( disabled
                . value (T.pack $ show cnt)
            )
        button
            (Inc cnt)
            (att "style" "cursor: pointer")
            "Count"

h1 :: Mod c -> View c () -> View c ()
h1 = tag "h1"

div :: Mod c -> View c () -> View c ()
div = tag "div"

input :: Mod c -> View c ()
input m = tag "input" m ""

disabled :: Mod c
disabled = att "disabled" ""
