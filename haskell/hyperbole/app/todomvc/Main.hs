module Main where

import Control.Monad (forM_)
import Data.ByteString.Lazy qualified as BSL
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.String.Interpolate
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Web.Hyperbole hiding (basicDocument)
import Web.Hyperbole.Data.Param qualified as HD
import Web.Hyperbole.View.Forms (Input (Input))
import Web.View.Style (addClass, cls, extClass, prop)
import Web.View.Types (AttValue)
import Prelude hiding (div, span)

import DevReload (devReloadPageJs)
import System.Environment qualified as SE
import Text.Read (readMaybe)

{-
rg --files -g '!/dist*' | entr -rc cabal run todomvc
ghcid -c 'cabal repl todomvc' -T :main --warnings
-}

data Todo = MkTodo
    { id_ :: Int
    , descr :: Text
    , done :: Bool
    }
    deriving
        ( Show
        , Read
        , Eq
        , Ord
        , Generic
        , ToJSON
        , FromJSON
        )

main :: IO ()
main = do
    mStr <- SE.lookupEnv "PORT"
    let port = fromMaybe 4321 $ readMaybe =<< mStr
    putStrLn $ "Starting web server on port: " <> show port
    run port app

updateVarReturning ::
    (Concurrent :> es) =>
    TVar a ->
    (a -> a) ->
    Eff es a
updateVarReturning var f = atomically $ do
    modifyTVar var f
    readTVar var

basicDocument :: Text -> BSL.ByteString -> BSL.ByteString
basicDocument title cnt =
    [i|<html>
      <head>
        <title>#{title}</title>
        <script type="text/javascript">#{scriptEmbed}</script>

        <link href="https://cdn.jsdelivr.net/npm/todomvc-app-css@2.4.3/index.min.css"
              rel="stylesheet">

        <script>
          #{devReloadPageJs}
        </script>
      </head>
      <body>#{cnt}</body>
  </html>|]

filterParam :: HD.Param
filterParam = "filter"

data Filter
    = FilterAll
    | FilterActive
    | FilterCompleted
    deriving
        ( Show
        , Read
        , Eq
        , Generic
        , FromJSON
        , ToJSON
        )

toFilter :: Maybe Text -> Filter
toFilter = \case
    Just "active" -> FilterActive
    Just "completed" -> FilterCompleted
    _ -> FilterAll

getFilterParam :: (Hyperbole :> es) => Eff es Filter
getFilterParam = toFilter <$> lookupParam filterParam

app :: Application
app = do
    liveApp
        (basicDocument "TodoMVC: Haskell/Hyperbole")
        (runLogging . runTodosSession $ runPage page)

data AppRoute
    = Main
    deriving (Eq, Generic)

instance Route AppRoute where
    baseRoute = Just Main

newtype TodoStore = MkTodoStore (Map Int Todo)
    -- deriving
    --     ( Show
    --     , Read
    --     , Generic
    --     )
    deriving newtype
        ( Ord
        , Eq
        , FromJSON
        , ToJSON
        )

instance Session TodoStore where
    sessionKey = "todo-store"

initStore :: Map Int Todo
initStore =
    M.fromList
        [ (1, MkTodo 1 "Buy milk" False)
        , (2, MkTodo 2 "Clean room" False)
        , (3, MkTodo 3 "Go running" False)
        , (4, MkTodo 4 "Do the dishes" False)
        ]

instance Default TodoStore where
    def =
        MkTodoStore initStore

data TodosEff :: Effect where
    LoadAllEff :: TodosEff m [Todo]
    SaveEff :: Todo -> TodosEff m ()
    ToggleOneEff :: Todo -> TodosEff m ()
    ClearCompletedEff :: TodosEff m ()

data Logging :: Effect where
    ConsoleLog :: (Show a) => String -> a -> Logging m ()

loadAll :: (TodosEff :> es) => Eff es [Todo]
loadAll = send LoadAllEff

clearCompleted :: (TodosEff :> es) => Eff es ()
clearCompleted = send ClearCompletedEff

save :: (TodosEff :> es) => Todo -> Eff es ()
save = send . SaveEff

toggleOne :: (TodosEff :> es) => Todo -> Eff es Todo
toggleOne todo = do
    let newTodo = todo{done = not todo.done}
    save newTodo
    pure newTodo

type instance DispatchOf TodosEff = 'Dynamic

runTodosSession ::
    forall es a.
    (Hyperbole :> es) =>
    Eff (TodosEff : es) a ->
    Eff es a
runTodosSession = interpret $ \_ -> \case
    LoadAllEff -> do
        MkTodoStore todos <- session
        pure $ M.elems todos
    SaveEff todo -> do
        modifySession_ $
            changeStore
                ( M.alter
                    (\_ -> Just todo)
                    todo.id_
                )
    ClearCompletedEff -> do
        modifySession_ $
            changeStore (M.filter (\t -> not t.done))
    ToggleOneEff todo -> do
        modifySession_ $
            changeStore
                ( M.update
                    (\_ -> Just todo{done = not todo.done})
                    todo.id_
                )
  where
    changeStore :: (Map Int Todo -> Map Int Todo) -> TodoStore -> TodoStore
    changeStore f (MkTodoStore old) =
        MkTodoStore (f old)

type instance DispatchOf Logging = 'Dynamic

runLogging ::
    forall a es.
    (IOE :> es) =>
    Eff (Logging : es) a ->
    Eff es a
runLogging = interpret $ \_ -> \case
    ConsoleLog label' msg ->
        liftIO $
            putStrLn $
                mconcat
                    [ "LOG[" <> label' <> "]"
                    , " "
                    , show msg
                    ]

consoleLog :: (Logging :> es, Show a) => String -> a -> Eff es ()
consoleLog str = send . ConsoleLog str

data TodosView = MkTodosView
    deriving
        ( Show
        , Read
        , Generic
        , ViewId
        )

pluralize :: Int -> Text -> Text -> Text
pluralize n singular plural =
    if n == 1
        then
            singular
        else
            plural

page ::
    ( TodosEff :> es
    , Hyperbole :> es
    ) =>
    Eff es (Page '[TodosView, TodoView])
page = do
    filter' <- getFilterParam
    todos <- loadAll
    pure $
        hyper MkTodosView (todosView True filter' todos)

instance
    ( TodosEff :> es
    , Logging :> es
    ) =>
    HyperView TodosView es
    where
    type Require TodosView = '[TodoView]

    data Action TodosView
        = ClearCompleted
        | FilterClicked Filter
        | SubmitTodo
        | Toggle Todo
        | ToggleAll
        deriving
            ( Show
            , Read
            , Generic
            , ViewAction
            )

    update = \case
        ClearCompleted -> do
            clearCompleted
            todos <- loadAll
            filter' <- getFilterParam
            pure $ todosView False filter' todos
        FilterClicked typ -> case typ of
            FilterAll -> do
                setParam filterParam ("all" :: Text)
                todos <- loadAll
                pure $ todosView False FilterAll todos
            FilterActive -> do
                setParam filterParam ("active" :: Text)
                todos <- loadAll
                pure $ todosView False FilterActive todos
            FilterCompleted -> do
                setParam filterParam ("completed" :: Text)
                todos <- loadAll
                pure $ todosView False FilterCompleted todos
        SubmitTodo -> do
            consoleLog "SubmitTodo" ("ENTER" :: Text)
            fd <- formData @(TodoForm Identity)
            consoleLog "FormData" (fd.descr')
            todos <- loadAll
            let furthest =
                    let ids = fmap (.id_) todos
                     in case ids of
                            [] -> 1
                            _ -> maximum ids

            let todo =
                    MkTodo
                        { id_ = furthest + 1
                        , descr = descr' fd
                        , done = False
                        }
            save todo -- TODO: should I return the collection here instead?
            filter' <- getFilterParam

            pure $ todosView False filter' (todos <> [todo])
        Toggle todo -> do
            _ <- toggleOne todo
            filter' <- getFilterParam
            todos <- loadAll
            pure $ todosView False filter' todos
        ToggleAll -> do
            consoleLog "ToggleAll" ("ENTER" :: Text)
            -- TODO: the filtering logic is a bit wonky here
            -- TODO: even from a UI's perspective so I'm not going to sweat it
            -- TODO: I could make the data access more "performant" by representing bulk updates though
            filter' <- getFilterParam
            todos <- loadAll
            let newTodos =
                    fmap
                        (\t -> t{done = not $ t.done})
                        (filterTodos filter' todos)
            mapM_ save newTodos
            consoleLog "ToggleAll" newTodos
            pure $ todosView False FilterAll newTodos

red :: HexColor
red = HexColor "#FF0000"

filterTodos :: Filter -> [Todo] -> [Todo]
filterTodos = \case
    FilterAll -> id
    FilterActive -> filter (not . isDone)
    FilterCompleted -> filter isDone
  where
    isDone :: Todo -> Bool
    isDone = (.done)

todosView :: Bool -> Filter -> [Todo] -> View TodosView ()
todosView shouldFocus filter' todos = do
    section
        (klass "todoapp") -- cspell: disable-line
        ( do
            header (klass "header") $ do
                let f :: TodoForm FieldName = fieldNames
                h1 id "todos"
                form SubmitTodo id $ do
                    field f.descr' id $ do
                        -- WARN: TextInput sets the autofocus field to "text-field" (which is not valid anyways)
                        -- <input autocomplete="off" autofocus="" class="new-todo new-todo?" name="descr'" placeholder="What needs to be done?" type="text" value="">
                        -- input
                        --     TextInput
                        --     ( extClass "new-todo?"
                        --         . extClass "new-todo"
                        --         . (if shouldFocus then autofocus else id)
                        --         . value ""
                        --         . placeholder "What needs to be done?"
                        --         . att "autocomplete" "off"
                        --     )
                        -- <input autocomplete="off" autofocus="" class="new-todo" placeholder="What needs to be done?" value="">

                        Input (FieldName nm) <- context
                        tag
                            "input"
                            ( extClass "new-todo"
                                . (if shouldFocus then autofocus else id)
                                . value ""
                                . name nm
                                . att "placeholder" "What needs to be done?"
                                . att "autocomplete" "off"
                            )
                            ""

            main' (klass "main") $ do
                div (klass "toggle-all-container") $ do
                    tag
                        -- FIXME: CSS relies on ":checked" state
                        -- FIXME: and it seems that this state is not tracked by Hyperbole
                        -- TODO: should add a delete "cross" to the item line
                        "input"
                        ( klass "toggle-all"
                            . att "id" "toggle-all"
                            . att "type" "checkbox"
                        )
                        ""
                    tag
                        "label"
                        ( klass "toggle-all-label"
                            . for "toggle-all"
                            . onClick ToggleAll
                        )
                        $ do
                            text "Mark all as complete"

                tag "ul" (klass "todo-list") $ do
                    forM_ (filterTodos filter' todos) $ \todo -> do
                        hyper (MkTodoView todo.id_) $ todoView todo

                tag
                    "footer"
                    (klass "footer")
                    $ do
                        tag "span" (klass "todo-count") $ do
                            text $
                                let len =
                                        length
                                            ( filter
                                                (\t -> not t.done)
                                                (filterTodos FilterActive todos)
                                            )
                                 in T.unwords
                                        [ T.pack $ show $ len
                                        , pluralize len "item" "items"
                                        , "left"
                                        ]

                        tag "ul" (klass "filters") $ do
                            let lnk target' txt =
                                    tag
                                        "a"
                                        ( ( if filter' == target'
                                                then klass "selected"
                                                else
                                                    id
                                          )
                                            . att "href" ""
                                            . onClick (FilterClicked target')
                                        )
                                        txt
                            tag
                                "li"
                                (klass "filter")
                                (lnk FilterAll "All")
                            tag
                                "li"
                                (klass "filter")
                                (lnk FilterActive "Active")
                            tag
                                "li"
                                (klass "filter")
                                (lnk FilterCompleted "Completed")

                        button ClearCompleted (klass "clear-completed") $ do
                            text "Clear completed"
        )

data TodoView = MkTodoView Int
    deriving
        ( Show
        , Read
        , Generic
        , ViewId
        )

instance
    ( TodosEff :> es
    , Logging :> es
    ) =>
    HyperView TodoView es
    where
    type Require TodoView = '[TodosView]

    data Action TodoView
        = EnterEdit Todo
        | SubmitEdit Todo
        deriving
            ( Show
            , Read
            , Generic
            , ViewAction
            )

    update = \case
        EnterEdit todo -> do
            pure $ todoEditView todo
        SubmitEdit old -> do
            fd <- formData @(TodoForm Identity)
            let new = old{descr = descr' fd}
            save new
            consoleLog "saved" new
            pure $ todoView new

lineThrough :: Mod id
lineThrough =
    addClass $
        (prop @Text "text-decoration" "line-through")
            (cls "line-through")

data TodoForm f = TodoForm
    { descr' :: Field f Text
    }
    deriving
        ( Generic
        , FromFormF
        , GenFields FieldName
        )

-- instance Form (TodoForm Maybe)

todoEditView :: Todo -> View TodoView ()
todoEditView todo = do
    let f = fieldNames @(TodoForm)
    tag "li" (klass "editing") $ do
        form (SubmitEdit todo) id $ do
            field f.descr' id $ do
                input
                    TextInput
                    ( klass "edit"
                        . value todo.descr
                    )

classList :: [(Bool, Text)] -> Mod a
classList lst =
    klass
        ( T.unwords $
            catMaybes $
                ( fmap
                    ( \(bool', name') ->
                        if bool'
                            then
                                Just name'
                            else
                                Nothing
                    )
                    lst
                )
        )

todoView :: Todo -> View TodoView ()
todoView todo =
    tag
        "li"
        ( onDblClick (EnterEdit todo)
            . classList
                [
                    ( todo.done
                    , "completed"
                    )
                ]
        )
        $ do
            div (klass "view") $ do
                target MkTodosView $ do
                    tag
                        -- FIXME: should focus field
                        "input"
                        ( klass "toggle"
                            . att "type" "checkbox"
                            . onClick (Toggle todo)
                            . (if todo.done then att "checked" "" else id)
                            . checked todo.done
                        )
                        ""
                    tag "label" (klass "label") $ text todo.descr

toggleCheckbox :: (ViewAction (Action id)) => (Bool -> Action id) -> Bool -> View id ()
toggleCheckbox clickAction isSelected = do
    tag
        "input"
        ( att "type" "checkbox"
            . checked isSelected
            . onClick (clickAction (not isSelected))
            . big
        )
        none
  where
    big = width 32 . height 32

klass :: AttValue -> Mod c
klass = att "class"

for :: AttValue -> Mod c
for = att "for"

div :: Mod c -> View c () -> View c ()
div = tag "div"

span :: Mod c -> View c () -> View c ()
span = tag "span"

section :: Mod c -> View c () -> View c ()
section = tag "section"

header :: Mod c -> View c () -> View c ()
header = tag "header"

main' :: Mod c -> View c () -> View c ()
main' = tag "main"

h1 :: Mod c -> View c () -> View c ()
h1 = tag "h1"

-- input :: Mod c -> View c ()
-- input = tag' "input"

label :: Mod c -> View c () -> View c ()
label = tag "label"

-- To define a tag with no children
tag' :: Text -> Mod c -> View c ()
tag' txt m = (tag txt) m ""

aside :: Mod c -> View c () -> View c ()
aside = tag "aside"

p :: Mod c -> View c () -> View c ()
p = tag "p"
