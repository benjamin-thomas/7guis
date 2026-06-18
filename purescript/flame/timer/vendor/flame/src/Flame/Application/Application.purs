-- | Run TEA like applications
module Flame.Application
      ( Update
      , App
      , Application
      , noMessages
      , mount
      , mount_
      , mountDebug
      , DevTools
      , Observer
      , ResumedApplication
      , resumeMount
      , resumeMount_
      ) where

import Data.Either (Either(..))
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Console as EC
import Effect.Exception as EE
import Effect.Ref as ER
import Flame.Application.Internal.Dom as FAD
import Flame.Application.Internal.PreMount as FAP
import Flame.Internal.Equality as FIE
import Flame.Renderer.Internal.Dom as FRD
import Flame.Serialization (class UnserializeState)
import Flame.Subscription.Internal.Listener as FSIL
import Flame.Types (AppId(..), ApplicationId, DomNode, DomRenderingState, Html, Subscription)
import Prelude (class Show, Unit, bind, discard, map, pure, show, unit, unless, void, when, ($), (<>))
import Unsafe.Coerce as UC
import Web.DOM.ParentNode (QuerySelector(..))

-- | An external devtools, if installed on `window.__FLAME_DEVTOOLS__`. Flame's
-- | runtime detects it and emits state transitions to it + hands it a control
-- | handle. Opaque: the app and Flame don't know its shape.
foreign import data DevtoolsHook ∷ Type

-- | `window.__FLAME_DEVTOOLS__` if present (null otherwise).
foreign import findDevtools ∷ Effect (Nullable DevtoolsHook)

-- | Notify the devtools of one transition: previous model, message, next model.
foreign import devtoolsRecord ∷ ∀ model message. DevtoolsHook → model → message → model → Effect Unit

-- | Hand the devtools the control handle (set model / inject messages).
foreign import devtoolsConnect ∷ ∀ model message. DevtoolsHook → DevTools model message → Effect Unit

type Update model message = model → message → Tuple model (Array (Aff (Maybe message)))

-- | Abstracts over common fields of an `Application`
type App model message extension =
      { view ∷ model → Html message
      , subscribe ∷ Array (Subscription message)
      | extension
      }

-- | `Application` contains
-- | * `model` – starting model
-- | * `view` – a function to update your markup
-- | * `update` – a function to update your model
-- | * `subscribe` – list of external events
type Application model message = App model message
      ( model ∷ model
      , update ∷ Update model message
      )

-- | `ResumedApplication` contains
-- | * `view` – a function to update your markup
-- | * `update` – a function to update your model
-- | * `subscribe` – list of external events
type ResumedApplication model message = App model message
      ( update ∷ Update model message
      )

-- | Observes every state transition: previous model, the message, next model.
type Observer model message = model → message → model → Effect Unit

-- | Handle returned by `mountDebug` for driving a running app from the outside
-- | (e.g. a time-travel debugger):
-- | * `setModel` – replace the current model and re-render via the existing
-- |   diff (no re-mount), e.g. to jump to a recorded state
-- | * `setPaused` – when paused, incoming messages (subscriptions, dispatch)
-- |   are ignored, so the app freezes while you time-travel. `setModel` still
-- |   works (it bypasses the update loop).
-- | * `dispatch` – inject a message, same path as a normal update
type DevTools model message =
      { setModel ∷ model → Effect Unit
      , setPaused ∷ Boolean → Effect Unit
      , dispatch ∷ message → Effect Unit
      }

noObserve ∷ ∀ model message. Observer model message
noObserve _ _ _ = pure unit

noMessages ∷ ∀ model message. model → Tuple model (Array (Aff (Maybe message)))
noMessages model = model /\ []

noAppId ∷ ∀ message. Maybe (AppId Unit message)
noAppId = Nothing

showId ∷ ∀ id message. Show id ⇒ (AppId id message) → String
showId (AppId id) = show id

-- | Mount a Flame application on the given selector which was rendered server-side
resumeMount_ ∷ ∀ model message. UnserializeState model ⇒ QuerySelector → ResumedApplication model message → Effect model
resumeMount_ selector = resumeMountWith selector noAppId

-- | Mount on the given selector a Flame application which was rendered server-side and can be fed arbitrary external messages
resumeMount ∷ ∀ id model message. UnserializeState model ⇒ Show id ⇒ QuerySelector → AppId id message → ResumedApplication model message → Effect model
resumeMount selector appId = resumeMountWith selector (Just appId)

-- | Mount on the given selector a Flame application which was rendered server-side and can be fed arbitrary external messages
resumeMountWith ∷ ∀ id model message. UnserializeState model ⇒ Show id ⇒ QuerySelector → Maybe (AppId id message) → ResumedApplication model message → Effect model
resumeMountWith (QuerySelector selector) appId resumed = do
      model ← FAP.serializedState selector
      maybeElement ← FAD.querySelector selector
      case maybeElement of
            Just parent → do
                  _ ← run parent true (map showId appId) noObserve
                        { model
                        , view: resumed.view
                        , update: resumed.update
                        , subscribe: resumed.subscribe
                        }
                  pure model
            Nothing → EE.throw $ "Error resuming application mount: no element matching selector " <> selector <> " found!"

-- | Mount a Flame application on the given selector
mount_ ∷ ∀ model message. QuerySelector → Application model message → Effect Unit
mount_ selector = mountWith selector noAppId

-- | Mount a Flame application that can be fed arbitrary external messages
mount ∷ ∀ id model message. Show id ⇒ QuerySelector → AppId id message → Application model message → Effect Unit
mount selector appId = mountWith selector (Just appId)

mountWith ∷ ∀ id model message. Show id ⇒ QuerySelector → Maybe (AppId id message) → Application model message → Effect Unit
mountWith (QuerySelector selector) appId application = do
      maybeElement ← FAD.querySelector selector
      case maybeElement of
            Just parent → void $ run parent false (map showId appId) noObserve application
            Nothing → EE.throw $ "Error mounting application"

-- | Mount a Flame application while observing every state transition, returning
-- | a `DevTools` handle (set the model / inject messages) — e.g. for an
-- | external time-travel debugger. `setModel` re-renders via the existing diff,
-- | so jumping to a past state needs no re-mount.
mountDebug ∷ ∀ model message. QuerySelector → Observer model message → Application model message → Effect (DevTools model message)
mountDebug (QuerySelector selector) observe application = do
      maybeElement ← FAD.querySelector selector
      case maybeElement of
            Just parent → run parent false Nothing observe application
            Nothing → EE.throw $ "Error mounting application"

-- | Keeps the state in a `Ref` and call `Flame.Renderer.render` for every update
run ∷ ∀ model message. DomNode → Boolean → Maybe ApplicationId → Observer model message → Application model message → Effect (DevTools model message)
run parent isResumed appId observe application = do
      modelState ← ER.new application.model
      renderingState ← ER.new (UC.unsafeCoerce 21 ∷ DomRenderingState)
      pausedState ← ER.new false
      devtools ← map toMaybe findDevtools

      let --record every transition: the explicit observer + any external devtools
            observeAll currentModel message model = do
                  observe currentModel message model
                  case devtools of
                        Just hook → devtoolsRecord hook currentModel message model
                        Nothing → pure unit

            --the function which actually run events
            runUpdate message = do
                  isPaused ← ER.read pausedState
                  unless isPaused do
                        currentModel ← ER.read modelState
                        let Tuple model affs = application.update currentModel message
                        observeAll currentModel message model
                        when (FIE.modelHasChanged currentModel model) $ render model
                        DF.for_ affs $ EA.runAff_
                              ( case _ of
                                      Left error → EC.log $ EE.message error
                                      Right (Just msg) → runUpdate msg
                                      _ → pure unit
                              )

            --the function which renders to the dom (also exposed as setModel)
            render model = do
                  rendering ← ER.read renderingState
                  FRD.resume rendering $ application.view model
                  ER.write model modelState

      rendering ←
            if isResumed then
                  FRD.startFrom parent runUpdate $ application.view application.model
            else
                  FRD.start parent runUpdate $ application.view application.model
      ER.write rendering renderingState

      --subscriptions are used for external events
      case appId of
            Nothing → pure unit
            Just id → FSIL.createMessageListener id runUpdate
      DF.traverse_ (FSIL.createSubscription runUpdate) application.subscribe

      let handle = { setModel: render, setPaused: \paused → ER.write paused pausedState, dispatch: runUpdate }
      --hand the control handle to an external devtools, if one is installed
      case devtools of
            Just hook → devtoolsConnect hook handle
            Nothing → pure unit
      pure handle