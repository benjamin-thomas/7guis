# Flame devtools patch

This documents the changes made to the **vendored copy of Flame** in
`timer/vendor/flame/` so they can be reviewed, maintained, and — if it turns out
to be worth it — proposed upstream.

The patch adds a tiny **time-travel devtools hook to Flame's runtime**, following
the same model as React/Vue devtools: the runtime looks for an external hook on a
global, and if it's present, streams state transitions to it and hands it a small
control handle. **If no hook is installed, behaviour is unchanged** and the cost
is one property read at mount.

Everything lives in a single module: `src/Flame/Application/Application.purs`
(+ a new FFI file `Application.js`). The user-facing app never changes — it keeps
calling `mount`/`mount_`.

---

## Why touch the runtime at all?

Time-travel is fundamentally a **runtime** concern:

- To **record** every transition you need a tap on the update loop.
- To **jump** to a past state you must *replace the current model and re-render*
  — which only the runtime can do without tearing the app down and re-mounting
  (re-mount loses focus/scroll/subscriptions/in-flight effects).

Flame's public API (`mount`, `send`, `subscribe`) exposes neither a transition
tap nor a "set model" operation, so a faithful debugger can't be built purely in
userland. The good news: the runtime already had everything needed internally —
the model lives in a `Ref`, and there is already a `render` function that writes
that `Ref` and re-renders **through the existing diff**. So `setModel` is just
that `render` function, exposed. The patch is small precisely because of this.

---

## What changed (summary)

In `Flame.Application`:

1. **New public types**
   - `Observer model message = model -> message -> model -> Effect Unit`
     (prev model, message, next model)
   - `DevTools model message = { setModel :: model -> Effect Unit, setPaused :: Boolean -> Effect Unit, dispatch :: message -> Effect Unit }`

2. **New FFI** (`Application.js`) for the global hook:
   - `findDevtools :: Effect (Nullable DevtoolsHook)` — reads `window.__FLAME_DEVTOOLS__`
   - `devtoolsRecord :: DevtoolsHook -> model -> message -> model -> Effect Unit`
   - `devtoolsConnect :: DevtoolsHook -> DevTools model message -> Effect Unit`

3. **`run` changed** to take an `Observer` and **return a `DevTools` handle**, and
   to:
   - keep a `pausedState :: Ref Boolean`; `runUpdate` is wrapped in
     `unless isPaused …` (so a paused app ignores incoming messages/subscriptions);
   - call the observer **and** any installed global hook on every transition;
   - after mounting, hand the global hook the control handle (`connect`);
   - return `{ setModel: render, setPaused: …, dispatch: runUpdate }`.

4. **`mount` / `mount_` / `mountWith` / `resumeMount*`**: unchanged behaviour —
   they pass a no-op observer and ignore the returned handle (`void`/`_ <-`).

5. **New public `mountDebug`** — like `mount_` but takes an `Observer` and returns
   the `DevTools` handle, for programmatic/explicit use (tests, custom tooling).

The whole user app is untouched: `Main` still calls `mount_`. The external
debugger script installs `window.__FLAME_DEVTOOLS__` *before* the app mounts;
that's the only thing that turns the hook "on".

---

## The key insight: `setModel` is the existing `render`

`run` already contains (unchanged):

```purescript
render model = do
      rendering <- ER.read renderingState
      FRD.resume rendering $ application.view model   -- diff + patch the DOM
      ER.write model modelState                       -- update the model Ref
```

`render` *is* a model setter that re-renders via the existing diff. Exposing it
as `setModel` gives time-travel jumps **for free, with no re-mount**. This is the
load-bearing observation of the whole patch.

## The pause: freeze without stopping the world

`runUpdate` now reads a `pausedState` ref and short-circuits while paused:

```purescript
runUpdate message = do
      isPaused <- ER.read pausedState
      unless isPaused do
            currentModel <- ER.read modelState
            let Tuple model affs = application.update currentModel message
            observeAll currentModel message model
            when (FIE.modelHasChanged currentModel model) $ render model
            DF.for_ affs $ EA.runAff_ (…)
```

While time-travelling we set `paused = true`, so subscriptions and external
`send`s are simply **dropped** (e.g. a timer keeps firing `tick` events; the
runtime ignores them) — the app freezes at the inspected state. `setModel`
bypasses `runUpdate` entirely, so jumps still work while paused.

## Detecting the hook (React/Vue style)

```purescript
devtools <- map toMaybe findDevtools          -- window.__FLAME_DEVTOOLS__ or Nothing

let observeAll prev msg next = do
      observe prev msg next                    -- the explicit Observer arg
      case devtools of
            Just hook -> devtoolsRecord hook prev msg next
            Nothing   -> pure unit
    …
-- after mounting + wiring subscriptions:
case devtools of
      Just hook -> devtoolsConnect hook { setModel: render, setPaused: …, dispatch: runUpdate }
      Nothing   -> pure unit
pure { setModel: render, setPaused: …, dispatch: runUpdate }
```

No external hook installed ⇒ `Nothing` ⇒ no observation, no handle handed out,
no behaviour change. (This is exactly how React ships the
`__REACT_DEVTOOLS_GLOBAL_HOOK__` check in production: present but inert.)

## The FFI (`Application.js`)

```js
export const findDevtools = () =>
  (typeof window !== "undefined" && window.__FLAME_DEVTOOLS__) || null;

export const devtoolsRecord = (hook) => (prev) => (msg) => (next) => () => {
  hook.record(prev, msg, next);
};

export const devtoolsConnect = (hook) => (handle) => () => {
  // unwrap the curried PureScript effects (a -> Effect Unit === (a) => () => Unit)
  hook.connect({
    setModel: (model) => handle.setModel(model)(),
    setPaused: (paused) => handle.setPaused(paused)(),
    dispatch: (msg) => handle.dispatch(msg)(),
  });
};
```

---

## Design decisions & trade-offs

- **Global hook vs. a typed PS API.** The global (`window.__FLAME_DEVTOOLS__`)
  decouples the debugger entirely from the app and lets it be any script (even a
  browser extension). The cost: `model`/`message` cross the boundary as
  `Foreign` (opaque). The debugger pretty-prints them heuristically and stores
  the model objects to pass back to `setModel`. A typed in-process API
  (`mountDebug`) is also exposed for when you want type safety.

- **No re-mount for jumps.** Reusing `render` keeps DOM identity (focus, scroll,
  listeners) — strictly better than re-mounting with a chosen initial model.

- **Pause at `runUpdate`, not at the source.** Generic: it freezes *all* inbound
  messages (subscriptions, `send`, effect callbacks) without the runtime needing
  to know about any particular effect (timers, sockets, …).

- **Backwards compatible.** Only `run`'s (internal) signature changed; every
  public entry point keeps its behaviour. `mountDebug` + the types are additive.

- **Memory.** Recording holds references to past models; PureScript's immutable
  updates give structural sharing, so snapshots are cheap. The *debugger* (not
  Flame) bounds how many it keeps.

## Open questions for an upstream proposal

- **Multiple apps / `AppId`.** The hook currently carries no app identity, so
  several mounted Flame apps would interleave in one timeline. A real version
  should namespace transitions by `AppId` (Flame already has it) — `record`
  would take the id, and `connect` would register a per-app handle.

- **Production gating.** Should the `findDevtools` read be compiled out of
  production builds (à la React's `NODE_ENV` checks)? It's a single property read,
  so the patch leaves it always-on; a flag/`spago` build option could strip it.

- **Hook shape & versioning.** `{ record, connect }` is minimal. A real protocol
  would want a version field and a documented `DevTools` handle contract.

- **`setPaused` semantics.** Dropping messages while paused is simple but means
  `dispatch` is also ignored while paused (fine for our use — we jump with
  `setModel`, not `dispatch`). An alternative is a queue that replays on resume.

---

## Files changed in the vendored copy

- `src/Flame/Application/Application.purs` — types, FFI imports, `run` rewrite,
  `mountDebug`, no-op wiring in the existing entry points.
- `src/Flame/Application/Application.js` — **new**; the three FFI functions above.

To see the exact diff against upstream, compare `timer/vendor/flame/` to the
`flame@1.6.0` release on the registry / `easafe/purescript-flame` at tag `v1.6.0`.
