# 7GUIs — PureScript / Flame

The [7GUIs tasks](https://eugenkiss.github.io/7guis/tasks) implemented in
[Flame](https://github.com/easafe/purescript-flame), a small Elm-Architecture
(MVU) framework for PureScript.

Deployed at <https://benjamin-thomas.github.io/7guis/purescript/flame/>.

## Goals (what's different about this one)

1. **Test-first.** Unlike the other implementations in this repo, the goal here
   is to build every task in **full TDD** — write the failing test, then the
   code. The architecture below makes that genuinely easy.

2. **The effect pattern.** Each task's `update` is a **pure** function returning
   the next model *and a list of effects represented as data* — not opaque
   commands. So a test asserts on the exact `(model, [effect])` a message
   produces, with no mocks and no DOM. (Search the web for "the Elm effect
   pattern" / "effects as data" for background.) The impure interpreter that
   actually performs those effects lives separately, at the edge.

3. **A time-travel debugger.** To explore the idea, Flame is **vendored and
   patched** (see `timer/vendor/flame/`) with a tiny, app-agnostic devtools hook.
   A standalone debugger (`timer/devtools.js`) records every transition and lets
   you scrub/jump through state. It is intentionally **left enabled on the
   deployed GitHub Pages build** so you can play with it live — click around the
   Timer, then use the panel on the right.

   The patch and its design are documented in
   [`FLAME_DEVTOOLS_PATCH.md`](./FLAME_DEVTOOLS_PATCH.md) (written so it could be
   turned into an upstream proposal).

## Status

| Task | Status |
|------|--------|
| Counter | ⬜ todo |
| Temperature Converter | ⬜ todo |
| Flight Booker | ⬜ todo |
| **Timer** | ✅ done |
| CRUD | ⬜ todo |
| Circle Drawer | ⬜ todo |
| Cells | ⬜ todo |

The landing grid (`index.html`) shows all seven; unimplemented ones are dimmed.

## Layout

```
purescript/flame/
  index.html         ← plain grid linking to each task (an index; no docs button)
  docs.html          ← "Technical decisions" write-up (diagrams + highlighted code);
                       linked from the ROOT landing page, not from this index
  7guis-theme.css    ← shared site theme (copied from ../../shared on deploy)
  README.md
  FLAME_DEVTOOLS_PATCH.md   ← the Flame runtime changes, for an upstream proposal
  timer/             ← one self-contained Flame app per task (only Timer so far)
    src/Timer.purs     pure Model/Msg/effects + update, the interpreter, the view
    src/Main.purs      mounts the app (adapts pure update → Flame)
    test/Main.purs     spec tests over the pure update (no mocks, no DOM)
    devtools.js        the standalone time-travel debugger
    vendor/flame/      patched Flame (devtools hook in the runtime)
    styles/, app.js, index.html, vite.config(none), package.json, spago.yaml
```

Each task is its **own self-contained app** (its own `spago.yaml`, its own
`vendor/flame`). That keeps tasks independent and easy to build/test one at a
time — at the cost of duplicating the vendored Flame. A future cleanup could
hoist `vendor/flame` to a shared workspace.

## Adding the next task (the TDD loop)

1. `cp -r timer counter` (gives you the whole effect-pattern + debugger setup).
2. Gut `src/Timer.purs` → `src/<Task>.purs`: define `Model`, `Msg`, the effect
   ADT, and a pure `update :: Model -> Msg -> Tuple Model (Array <Effect>)`.
3. **Write `test/Main.purs` first** — assert the `(model, [effect])` transitions.
   `spago test` (red → green).
4. Fill in the `interpret`er (effects → `Aff`), the `view`, and wire `Main.purs`.
5. Enable its card in `index.html` and add a build step in the deploy workflow.

## Running it

Per task (from `timer/`):

```bash
yarn install        # one-time, for vite
yarn test           # spago test — the TDD inner loop
yarn dev            # spago build + vite dev server (debugger on) → localhost:5173
yarn dev:no-dbg     # same, debugger off
yarn build          # production bundle into dist/
```

To preview the whole grid locally, serve `purescript/flame/` with any static
server after building each task into its `dist/` (or just open `index.html` and
click through — the Timer link points at `timer/`).

## Deploy

The repo's GitHub Actions workflow (`.github/workflows/deploy.yml`) builds each
task with the right base path and assembles everything under `docs/`. The Flame
Timer is built with `VITE_DEBUG=1` so the debugger ships on the live site.
