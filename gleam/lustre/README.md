# 7GUIs - Gleam / Lustre

Timer implemented with [Gleam](https://gleam.run), [Lustre](https://hexdocs.pm/lustre), effects as data, and the local Lustre time-travel debugger.

## Run

```sh
gleam test
gleam run -m lustre/dev start
```

The dev server defaults to <http://localhost:1234>.

## Build

```sh
gleam run -m lustre/dev build
```

The deployable files are written to `docs/`. They use relative links so the app can live at `/7guis/gleam/lustre/` on GitHub Pages.

## Layout

```text
gleam/lustre/
  src/timer.gleam               pure model/messages/instructions/update/view
  src/lustre_timer.gleam        Lustre runtime wiring and instruction interpreter
  src/timer_ffi.js              browser timer/server/log side effects
  test/lustre_timer_test.gleam  effects-as-data tests
  assets/                       files copied into docs/ for deployment
  lustre_time_travel/           internal debugger package
```

## Deploy

The repo-level GitHub Pages workflow builds this app from `gleam/lustre`:

```bash
gleam test
gleam run -m lustre/dev build
```

The build writes relative-path assets into `gleam/lustre/docs/`, and the
workflow copies those files into the final Pages artifact at `docs/gleam/lustre/`.
