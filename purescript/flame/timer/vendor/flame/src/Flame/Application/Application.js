// Devtools bridge. Flame's runtime looks for an external devtools installed on
// `window.__FLAME_DEVTOOLS__`; if present it emits state transitions to it and
// hands it a control handle. The app stays unaware.

export const findDevtools = () =>
  (typeof window !== "undefined" && window.__FLAME_DEVTOOLS__) || null;

export const devtoolsRecord = (hook) => (prev) => (msg) => (next) => () => {
  hook.record(prev, msg, next);
};

export const devtoolsConnect = (hook) => (handle) => () => {
  // `handle.setModel` / `handle.dispatch` are curried PureScript effects
  // (`a -> Effect Unit` === `(a) => () => Unit`); unwrap them for plain JS use.
  hook.connect({
    setModel: (model) => handle.setModel(model)(),
    setPaused: (paused) => handle.setPaused(paused)(),
    dispatch: (msg) => handle.dispatch(msg)(),
  });
};
