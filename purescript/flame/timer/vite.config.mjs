import { defineConfig } from "vite";

export default defineConfig({
  // The time-travel debugger identifies messages/ADTs by `value.constructor.name`
  // (PureScript compiles each data constructor to a JS class). Minification would
  // rename those (e.g. `Ticked` -> `e`), so the panel would show every message as
  // "e". `keepNames` preserves them through minification.
  esbuild: { keepNames: true },
});
