// Vite entry point: pull in the copied CSS and boot the compiled PureScript app.
import "./styles/index.css";
import "./styles/timer.css";
import "./styles/timer2.css";

import { main } from "./output/Main/index.js";

// The debugger loads when:
//   - dev, unless turned off    (`VITE_DEBUG=0 yarn dev`), OR
//   - forced on at build time   (`VITE_DEBUG=1 vite build`) — this is how the
//     GitHub Pages deploy ships it on a *production* build.
// Otherwise (a plain prod build) `import.meta.env.DEV` is statically false and
// VITE_DEBUG is unset, so Vite drops this branch and devtools.js never ships.
// devtools.js installs window.__FLAME_DEVTOOLS__ before the app mounts; Flame's
// patched runtime then detects it. The app (Main/Timer) is unaware either way.
if ((import.meta.env.DEV && import.meta.env.VITE_DEBUG !== "0") || import.meta.env.VITE_DEBUG === "1") {
  // Pass config via a global: import.meta.env isn't reliably inlined inside the
  // dynamically-imported devtools.js, but it is here in the entry module.
  window.__FLAME_DEBUG_HISTORY_CAP__ = Number(import.meta.env.VITE_DEBUG_HISTORY_CAP) || undefined;
  // Load the debugger (installs the hook) THEN mount. Using `.then` instead of a
  // top-level await keeps the prod bundle within esbuild's default target, which
  // doesn't allow top-level await (matters because VITE_DEBUG=1 *includes* this
  // branch in a production build).
  import("./devtools.js").then(() => main());
} else {
  main();
}
