// Flame time-travel devtools — a standalone, app-agnostic script.
//
// Installs `window.__FLAME_DEVTOOLS__`. Flame's runtime detects that global and
// (a) emits every state transition here, (b) hands us a control handle
// { setModel, setPaused, dispatch }. The app under test knows nothing about it.
//
// Loaded in dev unless turned off (see app.js: VITE_DEBUG=0). Memory is bounded
// by VITE_DEBUG_HISTORY_CAP rows (each holds one structurally-shared model).

(function () {
  // If this script re-runs (dev HMR), drop any panel we already built.
  document.querySelectorAll(".ftt, .ftt-overlay, .ftt-restore").forEach((n) => n.remove());

  const CAP = Math.max(1, window.__FLAME_DEBUG_HISTORY_CAP__ || 1000); // set by app.js from VITE_DEBUG_HISTORY_CAP

  let control = null; // { setModel, setPaused, dispatch } from Flame
  let paused = false; // frozen? (manual pause or time-travel)
  let selectedRow = null; // the row being viewed, or null = latest
  let minimized = false;
  let total = 0; // absolute count of recorded transitions (for row numbers)
  let lastRow = null; // newest row, for collapsing runs + capping
  let lastTag = null;
  let prevModel; // model before the latest (for the live diff)
  let latestModel;
  let pending = []; // transient buffer drained each animation frame
  let rafScheduled = false;
  let includeTags = []; // "filter": if non-empty, show ONLY these (exact)
  let hideTags = []; // "hide": hide these (exact)
  const knownTags = new Set(); // for autosuggest

  // ---- pretty-print PureScript values (best effort over the JS rep) -------
  function ctorName(v) {
    if (v && typeof v === "object") {
      const n = Object.getPrototypeOf(v)?.constructor?.name;
      if (n && n !== "Object" && n !== "Array") return n;
    }
    return null;
  }
  // single-line (history titles, diff leaves)
  function show(v) {
    if (v === null || v === undefined) return String(v);
    if (typeof v === "string") return JSON.stringify(v);
    if (typeof v !== "object") return String(v);
    if (Array.isArray(v)) return "[" + v.map(show).join(", ") + "]";
    const ctor = ctorName(v), keys = Object.keys(v);
    if (ctor) {
      const pos = keys.filter((k) => /^value\d+$/.test(k));
      if (pos.length) return ctor + "(" + pos.map((k) => show(v[k])).join(", ") + ")";
      const rec = keys.filter((k) => !/^value\d+$/.test(k));
      return rec.length ? ctor + " { " + rec.map((k) => k + ": " + show(v[k])).join(", ") + " }" : ctor;
    }
    return "{ " + keys.map((k) => k + ": " + show(v[k])).join(", ") + " }";
  }
  // multi-line, indented (the Current Model pane)
  function pretty(v, ind) {
    const pad = "  ".repeat(ind), pad1 = "  ".repeat(ind + 1);
    if (v === null || v === undefined) return String(v);
    if (typeof v === "string") return JSON.stringify(v);
    if (typeof v !== "object") return String(v);
    if (Array.isArray(v)) return v.length ? "[\n" + v.map((x) => pad1 + pretty(x, ind + 1)).join(",\n") + "\n" + pad + "]" : "[]";
    const ctor = ctorName(v), keys = Object.keys(v);
    if (ctor) {
      const pos = keys.filter((k) => /^value\d+$/.test(k));
      if (pos.length === 1 && v[pos[0]] && typeof v[pos[0]] === "object") return ctor + " " + pretty(v[pos[0]], ind);
      if (pos.length) return ctor + "(" + pos.map((k) => pretty(v[k], ind)).join(", ") + ")";
      const rec = keys.filter((k) => !/^value\d+$/.test(k));
      return rec.length ? ctor + " " + prettyRec(v, rec, ind) : ctor;
    }
    return prettyRec(v, keys, ind);
  }
  function prettyRec(v, keys, ind) {
    const pad = "  ".repeat(ind), pad1 = "  ".repeat(ind + 1);
    return keys.length ? "{\n" + keys.map((k) => pad1 + k + ": " + pretty(v[k], ind + 1)).join(",\n") + "\n" + pad + "}" : "{}";
  }
  // structural diff → [{ path, old, new }] of changed leaves. "value0" wrapper
  // segments are elided so paths read like `timerState.elapsedMs`.
  function diff(a, b, path, out) {
    if (a === b) return;
    const ao = a && typeof a === "object", bo = b && typeof b === "object";
    if (ao && bo && !Array.isArray(a) && !Array.isArray(b)) {
      if (ctorName(a) !== ctorName(b)) { out.push({ path: path || "(root)", old: show(a), new: show(b) }); return; }
      for (const k of new Set([...Object.keys(a), ...Object.keys(b)])) {
        const seg = /^value\d+$/.test(k) ? "" : k;
        diff(a[k], b[k], seg ? (path ? path + "." + seg : seg) : path, out);
      }
      return;
    }
    if (Array.isArray(a) && Array.isArray(b)) {
      for (let i = 0; i < Math.max(a.length, b.length); i++) diff(a[i], b[i], (path || "") + "[" + i + "]", out);
      return;
    }
    out.push({ path: path || "(root)", old: show(a), new: show(b) });
  }

  // ---- styles ------------------------------------------------------------
  const style = document.createElement("style");
  style.textContent = `
    .ftt { position: fixed; top: 0; right: 0; width: 360px; height: 100vh;
      background: #161616; color: #ddd; font: 12px/1.5 ui-monospace, Menlo, monospace;
      border-left: 1px solid #333; display: flex; flex-direction: column; z-index: 99999; }
    .ftt--left { left: 0; right: auto; border-left: 0; border-right: 1px solid #333; }
    .ftt-h { display: flex; align-items: center; gap: 6px; padding: 8px 10px; border-bottom: 1px solid #333; }
    .ftt-h b { color: #f59e0b; font-weight: 600; }
    .ftt-h .ftt-hbtns { margin-left: auto; display: flex; gap: 6px; }
    .ftt button { font: inherit; background: #2a2a2a; color: #ddd; border: 1px solid #444;
      border-radius: 4px; padding: 2px 8px; cursor: pointer; }
    .ftt button:hover { background: #333; }
    .ftt .ftt-active { border-color: #f59e0b; color: #f59e0b; }
    .ftt-sec { padding: 8px 10px; border-bottom: 1px solid #333; }
    .ftt-t { color: #888; text-transform: uppercase; font-size: 10px; letter-spacing: .05em; margin-bottom: 4px; }
    .ftt-model { white-space: pre; overflow: auto; max-height: 200px; margin: 0; color: #cbd5e1;
      background: #1b1b1b; border: 1px solid #2a2a2a; border-radius: 4px; padding: 6px 8px; }
    .ftt-diff { max-height: 120px; overflow: auto; }
    .ftt-diff-row { display: flex; flex-wrap: wrap; gap: 6px; }
    .ftt-diff-path { color: #cbd5e1; }
    .ftt-diff-old { color: #f87171; text-decoration: line-through; }
    .ftt-diff-new { color: #4ade80; }
    .ftt-diff-arrow { color: #666; }
    .ftt-diff-none { color: #555; }
    .ftt-hist { display: flex; flex-direction: column; gap: 6px; }
    .ftt-hist-head { display: flex; align-items: center; }
    .ftt-hist-head .ftt-t { margin: 0; }
    .ftt-hist-head .ftt-clear { margin-left: auto; }
    .ftt-fgroup { display: flex; flex-direction: column; gap: 4px; }
    .ftt-fwrap { position: relative; }
    .ftt-fwrap input { width: 100%; box-sizing: border-box; font: inherit;
      background: #1f1f1f; color: #ddd; border: 1px solid #444; border-radius: 4px; padding: 3px 22px 3px 6px; }
    .ftt-fwrap input::placeholder { color: #666; }
    .ftt-clearicon { position: absolute; right: 5px; top: 50%; transform: translateY(-50%);
      display: none; color: #777; cursor: pointer; font-size: 14px; line-height: 1; padding: 0 2px; }
    .ftt-clearicon:hover { color: #ddd; }
    .ftt-suggest { flex-basis: 100%; display: flex; flex-wrap: wrap; gap: 4px; }
    .ftt-suggest:not(:empty) { margin-top: 2px; }
    .ftt-sugg { background: #20303a; color: #9bd; border-color: #2c4a5a; padding: 1px 6px; }
    .ftt-sugg:hover { background: #284050; }
    .ftt-list { flex: 1; overflow: auto; overflow-anchor: auto; }
    .ftt-row { display: flex; gap: 8px; padding: 4px 10px; cursor: pointer; border-bottom: 1px solid #222; }
    .ftt-row:hover { background: #1f1f1f; }
    .ftt-row.sel { background: #3a2d10; box-shadow: inset 3px 0 0 #f59e0b; }
    .ftt-row .n { color: #555; min-width: 26px; text-align: right; }
    .ftt-row .a { color: #e2e8f0; }
    .ftt-row .c { color: #f59e0b; margin-left: auto; }
    .ftt-overlay { position: fixed; inset: 0; background: rgba(0,0,0,.5); z-index: 99998; display: none; cursor: not-allowed; }
    .ftt-restore { position: fixed; top: 8px; right: 8px; z-index: 99999; display: none;
      font: 12px ui-monospace, Menlo, monospace; background: #2a2a2a; color: #f59e0b;
      border: 1px solid #f59e0b; border-radius: 4px; padding: 4px 10px; cursor: pointer; }
    .ftt-restore--left { left: 8px; right: auto; }
  `;
  document.head.appendChild(style);

  // ---- DOM ---------------------------------------------------------------
  const el = document.createElement("div");
  el.className = "ftt";
  el.innerHTML = `
    <div class="ftt-h">
      <b>Time Travel Debugger</b>
      <span class="ftt-hbtns">
        <button class="ftt-playpause">Pause</button>
        <button class="ftt-side">Left</button>
        <button class="ftt-min">Min</button>
      </span>
    </div>
    <div class="ftt-sec">
      <div class="ftt-t">Current model</div>
      <pre class="ftt-model">—</pre>
    </div>
    <div class="ftt-sec">
      <div class="ftt-t ftt-diff-label">Diff (live)</div>
      <div class="ftt-diff"></div>
    </div>
    <div class="ftt-sec ftt-hist">
      <div class="ftt-hist-head">
        <div class="ftt-t ftt-hist-title">History (0/0)</div>
        <button class="ftt-clear">Clear</button>
      </div>
      <div class="ftt-fgroup">
        <div class="ftt-fwrap">
          <input class="ftt-filter" type="text" placeholder="filter: only these (comma-sep)" />
          <span class="ftt-clearicon" title="clear">×</span>
        </div>
      </div>
      <div class="ftt-fgroup">
        <div class="ftt-fwrap">
          <input class="ftt-hide" type="text" placeholder="hide: these (comma-sep)" />
          <span class="ftt-clearicon" title="clear">×</span>
        </div>
      </div>
    </div>
    <div class="ftt-list"></div>
  `;
  document.body.appendChild(el);

  const overlay = document.createElement("div");
  overlay.className = "ftt-overlay";
  document.body.appendChild(overlay);

  const restore = document.createElement("button");
  restore.className = "ftt-restore";
  restore.textContent = "🐞 Time Travel";
  restore.onclick = () => setMinimized(false);
  document.body.appendChild(restore);

  const $model = el.querySelector(".ftt-model");
  const $diff = el.querySelector(".ftt-diff");
  const $diffLabel = el.querySelector(".ftt-diff-label");
  const $list = el.querySelector(".ftt-list");
  const $histTitle = el.querySelector(".ftt-hist-title");
  const $playpause = el.querySelector(".ftt-playpause");
  const $side = el.querySelector(".ftt-side");

  $playpause.onclick = () => (paused ? play() : pause());
  $side.onclick = () => { el.classList.toggle("ftt--left"); $side.textContent = el.classList.contains("ftt--left") ? "Right" : "Left"; };
  el.querySelector(".ftt-min").onclick = () => setMinimized(true);
  el.querySelector(".ftt-clear").onclick = () => { $list.innerHTML = ""; lastRow = null; lastTag = null; total = 0; play(); };

  // ---- filters + autosuggest --------------------------------------------
  const parseTags = (s) => s.split(",").map((x) => x.trim().toLowerCase()).filter(Boolean);
  function attachSuggest(input, onChange) {
    const fwrap = input.parentNode; // .ftt-fwrap (input + inline clear icon)
    const group = fwrap.parentNode; // .ftt-fgroup
    const clearIcon = fwrap.querySelector(".ftt-clearicon");
    const box = document.createElement("div");
    box.className = "ftt-suggest";
    group.appendChild(box);
    let hideTimer = null;
    const commit = () => onChange(parseTags(input.value));
    const updateClear = () => { clearIcon.style.display = input.value ? "block" : "none"; };
    function refresh() {
      const segs = input.value.split(",");
      const tok = segs[segs.length - 1].trim().toLowerCase();
      const used = segs.slice(0, -1).map((s) => s.trim().toLowerCase()).filter(Boolean);
      box.innerHTML = "";
      [...knownTags].filter((t) => { const tl = t.toLowerCase(); return !used.includes(tl) && tl.startsWith(tok); })
        .slice(0, 12).forEach((t) => {
          const b = document.createElement("button");
          b.className = "ftt-sugg"; b.type = "button"; b.textContent = t;
          b.onclick = () => complete(t); box.appendChild(b);
        });
    }
    function complete(tag) {
      const segs = input.value.split(",");
      segs[segs.length - 1] = " " + tag;
      input.value = segs.join(",").replace(/^[\s,]+/, "") + ", ";
      commit(); refresh(); updateClear(); input.focus();
    }
    // clear means "done": wipe value, drop suggestions, and blur (no re-focus)
    clearIcon.onclick = () => { input.value = ""; commit(); updateClear(); box.innerHTML = ""; input.blur(); };
    input.addEventListener("input", () => { commit(); refresh(); updateClear(); });
    input.addEventListener("focus", () => { clearTimeout(hideTimer); refresh(); });
    input.addEventListener("keydown", (e) => {
      if (e.key === "Tab" && box.firstChild) { e.preventDefault(); complete(box.firstChild.textContent); }
      else if (e.key === "Escape") box.innerHTML = "";
    });
    input.addEventListener("blur", () => { hideTimer = setTimeout(() => { box.innerHTML = ""; }, 150); });
    updateClear();
  }
  attachSuggest(el.querySelector(".ftt-filter"), (tags) => { includeTags = tags; applyFilters(); });
  attachSuggest(el.querySelector(".ftt-hide"), (tags) => { hideTags = tags; applyFilters(); });

  // pointerdown, not click: while live the list auto-scrolls every tick, so a
  // press+release can land on different rows; pointerdown fires on the row under
  // the cursor at press time, and jumpTo() pauses immediately, freezing the list.
  $list.addEventListener("pointerdown", (e) => {
    const row = e.target.closest(".ftt-row");
    if (row) { e.preventDefault(); jumpTo(row); }
  });

  // ---- rendering ---------------------------------------------------------
  function rowVisible(tag) {
    const t = (tag || "").toLowerCase();
    if (includeTags.length && !includeTags.includes(t)) return false;
    if (hideTags.includes(t)) return false;
    return true;
  }
  function applyFilters() {
    for (const row of $list.children) row.style.display = rowVisible(row._tag) ? "" : "none";
    updateCounts();
  }
  function updateCounts() {
    let shown = 0;
    for (const row of $list.children) if (row.style.display !== "none") shown++;
    $histTitle.textContent = `History (${shown}/${$list.children.length})`;
  }
  function renderModel(m) { $model.textContent = m === undefined ? "—" : pretty(m, 0); }
  function renderDiff() {
    let a, b, label;
    if (selectedRow) { b = selectedRow._model; a = selectedRow.previousElementSibling?._model; label = "Diff (step " + selectedRow._num + ")"; }
    else { a = prevModel; b = latestModel; label = paused ? "Diff (paused)" : "Diff (live)"; }
    $diffLabel.textContent = label;
    $diff.innerHTML = "";
    if (b === undefined) { $diff.innerHTML = `<span class="ftt-diff-none">—</span>`; return; }
    const out = [];
    if (a === undefined) out.push({ path: "(initial)", old: "", new: "" });
    else diff(a, b, "", out);
    if (!out.length) { $diff.innerHTML = `<span class="ftt-diff-none">(no change)</span>`; return; }
    for (const d of out) {
      const row = document.createElement("div");
      row.className = "ftt-diff-row";
      row.innerHTML = `<span class="ftt-diff-path"></span><span class="ftt-diff-old"></span><span class="ftt-diff-arrow">→</span><span class="ftt-diff-new"></span>`;
      row.querySelector(".ftt-diff-path").textContent = d.path;
      row.querySelector(".ftt-diff-old").textContent = d.old;
      row.querySelector(".ftt-diff-new").textContent = d.new;
      $diff.appendChild(row);
    }
  }
  function markSelection() {
    for (const row of $list.children) row.classList.toggle("sel", row === selectedRow);
  }

  // ---- ingest ------------------------------------------------------------
  function addEntry(p) {
    total += 1;
    prevModel = latestModel;
    latestModel = p.model;
    knownTags.add(p.tag);
    if (p.tag === lastTag && lastRow) { // fold identical consecutive (raw stream)
      lastRow._count += 1; lastRow._model = p.model; lastRow._msg = p.msg; lastRow._num = total;
      lastRow.title = show(p.msg);
      lastRow.querySelector(".n").textContent = total;
      lastRow.querySelector(".c").textContent = "×" + lastRow._count;
      return;
    }
    const row = document.createElement("div");
    row.className = "ftt-row";
    row._model = p.model; row._tag = p.tag; row._msg = p.msg; row._count = 1; row._num = total;
    row.style.display = rowVisible(p.tag) ? "" : "none";
    row.innerHTML = `<span class="n">${total}</span><span class="a"></span><span class="c"></span>`;
    row.querySelector(".a").textContent = p.tag;
    row.title = show(p.msg);
    $list.appendChild(row);
    lastRow = row; lastTag = p.tag;
  }
  function scheduleFlush() { if (!rafScheduled) { rafScheduled = true; requestAnimationFrame(flush); } }
  function flush() {
    rafScheduled = false;
    const atBottom = $list.scrollHeight - $list.scrollTop - $list.clientHeight < 4;
    for (const p of pending) addEntry(p);
    pending = [];
    while ($list.children.length > CAP) $list.removeChild($list.firstChild); // only while live → never drops selectedRow
    if (!paused) { renderModel(latestModel); renderDiff(); if (atBottom) $list.scrollTop = $list.scrollHeight; }
    updateCounts();
  }

  // ---- controls ----------------------------------------------------------
  function setStatus() {
    $playpause.textContent = paused ? "Play" : "Pause";
    $playpause.classList.toggle("ftt-active", paused);
    overlay.style.display = paused && !minimized ? "block" : "none";
  }
  function jumpTo(row) {
    selectedRow = row; paused = true;
    if (control) { control.setPaused(true); control.setModel(row._model); }
    renderModel(row._model); renderDiff(); markSelection(); setStatus();
  }
  function pause() {
    paused = true; selectedRow = null;
    if (control) control.setPaused(true);
    renderModel(latestModel); renderDiff(); markSelection(); setStatus();
  }
  function play() {
    paused = false; selectedRow = null;
    if (control) { control.setPaused(false); if (latestModel !== undefined) control.setModel(latestModel); }
    renderModel(latestModel); renderDiff(); markSelection(); $list.scrollTop = $list.scrollHeight; setStatus();
  }
  function setMinimized(b) {
    minimized = b;
    el.style.display = b ? "none" : "";
    // put the restore pill on whichever side the panel was on (one edge is
    // usually crowded by the app, rarely both)
    if (b) restore.classList.toggle("ftt-restore--left", el.classList.contains("ftt--left"));
    restore.style.display = b ? "block" : "none"; // CSS default is none, so be explicit
    setStatus();
  }

  // ---- the hook Flame talks to ------------------------------------------
  window.__FLAME_DEVTOOLS__ = {
    record(prev, msg, next) {
      pending.push({ tag: ctorName(msg) || show(msg), msg, model: next });
      scheduleFlush();
    },
    connect(handle) { control = handle; },
  };

  setStatus();
})();
