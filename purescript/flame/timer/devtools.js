// Flame time-travel devtools — a standalone, app-agnostic script.
//
// Installs `window.__FLAME_DEVTOOLS__`. Flame's patched runtime detects that
// global and (a) emits every transition via `record(prev, msg, next)`, (b) hands
// us a control handle via `connect({ setModel, setPaused, dispatch })`. The app
// under test knows nothing about it.
//
// Loaded in dev (or forced on with VITE_DEBUG=1). Memory bounded by
// window.__FLAME_DEBUG_HISTORY_CAP__ entries (each a structurally-shared model).

(function () {
  // If this script re-runs (dev HMR), drop any panel we already built.
  document.querySelectorAll(".ftt, .ftt-overlay, .ftt-restore").forEach((n) => n.remove());

  const CAP = Math.max(1, window.__FLAME_DEBUG_HISTORY_CAP__ || 1000);
  const MIN_PANEL_WIDTH = 280;
  const MAX_PANEL_WIDTH = 760;

  let control = null; // { setModel, setPaused, dispatch } from Flame
  let paused = false;
  let minimized = false;
  let selectedNum = null; // num of the entry being viewed, or null = live
  let total = 0; // absolute entry counter (row numbers)
  let previousModel;
  let latestModel;
  let history = []; // [{ tag, title, previous, model, num }] — full, capped
  let pending = []; // transient, drained each frame
  let rafScheduled = false;
  let includeTags = []; // "filter": if non-empty, show ONLY these (exact)
  let hideTags = []; // "hide": hide these (exact)
  let expandedRuns = new Set(); // runKeys the user clicked to expand
  const knownTags = new Set(); // for autosuggest
  let panelWidth = 360;

  // ---- pretty-print PureScript values (best effort over the JS rep) -------
  function ctorName(v) {
    if (!v || typeof v !== "object" || Array.isArray(v)) return null;
    const n = Object.getPrototypeOf(v)?.constructor?.name;
    if (!n || n === "Object" || n === "Array") return null;
    return n;
  }
  function tagOf(msg) { return ctorName(msg) || show(msg); }
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
  function diff(a, b, path, out) {
    if (a === b) return;
    const ao = a && typeof a === "object", bo = b && typeof b === "object";
    if (ao && bo && !Array.isArray(a) && !Array.isArray(b)) {
      if (ctorName(a) !== ctorName(b)) { out.push({ path: path || "(root)", old: show(a), next: show(b) }); return; }
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
    out.push({ path: path || "(root)", old: show(a), next: show(b) });
  }

  // ---- styles ------------------------------------------------------------
  const style = document.createElement("style");
  style.textContent = `
    .ftt { position: fixed; top: 0; right: 0; width: 360px; height: 100vh;
      background: #161616; color: #ddd; font: 12px/1.5 ui-monospace, Menlo, monospace;
      border-left: 1px solid #333; display: flex; flex-direction: column; z-index: 99999; }
    .ftt--left { left: 0; right: auto; border-left: 0; border-right: 1px solid #333; }
    .ftt * { box-sizing: border-box; }
    .ftt-resize { position: absolute; top: 0; bottom: 0; left: -4px; width: 8px; cursor: ew-resize; z-index: 2; }
    .ftt--left .ftt-resize { left: auto; right: -4px; }
    .ftt-resize::after { content: ""; position: absolute; top: 0; bottom: 0; left: 3px; width: 1px; background: transparent; }
    .ftt-resize:hover::after, .ftt-resize:active::after { background: #f59e0b; }
    .ftt-h { display: flex; align-items: center; gap: 6px; padding: 8px 10px; border-bottom: 1px solid #333; }
    .ftt-h b { color: #f59e0b; font-weight: 600; }
    .ftt-h .ftt-hbtns { margin-left: auto; display: flex; gap: 6px; }
    .ftt button { font: inherit; background: #2a2a2a; color: #ddd; border: 1px solid #444;
      border-radius: 4px; padding: 2px 8px; cursor: pointer; }
    .ftt button:hover { background: #333; }
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
    .ftt-suggest { display: flex; flex-wrap: wrap; gap: 4px; }
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
    .ftt-row--collapsed .a::before { content: "▸ "; color: #f59e0b; }
    .ftt-overlay { position: fixed; inset: 0; background: rgba(0,0,0,.5); z-index: 99998; display: none; cursor: pointer; }
    .ftt-restore { position: fixed; top: 8px; right: 8px; z-index: 99999; display: none;
      font: 12px ui-monospace, Menlo, monospace; background: #2a2a2a; color: #f59e0b;
      border: 1px solid #f59e0b; border-radius: 4px; padding: 4px 10px; cursor: pointer; }
    .ftt-restore--left { left: 8px; right: auto; }
  `;
  document.head.appendChild(style);

  // ---- DOM ---------------------------------------------------------------
  const root = document.createElement("div");
  root.className = "ftt";
  root.innerHTML = `
    <div class="ftt-resize" title="Drag to resize"></div>
    <div class="ftt-h">
      <b>Time Travel Debugger</b>
      <span class="ftt-hbtns">
        <button class="ftt-side" type="button">Left</button>
        <button class="ftt-min" type="button">Min</button>
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
        <button class="ftt-clear" type="button">Clear</button>
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
  document.body.appendChild(root);

  const overlay = document.createElement("div");
  overlay.className = "ftt-overlay";
  document.body.appendChild(overlay);

  const restore = document.createElement("button");
  restore.className = "ftt-restore";
  restore.textContent = "🐞 Time Travel";
  restore.onclick = () => setMinimized(false);
  document.body.appendChild(restore);

  const modelEl = root.querySelector(".ftt-model");
  const diffEl = root.querySelector(".ftt-diff");
  const diffLabelEl = root.querySelector(".ftt-diff-label");
  const listEl = root.querySelector(".ftt-list");
  const histTitleEl = root.querySelector(".ftt-hist-title");
  const sideEl = root.querySelector(".ftt-side");

  sideEl.addEventListener("click", () => {
    root.classList.toggle("ftt--left");
    sideEl.textContent = root.classList.contains("ftt--left") ? "Right" : "Left";
    applyPanelWidth();
  });
  root.querySelector(".ftt-min").addEventListener("click", () => setMinimized(true));
  root.querySelector(".ftt-clear").addEventListener("click", clearHistory);
  // No Play/Pause button: clicking a history row pauses+jumps; clicking the
  // (dimmed) overlay resumes live.
  overlay.addEventListener("pointerdown", (e) => { if (paused) { e.preventDefault(); play(); } });
  attachWidthResize(root.querySelector(".ftt-resize"));

  // pointerdown (not click): the list re-renders as messages stream in, so a
  // click can straddle two rows. pointerdown acts at press time. A collapsed run
  // expands (so its individual entries become jumpable — e.g. each keystroke of
  // a text input); a normal row jumps to that state.
  listEl.addEventListener("pointerdown", (e) => {
    const row = e.target.closest(".ftt-row");
    if (!row || !listEl.contains(row)) return;
    e.preventDefault();
    if (row._collapsed) { expandedRuns.add(row._runKey); renderHistory(); return; }
    jumpTo(row);
  });

  attachSuggest(root.querySelector(".ftt-filter"), (tags) => { includeTags = tags; renderHistory(); });
  attachSuggest(root.querySelector(".ftt-hide"), (tags) => { hideTags = tags; renderHistory(); });

  applyPanelWidth();

  // ---- ingest ------------------------------------------------------------
  function addEntry(entry) {
    total += 1;
    previousModel = entry.previous;
    latestModel = entry.model;
    knownTags.add(entry.tag);
    history.push({ ...entry, num: total });
    while (history.length > CAP) {
      const removed = history.shift();
      if (removed && selectedNum === removed.num) selectedNum = null;
    }
  }
  function scheduleFlush() { if (!rafScheduled) { rafScheduled = true; requestAnimationFrame(flush); } }
  function flush() {
    rafScheduled = false;
    // seed an INIT row from the first transition's previous model
    if (total === 0 && pending.length) {
      addEntry({ tag: "INIT", title: "INIT", previous: undefined, model: pending[0].previous });
    }
    for (const e of pending) addEntry(e);
    pending = [];
    renderHistory();
    if (!paused) { renderModel(latestModel); renderDiff(previousModel, latestModel, "Diff (live)"); }
  }

  // ---- grouping & rendering ---------------------------------------------
  function rowVisible(tag) {
    const t = (tag || "").toLowerCase();
    if (includeTags.length && !includeTags.includes(t)) return false;
    if (hideTags.includes(t)) return false;
    return true;
  }
  // Group consecutive identical messages on the RAW stream (a different message
  // — even a hidden one — breaks a run, so the timeline stays truthful).
  function groupedHistory() {
    const groups = []; let current = [];
    for (const entry of history) {
      if (current.length && entry.tag === current[0].tag) current.push(entry);
      else { if (current.length) groups.push(current); current = [entry]; }
    }
    if (current.length) groups.push(current);
    return groups;
  }
  function runKey(group) { return group[0].num + ":" + group[0].tag; }
  function shouldRenderCollapsed(group) {
    if (group.length < 2) return false;
    if (expandedRuns.has(runKey(group))) return false;
    return !group.some((e) => e.num === selectedNum); // expand the run we're viewing
  }
  function baseRow(entry) {
    const row = document.createElement("div");
    row.className = "ftt-row";
    row._collapsed = false;
    row._model = entry.model;
    row._previous = entry.previous;
    row._tag = entry.tag;
    row._num = entry.num;
    row.innerHTML = '<span class="n"></span><span class="a"></span><span class="c"></span>';
    row.querySelector(".n").textContent = entry.num;
    row.querySelector(".a").textContent = entry.tag;
    return row;
  }
  function entryRow(entry) { const r = baseRow(entry); r.title = entry.title; return r; }
  function collapsedRow(group) {
    const row = baseRow(group[group.length - 1]);
    row.classList.add("ftt-row--collapsed");
    row._collapsed = true;
    row._runKey = runKey(group);
    row.title = "Click to expand " + group.length + " " + group[0].tag + " actions";
    row.querySelector(".c").textContent = "×" + group.length;
    return row;
  }
  function renderHistory() {
    const atBottom = listEl.scrollHeight - listEl.scrollTop - listEl.clientHeight < 4;
    listEl.innerHTML = "";
    for (const group of groupedHistory()) {
      if (!rowVisible(group[0].tag)) continue; // hide/filter applies per run
      if (shouldRenderCollapsed(group)) listEl.appendChild(collapsedRow(group));
      else for (const e of group) listEl.appendChild(entryRow(e));
    }
    markSelection();
    updateCounts();
    if (atBottom) listEl.scrollTop = listEl.scrollHeight;
  }
  function markSelection() {
    for (const row of listEl.children) row.classList.toggle("sel", !row._collapsed && row._num === selectedNum);
  }
  function updateCounts() { histTitleEl.textContent = `History (${listEl.children.length}/${history.length})`; }
  function renderModel(m) { modelEl.textContent = m === undefined ? "—" : pretty(m, 0); }
  function renderDiff(prev, cur, label) {
    diffLabelEl.textContent = label;
    diffEl.innerHTML = "";
    if (cur === undefined) { muted("—"); return; }
    const out = [];
    if (prev === undefined) out.push({ path: "(initial)", old: "", next: "" });
    else diff(prev, cur, "", out);
    if (!out.length) { muted("(no change)"); return; }
    for (const d of out) {
      const row = document.createElement("div");
      row.className = "ftt-diff-row";
      row.innerHTML = '<span class="ftt-diff-path"></span><span class="ftt-diff-old"></span><span class="ftt-diff-arrow">→</span><span class="ftt-diff-new"></span>';
      row.querySelector(".ftt-diff-path").textContent = d.path;
      row.querySelector(".ftt-diff-old").textContent = d.old;
      row.querySelector(".ftt-diff-new").textContent = d.next;
      diffEl.appendChild(row);
    }
  }
  function muted(text) { const s = document.createElement("span"); s.className = "ftt-diff-none"; s.textContent = text; diffEl.appendChild(s); }

  // ---- controls ----------------------------------------------------------
  function jumpTo(row) {
    selectedNum = row._num;
    paused = true;
    if (control) { control.setPaused(true); control.setModel(row._model); }
    renderModel(row._model);
    renderDiff(row._previous, row._model, "Diff (step " + row._num + ")");
    renderHistory();
    setStatus();
  }
  function play() {
    paused = false;
    selectedNum = null;
    if (control) { control.setPaused(false); if (latestModel !== undefined) control.setModel(latestModel); }
    renderModel(latestModel);
    renderDiff(previousModel, latestModel, "Diff (live)");
    renderHistory();
    listEl.scrollTop = listEl.scrollHeight;
    setStatus();
  }
  function clearHistory() {
    selectedNum = null; total = 0; history = []; expandedRuns = new Set();
    listEl.innerHTML = "";
    play();
  }
  function setMinimized(value) {
    minimized = value;
    root.style.display = value ? "none" : "";
    if (value) restore.classList.toggle("ftt-restore--left", root.classList.contains("ftt--left"));
    restore.style.display = value ? "block" : "none";
    setStatus();
  }
  function setStatus() { overlay.style.display = paused && !minimized ? "block" : "none"; }

  // ---- panel resize ------------------------------------------------------
  function attachWidthResize(handle) {
    let dragging = false, startX = 0, startWidth = panelWidth;
    function onMove(e) {
      if (!dragging) return;
      const delta = e.clientX - startX;
      const onLeft = root.classList.contains("ftt--left");
      panelWidth = clampPanelWidth(startWidth + (onLeft ? delta : -delta));
      applyPanelWidth();
    }
    function onUp() {
      dragging = false;
      document.removeEventListener("pointermove", onMove);
      document.removeEventListener("pointerup", onUp);
      document.body.style.cursor = "";
      document.body.style.userSelect = "";
    }
    handle.addEventListener("pointerdown", (e) => {
      e.preventDefault();
      dragging = true; startX = e.clientX; startWidth = panelWidth;
      document.body.style.cursor = "ew-resize";
      document.body.style.userSelect = "none";
      document.addEventListener("pointermove", onMove);
      document.addEventListener("pointerup", onUp);
    });
  }
  function applyPanelWidth() { panelWidth = clampPanelWidth(panelWidth); root.style.width = panelWidth + "px"; }
  function clampPanelWidth(w) {
    const max = Math.min(MAX_PANEL_WIDTH, Math.max(MIN_PANEL_WIDTH, window.innerWidth - 24));
    return Math.min(max, Math.max(MIN_PANEL_WIDTH, Math.round(w)));
  }

  // ---- filters + autosuggest --------------------------------------------
  const parseTags = (s) => s.split(",").map((x) => x.trim().toLowerCase()).filter(Boolean);
  function attachSuggest(input, onChange) {
    const wrap = input.parentNode;
    const group = wrap.parentNode;
    const clearIcon = wrap.querySelector(".ftt-clearicon");
    const box = document.createElement("div");
    box.className = "ftt-suggest";
    group.appendChild(box);
    let hideTimer = null;
    const commit = () => onChange(parseTags(input.value));
    const updateClear = () => { clearIcon.style.display = input.value ? "block" : "none"; };
    function refresh() {
      const segs = input.value.split(",");
      const tok = (segs.at(-1) || "").trim().toLowerCase();
      const used = segs.slice(0, -1).map((s) => s.trim().toLowerCase()).filter(Boolean);
      box.innerHTML = "";
      if (tok === "") return; // only suggest once you've started typing a name
      [...knownTags].filter((t) => { const tl = t.toLowerCase(); return !used.includes(tl) && tl.startsWith(tok); })
        .slice(0, 12).forEach((t) => {
          const b = document.createElement("button");
          b.className = "ftt-sugg"; b.type = "button"; b.textContent = t;
          // pointerdown (not click) fires before the input blurs, so completion
          // is reliable and we never need to fight the blur-hide timer.
          b.addEventListener("pointerdown", (e) => { e.preventDefault(); complete(t); });
          box.appendChild(b);
        });
    }
    function complete(tag) {
      const segs = input.value.split(",");
      segs[segs.length - 1] = " " + tag;
      input.value = segs.join(",").replace(/^[\s,]+/, "") + ", ";
      commit(); refresh(); updateClear(); input.focus();
    }
    clearIcon.addEventListener("click", () => { input.value = ""; commit(); updateClear(); box.innerHTML = ""; input.blur(); });
    input.addEventListener("input", () => { commit(); refresh(); updateClear(); });
    input.addEventListener("focus", () => { clearTimeout(hideTimer); refresh(); });
    input.addEventListener("keydown", (e) => {
      if (e.key === "Tab" && box.firstChild) { e.preventDefault(); complete(box.firstChild.textContent); }
      else if (e.key === "Escape") box.innerHTML = "";
    });
    input.addEventListener("blur", () => { hideTimer = setTimeout(() => { box.innerHTML = ""; }, 150); });
    updateClear();
  }

  // ---- the hook Flame talks to ------------------------------------------
  window.__FLAME_DEVTOOLS__ = {
    record(prev, msg, next) {
      if (paused) return; // Flame won't call us while paused, but guard anyway
      pending.push({ tag: tagOf(msg), title: show(msg), previous: prev, model: next });
      scheduleFlush();
    },
    connect(handle) {
      control = handle;
      // Seed INIT with the initial model so the panel shows state on page load,
      // before any message has been dispatched.
      if (total === 0 && typeof handle.getModel === "function") {
        addEntry({ tag: "INIT", title: "INIT", previous: undefined, model: handle.getModel() });
        renderHistory();
        renderModel(latestModel);
        renderDiff(previousModel, latestModel, "Diff (live)");
      }
    },
  };

  setStatus();
})();
