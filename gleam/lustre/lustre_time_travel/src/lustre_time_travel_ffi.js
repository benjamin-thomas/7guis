let installed = false;
let sendModel = null;
let paused = false;
let minimized = false;
let selectedRow = null;
let selectedNum = null;
let total = 0;
let previousModel;
let latestModel;
let history = [];
let pending = [];
let rafScheduled = false;
let includeTags = [];
let hideTags = [];
let expandedRuns = new Set();
let knownTags = new Set();
let panelWidth = 360;

const CAP = 1000;
const MIN_PANEL_WIDTH = 280;
const MAX_PANEL_WIDTH = 760;

let root;
let overlay;
let restore;
let modelEl;
let diffEl;
let diffLabelEl;
let listEl;
let historyTitleEl;
let sideEl;

export function install(initialModel, send_model) {
  sendModel = send_model;
  ensurePanel();
  reset(initialModel);
}

export function record(previous_model, msg, next_model) {
  ensurePanel();
  if (paused) return;

  pending.push({
    tag: tagOf(msg),
    title: show(msg),
    previous: previous_model,
    model: next_model,
  });
  scheduleFlush();
}

export function is_paused() {
  return paused;
}

export function identity(value) {
  return value;
}

function ensurePanel() {
  if (installed || typeof document === "undefined") return;
  installed = true;

  document.querySelectorAll(".ftt, .ftt-overlay, .ftt-restore").forEach((node) => {
    node.remove();
  });

  const style = document.createElement("style");
  style.textContent = `
    .ftt {
      position: fixed;
      top: 0;
      right: 0;
      width: min(360px, 100vw);
      height: 100vh;
      z-index: 9999;
      display: flex;
      flex-direction: column;
      background: #161616;
      border-left: 1px solid #333;
      color: #ddd;
      font: 12px/1.5 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
      box-shadow: -2px 0 14px rgba(0, 0, 0, 0.34);
    }

    .ftt--left {
      left: 0;
      right: auto;
      border-left: 0;
      border-right: 1px solid #333;
      box-shadow: 2px 0 14px rgba(0, 0, 0, 0.34);
    }

    .ftt-resize {
      position: absolute;
      top: 0;
      bottom: 0;
      left: -4px;
      width: 8px;
      cursor: ew-resize;
      z-index: 1;
    }

    .ftt--left .ftt-resize {
      left: auto;
      right: -4px;
    }

    .ftt-resize::after {
      content: "";
      position: absolute;
      top: 0;
      bottom: 0;
      left: 3px;
      width: 1px;
      background: transparent;
    }

    .ftt-resize:hover::after,
    .ftt-resize:active::after {
      background: #f59e0b;
    }

    .ftt * {
      box-sizing: border-box;
      scrollbar-width: thin;
      scrollbar-color: #555 transparent;
    }

    .ftt-h {
      display: flex;
      align-items: center;
      gap: 6px;
      padding: 8px 10px;
      border-bottom: 1px solid #333;
      flex: 0 0 auto;
    }

    .ftt-h b {
      color: #f59e0b;
      font-weight: 600;
    }

    .ftt-hbtns {
      display: flex;
      gap: 6px;
      margin-left: auto;
    }

    .ftt button {
      appearance: none;
      border: 1px solid #444;
      border-radius: 4px;
      background: #2a2a2a;
      color: #ddd;
      cursor: pointer;
      font: inherit;
      height: auto;
      line-height: 1.2;
      padding: 2px 8px;
    }

    .ftt button:hover {
      background: #333;
    }

    .ftt .ftt-active {
      border-color: #f59e0b;
      color: #f59e0b;
    }

    .ftt-sec {
      padding: 8px 10px;
      border-bottom: 1px solid #333;
      flex: 0 0 auto;
    }

    .ftt-t {
      color: #888;
      font-size: 10px;
      font-weight: 700;
      letter-spacing: 0.05em;
      margin-bottom: 4px;
      text-transform: uppercase;
    }

    .ftt-model {
      max-height: 200px;
      margin: 0;
      overflow: auto;
      white-space: pre;
      border: 1px solid #2a2a2a;
      border-radius: 4px;
      background: #1b1b1b;
      color: #cbd5e1;
      padding: 6px 8px;
    }

    .ftt-diff {
      max-height: 120px;
      overflow: auto;
    }

    .ftt-diff-row {
      display: flex;
      flex-wrap: wrap;
      gap: 6px;
      align-items: baseline;
      padding: 1px 0;
    }

    .ftt-diff-path {
      color: #cbd5e1;
    }

    .ftt-diff-old {
      color: #f87171;
      text-decoration: line-through;
    }

    .ftt-diff-arrow {
      color: #666;
    }

    .ftt-diff-new {
      color: #4ade80;
    }

    .ftt-diff-none {
      color: #555;
    }

    .ftt-hist {
      display: flex;
      flex-direction: column;
      gap: 6px;
    }

    .ftt-hist-head {
      display: flex;
      align-items: center;
    }

    .ftt-hist-head .ftt-t {
      margin: 0;
    }

    .ftt-clear {
      margin-left: auto;
    }

    .ftt-fgroup {
      display: flex;
      flex-direction: column;
      gap: 4px;
    }

    .ftt-fwrap {
      position: relative;
    }

    .ftt-fwrap input {
      width: 100%;
      min-width: 0;
      border: 1px solid #444;
      border-radius: 4px;
      background: #1f1f1f;
      color: #ddd;
      font: inherit;
      outline: none;
      padding: 3px 22px 3px 6px;
    }

    .ftt-fwrap input::placeholder {
      color: #666;
    }

    .ftt-fwrap input:focus {
      border-color: #f59e0b;
    }

    .ftt-clearicon {
      position: absolute;
      top: 50%;
      right: 5px;
      display: none;
      transform: translateY(-50%);
      color: #777;
      cursor: pointer;
      font-size: 14px;
      line-height: 1;
      padding: 0 2px;
    }

    .ftt-clearicon:hover {
      color: #ddd;
    }

    .ftt-suggest {
      display: flex;
      flex-wrap: wrap;
      gap: 4px;
    }

    .ftt-suggest:not(:empty) {
      margin-top: 2px;
    }

    .ftt-sugg {
      border-color: #2c4a5a;
      background: #20303a;
      color: #9bd;
      padding: 1px 6px;
    }

    .ftt-sugg:hover {
      background: #284050;
    }

    .ftt-list {
      flex: 1 1 auto;
      min-height: 0;
      overflow: auto;
      overflow-anchor: auto;
    }

    .ftt-row {
      display: flex;
      gap: 8px;
      padding: 4px 10px;
      border-bottom: 1px solid #222;
      cursor: pointer;
      user-select: none;
    }

    .ftt-row:hover {
      background: #1f1f1f;
    }

    .ftt-row.sel {
      background: #3a2d10;
      box-shadow: inset 3px 0 0 #f59e0b;
    }

    .ftt-row .n {
      min-width: 26px;
      color: #555;
      text-align: right;
      flex: 0 0 auto;
    }

    .ftt-row .a {
      min-width: 0;
      overflow: hidden;
      color: #e2e8f0;
      text-overflow: ellipsis;
      white-space: nowrap;
    }

    .ftt-row .c {
      margin-left: auto;
      color: #f59e0b;
      flex: 0 0 auto;
    }

    .ftt-row--collapsed .a::before {
      content: "> ";
      color: #f59e0b;
    }

    .ftt-overlay {
      position: fixed;
      inset: 0;
      z-index: 9998;
      display: none;
      background: rgba(0, 0, 0, 0.5);
      cursor: not-allowed;
    }

    .ftt-restore {
      position: fixed;
      top: 8px;
      right: 8px;
      z-index: 9999;
      display: none;
      border: 1px solid #f59e0b;
      border-radius: 4px;
      background: #2a2a2a;
      color: #f59e0b;
      cursor: pointer;
      font: 12px/1.2 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
      padding: 4px 10px;
    }

    .ftt-restore--left {
      left: 8px;
      right: auto;
    }
  `;
  document.head.appendChild(style);

  root = document.createElement("div");
  root.className = "ftt";
  root.innerHTML = `
    <div class="ftt-resize" title="Resize"></div>
    <div class="ftt-h">
      <b>Time Travel Debugger</b>
      <span class="ftt-hbtns">
        <button class="ftt-side" type="button">Left</button>
        <button class="ftt-min" type="button">Min</button>
      </span>
    </div>
    <div class="ftt-sec">
      <div class="ftt-t">Current model</div>
      <pre class="ftt-model">-</pre>
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
          <span class="ftt-clearicon" title="clear">x</span>
        </div>
      </div>
      <div class="ftt-fgroup">
        <div class="ftt-fwrap">
          <input class="ftt-hide" type="text" placeholder="hide: these (comma-sep)" />
          <span class="ftt-clearicon" title="clear">x</span>
        </div>
      </div>
    </div>
    <div class="ftt-list"></div>
  `;
  document.body.appendChild(root);

  overlay = document.createElement("div");
  overlay.className = "ftt-overlay";
  document.body.appendChild(overlay);

  restore = document.createElement("button");
  restore.className = "ftt-restore";
  restore.type = "button";
  restore.textContent = "Time Travel";
  document.body.appendChild(restore);

  modelEl = root.querySelector(".ftt-model");
  diffEl = root.querySelector(".ftt-diff");
  diffLabelEl = root.querySelector(".ftt-diff-label");
  listEl = root.querySelector(".ftt-list");
  historyTitleEl = root.querySelector(".ftt-hist-title");
  sideEl = root.querySelector(".ftt-side");

  sideEl.addEventListener("click", () => {
    root.classList.toggle("ftt--left");
    sideEl.textContent = root.classList.contains("ftt--left") ? "Right" : "Left";
    applyPanelWidth();
  });

  root.querySelector(".ftt-min").addEventListener("click", () => setMinimized(true));
  root.querySelector(".ftt-clear").addEventListener("click", clearHistory);
  restore.addEventListener("click", () => setMinimized(false));
  overlay.addEventListener("pointerdown", (event) => {
    if (!paused) return;

    event.preventDefault();
    play();
  });

  attachWidthResize(root.querySelector(".ftt-resize"));

  listEl.addEventListener("pointerdown", (event) => {
    const row = event.target.closest(".ftt-row");
    if (!row || !listEl.contains(row)) return;

    event.preventDefault();

    if (row._collapsed) {
      expandedRuns.add(row._runKey);
      renderHistory();
      return;
    }

    jumpTo(row);
  });

  attachSuggest(root.querySelector(".ftt-filter"), (tags) => {
    includeTags = tags;
    applyFilters();
  });
  attachSuggest(root.querySelector(".ftt-hide"), (tags) => {
    hideTags = tags;
    applyFilters();
  });
}

function reset(initialModel) {
  paused = false;
  minimized = false;
  selectedRow = null;
  selectedNum = null;
  total = 0;
  previousModel = undefined;
  latestModel = initialModel;
  history = [];
  pending = [];
  rafScheduled = false;
  expandedRuns = new Set();
  knownTags = new Set();
  listEl.innerHTML = "";
  root.style.display = "";
  restore.style.display = "none";

  addEntry({
    tag: "Init",
    title: "Initial model",
    previous: undefined,
    model: initialModel,
  });
  applyPanelWidth();
  renderHistory();
  renderModel(initialModel);
  renderDiff(undefined, initialModel, "Diff (live)");
  markSelection();
  updateCounts();
  setStatus();
}

function attachWidthResize(handle) {
  let dragging = false;
  let startX = 0;
  let startWidth = panelWidth;

  function onPointerMove(event) {
    if (!dragging) return;

    const delta = event.clientX - startX;
    const onLeft = root.classList.contains("ftt--left");
    panelWidth = clampPanelWidth(startWidth + (onLeft ? delta : -delta));
    applyPanelWidth();
  }

  function onPointerUp() {
    if (!dragging) return;

    dragging = false;
    document.removeEventListener("pointermove", onPointerMove);
    document.removeEventListener("pointerup", onPointerUp);
    document.body.style.cursor = "";
    document.body.style.userSelect = "";
  }

  handle.addEventListener("pointerdown", (event) => {
    event.preventDefault();
    dragging = true;
    startX = event.clientX;
    startWidth = panelWidth;
    document.body.style.cursor = "ew-resize";
    document.body.style.userSelect = "none";
    document.addEventListener("pointermove", onPointerMove);
    document.addEventListener("pointerup", onPointerUp);
  });
}

function applyPanelWidth() {
  panelWidth = clampPanelWidth(panelWidth);
  root.style.width = panelWidth + "px";
}

function clampPanelWidth(width) {
  const max = Math.min(MAX_PANEL_WIDTH, Math.max(MIN_PANEL_WIDTH, window.innerWidth - 24));
  return Math.min(max, Math.max(MIN_PANEL_WIDTH, Math.round(width)));
}

function clearHistory() {
  selectedRow = null;
  selectedNum = null;
  total = 0;
  history = [];
  expandedRuns = new Set();
  renderHistory();
  play();
  updateCounts();
}

function addEntry(entry) {
  total += 1;
  previousModel = entry.previous;
  latestModel = entry.model;
  knownTags.add(entry.tag);
  history.push({ ...entry, num: total });

  while (history.length > CAP) {
    const removed = history.shift();
    if (removed && selectedNum === removed.num) {
      selectedNum = null;
      selectedRow = null;
    }
  }
}

function scheduleFlush() {
  if (rafScheduled) return;
  rafScheduled = true;

  const schedule =
    typeof requestAnimationFrame === "function"
      ? requestAnimationFrame
      : (callback) => setTimeout(callback, 16);
  schedule(flush);
}

function flush() {
  rafScheduled = false;
  const atBottom = listEl.scrollHeight - listEl.scrollTop - listEl.clientHeight < 4;

  for (const entry of pending) addEntry(entry);
  pending = [];
  renderHistory();

  if (!paused) {
    renderModel(latestModel);
    renderDiff(previousModel, latestModel, "Diff (live)");
    if (atBottom) listEl.scrollTop = listEl.scrollHeight;
  }

  markSelection();
  updateCounts();
}

function rowVisible(tag) {
  const normalized = tag.toLowerCase();
  if (includeTags.length > 0 && !includeTags.includes(normalized)) return false;
  if (hideTags.includes(normalized)) return false;
  return true;
}

function renderHistory() {
  const atBottom = listEl.scrollHeight - listEl.scrollTop - listEl.clientHeight < 4;
  listEl.innerHTML = "";

  for (const group of groupedHistory()) {
    if (shouldRenderCollapsed(group)) {
      listEl.appendChild(collapsedRow(group));
    } else {
      for (const entry of group) listEl.appendChild(entryRow(entry));
    }
  }

  markSelection();
  updateCounts();
  if (atBottom) listEl.scrollTop = listEl.scrollHeight;
}

function groupedHistory() {
  const groups = [];
  let current = [];

  for (const entry of history) {
    if (!rowVisible(entry.tag)) continue;

    const currentTag = current.length > 0 ? current[0].tag : null;
    if (current.length > 0 && entry.tag === currentTag) {
      current.push(entry);
    } else {
      if (current.length > 0) groups.push(current);
      current = [entry];
    }
  }

  if (current.length > 0) groups.push(current);
  return groups;
}

function shouldRenderCollapsed(group) {
  if (group.length < 2) return false;
  if (expandedRuns.has(runKey(group))) return false;
  return !group.some((entry) => entry.num === selectedNum);
}

function runKey(group) {
  return group[0].num + ":" + group[0].tag;
}

function collapsedRow(group) {
  const first = group[0];
  const last = group[group.length - 1];
  const row = baseRow(last);
  row.classList.add("ftt-row--collapsed");
  row._collapsed = true;
  row._runKey = runKey(group);
  row.title = "Click to expand " + group.length + " " + first.tag + " actions";
  row.querySelector(".c").textContent = "x" + group.length;
  return row;
}

function entryRow(entry) {
  const row = baseRow(entry);
  row.title = entry.title;
  return row;
}

function baseRow(entry) {
  const row = document.createElement("div");
  row.className = "ftt-row";
  row._collapsed = false;
  row._model = entry.model;
  row._previous = entry.previous;
  row._tag = entry.tag;
  row._title = entry.title;
  row._num = entry.num;
  row.innerHTML = '<span class="n"></span><span class="a"></span><span class="c"></span>';
  row.querySelector(".n").textContent = entry.num;
  row.querySelector(".a").textContent = entry.tag;
  return row;
}

function applyFilters() {
  renderHistory();
}

function updateCounts() {
  historyTitleEl.textContent = `History (${listEl.children.length}/${history.length})`;
}

function renderModel(model) {
  modelEl.textContent = model === undefined ? "-" : pretty(model, 0);
}

function renderDiff(previous, current, label) {
  diffLabelEl.textContent = label;
  diffEl.innerHTML = "";

  if (current === undefined) {
    appendMuted(diffEl, "-");
    return;
  }

  const entries = [];
  if (previous === undefined) entries.push({ path: "(initial)", old: "", next: show(current) });
  else diff(previous, current, "", entries);

  if (entries.length === 0) {
    appendMuted(diffEl, "(no change)");
    return;
  }

  for (const entry of entries) {
    const row = document.createElement("div");
    row.className = "ftt-diff-row";
    row.innerHTML =
      '<span class="ftt-diff-path"></span><span class="ftt-diff-old"></span><span class="ftt-diff-arrow">-&gt;</span><span class="ftt-diff-new"></span>';
    row.querySelector(".ftt-diff-path").textContent = entry.path;
    row.querySelector(".ftt-diff-old").textContent = entry.old;
    row.querySelector(".ftt-diff-new").textContent = entry.next;
    diffEl.appendChild(row);
  }
}

function markSelection() {
  selectedRow = null;
  for (const row of listEl.children) {
    const selected = !row._collapsed && row._num === selectedNum;
    row.classList.toggle("sel", selected);
    if (selected) selectedRow = row;
  }
}

function jumpTo(row) {
  selectedRow = row;
  selectedNum = row._num;
  paused = true;
  if (sendModel) sendModel(row._model);
  renderModel(row._model);
  renderDiff(row._previous, row._model, "Diff (step " + row._num + ")");
  markSelection();
  setStatus();
}

function pause() {
  paused = true;
  selectedRow = null;
  selectedNum = null;
  renderModel(latestModel);
  renderDiff(previousModel, latestModel, "Diff (paused)");
  markSelection();
  setStatus();
}

function play() {
  paused = false;
  selectedRow = null;
  selectedNum = null;
  if (sendModel && latestModel !== undefined) sendModel(latestModel);
  renderModel(latestModel);
  renderDiff(previousModel, latestModel, "Diff (live)");
  markSelection();
  listEl.scrollTop = listEl.scrollHeight;
  setStatus();
}

function setMinimized(value) {
  minimized = value;
  root.style.display = minimized ? "none" : "";
  if (value) restore.classList.toggle("ftt-restore--left", root.classList.contains("ftt--left"));
  restore.style.display = minimized ? "block" : "none";
  setStatus();
}

function setStatus() {
  overlay.style.display = paused && !minimized ? "block" : "none";
}

function attachSuggest(input, onChange) {
  const wrap = input.parentNode;
  const group = wrap.parentNode;
  const clearIcon = wrap.querySelector(".ftt-clearicon");
  const box = document.createElement("div");
  box.className = "ftt-suggest";
  group.appendChild(box);
  let hideTimer = null;

  const commit = () => onChange(parseTags(input.value));
  const updateClear = () => {
    clearIcon.style.display = input.value ? "block" : "none";
  };

  function refresh() {
    const segments = input.value.split(",");
    const token = (segments.at(-1) || "").trim().toLowerCase();
    const used = segments.slice(0, -1).map((part) => part.trim().toLowerCase()).filter(Boolean);
    box.innerHTML = "";

    if (token === "") return;

    [...knownTags]
      .filter((tag) => {
        const lower = tag.toLowerCase();
        return !used.includes(lower) && lower.startsWith(token);
      })
      .slice(0, 12)
      .forEach((tag) => {
        const button = document.createElement("button");
        button.className = "ftt-sugg";
        button.type = "button";
        button.textContent = tag;
        button.addEventListener("pointerdown", (event) => {
          event.preventDefault();
          complete(tag);
        });
        box.appendChild(button);
      });
  }

  function complete(tag) {
    const segments = input.value.split(",");
    segments[segments.length - 1] = " " + tag;
    input.value = segments.join(",").replace(/^[\s,]+/, "") + ", ";
    commit();
    refresh();
    updateClear();
    input.focus();
  }

  clearIcon.addEventListener("click", () => {
    input.value = "";
    commit();
    updateClear();
    box.innerHTML = "";
    input.blur();
  });
  input.addEventListener("input", () => {
    commit();
    refresh();
    updateClear();
  });
  input.addEventListener("focus", () => {
    clearTimeout(hideTimer);
    refresh();
  });
  input.addEventListener("keydown", (event) => {
    if (event.key === "Tab" && box.firstChild) {
      event.preventDefault();
      complete(box.firstChild.textContent);
    } else if (event.key === "Escape") {
      box.innerHTML = "";
    }
  });
  input.addEventListener("blur", () => {
    hideTimer = setTimeout(() => {
      box.innerHTML = "";
    }, 150);
  });

  updateClear();
}

function parseTags(value) {
  return value
    .split(",")
    .map((part) => part.trim().toLowerCase())
    .filter(Boolean);
}

function appendMuted(parent, text) {
  const el = document.createElement("span");
  el.className = "ftt-diff-none";
  el.textContent = text;
  parent.appendChild(el);
}

function tagOf(value) {
  const ctor = ctorName(value);
  if (!ctor) return show(value);

  if (ctor === "RouterSentMsg" && value[0]) return `Router.${tagOf(value[0])}`;
  if (ctor.endsWith("PageSentMsg") && value[0]) {
    return `${ctor.replace(/PageSentMsg$/, "")}.${tagOf(value[0])}`;
  }
  if (ctor.startsWith("FormMsg") && value[0]) return `Form.${tagOf(value[0])}`;
  if (ctor === "OnRouteChanged" && value[0]) return `Route.${tagOf(value[0])}`;
  if (ctor === "MenuSentEvent" && value[0]) return `Menu.${value[0]}`;
  if (ctor.startsWith("Api") && value[0]) {
    const result = ctorName(value[0]);
    return result ? `${ctor}.${result}` : ctor;
  }

  return ctor;
}

function ctorName(value) {
  if (!value || typeof value !== "object") return null;
  if (Array.isArray(value)) return null;
  const name = Object.getPrototypeOf(value)?.constructor?.name;
  if (!name || name === "Object" || name === "Array") return null;
  return name;
}

function isGleamList(value) {
  const ctor = ctorName(value);
  return ctor === "Empty" || ctor === "NonEmpty";
}

function listToArray(value) {
  if (value && typeof value.toArray === "function") return value.toArray();

  const items = [];
  let current = value;
  while (ctorName(current) === "NonEmpty") {
    items.push(current.head);
    current = current.tail;
  }
  return items;
}

function fields(value) {
  return Object.keys(value).filter((key) => typeof value[key] !== "function");
}

function show(value) {
  if (value === undefined) return "undefined";
  if (value === null) return "null";
  if (typeof value === "string") return JSON.stringify(value);
  if (typeof value === "number" || typeof value === "boolean" || typeof value === "bigint") {
    return String(value);
  }
  if (Array.isArray(value)) return "[" + value.map(show).join(", ") + "]";
  if (isGleamList(value)) return "[" + listToArray(value).map(show).join(", ") + "]";
  if (typeof value !== "object") return String(value);

  const ctor = ctorName(value);
  const keys = fields(value);
  if (ctor) {
    if (keys.length === 0) return ctor;
    if (keys.every((key) => /^\d+$/.test(key))) {
      return ctor + "(" + keys.map((key) => show(value[key])).join(", ") + ")";
    }
    return ctor + " { " + keys.map((key) => key + ": " + show(value[key])).join(", ") + " }";
  }

  return "{ " + keys.map((key) => key + ": " + show(value[key])).join(", ") + " }";
}

function pretty(value, indent) {
  const pad = "  ".repeat(indent);
  const pad1 = "  ".repeat(indent + 1);

  if (value === undefined) return "undefined";
  if (value === null) return "null";
  if (typeof value === "string") return JSON.stringify(value);
  if (typeof value === "number" || typeof value === "boolean" || typeof value === "bigint") {
    return String(value);
  }
  if (Array.isArray(value)) {
    if (value.length === 0) return "[]";
    return "[\n" + value.map((item) => pad1 + pretty(item, indent + 1)).join(",\n") + "\n" + pad + "]";
  }
  if (isGleamList(value)) return pretty(listToArray(value), indent);
  if (typeof value !== "object") return String(value);

  const ctor = ctorName(value);
  const keys = fields(value);
  if (ctor) {
    if (keys.length === 0) return ctor;
    if (keys.length === 1 && keys[0] === "0") return ctor + " " + pretty(value[0], indent);
    if (keys.every((key) => /^\d+$/.test(key))) {
      return ctor + "(" + keys.map((key) => pretty(value[key], indent)).join(", ") + ")";
    }
    return ctor + " " + prettyObject(value, keys, indent);
  }

  return prettyObject(value, keys, indent);
}

function prettyObject(value, keys, indent) {
  const pad = "  ".repeat(indent);
  const pad1 = "  ".repeat(indent + 1);
  if (keys.length === 0) return "{}";
  return "{\n" + keys.map((key) => pad1 + key + ": " + pretty(value[key], indent + 1)).join(",\n") + "\n" + pad + "}";
}

function diff(oldValue, nextValue, path, out) {
  if (Object.is(oldValue, nextValue)) return;

  if (isGleamList(oldValue) && isGleamList(nextValue)) {
    diff(listToArray(oldValue), listToArray(nextValue), path, out);
    return;
  }

  if (Array.isArray(oldValue) && Array.isArray(nextValue)) {
    const length = Math.max(oldValue.length, nextValue.length);
    for (let i = 0; i < length; i += 1) {
      diff(oldValue[i], nextValue[i], (path || "") + "[" + i + "]", out);
    }
    return;
  }

  const oldObject = oldValue && typeof oldValue === "object" && !Array.isArray(oldValue);
  const nextObject = nextValue && typeof nextValue === "object" && !Array.isArray(nextValue);

  if (oldObject && nextObject) {
    const oldCtor = ctorName(oldValue);
    const nextCtor = ctorName(nextValue);
    if (oldCtor !== nextCtor) {
      out.push({ path: path || "(root)", old: show(oldValue), next: show(nextValue) });
      return;
    }

    const allKeys = [...new Set([...fields(oldValue), ...fields(nextValue)])];
    for (const key of allKeys) {
      const nextPath = pathForKey(path, key, allKeys);
      diff(oldValue[key], nextValue[key], nextPath, out);
    }
    return;
  }

  out.push({ path: path || "(root)", old: show(oldValue), next: show(nextValue) });
}

function pathForKey(path, key, allKeys) {
  if (/^\d+$/.test(key)) {
    if (allKeys.length === 1) return path;
    return (path || "") + "[" + key + "]";
  }
  return path ? path + "." + key : key;
}
