const REQUIRED_TABLES = [
  "market_events",
  "strategy_snapshots",
  "events",
  "account_snapshots",
  "risk_snapshots",
  "orders",
  "fills",
  "agent_commands",
  "order_requests",
  "order_cancellations",
  "agent_orders",
  "agents",
  "agent_decisions",
  "agent_rankings"
];

const state = {
  manifest: [],
  tables: {},
  feed: {
    timer: null,
    nextTickAt: null,
    running: false,
    starting: false
  },
  stateRefresh: {
    timer: null
  },
  agentRefresh: {
    timer: null
  },
  replay: {
    cursor: null,
    windowSize: 50,
    timer: null
  },
  selectedAsset: "all"
};

document.addEventListener("DOMContentLoaded", () => {
  applyDashboardMode();
  bind("file-picker", "change", handleFilePick);
  bind("order-form", "submit", submitOrder);
  bind("register-human", "click", registerHumanAgent);
  bind("cancel-form", "submit", submitCancel);
  bind("refresh-state", "click", refreshServiceState);
  bind("agent-refresh-interval", "change", configureAgentAutoRefresh);
  bind("feed-form", "submit", applyFeedConfig);
  bind("feed-refresh", "click", refreshFeedStatus);
  bind("feed-warmup", "click", feedWarmup);
  bind("feed-start", "click", feedStart);
  bind("feed-stop", "click", feedStop);
  bind("feed-step", "click", feedStep);
  bind("feed-form", "change", updateFeedRunMode);
  bind("agent-admin-form", "submit", addAiAgent);
  bind("agent-status-form", "submit", setAiAgentStatus);
  bind("replay-window", "change", updateReplayWindow);
  bind("replay-step-unit", "change", renderReplayStatus);
  bind("asset-filter", "change", updateAssetFilter);
  bind("replay-reset", "click", replayReset);
  bind("replay-prev", "click", replayPrev);
  bind("replay-next", "click", replayNext);
  bind("replay-play", "click", replayPlayToggle);
  applyServiceUrlParam();
  updateFeedRunMode();
  loadFromFolder();
  configureAgentAutoRefresh();
  configureStateAutoRefresh();
});

function bind(id, event, handler) {
  const el = document.getElementById(id);
  if (el) el.addEventListener(event, handler);
}

async function loadFromFolder() {
  try {
    const manifest = parseCsv(await fetchText("manifest.csv"));
    const files = Object.fromEntries(manifest.map(row => [row.table, row.file]));
    const tables = {};
    for (const table of REQUIRED_TABLES) {
      const file = files[table] || `${table}.csv`;
      try {
        tables[table] = parseCsv(await fetchText(file));
      } catch (err) {
        tables[table] = [];
      }
    }
    state.manifest = manifest;
    state.tables = tables;
    initializeReplayCursor();
    render();
    setStatus("Loaded dashboard data from exported CSV files.");
  } catch (err) {
    setStatus("Auto-load failed. Select the dashboard export folder to load CSV files in this browser.");
  }
}

async function handleFilePick(event) {
  const files = Array.from(event.target.files || []);
  const byName = new Map(files.map(file => [basename(file.webkitRelativePath || file.name), file]));
  const manifestFile = byName.get("manifest.csv");
  if (!manifestFile) {
    setStatus("manifest.csv was not found in the selected files.");
    return;
  }
  const manifest = parseCsv(await manifestFile.text());
  const manifestFiles = Object.fromEntries(manifest.map(row => [row.table, row.file]));
  const tables = {};
  for (const table of REQUIRED_TABLES) {
    const fileName = manifestFiles[table] || `${table}.csv`;
    const file = byName.get(basename(fileName));
    tables[table] = file ? parseCsv(await file.text()) : [];
  }
  state.manifest = manifest;
  state.tables = tables;
  initializeReplayCursor();
  render();
  setStatus("Loaded dashboard data from selected files.");
}

async function fetchText(path) {
  const response = await fetch(path, { cache: "no-store" });
  if (!response.ok) throw new Error(`Unable to load ${path}`);
  return response.text();
}

function parseCsv(text) {
  const rows = [];
  let row = [];
  let value = "";
  let quoted = false;

  for (let i = 0; i < text.length; i++) {
    const ch = text[i];
    const next = text[i + 1];
    if (quoted) {
      if (ch === '"' && next === '"') {
        value += '"';
        i++;
      } else if (ch === '"') {
        quoted = false;
      } else {
        value += ch;
      }
    } else if (ch === '"') {
      quoted = true;
    } else if (ch === ",") {
      row.push(value);
      value = "";
    } else if (ch === "\n") {
      row.push(value);
      rows.push(row);
      row = [];
      value = "";
    } else if (ch !== "\r") {
      value += ch;
    }
  }
  if (value.length || row.length) {
    row.push(value);
    rows.push(row);
  }
  if (!rows.length) return [];

  const headers = rows.shift();
  return rows
    .filter(items => items.some(item => item !== ""))
    .map(items => Object.fromEntries(headers.map((header, i) => [header, items[i] ?? ""])));
}

function render() {
  renderAssetFilter();
  renderManifest();
  renderReplayStatus();
  renderKpis();
  renderCandles();
  renderEquity();
  renderTargets();
  renderRiskSummary();
  renderTimeline();
  renderTable("orders-table", filterRowsByAsset(state.tables.orders), ["timestamp", "order_id", "agent_id", "symbol", "asset_id", "side", "qty_type", "qty", "order_type", "status", "action_label", "dir_label", "ctr_qty", "price", "status_label"]);
  renderTable("orders-fills-table", filterRowsByAsset(combinedOrderFillRows()), ["timestamp", "source", "order_id", "agent_id", "symbol", "asset_id", "side", "qty_type", "qty", "order_type", "status", "action_label", "dir_label", "ctr_qty", "price", "fee", "realized_pnl"], 80);
  renderTable("commands-table", state.tables.agent_commands, ["timestamp", "command_id", "agent_id", "command_type", "status", "ref_id", "message"]);
  renderTable("requests-table", filterRowsByAsset(state.tables.order_requests), ["timestamp", "command_id", "agent_id", "symbol", "asset_id", "side", "qty_type", "qty", "order_type", "status", "order_id", "message"]);
  renderTable("fills-table", state.tables.fills, ["timestamp", "action_label", "dir_label", "ctr_qty", "price", "fee", "realized_pnl"]);
  renderTable("risk-table", state.tables.risk_snapshots, ["timestamp", "equity", "abs_notional", "leverage", "maintenance_margin", "margin_buffer"], 25);
  renderTable("agents-table", state.tables.agents, ["agent_id", "agent_type", "status", "config", "created_at"], 80);
  renderTable("agent-decisions-table", filterRowsByAsset(state.tables.agent_decisions), ["timestamp", "agent_id", "agent_type", "symbol", "asset_id", "side", "intended_action", "intended_dir", "qty", "order_type", "reason", "command_id", "status"], 80);
  renderTable("agent-rankings-table", state.tables.agent_rankings, ["rank", "agent_id", "agent_type", "status", "equity", "cash", "unrealized_pnl", "orders", "filled_orders", "net_qty", "last_side"], 80, { newestFirst: false });
}

function applyDashboardMode() {
  const params = new URLSearchParams(window.location.search);
  const mode = params.get("mode") || "default";
  if (mode === "backtest" || mode === "replay") {
    document.body.classList.add("backtest-mode");
    document.getElementById("dashboard-title").textContent = "Backtest Replay Review";
    document.getElementById("dashboard-lede").textContent =
      "Reads exported market, target exposure, account, risk, order, fill, and event tables. Live agent controls are hidden in replay mode.";
  }
}

function isReplayDashboard() {
  return document.body.classList.contains("backtest-mode") || Boolean(document.getElementById("replay-status"));
}

async function submitOrder(event) {
  event.preventDefault();
  const form = new FormData(event.currentTarget);
  const payload = Object.fromEntries(form.entries());
  if (!payload.agent_id) {
    setAgentStatus("Register or enter a human agent ID before submitting orders.");
    return;
  }
  for (const key of ["qty", "limit_price"]) {
    if (payload[key] === "") delete payload[key];
  }
  if (payload.qty_type === "target_pos") {
    payload.tgt_pos = payload.qty;
  }
  await postService("/orders", payload, "Order submitted.");
}

async function registerHumanAgent() {
  const form = document.getElementById("order-form");
  const data = form ? Object.fromEntries(new FormData(form).entries()) : {};
  const agentId = data.agent_id;
  if (!agentId) {
    setAgentStatus("Enter a human agent ID before registering.");
    return;
  }
  const response = await postAdmin("/agents", {
    agent_id: agentId,
    agent_type: "human",
    status: "active",
    initial_cash: numericOrDefault(data.initial_cash, 10000)
  }, "Human agent registered.");
  if (response?.state) applyServiceState(response.state);
}

async function submitCancel(event) {
  event.preventDefault();
  const form = new FormData(event.currentTarget);
  const payload = Object.fromEntries(form.entries());
  if (!payload.order_id) {
    setAgentStatus("Order ID is required for cancellation.");
    return;
  }
  if (!payload.agent_id) {
    setAgentStatus("Agent ID is required for cancellation.");
    return;
  }
  await postService("/cancel", payload, "Cancel submitted.");
}

async function refreshServiceState() {
  const base = serviceBase();
  try {
    const response = await fetch(`${base}/state`, { cache: "no-store" });
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    const data = await response.json();
    applyServiceState(data);
    setAgentStatus("State refreshed from live service.");
  } catch (err) {
    setAgentStatus(`Unable to refresh service state: ${err.message}`);
  }
}

function configureAgentAutoRefresh() {
  const field = document.getElementById("agent-refresh-interval");
  if (!field) return;
  stopAgentAutoRefresh();
  const intervalMs = Number(field.value);
  if (!Number.isFinite(intervalMs) || intervalMs <= 0) {
    setAgentStatus("Auto refresh is off.");
    return;
  }
  state.agentRefresh.timer = window.setInterval(refreshServiceState, intervalMs);
  setAgentStatus(`Auto refresh every ${formatDuration(intervalMs)}.`);
}

function stopAgentAutoRefresh() {
  if (state.agentRefresh.timer) {
    window.clearInterval(state.agentRefresh.timer);
    state.agentRefresh.timer = null;
  }
}

function configureStateAutoRefresh() {
  if (!document.querySelector(".feed-console") || document.getElementById("order-form")) return;
  stopStateAutoRefresh();
  refreshServiceState();
  state.stateRefresh.timer = window.setInterval(refreshLiveStateTick, 2000);
}

async function refreshLiveStateTick() {
  if (isFeedAutoMode() && state.feed.running) {
    await feedStep("Auto feed checked for completed bars.");
    return;
  }
  await refreshServiceState();
}

function stopStateAutoRefresh() {
  if (state.stateRefresh.timer) {
    window.clearInterval(state.stateRefresh.timer);
    state.stateRefresh.timer = null;
  }
}

async function postService(path, payload, okMessage) {
  const base = serviceBase();
  try {
    const response = await fetch(`${base}${path}`, {
      method: "POST",
      headers: { "Content-Type": "text/plain" },
      body: JSON.stringify(payload)
    });
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    const data = await response.json();
    applyServiceState(data.state || data);
    setAgentStatus(`${okMessage} Command ${data.command_id || "processed"}.`);
  } catch (err) {
    setAgentStatus(`Service request failed: ${err.message}`);
  }
}

async function applyFeedConfig(event) {
  event.preventDefault();
  const data = await postFeed("/feed/config", feedConfigPayload(), "Feed config applied.");
  if (data) applyFeedStatus(data);
  updateFeedRunMode();
}

async function refreshFeedStatus() {
  try {
    const response = await fetch(`${serviceBase()}/feed/status`, { cache: "no-store" });
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    applyFeedStatus(await response.json());
  } catch (err) {
    setFeedStatus(`Unable to refresh feed status: ${err.message}`);
  }
}

async function feedStart() {
  const data = await postFeed("/feed/start", {}, "Feed started.");
  if (data) applyFeedStatus(data);
  if (isFeedAutoMode()) {
    if (!isLiveStateDashboard()) startFeedAutoTimer();
    await feedStep("Auto feed checked for completed bars.");
  }
}

async function feedStop() {
  stopFeedAutoTimer();
  const data = await postFeed("/feed/stop", {}, "Feed stopped.");
  if (data) applyFeedStatus(data);
}

async function feedStep(okMessage = "Feed stepped.") {
  const data = await postFeed("/feed/step", {}, okMessage);
  if (!data) return;
  if (data.state) applyServiceState(data.state);
  applyFeedStatus(data.feed || data);
}

async function feedWarmup() {
  const config = feedConfigPayload();
  if (config.feed_mode !== "simulation") {
    setFeedStatus("Historical warmup is available for simulation mode only.");
    return;
  }
  const configResult = await postFeed("/feed/config", config, "Feed config applied.");
  if (configResult) applyFeedStatus(configResult);
  const form = new FormData(document.getElementById("feed-form"));
  const nBars = Math.trunc(numericOrDefault(Object.fromEntries(form.entries()).warmup_bars, 100));
  const data = await postFeed("/feed/warmup", { n_bars: nBars }, `Simulated ${nBars} historical bars.`);
  if (!data) return;
  if (data.state) applyServiceState(data.state);
  applyFeedStatus(data.feed || data);
}

function updateFeedRunMode() {
  const stepButton = document.getElementById("feed-step");
  if (!stepButton) return;
  const auto = isFeedAutoMode();
  stepButton.disabled = auto;
  stepButton.title = auto ? "Step is disabled in Auto mode; the live-state loop advances the feed." : "";
  if (!auto) {
    stopFeedAutoTimer();
  } else if (!isLiveStateDashboard() && state.feed.running && !state.feed.timer) {
    startFeedAutoTimer();
  }
}

function isFeedAutoMode() {
  const field = document.querySelector('#feed-form [name="run_mode"]');
  return field && field.value === "auto";
}

function startFeedAutoTimer() {
  stopFeedAutoTimer();
  const intervalMs = Math.max(1000, parseTimeframeMs(feedConfigPayload().timeframe));
  const schedulerMs = Math.min(5000, Math.max(1000, Math.floor(intervalMs / 10)));
  state.feed.nextTickAt = Date.now() + schedulerMs;
  state.feed.timer = window.setInterval(async () => {
    state.feed.nextTickAt = Date.now() + schedulerMs;
    await feedStep("Auto feed checked for completed bars.");
  }, schedulerMs);
  setFeedStatus(`Auto feed scheduler started; checking every ${formatDuration(schedulerMs)} for completed ${formatDuration(intervalMs)} bars.`);
}

function stopFeedAutoTimer() {
  if (state.feed.timer) {
    window.clearInterval(state.feed.timer);
    state.feed.timer = null;
    state.feed.nextTickAt = null;
  }
}

async function addAiAgent(event) {
  event.preventDefault();
  const form = new FormData(event.currentTarget);
  const raw = Object.fromEntries(form.entries());
  const payload = {
    agent_type: raw.agent_type || "chaos",
    qty: numericOrDefault(raw.qty, 1),
    lookback: Math.trunc(numericOrDefault(raw.lookback, 12)),
    initial_cash: numericOrDefault(raw.initial_cash, 10000)
  };
  if (raw.agent_id) payload.agent_id = raw.agent_id;
  const data = await postAdmin("/agents", payload, "AI agent added.");
  if (data?.state) applyServiceState(data.state);
}

async function setAiAgentStatus(event) {
  event.preventDefault();
  const submitter = event.submitter;
  const form = new FormData(event.currentTarget);
  const raw = Object.fromEntries(form.entries());
  const action = submitter?.value || raw.status_action || "start";
  if (!raw.agent_id) {
    setAiStatus("Agent ID is required.");
    return;
  }
  const path = action === "remove" ? "/agents/remove" : action === "stop" ? "/agents/stop" : "/agents/start";
  const data = await postAdmin(path, { agent_id: raw.agent_id }, `Agent ${action} submitted.`);
  if (data) applyServiceState(data.state || data);
}

async function refreshAgents() {
  try {
    const response = await fetch(`${serviceBase()}/agents`, { cache: "no-store" });
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    const data = await response.json();
    state.tables.agents = data.agents || [];
    state.tables.agent_rankings = data.rankings || [];
    render();
    setAiStatus("Agents refreshed.");
  } catch (err) {
    setAiStatus(`Unable to refresh agents: ${err.message}`);
  }
}

async function stepAiAgents() {
  const data = await postAdmin("/agents/step", {}, "AI agents stepped.");
  if (data) applyServiceState(data.state || data);
}

async function postAdmin(path, payload, okMessage) {
  try {
    const response = await fetch(`${serviceBase()}${path}`, {
      method: "POST",
      headers: { "Content-Type": "text/plain" },
      body: JSON.stringify(payload)
    });
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    const data = await response.json();
    setAiStatus(okMessage);
    return data;
  } catch (err) {
    setAiStatus(`AI request failed: ${err.message}`);
    return null;
  }
}

async function postFeed(path, payload, okMessage) {
  try {
    const response = await fetch(`${serviceBase()}${path}`, {
      method: "POST",
      headers: { "Content-Type": "text/plain" },
      body: JSON.stringify(payload)
    });
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    const data = await response.json();
    setFeedStatus(okMessage);
    return data;
  } catch (err) {
    setFeedStatus(`Feed request failed: ${err.message}`);
    return null;
  }
}

function applyServiceState(data) {
  state.tables.account_snapshots = data.account || state.tables.account_snapshots || [];
  state.tables.risk_snapshots = deriveRiskRows(data.account || []);
  state.tables.market_events = data.market_events || state.tables.market_events || [];
  state.tables.orders = data.agent_orders || state.tables.orders || [];
  state.tables.agent_orders = data.agent_orders || [];
  state.tables.agent_commands = data.agent_commands || [];
  state.tables.order_requests = data.order_requests || [];
  state.tables.order_cancellations = data.order_cancellations || [];
  state.tables.agents = data.agents || [];
  state.tables.agent_decisions = data.agent_decisions || [];
  state.tables.agent_rankings = data.agent_rankings || [];
  state.tables.events = data.events || state.tables.events || [];
  if (data.feed) applyFeedStatus(data.feed);
  render();
}

function updateAssetFilter(event) {
  state.selectedAsset = event.currentTarget.value || "all";
  render();
}

function renderAssetFilter() {
  const el = document.getElementById("asset-filter");
  if (!el) return;
  const assets = assetOptions(state.tables.market_events || []);
  const current = state.selectedAsset || "all";
  const options = [`<option value="all">All assets</option>`].concat(assets.map(asset => {
    const selected = asset.key === current ? " selected" : "";
    return `<option value="${escapeHtml(asset.key)}"${selected}>${escapeHtml(asset.label)}</option>`;
  }));
  el.innerHTML = options.join("");
  if (current !== "all" && !assets.some(asset => asset.key === current)) {
    state.selectedAsset = "all";
    el.value = "all";
  }
}

function assetOptions(rows) {
  const seen = new Map();
  replayRows(rows).forEach(row => {
    const symbol = String(scalarValue(row.symbol) || "default");
    const assetId = scalarValue(row.asset_id);
    const key = `${symbol}|${assetId ?? ""}`;
    if (!seen.has(key)) seen.set(key, { key, label: `${symbol} (${assetId ?? "na"})` });
  });
  return [...seen.values()].sort((a, b) => a.label.localeCompare(b.label));
}

function filterRowsByAsset(rows = []) {
  const selected = state.selectedAsset || "all";
  if (selected === "all") return rows;
  return rows.filter(row => {
    const symbol = String(scalarValue(row.symbol) || "default");
    const assetId = scalarValue(row.asset_id);
    return `${symbol}|${assetId ?? ""}` === selected;
  });
}

function initializeReplayCursor() {
  if (!isReplayDashboard()) return;
  const bars = normalizeBars(state.tables.market_events || []);
  if (!bars.length) {
    state.replay.cursor = null;
    return;
  }
  const windowEl = document.getElementById("replay-window");
  const windowSize = Math.max(5, Math.trunc(number(windowEl?.value)) || state.replay.windowSize || 50);
  state.replay.windowSize = windowSize;
  state.replay.cursor = Math.min(bars.length, windowSize);
}

function updateReplayWindow() {
  const bars = normalizeBars(state.tables.market_events || []);
  const windowEl = document.getElementById("replay-window");
  state.replay.windowSize = Math.max(5, Math.trunc(number(windowEl?.value)) || 50);
  if (state.replay.cursor === null && bars.length) state.replay.cursor = Math.min(bars.length, state.replay.windowSize);
  render();
}

function replayReset() {
  stopReplayTimer();
  const bars = normalizeBars(state.tables.market_events || []);
  state.replay.cursor = bars.length ? Math.min(bars.length, state.replay.windowSize) : null;
  render();
}

function replayPrev() {
  stopReplayTimer();
  moveReplayCursor(-replayStepSize());
}

function replayNext() {
  moveReplayCursor(replayStepSize());
}

function replayPlayToggle() {
  const button = document.getElementById("replay-play");
  if (state.replay.timer) {
    stopReplayTimer();
    return;
  }
  if (button) button.textContent = "Pause";
  state.replay.timer = window.setInterval(() => {
    const bars = normalizeBars(state.tables.market_events || []);
    if (!bars.length || state.replay.cursor >= bars.length) {
      stopReplayTimer();
      return;
    }
    moveReplayCursor(replayStepSize(), false);
  }, 650);
}

function stopReplayTimer() {
  if (state.replay.timer) {
    window.clearInterval(state.replay.timer);
    state.replay.timer = null;
  }
  const button = document.getElementById("replay-play");
  if (button) button.textContent = "Play";
}

function replayStepSize() {
  const bars = normalizeBars(state.tables.market_events || []);
  const unit = document.getElementById("replay-step-unit")?.value || "bar";
  if (unit === "10bar") return 10;
  if (unit === "all") return bars.length;
  if (unit === "month") return replayMonthStep(bars);
  return 1;
}

function replayMonthStep(bars) {
  if (!bars.length || state.replay.cursor === null) return 1;
  const current = bars[Math.max(0, Math.min(bars.length - 1, state.replay.cursor - 1))];
  const currentMonth = monthKey(current.timestamp);
  let next = state.replay.cursor;
  while (next < bars.length && monthKey(bars[next].timestamp) === currentMonth) next++;
  return Math.max(1, next - state.replay.cursor);
}

function moveReplayCursor(delta, stopAtEnd = true) {
  const bars = normalizeBars(state.tables.market_events || []);
  if (!bars.length) return;
  const start = Math.min(bars.length, Math.max(1, state.replay.windowSize));
  const next = Math.max(start, Math.min(bars.length, (state.replay.cursor || start) + delta));
  state.replay.cursor = next;
  if (stopAtEnd && next >= bars.length) stopReplayTimer();
  render();
}

function replayRows(rows) {
  if (!isReplayDashboard()) return rows;
  const bars = normalizeBars(state.tables.market_events || []);
  if (!bars.length || state.replay.cursor === null) return rows;
  const visibleBars = replayVisibleBars();
  const maxTime = visibleBars[visibleBars.length - 1]?.timestamp;
  if (!maxTime) return rows;
  const maxMs = timeValue(maxTime);
  return rows.filter(row => {
    const rowMs = timeValue(row.timestamp);
    return Number.isFinite(rowMs) ? rowMs <= maxMs : true;
  });
}

function replayVisibleBars() {
  const bars = normalizeBars(state.tables.market_events || []);
  if (!isReplayDashboard() || !bars.length || state.replay.cursor === null) return bars.slice(-80);
  const end = Math.max(1, Math.min(bars.length, state.replay.cursor));
  const start = Math.max(0, end - state.replay.windowSize);
  return bars.slice(start, end);
}

function renderReplayStatus() {
  const el = document.getElementById("replay-status");
  if (!el) return;
  const bars = normalizeBars(state.tables.market_events || []);
  if (!bars.length || state.replay.cursor === null) {
    el.textContent = "Waiting for exported market data.";
    return;
  }
  const visible = replayVisibleBars();
  const first = visible[0]?.timestamp || "n/a";
  const last = visible[visible.length - 1]?.timestamp || "n/a";
  el.textContent = `Showing bars ${Math.max(1, state.replay.cursor - visible.length + 1)}-${state.replay.cursor} of ${bars.length}: ${first} to ${last}.`;
}

function feedConfigPayload() {
  const form = new FormData(document.getElementById("feed-form"));
  const raw = Object.fromEntries(form.entries());
  const randomWalk = {
    start_price: numericOrDefault(raw.start_price, 100),
    drift: numericOrDefault(raw.drift, 0),
    vol: numericOrDefault(raw.vol, 0.02),
    seed: Math.trunc(numericOrDefault(raw.seed, 1))
  };
  return {
    symbol: raw.symbol || "BTC-USDT-SWAP",
    timeframe: raw.timeframe || "4h",
    tz: raw.tz || "UTC",
    feed_mode: raw.feed_mode || "simulation",
    random_walk: randomWalk
  };
}

function parseTimeframeMs(timeframe) {
  const raw = String(timeframe || "4h").trim().toLowerCase();
  const match = raw.match(/^([0-9.]+)\s*([a-z]+)$/);
  if (!match) return 4 * 60 * 60 * 1000;
  const value = Number(match[1]);
  const unit = match[2];
  const multiplier = {
    s: 1000,
    sec: 1000,
    secs: 1000,
    second: 1000,
    seconds: 1000,
    m: 60 * 1000,
    min: 60 * 1000,
    mins: 60 * 1000,
    minute: 60 * 1000,
    minutes: 60 * 1000,
    h: 60 * 60 * 1000,
    hr: 60 * 60 * 1000,
    hour: 60 * 60 * 1000,
    hours: 60 * 60 * 1000,
    d: 24 * 60 * 60 * 1000,
    day: 24 * 60 * 60 * 1000,
    days: 24 * 60 * 60 * 1000
  }[unit];
  return Number.isFinite(value) && multiplier ? value * multiplier : 4 * 60 * 60 * 1000;
}

function formatDuration(ms) {
  const seconds = Math.round(ms / 1000);
  if (seconds < 60) return `${seconds}s`;
  const minutes = seconds / 60;
  if (minutes < 60) return `${formatNumber(minutes, minutes % 1 ? 1 : 0)}m`;
  const hours = minutes / 60;
  if (hours < 24) return `${formatNumber(hours, hours % 1 ? 1 : 0)}h`;
  const days = hours / 24;
  return `${formatNumber(days, days % 1 ? 1 : 0)}d`;
}

function applyFeedStatus(feed = {}) {
  if (!feed || typeof feed !== "object") {
    setFeedStatus("Feed status is unavailable.");
    return;
  }
  const runningValue = scalarValue(feed.running);
  const running = runningValue === true || runningValue === "TRUE" || runningValue === "true" || runningValue === 1 || runningValue === "1";
  state.feed.running = running;
  if (!running) {
    stopFeedAutoTimer();
  } else if (!isLiveStateDashboard() && isFeedAutoMode() && !state.feed.timer) {
    startFeedAutoTimer();
  }
  const details = [
    `status=${running ? "running" : "stopped"}`,
    `mode=${scalarValue(feed.feed_mode) || "unknown"}`,
    `symbol=${scalarValue(feed.symbol) || "unknown"}`,
    `timeframe=${scalarValue(feed.timeframe) || "unknown"}`,
    `tz=${scalarValue(feed.tz) || "unknown"}`,
    `bars=${scalarValue(feed.bars) ?? "0"}`,
    `last_end=${scalarValue(feed.last_completed_end) || "none"}`,
    `last_price=${scalarValue(feed.last_price) ?? "n/a"}`
  ];
  setFeedStatus(details.join(" | "));
}

function isLiveStateDashboard() {
  return Boolean(document.querySelector(".feed-console")) &&
    !document.getElementById("order-form") &&
    !isReplayDashboard();
}

function setFeedField(name, value) {
  if (value === undefined || value === null || value === "") return;
  const field = document.querySelector(`#feed-form [name="${name}"]`);
  if (field) field.value = value;
}

function deriveRiskRows(accountRows) {
  if (!accountRows.length) return state.tables.risk_snapshots || [];
  return accountRows.map(row => {
    const equity = number(row.equity);
    const absNotional = number(row.abs_notional);
    const maintenance = number(row.maintenance_margin);
    return {
      timestamp: row.timestamp,
      equity: row.equity,
      abs_notional: row.abs_notional,
      leverage: Number.isFinite(equity) && equity > 0 ? absNotional / equity : "",
      maintenance_margin: row.maintenance_margin,
      margin_buffer: Number.isFinite(equity) && Number.isFinite(maintenance) ? equity - maintenance : ""
    };
  });
}

function serviceBase() {
  const el = document.getElementById("service-url");
  return (el ? el.value : "http://127.0.0.1:8080").replace(/\/+$/, "");
}

function applyServiceUrlParam() {
  const el = document.getElementById("service-url");
  const params = new URLSearchParams(window.location.search);
  if (el) {
    const serviceUrl = params.get("service_url") || params.get("service");
    if (serviceUrl) el.value = serviceUrl;
  }
  const agentId = params.get("agent_id") || params.get("agent");
  if (agentId) {
    document.querySelectorAll('input[name="agent_id"]').forEach(input => {
      input.value = agentId;
    });
  }
}

function setAgentStatus(message) {
  const el = document.getElementById("agent-status");
  if (el) el.textContent = message;
}

function setFeedStatus(message) {
  const el = document.getElementById("feed-status");
  if (el) el.textContent = message;
}

function setAiStatus(message) {
  const el = document.getElementById("ai-status");
  if (el) el.textContent = message;
}

function renderManifest() {
  const summary = document.getElementById("manifest-summary");
  const counts = document.getElementById("table-counts");
  if (!summary || !counts) return;
  const manifest = state.manifest;
  const version = manifest[0]?.schema_version || "unknown";
  const packageVersion = manifest[0]?.package_version || "unknown";
  summary.textContent =
    `Schema ${version}, tradesimr ${packageVersion}, ${manifest.length} declared tables.`;
  counts.innerHTML = manifest.map(row =>
    `<span><strong>${escapeHtml(row.table)}</strong>${escapeHtml(row.rows || "0")} rows</span>`
  ).join("");
}

function renderKpis() {
  const el = document.getElementById("kpis");
  if (!el) return;
  const account = replayRows(state.tables.account_snapshots || []);
  const risk = replayRows(state.tables.risk_snapshots || []);
  const lastAccount = account[account.length - 1] || {};
  const lastRisk = risk[risk.length - 1] || {};
  const firstEquity = number(account[0]?.equity);
  const lastEquity = number(lastAccount.equity);
  const pnl = isFinite(firstEquity) && isFinite(lastEquity) ? lastEquity - firstEquity : NaN;
  const items = [
    ["Equity", formatNumber(lastEquity)],
    ["Cash", formatNumber(number(lastAccount.cash))],
    ["Notional", formatNumber(number(lastAccount.abs_notional ?? lastAccount.notional))],
    ["Leverage", formatNumber(number(lastRisk.leverage), 3)],
    ["Margin Buffer", formatNumber(number(lastRisk.margin_buffer))],
    ["P&L", formatNumber(pnl)]
  ];
  el.innerHTML = items.map(([label, value]) =>
    `<article class="kpi"><span>${label}</span><strong>${value}</strong></article>`
  ).join("");
}

function renderCandles() {
  const bars = normalizeBars(filterRowsByAsset(state.tables.market_events || []));
  const el = document.getElementById("candle-chart");
  const summary = document.getElementById("candle-summary");
  if (!el || !summary) return;
  if (!bars.length) {
    el.innerHTML = "<p class=\"empty\">No OHLC bars available. Export market_events.csv or refresh live service state.</p>";
    summary.textContent = "No market data loaded.";
    return;
  }

  const shown = replayVisibleBars();
  const fills = normalizeMarkers(replayRows(filterRowsByAsset(state.tables.fills || [])), "fill");
  const orders = normalizeMarkers(replayRows(filterRowsByAsset(state.tables.orders || [])), "order");
  const visibleTimes = new Set(shown.map(row => String(row.timestamp)));
  const markerRows = [...fills, ...orders].filter(marker => marker.price > 0 && visibleTimes.has(String(marker.timestamp)));
  const width = 960;
  const height = 360;
  const pad = { top: 24, right: 72, bottom: 44, left: 82 };
  const plotW = width - pad.left - pad.right;
  const plotH = height - pad.top - pad.bottom;
  const lows = shown.map(row => row.low);
  const highs = shown.map(row => row.high);
  const minY = Math.min(...lows);
  const maxY = Math.max(...highs);
  const yRange = maxY - minY || 1;
  const step = plotW / Math.max(1, shown.length);
  const bodyW = Math.max(3, Math.min(14, step * 0.58));
  const xFor = i => pad.left + step * i + step / 2;
  const yFor = price => pad.top + (maxY - price) / yRange * plotH;
  const timeIndex = new Map(shown.map((bar, i) => [String(bar.timestamp), i]));
  const yTicks = axisYLabels(minY, maxY, yFor, pad.left - 10, 4, 4);

  const candleSvg = shown.map((bar, i) => {
    const x = xFor(i);
    const openY = yFor(bar.open);
    const closeY = yFor(bar.close);
    const highY = yFor(bar.high);
    const lowY = yFor(bar.low);
    const top = Math.min(openY, closeY);
    const bodyH = Math.max(1, Math.abs(openY - closeY));
    const cls = bar.close >= bar.open ? "up" : "down";
    return `
      <line x1="${x.toFixed(2)}" y1="${highY.toFixed(2)}" x2="${x.toFixed(2)}" y2="${lowY.toFixed(2)}" class="candle-wick ${cls}"></line>
      <rect x="${(x - bodyW / 2).toFixed(2)}" y="${top.toFixed(2)}" width="${bodyW.toFixed(2)}" height="${bodyH.toFixed(2)}" class="candle-body ${cls}">
        <title>${escapeHtml(bar.timestamp)} O ${formatNumber(bar.open, 4)} H ${formatNumber(bar.high, 4)} L ${formatNumber(bar.low, 4)} C ${formatNumber(bar.close, 4)}</title>
      </rect>`;
  }).join("");

  const markerSvg = markerRows.map(marker => {
    const idx = timeIndex.get(String(marker.timestamp));
    if (idx === undefined) return "";
    const x = xFor(idx);
    const y = yFor(marker.price);
    const cls = marker.kind === "fill" ? "fill-marker" : "order-marker";
    const label = `${marker.kind} ${marker.side || ""} ${marker.price}`;
    return `<circle cx="${x.toFixed(2)}" cy="${y.toFixed(2)}" r="${marker.kind === "fill" ? 4.5 : 3.5}" class="${cls}"><title>${escapeHtml(label)}</title></circle>`;
  }).join("");

  const last = shown[shown.length - 1];
  summary.textContent = `Visible bars ${shown.length} of ${bars.length}; latest visible ${last.timestamp}; close ${formatNumber(last.close, 4)}. Y-axis is scaled to visible candles.`;
  el.innerHTML = `
    <svg viewBox="0 0 ${width} ${height}" role="img" aria-label="OHLC candle chart">
      <rect x="${pad.left}" y="${pad.top}" width="${plotW}" height="${plotH}" class="plot-bg"></rect>
      <line x1="${pad.left}" y1="${pad.top}" x2="${pad.left}" y2="${height - pad.bottom}" class="axis"></line>
      <line x1="${pad.left}" y1="${height - pad.bottom}" x2="${width - pad.right}" y2="${height - pad.bottom}" class="axis"></line>
      <text x="${width - pad.right}" y="${pad.top - 8}" text-anchor="end" class="chart-label">last ${formatNumber(last.close, 4)}</text>
      ${yTicks}
      ${axisTimeLabels(shown, xFor, height - 18)}
      ${candleSvg}
      ${markerSvg}
    </svg>`;
}

function renderTargets() {
  const rows = normalizeTargetRows(replayRows(state.tables.strategy_snapshots || []));
  const el = document.getElementById("target-chart");
  const summary = document.getElementById("target-summary");
  if (!el || !summary) return;
  if (!rows.length) {
    el.innerHTML = "<p class=\"empty\">No target exposure snapshots available.</p>";
    summary.textContent = "No strategy snapshots loaded.";
    return;
  }

  const shown = rows.slice(-240);
  const width = 960;
  const height = 260;
  const pad = { top: 22, right: 60, bottom: 36, left: 56 };
  const plotW = width - pad.left - pad.right;
  const plotH = height - pad.top - pad.bottom;
  const values = shown.flatMap(row => [row.tgt_pos, row.ctr_unit * row.pos_dir]).filter(Number.isFinite);
  const minY = Math.min(-1, ...values);
  const maxY = Math.max(1, ...values);
  const yRange = maxY - minY || 1;
  const xFor = i => pad.left + (i / Math.max(1, shown.length - 1)) * plotW;
  const yFor = value => pad.top + (maxY - value) / yRange * plotH;
  const targetPath = shown.map((row, i) => `${xFor(i).toFixed(2)},${yFor(row.tgt_pos).toFixed(2)}`).join(" ");
  const positionPath = shown.map((row, i) => `${xFor(i).toFixed(2)},${yFor(row.ctr_unit * row.pos_dir).toFixed(2)}`).join(" ");
  const zeroY = yFor(0);
  const last = rows[rows.length - 1];

  summary.textContent = `Snapshots ${rows.length}; latest target ${formatNumber(last.tgt_pos, 3)}, position ${formatNumber(last.ctr_unit * last.pos_dir, 3)}, strategy ${last.pos_strat}.`;
  el.innerHTML = `
    <svg viewBox="0 0 ${width} ${height}" role="img" aria-label="Target exposure replay">
      <rect x="${pad.left}" y="${pad.top}" width="${plotW}" height="${plotH}" class="plot-bg"></rect>
      <line x1="${pad.left}" y1="${zeroY.toFixed(2)}" x2="${width - pad.right}" y2="${zeroY.toFixed(2)}" class="zero-line"></line>
      <line x1="${pad.left}" y1="${pad.top}" x2="${pad.left}" y2="${height - pad.bottom}" class="axis"></line>
      <line x1="${pad.left}" y1="${height - pad.bottom}" x2="${width - pad.right}" y2="${height - pad.bottom}" class="axis"></line>
      <polyline points="${targetPath}" class="target-line"><title>Target position</title></polyline>
      <polyline points="${positionPath}" class="position-line"><title>Realized position direction * contracts</title></polyline>
      <text x="${pad.left}" y="${pad.top - 8}" class="chart-label">${formatNumber(maxY, 3)}</text>
      <text x="${pad.left}" y="${height - 10}" class="chart-label">${formatNumber(minY, 3)}</text>
      <text x="${width - pad.right}" y="${pad.top - 8}" text-anchor="end" class="chart-label">target / position</text>
    </svg>`;
}

function normalizeBars(rows) {
  return rows.map(row => ({
    timestamp: scalarValue(row.timestamp),
    symbol: String(scalarValue(row.symbol) || "default"),
    asset_id: scalarValue(row.asset_id),
    open: number(row.open),
    high: number(row.high),
    low: number(row.low),
    close: number(row.close)
  })).filter(row =>
    row.timestamp !== undefined &&
    Number.isFinite(row.open) &&
    Number.isFinite(row.high) &&
    Number.isFinite(row.low) &&
    Number.isFinite(row.close)
  );
}

function normalizeTargetRows(rows) {
  return rows.map(row => ({
    timestamp: row.timestamp,
    tgt_pos: number(row.tgt_pos),
    pos_strat: row.pos_strat || "",
    pos_dir: number(row.pos_dir || 0),
    ctr_unit: number(row.ctr_unit || 0),
    tol_pos: number(row.tol_pos)
  })).filter(row => row.timestamp !== undefined && Number.isFinite(row.tgt_pos));
}

function normalizeMarkers(rows, kind) {
  return rows.map(row => ({
    kind,
    timestamp: row.timestamp,
    side: row.side || row.dir_label || row.action_label || "",
    price: number(row.price || row.limit_price)
  })).filter(row => row.timestamp !== undefined && Number.isFinite(row.price));
}

function renderEquity() {
  const account = replayRows(state.tables.account_snapshots || []);
  const points = account.map(row => ({
    agent: String(scalarValue(row.agent_id) || "account"),
    y: number(scalarValue(row.equity)),
    t: scalarValue(row.timestamp)
  })).filter(point => isFinite(point.y));
  const el = document.getElementById("equity-chart");
  if (!el) return;
  if (points.length < 2) {
    el.innerHTML = "<p class=\"empty\">Not enough account snapshots to draw an equity curve.</p>";
    return;
  }
  const width = 900;
  const height = 280;
  const pad = { top: 30, right: 28, bottom: 52, left: 82 };
  const ys = points.map(point => point.y);
  const minY = Math.min(...ys);
  const maxY = Math.max(...ys);
  const yRange = maxY - minY || 1;
  const agents = [...new Set(points.map(point => point.agent))];
  const groups = agents.map(agent => ({
    agent,
    points: points.filter(point => point.agent === agent)
      .sort((a, b) => timeValue(a.t) - timeValue(b.t))
  }));
  const maxLen = Math.max(...groups.map(group => group.points.length));
  const palette = ["#d45f2f", "#0f766e", "#2456a6", "#9f7a16", "#8b3f63", "#4d6b2f", "#6d4ba3"];
  const colorFor = i => palette[i % palette.length];
  const xFor = (i, n = maxLen) => pad.left + (i / Math.max(1, n - 1)) * (width - pad.left - pad.right);
  const yFor = y => height - pad.bottom - ((y - minY) / yRange) * (height - pad.top - pad.bottom);
  const yTicks = axisYLabels(minY, maxY, yFor, pad.left - 10, 4, 2);
  const paths = groups.map((group, gi) => {
    if (group.points.length < 2) return "";
    const path = group.points.map((point, i) => `${xFor(i, group.points.length).toFixed(2)},${yFor(point.y).toFixed(2)}`).join(" ");
    return `<polyline points="${path}" class="equity-line agent-equity-line" style="stroke:${colorFor(gi)}"><title>${escapeHtml(group.agent)}</title></polyline>`;
  }).join("");
  const legend = groups.map((group, gi) =>
    `<span><i style="background:${colorFor(gi)}"></i>${escapeHtml(group.agent)}</span>`
  ).join("");
  el.innerHTML = `
    <svg viewBox="0 0 ${width} ${height}" role="img" aria-label="Equity curve">
      <line x1="${pad.left}" y1="${height - pad.bottom}" x2="${width - pad.right}" y2="${height - pad.bottom}" class="axis"></line>
      <line x1="${pad.left}" y1="${pad.top}" x2="${pad.left}" y2="${height - pad.bottom}" class="axis"></line>
      ${paths}
      ${yTicks}
      ${axisTimeLabels(points, xFor, height - 16, point => point.t)}
    </svg>
    <div class="chart-legend">${legend}</div>`;
}

function renderRiskSummary() {
  const el = document.getElementById("risk-summary");
  if (!el) return;
  const risk = replayRows(state.tables.risk_snapshots || []);
  const last = risk[risk.length - 1] || {};
  const rows = [
    ["Equity", last.equity],
    ["Abs Notional", last.abs_notional],
    ["Leverage", last.leverage],
    ["Maintenance Margin", last.maintenance_margin],
    ["Margin Buffer", last.margin_buffer]
  ];
  el.innerHTML = rows.map(([label, raw]) =>
    `<div><span>${label}</span><strong>${formatNumber(number(raw), label === "Leverage" ? 3 : 2)}</strong></div>`
  ).join("");
}

function renderTimeline() {
  const events = replayRows(state.tables.events || []);
  const el = document.getElementById("event-timeline");
  if (!el) return;
  if (!events.length) {
    el.innerHTML = "<p class=\"empty\">No recorded events.</p>";
    return;
  }
  el.innerHTML = events.slice(-80).map(row => `
    <div class="event">
      <span class="dot ${escapeHtml(row.event_type_label || "event")}"></span>
      <div>
        <strong>${escapeHtml(row.event_type_label || "event")} / ${escapeHtml(row.status_label || "")}</strong>
        <p>${escapeHtml(row.timestamp || "")} · ${escapeHtml(row.action_label || "")} ${escapeHtml(row.dir_label || "")} · qty ${escapeHtml(row.ctr_qty || "")} @ ${escapeHtml(row.price || "")}</p>
      </div>
    </div>
  `).join("");
}

function combinedOrderFillRows() {
  const orders = replayRows(state.tables.orders || []).map(row => ({ source: "order", ...row }));
  const fills = replayRows(state.tables.fills || []).map(row => ({ source: "fill", ...row }));
  return [...orders, ...fills].sort((a, b) => timeValue(a.timestamp) - timeValue(b.timestamp));
}

function renderTable(id, rows = [], preferred = [], limit = 50, options = {}) {
  const el = document.getElementById(id);
  if (!el) return;
  if (!rows.length) {
    el.innerHTML = "<p class=\"empty\">No rows.</p>";
    return;
  }
  let columns = preferred.filter(col => col in rows[0]);
  if (!columns.length) columns = Object.keys(rows[0]);
  const newestFirst = options.newestFirst !== false;
  const shown = newestFirst ? rows.slice(-limit).reverse() : rows.slice(0, limit);
  el.innerHTML = `
    <table>
      <thead><tr>${columns.map(col => `<th>${escapeHtml(col)}</th>`).join("")}</tr></thead>
      <tbody>${shown.map(row => `<tr>${columns.map(col => `<td>${escapeHtml(row[col])}</td>`).join("")}</tr>`).join("")}</tbody>
    </table>`;
}

function basename(path) {
  return String(path).split(/[\\/]/).pop();
}

function setStatus(message) {
  const el = document.getElementById("load-status");
  if (el) el.textContent = message;
}

function number(value) {
  value = scalarValue(value);
  const parsed = Number(value);
  return Number.isFinite(parsed) ? parsed : NaN;
}

function timeValue(value) {
  value = scalarValue(value);
  if (value === undefined || value === null || value === "") return NaN;
  const parsed = Date.parse(value);
  if (Number.isFinite(parsed)) return parsed;
  const numeric = Number(value);
  return Number.isFinite(numeric) ? numeric : NaN;
}

function scalarValue(value) {
  if (Array.isArray(value)) return value.length ? scalarValue(value[0]) : "";
  if (value && typeof value === "object") {
    if ("value" in value) return scalarValue(value.value);
    const keys = Object.keys(value);
    if (keys.length === 1) return scalarValue(value[keys[0]]);
  }
  return value;
}

function monthKey(value) {
  const ms = timeValue(value);
  if (!Number.isFinite(ms)) return String(value).slice(0, 7);
  const d = new Date(ms);
  return `${d.getUTCFullYear()}-${String(d.getUTCMonth() + 1).padStart(2, "0")}`;
}

function shortTimeLabel(value) {
  const ms = timeValue(value);
  if (!Number.isFinite(ms)) return String(value ?? "");
  const d = new Date(ms);
  const date = `${String(d.getUTCMonth() + 1).padStart(2, "0")}-${String(d.getUTCDate()).padStart(2, "0")}`;
  const time = `${String(d.getUTCHours()).padStart(2, "0")}:${String(d.getUTCMinutes()).padStart(2, "0")}`;
  return `${date} ${time}`;
}

function axisTimeLabels(rows, xFor, y, timeAccessor = row => row.timestamp) {
  if (!rows.length) return "";
  const tickCount = Math.min(5, rows.length);
  const indexes = Array.from({ length: tickCount }, (_, i) =>
    Math.round((i / Math.max(1, tickCount - 1)) * (rows.length - 1))
  );
  return [...new Set(indexes)].map(i => {
    const anchor = i === 0 ? "start" : i === rows.length - 1 ? "end" : "middle";
    return `<text x="${xFor(i).toFixed(2)}" y="${y}" text-anchor="${anchor}" class="chart-label x-tick">${escapeHtml(shortTimeLabel(timeAccessor(rows[i])))}</text>`;
  }).join("");
}

function axisYLabels(minY, maxY, yFor, x, tickCount = 4, digits = 2) {
  if (!Number.isFinite(minY) || !Number.isFinite(maxY)) return "";
  const count = Math.max(2, tickCount);
  const values = Array.from({ length: count }, (_, i) =>
    minY + (i / Math.max(1, count - 1)) * (maxY - minY)
  );
  return values.map(value => {
    const y = yFor(value);
    return `
      <line x1="${x + 10}" y1="${y.toFixed(2)}" x2="${x + 16}" y2="${y.toFixed(2)}" class="axis-tick"></line>
      <text x="${x}" y="${(y + 4).toFixed(2)}" text-anchor="end" class="chart-label y-tick">${formatNumber(value, digits)}</text>`;
  }).join("");
}

function niceCeil(value) {
  if (!Number.isFinite(value) || value === 0) return 1;
  const abs = Math.abs(value);
  const pow = 10 ** Math.floor(Math.log10(abs));
  return Math.ceil(value / pow) * pow;
}

function niceFloor(value) {
  if (!Number.isFinite(value) || value === 0) return 0;
  const abs = Math.abs(value);
  const pow = 10 ** Math.floor(Math.log10(abs));
  return Math.floor(value / pow) * pow;
}

function numericOrDefault(value, fallback) {
  const parsed = Number(value);
  return Number.isFinite(parsed) ? parsed : fallback;
}

function formatNumber(value, digits = 2) {
  if (!Number.isFinite(value)) return "n/a";
  return value.toLocaleString(undefined, { maximumFractionDigits: digits, minimumFractionDigits: Math.min(digits, 2) });
}

function escapeHtml(value) {
  return String(value ?? "").replace(/[&<>"']/g, ch => ({
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': "&quot;",
    "'": "&#39;"
  }[ch]));
}
