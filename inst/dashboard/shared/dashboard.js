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
  "agent_orders"
];

const state = {
  manifest: [],
  tables: {}
};

document.addEventListener("DOMContentLoaded", () => {
  applyDashboardMode();
  bind("file-picker", "change", handleFilePick);
  bind("order-form", "submit", submitOrder);
  bind("cancel-form", "submit", submitCancel);
  bind("refresh-state", "click", refreshServiceState);
  bind("feed-form", "submit", applyFeedConfig);
  bind("feed-refresh", "click", refreshFeedStatus);
  bind("feed-start", "click", feedStart);
  bind("feed-stop", "click", feedStop);
  bind("feed-step", "click", feedStep);
  loadFromFolder();
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
  renderManifest();
  renderKpis();
  renderCandles();
  renderEquity();
  renderTargets();
  renderRiskSummary();
  renderTimeline();
  renderTable("orders-table", state.tables.orders, ["timestamp", "order_id", "agent_id", "side", "qty_type", "qty", "order_type", "status", "action_label", "dir_label", "ctr_qty", "price", "status_label"]);
  renderTable("commands-table", state.tables.agent_commands, ["timestamp", "command_id", "agent_id", "command_type", "status", "ref_id", "message"]);
  renderTable("requests-table", state.tables.order_requests, ["timestamp", "command_id", "agent_id", "side", "qty_type", "qty", "order_type", "status", "order_id", "message"]);
  renderTable("fills-table", state.tables.fills, ["timestamp", "action_label", "dir_label", "ctr_qty", "price", "fee", "realized_pnl"]);
  renderTable("risk-table", state.tables.risk_snapshots, ["timestamp", "equity", "abs_notional", "leverage", "maintenance_margin", "margin_buffer"], 25);
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

async function submitOrder(event) {
  event.preventDefault();
  const form = new FormData(event.currentTarget);
  const payload = Object.fromEntries(form.entries());
  for (const key of ["qty", "limit_price"]) {
    if (payload[key] === "") delete payload[key];
  }
  if (payload.qty_type === "target_pos") {
    payload.tgt_pos = payload.qty;
  }
  await postService("/orders", payload, "Order submitted.");
}

async function submitCancel(event) {
  event.preventDefault();
  const form = new FormData(event.currentTarget);
  const payload = Object.fromEntries(form.entries());
  if (!payload.order_id) {
    setAgentStatus("Order ID is required for cancellation.");
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
}

async function feedStop() {
  const data = await postFeed("/feed/stop", {}, "Feed stopped.");
  if (data) applyFeedStatus(data);
}

async function feedStep() {
  const data = await postFeed("/feed/step", {}, "Feed stepped.");
  if (!data) return;
  if (data.state) applyServiceState(data.state);
  applyFeedStatus(data.feed || data);
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
  state.tables.events = data.events || state.tables.events || [];
  if (data.feed) applyFeedStatus(data.feed);
  render();
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

function applyFeedStatus(feed = {}) {
  if (!feed || typeof feed !== "object") {
    setFeedStatus("Feed status is unavailable.");
    return;
  }
  setFeedField("symbol", feed.symbol);
  setFeedField("timeframe", feed.timeframe);
  setFeedField("tz", feed.tz);
  setFeedField("feed_mode", feed.feed_mode);
  const running = feed.running === true || feed.running === "TRUE" || feed.running === "true";
  const details = [
    `status=${running ? "running" : "stopped"}`,
    `mode=${feed.feed_mode || "unknown"}`,
    `symbol=${feed.symbol || "unknown"}`,
    `timeframe=${feed.timeframe || "unknown"}`,
    `tz=${feed.tz || "unknown"}`,
    `bars=${feed.bars ?? "0"}`,
    `last_end=${feed.last_completed_end || "none"}`,
    `last_price=${feed.last_price ?? "n/a"}`
  ];
  setFeedStatus(details.join(" | "));
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

function setAgentStatus(message) {
  const el = document.getElementById("agent-status");
  if (el) el.textContent = message;
}

function setFeedStatus(message) {
  const el = document.getElementById("feed-status");
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
  const account = state.tables.account_snapshots || [];
  const risk = state.tables.risk_snapshots || [];
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
  const bars = normalizeBars(state.tables.market_events || []);
  const el = document.getElementById("candle-chart");
  const summary = document.getElementById("candle-summary");
  if (!el || !summary) return;
  if (!bars.length) {
    el.innerHTML = "<p class=\"empty\">No OHLC bars available. Export market_events.csv or refresh live service state.</p>";
    summary.textContent = "No market data loaded.";
    return;
  }

  const shown = bars.slice(-80);
  const fills = normalizeMarkers(state.tables.fills || [], "fill");
  const orders = normalizeMarkers(state.tables.orders || [], "order");
  const markerRows = [...fills, ...orders].filter(marker => marker.price > 0);
  const width = 960;
  const height = 360;
  const pad = { top: 24, right: 72, bottom: 44, left: 58 };
  const plotW = width - pad.left - pad.right;
  const plotH = height - pad.top - pad.bottom;
  const lows = shown.map(row => row.low);
  const highs = shown.map(row => row.high);
  const markerPrices = markerRows.map(row => row.price);
  const minY = Math.min(...lows, ...markerPrices);
  const maxY = Math.max(...highs, ...markerPrices);
  const yRange = maxY - minY || 1;
  const step = plotW / Math.max(1, shown.length);
  const bodyW = Math.max(3, Math.min(14, step * 0.58));
  const xFor = i => pad.left + step * i + step / 2;
  const yFor = price => pad.top + (maxY - price) / yRange * plotH;
  const timeIndex = new Map(shown.map((bar, i) => [String(bar.timestamp), i]));

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

  const last = bars[bars.length - 1];
  summary.textContent = `Bars ${bars.length}; latest ${last.timestamp}; close ${formatNumber(last.close, 4)}. Fills and orders are overlaid when timestamps match visible candles.`;
  el.innerHTML = `
    <svg viewBox="0 0 ${width} ${height}" role="img" aria-label="OHLC candle chart">
      <rect x="${pad.left}" y="${pad.top}" width="${plotW}" height="${plotH}" class="plot-bg"></rect>
      <line x1="${pad.left}" y1="${pad.top}" x2="${pad.left}" y2="${height - pad.bottom}" class="axis"></line>
      <line x1="${pad.left}" y1="${height - pad.bottom}" x2="${width - pad.right}" y2="${height - pad.bottom}" class="axis"></line>
      <text x="${pad.left}" y="${pad.top - 8}" class="chart-label">${formatNumber(maxY, 4)}</text>
      <text x="${pad.left}" y="${height - 12}" class="chart-label">${formatNumber(minY, 4)}</text>
      <text x="${width - pad.right}" y="${pad.top - 8}" text-anchor="end" class="chart-label">last ${formatNumber(last.close, 4)}</text>
      ${candleSvg}
      ${markerSvg}
    </svg>`;
}

function renderTargets() {
  const rows = normalizeTargetRows(state.tables.strategy_snapshots || []);
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
    timestamp: row.timestamp,
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
  const account = state.tables.account_snapshots || [];
  const points = account.map((row, i) => ({ x: i, y: number(row.equity), t: row.timestamp })).filter(point => isFinite(point.y));
  const el = document.getElementById("equity-chart");
  if (!el) return;
  if (points.length < 2) {
    el.innerHTML = "<p class=\"empty\">Not enough account snapshots to draw an equity curve.</p>";
    return;
  }
  const width = 900;
  const height = 280;
  const pad = 36;
  const ys = points.map(point => point.y);
  const minY = Math.min(...ys);
  const maxY = Math.max(...ys);
  const yRange = maxY - minY || 1;
  const path = points.map((point, i) => {
    const x = pad + (i / (points.length - 1)) * (width - pad * 2);
    const y = height - pad - ((point.y - minY) / yRange) * (height - pad * 2);
    return `${x.toFixed(2)},${y.toFixed(2)}`;
  }).join(" ");
  el.innerHTML = `
    <svg viewBox="0 0 ${width} ${height}" role="img" aria-label="Equity curve">
      <line x1="${pad}" y1="${height - pad}" x2="${width - pad}" y2="${height - pad}" class="axis"></line>
      <line x1="${pad}" y1="${pad}" x2="${pad}" y2="${height - pad}" class="axis"></line>
      <polyline points="${path}" class="equity-line"></polyline>
      <text x="${pad}" y="${pad - 10}" class="chart-label">${formatNumber(maxY)}</text>
      <text x="${pad}" y="${height - 8}" class="chart-label">${formatNumber(minY)}</text>
    </svg>`;
}

function renderRiskSummary() {
  const el = document.getElementById("risk-summary");
  if (!el) return;
  const risk = state.tables.risk_snapshots || [];
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
  const events = state.tables.events || [];
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

function renderTable(id, rows = [], preferred = [], limit = 50) {
  const el = document.getElementById(id);
  if (!el) return;
  if (!rows.length) {
    el.innerHTML = "<p class=\"empty\">No rows.</p>";
    return;
  }
  let columns = preferred.filter(col => col in rows[0]);
  if (!columns.length) columns = Object.keys(rows[0]);
  const shown = rows.slice(-limit).reverse();
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
  const parsed = Number(value);
  return Number.isFinite(parsed) ? parsed : NaN;
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
