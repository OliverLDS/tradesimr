const REQUIRED_TABLES = [
  "events",
  "account_snapshots",
  "risk_snapshots",
  "orders",
  "fills"
];

const state = {
  manifest: [],
  tables: {}
};

document.addEventListener("DOMContentLoaded", () => {
  document.getElementById("file-picker").addEventListener("change", handleFilePick);
  loadFromFolder();
});

async function loadFromFolder() {
  try {
    const manifest = parseCsv(await fetchText("manifest.csv"));
    const files = Object.fromEntries(manifest.map(row => [row.table, row.file]));
    const tables = {};
    for (const table of REQUIRED_TABLES) {
      const file = files[table] || `${table}.csv`;
      tables[table] = parseCsv(await fetchText(file));
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
  renderEquity();
  renderRiskSummary();
  renderTimeline();
  renderTable("orders-table", state.tables.orders, ["timestamp", "action_label", "dir_label", "ctr_qty", "price", "status_label"]);
  renderTable("fills-table", state.tables.fills, ["timestamp", "action_label", "dir_label", "ctr_qty", "price", "fee", "realized_pnl"]);
  renderTable("risk-table", state.tables.risk_snapshots, ["timestamp", "equity", "abs_notional", "leverage", "maintenance_margin", "margin_buffer"], 25);
}

function renderManifest() {
  const manifest = state.manifest;
  const version = manifest[0]?.schema_version || "unknown";
  const packageVersion = manifest[0]?.package_version || "unknown";
  document.getElementById("manifest-summary").textContent =
    `Schema ${version}, tradesimr ${packageVersion}, ${manifest.length} declared tables.`;
  document.getElementById("table-counts").innerHTML = manifest.map(row =>
    `<span><strong>${escapeHtml(row.table)}</strong>${escapeHtml(row.rows || "0")} rows</span>`
  ).join("");
}

function renderKpis() {
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
  document.getElementById("kpis").innerHTML = items.map(([label, value]) =>
    `<article class="kpi"><span>${label}</span><strong>${value}</strong></article>`
  ).join("");
}

function renderEquity() {
  const account = state.tables.account_snapshots || [];
  const points = account.map((row, i) => ({ x: i, y: number(row.equity), t: row.timestamp })).filter(point => isFinite(point.y));
  const el = document.getElementById("equity-chart");
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
  const risk = state.tables.risk_snapshots || [];
  const last = risk[risk.length - 1] || {};
  const rows = [
    ["Equity", last.equity],
    ["Abs Notional", last.abs_notional],
    ["Leverage", last.leverage],
    ["Maintenance Margin", last.maintenance_margin],
    ["Margin Buffer", last.margin_buffer]
  ];
  document.getElementById("risk-summary").innerHTML = rows.map(([label, raw]) =>
    `<div><span>${label}</span><strong>${formatNumber(number(raw), label === "Leverage" ? 3 : 2)}</strong></div>`
  ).join("");
}

function renderTimeline() {
  const events = state.tables.events || [];
  const el = document.getElementById("event-timeline");
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
  if (!rows.length) {
    el.innerHTML = "<p class=\"empty\">No rows.</p>";
    return;
  }
  const columns = preferred.filter(col => col in rows[0]);
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
  document.getElementById("load-status").textContent = message;
}

function number(value) {
  const parsed = Number(value);
  return Number.isFinite(parsed) ? parsed : NaN;
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
