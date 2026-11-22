import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

const COLOR_SCHEME_KEY = "color-scheme";
const darkSchemeQuery = window.matchMedia("(prefers-color-scheme: dark)");

function readStoredColorScheme() {
  try {
    const value = localStorage.getItem(COLOR_SCHEME_KEY);
    return value === "light" || value === "dark" ? value : null;
  } catch {
    return null;
  }
}

let colorSchemeOverride = readStoredColorScheme();

function applyColorScheme() {
  const meta = document.querySelector('meta[name="color-scheme"]');
  if (meta) {
    meta.content = colorSchemeOverride ?? "light dark";
  }
  const isDark =
    colorSchemeOverride === "dark" ||
    (colorSchemeOverride !== "light" && darkSchemeQuery.matches);
  document.documentElement.dataset.theme = isDark ? "dark" : "light";
}

applyColorScheme();
darkSchemeQuery.addEventListener("change", applyColorScheme);
window.addEventListener("storage", (e) => {
  if (e.key === COLOR_SCHEME_KEY || e.key === null) {
    colorSchemeOverride = readStoredColorScheme();
    applyColorScheme();
  }
});

document.addEventListener("click", (e) => {
  if (!e.target.closest("#theme-toggle")) {
    return;
  }
  const systemDark = darkSchemeQuery.matches;
  const currentlyDark =
    colorSchemeOverride === "dark" ||
    (colorSchemeOverride !== "light" && systemDark);
  const target = currentlyDark ? "light" : "dark";
  const matchesSystem = target === (systemDark ? "dark" : "light");

  colorSchemeOverride = matchesSystem ? null : target;
  try {
    if (matchesSystem) {
      localStorage.removeItem(COLOR_SCHEME_KEY);
    } else {
      localStorage.setItem(COLOR_SCHEME_KEY, target);
    }
  } catch {}
  applyColorScheme();
});

const args = [];
const env = ["GHCRTS=-H64m"];
const fds = [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
];
const wasi = new WASI(args, env, fds, { debug: false });

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("./app.wasm"), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
const loadingEl = document.getElementById("loading");
if (loadingEl) {
  loadingEl.remove();
}
await instance.exports.hs_start();
