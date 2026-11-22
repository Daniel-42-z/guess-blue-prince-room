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

const ownedInterestTriggers = new WeakSet();

function normalizeHintText(text) {
  return (text || "").replace(/\s+/g, " ").trim();
}

function getAccessibleName(el) {
  const labelledBy = el.getAttribute("aria-labelledby");
  if (labelledBy) {
    return normalizeHintText(
      labelledBy
        .split(/\s+/)
        .map((id) => document.getElementById(id)?.textContent || "")
        .join(" ")
    );
  }
  const ariaLabel = el.getAttribute("aria-label");
  if (ariaLabel) {
    return normalizeHintText(ariaLabel);
  }
  const textWithAlts = Array.from(el.childNodes)
    .map((node) => {
      if (node.nodeType === Node.TEXT_NODE) return node.textContent || "";
      if (node.nodeType === Node.ELEMENT_NODE) {
        if (node.tagName === "IMG") return node.getAttribute("alt") || "";
        return node.textContent || "";
      }
      return "";
    })
    .join(" ");
  const normalizedText = normalizeHintText(textWithAlts);
  if (normalizedText) return normalizedText;
  return normalizeHintText(el.getAttribute("title"));
}

function isPopoverOpen(el) {
  try {
    return el.matches(":popover-open") || el.classList.contains(":popover-open");
  } catch {
    return el.classList.contains(":popover-open");
  }
}

function wireInterestHints(root = document) {
  const triggers = root.querySelectorAll?.("[interestfor]") || [];
  for (const trigger of triggers) {
    const targetId = trigger.getAttribute("interestfor");
    if (!targetId) continue;
    const hint = document.getElementById(targetId);
    if (!hint || hint.getAttribute("popover") !== "hint") continue;

    const hintText = normalizeHintText(hint.textContent);
    const triggerName = getAccessibleName(trigger);
    if (hintText && triggerName === hintText) continue;

    if (!hint.hasAttribute("role")) {
      hint.setAttribute("role", "tooltip");
    }

    if (!triggerName) {
      if (!trigger.hasAttribute("aria-labelledby")) {
        trigger.setAttribute("aria-labelledby", targetId);
      }
    } else {
      const activatesHint =
        trigger.getAttribute("popovertarget") === targetId ||
        trigger.getAttribute("commandfor") === targetId;
      if (activatesHint) {
        if (!trigger.hasAttribute("aria-expanded") || ownedInterestTriggers.has(trigger)) {
          ownedInterestTriggers.add(trigger);
          trigger.setAttribute("aria-expanded", String(isPopoverOpen(hint)));
        }
      } else if (!trigger.hasAttribute("aria-describedby")) {
        trigger.setAttribute("aria-describedby", targetId);
      }
    }
  }
}

document.addEventListener(
  "toggle",
  ({ target, newState }) => {
    if (!(target instanceof HTMLElement) || !target.id) return;
    const open = newState === "open";
    const triggers = document.querySelectorAll(`[interestfor="${CSS.escape(target.id)}"]`);
    for (const trigger of triggers) {
      if (ownedInterestTriggers.has(trigger)) {
        trigger.setAttribute("aria-expanded", String(open));
      }
    }
  },
  true
);

wireInterestHints();
new MutationObserver(() => wireInterestHints()).observe(document.body, {
  childList: true,
  subtree: true,
});

if (!Object.hasOwn(HTMLElement.prototype, "popover")) {
  await import("https://unpkg.com/@oddbird/popover-polyfill@latest/dist/popover.min.js");
}

const hintProbe = document.createElement("div");
hintProbe.popover = "hint";
if (hintProbe.popover !== "hint") {
  document.addEventListener(
    "beforetoggle",
    ({ target, newState }) => {
      const toggletip =
        target instanceof HTMLElement &&
        target.getAttribute("popover") === "hint" &&
        document.querySelector(`[popovertarget="${CSS.escape(target.id)}"]`);
      if (newState === "open" && toggletip) {
        target.popover = "auto";
      }
    },
    true
  );
}

if (!("interestForElement" in HTMLButtonElement.prototype)) {
  await import("https://unpkg.com/interestfor@latest");
}

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
