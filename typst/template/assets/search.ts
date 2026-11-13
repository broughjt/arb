/* Command-palette search over the pagefind index.
 *
 * Progressive enhancement: opened with ⌘K / Ctrl-K (or `/` when not typing),
 * it lazy-loads the static pagefind runtime from /pagefind/ the first time it
 * opens, so the WASM + index stay off every page load. The index ships with the
 * build, so — unlike the popover island — this does NOT depend on the dev watch
 * backend (`data-weibian-connection`); it works in production from static files.
 * If the runtime is absent (e.g. a stale build with no /pagefind/), the palette
 * opens and reports that quietly rather than erroring.
 *
 * The result rows reconstruct the site's own chips from pagefind metadata: the
 * taxon badge (`filters.taxon`) and tag chips (`filters.tag`, hued by the same
 * deterministic hash as the template) reuse the `taxon-*` / `hue-*` classes, so
 * results look native. The excerpt's <mark> highlights come straight from
 * pagefind. */

const MAX_RESULTS = 12;
const DEBOUNCE_MS = 120;

// --- pagefind runtime (minimal surface we use) ---------------------------------

interface PagefindFragment {
  url: string;
  excerpt: string;
  meta: Record<string, string>;
  filters: Record<string, string[]>;
}
interface PagefindResult {
  id: string;
  data: () => Promise<PagefindFragment>;
}
interface Pagefind {
  init?: () => Promise<void>;
  search: (query: string) => Promise<{ results: PagefindResult[] }>;
}

let pagefindPromise: Promise<Pagefind | null> | undefined;

function loadPagefind(): Promise<Pagefind | null> {
  if (pagefindPromise === undefined) {
    pagefindPromise = (async () => {
      try {
        // Absolute path, kept external by esbuild (--external:/pagefind/*): the
        // pagefind crawl emits this at build time; it is not part of our bundle.
        const module = (await import("/pagefind/pagefind.js")) as Pagefind;
        await module.init?.();
        return module;
      } catch {
        return null;
      }
    })();
  }
  return pagefindPromise;
}

// --- the same tag-hue hash the template uses (node-metadata.typ) ---------------

const HUES = ["red", "yellow", "green", "cyan", "blue", "magenta"];

function tagHue(tag: string): string {
  let hash = 0;
  for (const character of tag) {
    hash = (hash * 31 + (character.codePointAt(0) ?? 0)) % 997;
  }
  return HUES[hash % 6];
}

// --- palette UI ----------------------------------------------------------------

let dialog: HTMLDialogElement | undefined;
let input: HTMLInputElement;
let list: HTMLUListElement;
let status: HTMLParagraphElement;

let fragments: PagefindFragment[] = [];
let activeIndex = -1;
let debounceTimer: number | undefined;
let querySeq = 0;

function chip(kind: "node-taxon" | "node-tag", modifier: string, text: string): HTMLElement {
  const span = document.createElement("span");
  span.className = `${kind} ${modifier}`;
  span.textContent = text;
  return span;
}

function renderResults(): void {
  list.replaceChildren();
  for (const [index, fragment] of fragments.entries()) {
    const taxon = fragment.filters.taxon?.[0];
    const tags = fragment.filters.tag ?? [];

    const meta = document.createElement("span");
    meta.className = "search-result-meta";
    if (taxon !== undefined) {
      meta.append(chip("node-taxon", `taxon-${taxon}`, taxon));
    }
    for (const tag of tags) {
      meta.append(chip("node-tag", `hue-${tagHue(tag)}`, tag));
    }

    const title = document.createElement("span");
    title.className = "search-result-title";
    title.textContent = fragment.meta.title ?? fragment.url;

    const excerpt = document.createElement("span");
    excerpt.className = "search-result-excerpt";
    // pagefind-generated markup (trusted build output); carries <mark> spans.
    excerpt.innerHTML = fragment.excerpt;

    const item = document.createElement("li");
    item.className = "search-result";
    item.id = `search-result-${index}`;
    item.setAttribute("role", "option");
    item.dataset.url = fragment.url;
    item.append(title, meta, excerpt);
    item.addEventListener("click", () => navigate(index));
    item.addEventListener("mousemove", () => setActive(index));
    list.append(item);
  }

  setActive(fragments.length > 0 ? 0 : -1);
  status.textContent =
    input.value.trim() === ""
      ? ""
      : `${fragments.length} result${fragments.length === 1 ? "" : "s"}`;
  input.setAttribute("aria-expanded", fragments.length > 0 ? "true" : "false");
}

function setActive(index: number): void {
  activeIndex = index;
  const options = list.children;
  for (let i = 0; i < options.length; i++) {
    const option = options[i] as HTMLElement;
    const isActive = i === index;
    option.classList.toggle("is-active", isActive);
    option.setAttribute("aria-selected", isActive ? "true" : "false");
  }
  if (index >= 0) {
    const option = options[index] as HTMLElement;
    input.setAttribute("aria-activedescendant", option.id);
    option.scrollIntoView({ block: "nearest" });
  } else {
    input.removeAttribute("aria-activedescendant");
  }
}

function move(delta: number): void {
  if (fragments.length === 0) return;
  const next = (activeIndex + delta + fragments.length) % fragments.length;
  setActive(next);
}

function navigate(index: number): void {
  const fragment = fragments[index];
  if (fragment !== undefined) window.location.assign(fragment.url);
}

async function runSearch(query: string): Promise<void> {
  const sequence = ++querySeq;
  if (query === "") {
    fragments = [];
    renderResults();
    return;
  }

  const pagefind = await loadPagefind();
  if (sequence !== querySeq) return; // superseded while loading
  if (pagefind === null) {
    fragments = [];
    list.replaceChildren();
    status.textContent = "Search index unavailable.";
    return;
  }

  const search = await pagefind.search(query);
  if (sequence !== querySeq) return;
  const data = await Promise.all(
    search.results.slice(0, MAX_RESULTS).map((result) => result.data()),
  );
  if (sequence !== querySeq) return;

  fragments = data;
  renderResults();
}

function onInput(): void {
  window.clearTimeout(debounceTimer);
  const query = input.value.trim();
  debounceTimer = window.setTimeout(() => void runSearch(query), DEBOUNCE_MS);
}

function ensureDialog(): HTMLDialogElement {
  if (dialog !== undefined) return dialog;

  dialog = document.createElement("dialog");
  dialog.className = "search-dialog";
  dialog.setAttribute("aria-label", "Search notes");

  input = document.createElement("input");
  input.className = "search-input";
  input.type = "search";
  input.placeholder = "Search notes…";
  input.setAttribute("role", "combobox");
  input.setAttribute("aria-autocomplete", "list");
  input.setAttribute("aria-expanded", "false");
  input.setAttribute("aria-label", "Search notes");
  input.setAttribute("autocomplete", "off");
  input.spellcheck = false;

  list = document.createElement("ul");
  list.className = "search-results";
  list.id = "search-results";
  list.setAttribute("role", "listbox");
  input.setAttribute("aria-controls", list.id);

  status = document.createElement("p");
  status.className = "search-status";
  status.setAttribute("aria-live", "polite");

  const icon = document.createElement("span");
  icon.className = "search-icon";
  icon.setAttribute("aria-hidden", "true");

  const field = document.createElement("div");
  field.className = "search-field";
  field.append(icon, input);

  const panel = document.createElement("div");
  panel.className = "search-panel";
  panel.append(field, status, list);
  dialog.append(panel);
  document.body.append(dialog);

  input.addEventListener("input", onInput);
  input.addEventListener("keydown", (event) => {
    switch (event.key) {
      case "ArrowDown":
        event.preventDefault();
        move(1);
        break;
      case "ArrowUp":
        event.preventDefault();
        move(-1);
        break;
      case "Enter":
        if (activeIndex >= 0) {
          event.preventDefault();
          navigate(activeIndex);
        }
        break;
      case "Escape":
        // Modal dialogs close on Escape natively; handle it explicitly so a
        // non-modal open (the #search dev affordance) closes too, and so the
        // browser's "clear the search input" default doesn't win.
        event.preventDefault();
        dialog?.close();
        break;
    }
  });

  // <dialog> gives us the focus trap, Esc-to-close, and background inerting;
  // add backdrop-click-to-close (a click landing on the dialog itself, i.e.
  // outside the panel).
  dialog.addEventListener("click", (event) => {
    if (event.target === dialog) dialog.close();
  });
  dialog.addEventListener("close", () => {
    input.value = "";
    fragments = [];
    list.replaceChildren();
    status.textContent = "";
    input.setAttribute("aria-expanded", "false");
  });

  return dialog;
}

function open(modal = true): void {
  const element = ensureDialog();
  if (element.open) return;
  // Keyboard opens modal (focus trap + background inerting); #search hash opens
  // non-modal so loading a shared URL does not trap focus on the page.
  if (modal) element.showModal();
  else element.show();
  input.focus();
  void loadPagefind(); // warm the runtime while the user types the first key
}

export function initSearch(): void {
  document.addEventListener("keydown", (event) => {
    const target = event.target;
    const typing =
      target instanceof HTMLElement &&
      (target.isContentEditable ||
        ["INPUT", "TEXTAREA", "SELECT"].includes(target.tagName));

    if ((event.metaKey || event.ctrlKey) && event.key.toLowerCase() === "k") {
      event.preventDefault();
      open();
    } else if (event.key === "/" && !typing && !(dialog?.open ?? false)) {
      event.preventDefault();
      open();
    }
  });

  // Hash affordance: #search opens the palette on load. Keep this non-modal so
  // following a plain URL fragment does not make the rest of the page inert.
  if (window.location.hash === "#search") open(false);
}
