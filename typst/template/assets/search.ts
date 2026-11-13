/* A search palette over the Pagefind index, opened with Cmd-K or Ctrl-K, or
 * with `/` when not typing in a field. It loads Pagefind's runtime from
 * /pagefind/ the first time it opens, so pages that never search never load
 * the runtime or the index. If the runtime is missing, the palette says so.
 *
 * Result rows rebuild the page's chips from Pagefind's filters: the taxon from
 * `taxon`, and each tag from a filter named `tag-<hue>` (see the template's
 * `pagefind-attrs`). The excerpt's <mark> highlights are Pagefind's. */

const MAX_RESULTS = 12;
const DEBOUNCE_MS = 120;

// The part of Pagefind's interface this file uses.

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
        // Pagefind writes this when it indexes the site; esbuild leaves the
        // import alone (--external:/pagefind/*).
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

    const meta = document.createElement("span");
    meta.className = "search-result-meta";
    if (taxon !== undefined) {
      meta.append(chip("node-taxon", `taxon-${taxon}`, taxon));
    }
    for (const [name, tags] of Object.entries(fragment.filters)) {
      if (!name.startsWith("tag-")) continue;
      for (const tag of tags) {
        meta.append(chip("node-tag", `hue-${name.slice("tag-".length)}`, tag));
      }
    }

    const title = document.createElement("span");
    title.className = "search-result-title";
    title.textContent = fragment.meta.title ?? fragment.url;

    const excerpt = document.createElement("span");
    excerpt.className = "search-result-excerpt";
    // Pagefind's markup, from the build; it carries the <mark> highlights.
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
        // Otherwise the search input's own Escape, which clears it, wins.
        event.preventDefault();
        dialog?.close();
        break;
    }
  });

  // A click on the dialog itself, outside the panel, is on the backdrop.
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

function open(): void {
  const element = ensureDialog();
  if (element.open) return;
  element.showModal();
  input.focus();
  void loadPagefind();
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
}
