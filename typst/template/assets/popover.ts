/* Cards shown on hovering or focusing a link. There are two kinds, which share
 * one card, the timers, the positioning and the dismissal, so that at most one
 * card is on screen:
 *
 * - Node previews: an `a.node-link` to another page shows that page's header
 *   and mainmatter, fetched once and cloned with their classes, so that the
 *   page's stylesheet styles them.
 * - Agda types: an identifier in an Agda block (`pre.Agda`, from the
 *   template's agda.typ) shows its definition's type, from the module's
 *   `types/<Module>.json`, which agda-annotate writes.
 */

const SHOW_DELAY_MS = 200;
const HIDE_DELAY_MS = 200;
const VIEWPORT_MARGIN_PX = 12;
const LINK_GAP_PX = 6;

const cache = new Map<string, Promise<HTMLElement | null>>();

let popover: HTMLElement | undefined;
let activeLink: HTMLElement | undefined;
let showTimer: number | undefined;
let hideTimer: number | undefined;

async function loadPreview(url: URL): Promise<HTMLElement | null> {
  const response = await fetch(url.pathname);
  if (!response.ok) return null;
  const page = new DOMParser().parseFromString(
    await response.text(),
    "text/html",
  );
  const article = page.querySelector("article.node");
  const mainmatter = article?.querySelector("section.node-mainmatter");
  if (!article || !mainmatter) return null;

  const content = document.createElement("div");
  content.className = "popover-content";
  // The stylesheet hides the header's metadata in the card.
  const header = article.querySelector("header.node-header");
  if (header !== null) content.append(header.cloneNode(true));
  content.append(mainmatter.cloneNode(true));
  // The page may have the same ids (headings, and the `g...` glyph symbols in
  // Typst's inline SVGs), so prefix the clone's. Its references must be
  // prefixed with them: a diagram draws each glyph with `<use href="#g...">`,
  // and a dangling one draws nothing.
  prefixIds(content, "popover-");
  return content;
}

/* Namespace every id in a cloned subtree and rewrite the `href` and
 * `xlink:href` references to those ids so they keep resolving inside the
 * clone. */
function prefixIds(root: ParentNode, prefix: string): void {
  const ids = new Set<string>();
  for (const element of root.querySelectorAll("[id]")) {
    ids.add(element.id);
    element.id = `${prefix}${element.id}`;
  }
  if (ids.size === 0) return;

  const rewriteHash = (value: string | null): string | null => {
    if (value === null || !value.startsWith("#")) return value;
    const id = value.slice(1);
    return ids.has(id) ? `#${prefix}${id}` : value;
  };

  for (const element of root.querySelectorAll("*")) {
    const href = rewriteHash(element.getAttribute("href"));
    if (href !== null) element.setAttribute("href", href);
    const xlink = rewriteHash(element.getAttribute("xlink:href"));
    if (xlink !== null) element.setAttribute("xlink:href", xlink);
  }
}

function cachedPreview(url: URL): Promise<HTMLElement | null> {
  let preview = cache.get(url.pathname);
  if (preview === undefined) {
    preview = loadPreview(url).catch(() => null);
    cache.set(url.pathname, preview);
  }
  return preview;
}

/* A preview kind pairs an eligibility test with a content loader; the card,
 * timers, positioning, and dismissal stay shared across kinds. Triggers are
 * elements, not necessarily anchors: builtin Agda identifiers hover from
 * `span[data-agda-def]`. */
interface PreviewKind {
  /* Toggled on the card so the stylesheet can restyle it per kind. */
  cardClass?: string;
  matches(trigger: HTMLElement): boolean;
  /* The card's contents, or null for none. */
  load(trigger: HTMLElement): Promise<HTMLElement | null>;
  /* Runs after the filled card is positioned. */
  shown?(card: HTMLElement, trigger: HTMLElement): void;
}

function nodePreviewUrl(trigger: HTMLElement): URL {
  return new URL((trigger as HTMLAnchorElement).href);
}

const nodePreviewKind: PreviewKind = {
  matches(trigger) {
    if (!(trigger instanceof HTMLAnchorElement)) return false;
    if (!trigger.classList.contains("node-link")) return false;
    const url = new URL(trigger.href);
    if (url.origin !== window.location.origin) return false;
    // A self-link (the frontmatter address chip, in-prose self-references)
    // would preview the page already being read.
    return !sameDocument(url, currentNodeUrl());
  },
  async load(trigger) {
    const content = await cachedPreview(nodePreviewUrl(trigger));
    if (content === null) return null;
    const shown = content.cloneNode(true) as HTMLElement;
    // Previews collapse transclusions whatever their authored state; the links
    // in the summaries still work.
    for (const details of shown.querySelectorAll(".transclusion-details[open]")) {
      details.removeAttribute("open");
    }
    return shown;
  },
  shown(card, trigger) {
    const url = nodePreviewUrl(trigger);
    if (url.hash === "") return;
    const fragment = card.querySelector(
      `#popover-${CSS.escape(url.hash.slice(1))}`,
    );
    if (fragment !== null) {
      card.scrollTop =
        fragment.getBoundingClientRect().top -
        card.getBoundingClientRect().top;
    }
  },
};

/* Agda types. An identifier's definition site is `<module>-<offset>`, the
 * anchor scheme agda.typ shares with `agda --html`, and `types/<Module>.json`
 * maps offsets to types. A link to a module has no fragment, so it never
 * reaches the loader. */
interface AgdaTypeEntry {
  name: string;
  type: string;
}

const agdaTypesCache = new Map<
  string,
  Promise<Record<string, AgdaTypeEntry> | null>
>();

function cachedAgdaTypes(
  module: string,
): Promise<Record<string, AgdaTypeEntry> | null> {
  let types = agdaTypesCache.get(module);
  if (types === undefined) {
    types = fetch(`types/${encodeURIComponent(module)}.json`)
      .then((response) => (response.ok ? response.json() : null))
      .catch(() => null);
    agdaTypesCache.set(module, types);
  }
  return types;
}

/* Offsets are 1-based character offsets, unique only within their module.
 * Module names can themselves contain hyphens, so the offset is the last
 * all-digit suffix. */
function parseAgdaAnchor(
  hash: string,
): { module: string; position: string } | null {
  const match = /^#(.+)-(\d+)$/.exec(hash);
  return match === null ? null : { module: match[1], position: match[2] };
}

/* The definition site an Agda identifier names: the fragment of its link, or,
 * for one defined in a module with no page, its `data-agda-def`. */
function agdaDefSite(
  trigger: HTMLElement,
): { module: string; position: string } | null {
  const data = trigger.dataset.agdaDef;
  if (data !== undefined) return parseAgdaAnchor(`#${data}`);
  if (trigger instanceof HTMLAnchorElement) {
    return parseAgdaAnchor(new URL(trigger.href).hash);
  }
  return null;
}

/* A type is one string. Split it so that the stylesheet can wrap it: at each
 * top-level arrow into `agda-type-segment` spans, which a line may break
 * between, and each bracketed argument into an `agda-type-group` span, which
 * a line should not break inside. Unbalanced brackets leave plain text. */
const AGDA_OPEN_BRACKETS = new Set(["(", "{", "[", "⦃"]);
const AGDA_CLOSE_BRACKETS = new Set([")", "}", "]", "⦄"]);

function appendAgdaSegment(parent: HTMLElement, text: string): void {
  const segment = document.createElement("span");
  segment.className = "agda-type-segment";
  let depth = 0;
  let runStart = 0;
  for (let i = 0; i < text.length; i++) {
    if (AGDA_OPEN_BRACKETS.has(text[i])) {
      if (depth === 0) {
        if (i > runStart) segment.append(text.slice(runStart, i));
        runStart = i;
      }
      depth++;
    } else if (AGDA_CLOSE_BRACKETS.has(text[i])) {
      if (depth > 0) depth--;
      if (depth === 0) {
        const group = document.createElement("span");
        group.className = "agda-type-group";
        group.textContent = text.slice(runStart, i + 1);
        segment.append(group);
        runStart = i + 1;
      }
    }
  }
  if (runStart < text.length) segment.append(text.slice(runStart));
  parent.append(segment);
}

function renderAgdaType(rawType: string): HTMLElement {
  // agda-annotate breaks long types across lines; the card wraps them itself.
  const type = rawType.replace(/\s+/g, " ").trim();
  const body = document.createElement("span");
  body.className = "agda-type-body";
  let depth = 0;
  let start = 0;
  for (let i = 0; i < type.length; i++) {
    if (AGDA_OPEN_BRACKETS.has(type[i])) depth++;
    else if (AGDA_CLOSE_BRACKETS.has(type[i])) depth = Math.max(0, depth - 1);
    else if (
      depth === 0 &&
      type[i] === "→" &&
      type[i - 1] === " " &&
      type[i + 1] === " "
    ) {
      // The arrow begins the next segment, so a wrapped line starts with it.
      appendAgdaSegment(body, type.slice(start, i - 1));
      body.append(" ");
      start = i;
    }
  }
  appendAgdaSegment(body, type.slice(start));
  return body;
}

/* The stylesheet sizes the card to its contents on one line, clamped to a
 * maximum width. Once the type wraps, its lines can be much narrower than
 * that, and CSS cannot size a box to its wrapped lines, so measure the widest
 * line of text and narrow the card to it. */
function shrinkToWidestLine(card: HTMLElement): void {
  const walker = document.createTreeWalker(card, NodeFilter.SHOW_TEXT);
  const range = document.createRange();
  let widest = 0;
  for (let node = walker.nextNode(); node !== null; node = walker.nextNode()) {
    // The spaces between segments can hang at the right edge of a line.
    if (!/\S/.test(node.nodeValue ?? "")) continue;
    range.selectNodeContents(node);
    for (const rect of range.getClientRects()) {
      widest = Math.max(widest, rect.right);
    }
  }
  if (widest === 0) return;
  const style = getComputedStyle(card);
  const rect = card.getBoundingClientRect();
  const contentRight =
    rect.right -
    Number.parseFloat(style.borderRightWidth) -
    Number.parseFloat(style.paddingRight);
  const overhang = contentRight - widest;
  if (overhang <= 0) return;
  // One pixel more, so that rounding never wraps the widest line again.
  card.style.width = `${Math.ceil(rect.width - overhang) + 1}px`;
}

const agdaTypeKind: PreviewKind = {
  cardClass: "agda-type-popover",
  matches(trigger) {
    if (trigger.closest("pre.Agda") === null) return false;
    return agdaDefSite(trigger) !== null;
  },
  async load(trigger) {
    const site = agdaDefSite(trigger);
    if (site === null) return null;
    const types = await cachedAgdaTypes(site.module);
    const entry = types === null ? undefined : types[site.position];
    if (entry === undefined) return null;
    const signature = document.createElement("code");
    signature.className = "agda-type-signature";
    const name = document.createElement("span");
    name.className = "agda-type-name";
    name.textContent = entry.name;
    const colon = document.createElement("span");
    colon.className = "agda-type-colon";
    colon.textContent = " : ";
    signature.append(name, colon, renderAgdaType(entry.type));
    return signature;
  },
  shown(card, trigger) {
    shrinkToWidestLine(card);
    // The narrower card may fit differently.
    position(card, trigger);
  },
};

const PREVIEW_KINDS = [nodePreviewKind, agdaTypeKind];

function kindFor(trigger: HTMLElement): PreviewKind | null {
  for (const kind of PREVIEW_KINDS) {
    if (kind.matches(trigger)) return kind;
  }
  return null;
}

function getPopover(): HTMLElement {
  if (popover === undefined) {
    popover = document.createElement("aside");
    popover.className = "popover";
    // The card duplicates content reachable through the link itself; keep it
    // out of the accessibility tree.
    popover.setAttribute("aria-hidden", "true");
    popover.addEventListener("mouseenter", cancelHide);
    popover.addEventListener("mouseleave", scheduleHide);
    document.body.append(popover);
  }
  return popover;
}

/* Place the card below the link, unless it does not fit there and there is
 * more room above. Cap its height to the room on that side, so that it
 * scrolls instead of leaving the viewport and never covers the link, and
 * clamp it horizontally into the viewport. */
function position(card: HTMLElement, link: HTMLElement): void {
  const rect = link.getBoundingClientRect();
  const viewportWidth = document.documentElement.clientWidth;
  const viewportHeight = document.documentElement.clientHeight;

  // Measure the card's natural size under its stylesheet max-height alone
  // (clear any inline cap from a previous placement first).
  card.style.maxHeight = "";
  card.style.visibility = "hidden";
  card.classList.add("visible");
  const width = card.offsetWidth;
  const naturalHeight = card.offsetHeight;

  const spaceBelow =
    viewportHeight - rect.bottom - LINK_GAP_PX - VIEWPORT_MARGIN_PX;
  const spaceAbove = rect.top - LINK_GAP_PX - VIEWPORT_MARGIN_PX;

  // Flip: below unless it doesn't fit there and above has more room.
  const below = naturalHeight <= spaceBelow || spaceBelow >= spaceAbove;
  // The card is border-box, so the cap and offsetHeight measure the same box.
  const space = Math.max(below ? spaceBelow : spaceAbove, 0);
  if (naturalHeight > space) card.style.maxHeight = `${space}px`;
  const height = card.offsetHeight;

  const minLeft = window.scrollX + VIEWPORT_MARGIN_PX;
  const maxLeft = window.scrollX + viewportWidth - width - VIEWPORT_MARGIN_PX;
  const left = Math.max(minLeft, Math.min(rect.left + window.scrollX, maxLeft));

  const top = below
    ? rect.bottom + window.scrollY + LINK_GAP_PX
    : Math.max(
        rect.top + window.scrollY - height - LINK_GAP_PX,
        window.scrollY + VIEWPORT_MARGIN_PX,
      );

  card.style.left = `${left}px`;
  card.style.top = `${top}px`;
  card.style.visibility = "";
}

async function show(link: HTMLElement): Promise<void> {
  const kind = kindFor(link);
  if (kind === null) return;
  const content = await kind.load(link);
  // The pointer may have moved on while the fetch was in flight.
  if (content === null || activeLink !== link) return;

  const card = getPopover();
  // The card persists across shows; make its kind classes reflect this one.
  for (const each of PREVIEW_KINDS) {
    if (each.cardClass !== undefined) {
      card.classList.toggle(each.cardClass, each === kind);
    }
  }
  card.replaceChildren(content);
  // Clear any width-hug from a previous show; the stylesheet width applies
  // until this kind's shown() hook (if any) re-hugs.
  card.style.width = "";
  position(card, link);

  card.scrollTop = 0;
  kind.shown?.(card, link);
}

function hide(): void {
  cancelShow();
  cancelHide();
  activeLink = undefined;
  if (popover !== undefined) {
    popover.classList.remove("visible");
    popover.replaceChildren();
  }
}

function cancelShow(): void {
  window.clearTimeout(showTimer);
  showTimer = undefined;
}

function cancelHide(): void {
  window.clearTimeout(hideTimer);
  hideTimer = undefined;
}

function scheduleShow(link: HTMLElement): void {
  cancelShow();
  activeLink = link;
  showTimer = window.setTimeout(() => void show(link), SHOW_DELAY_MS);
}

function scheduleHide(): void {
  cancelHide();
  hideTimer = window.setTimeout(hide, HIDE_DELAY_MS);
}

function currentNodeUrl(): URL {
  const nodeId = document.body.dataset.nodeId;
  if (nodeId !== undefined) return new URL(`${nodeId}.html`, window.location.href);
  return new URL(window.location.href);
}

function sameDocument(left: URL, right: URL): boolean {
  return left.origin === right.origin && left.pathname === right.pathname;
}

/* The shared preview-eligibility test for every handler: returns the trigger
 * element under the event target when some kind previews it, or null. */
function linkFrom(target: EventTarget | null): HTMLElement | null {
  if (!(target instanceof Element)) return null;
  const link = target.closest<HTMLElement>("a[href], pre.Agda [data-agda-def]");
  if (link === null) return null;
  // Links inside the card are ordinary links; previewing them would replace
  // the card under the pointer.
  if (link.closest(".popover") !== null) return null;
  return kindFor(link) === null ? null : link;
}

export function initPopovers(): void {
  document.addEventListener("mouseover", (event) => {
    const link = linkFrom(event.target);
    if (link === null) return;
    cancelHide();
    if (link !== activeLink) scheduleShow(link);
  });

  document.addEventListener("mouseout", (event) => {
    const link = linkFrom(event.target);
    if (link === null) return;
    // Ignore moves between the link's own descendants.
    if (event.relatedTarget instanceof Node && link.contains(event.relatedTarget)) {
      return;
    }
    cancelShow();
    scheduleHide();
  });

  document.addEventListener("focusin", (event) => {
    const link = linkFrom(event.target);
    if (link === null) hide();
    else if (link !== activeLink) scheduleShow(link);
  });

  document.addEventListener("keydown", (event) => {
    if (event.key === "Escape") hide();
  });

  // Navigation replaces the page anyway; drop the card immediately.
  document.addEventListener("click", (event) => {
    if (linkFrom(event.target) !== null) hide();
  });
}
