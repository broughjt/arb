// Citation rendering uses Typst's native `cite`/`bibliography` (hayagriva is
// compiled into Typst), not an external plugin. The authoring helpers below
// only emit Weibian link markers so the node graph records citation edges; the
// visible citation text and the per-page bibliography are produced natively in
// `cite-group`/`references`. This file renders no HTML; template.typ wraps
// what it returns.

// We use a CSL style file (fetched by arb's flake into this package) instead
// of the built-in "chicago-author-date" so that the bibliography style, in-text
// citations, and the derived bare theorem-head style all share one XML.
#let citation-style-xml = read("chicago-author-date.csl")

// Bare citations are normal author-date citations without the enclosing
// parentheses. Theorem heads supply their own parens (see math.typ / CSS), so
// the CSL layout parentheses must be stripped.
#let bare-citation-xml = {
  let layout = "<layout delimiter=\"; \" prefix=\"(\" suffix=\")\">"
  let bare = citation-style-xml.replace(layout, "<layout delimiter=\"; \">")
  if bare == citation-style-xml {
    panic("bare CSL derivation failed: citation layout not found in chicago-author-date-csl input")
  }
  bare
}

// The abbreviation of each locator label in the prelude's `locator-labels`,
// or `none` to print the locator bare.
#let _locator-labels = (
  page: none,
  theorem: "thm.",
  proposition: "prop.",
  lemma: "lem.",
  corollary: "cor.",
  definition: "def.",
  example: "ex.",
  remark: "rem.",
  exercise: "exer.",
  appendix: "appx.",
  chapter: "ch.",
  section: "§",
  equation: "eq.",
  figure: "fig.",
  table: "table",
)

// Build a native `cite` supplement (page/locator) from our locator policy.
#let _supplement(locator, locator-label) = {
  if locator == none {
    none
  } else {
    if locator-label not in _locator-labels {
      panic(
        "unknown locator-label `" + locator-label + "`; known labels: "
          + _locator-labels.keys().join(", "),
      )
    }
    let label = _locator-labels.at(locator-label)
    if label == none { locator } else if label == "§" { [#label#locator] } else { [#label #locator] }
  }
}

// Map our citation `form` onto a native `cite` form. "bare" is not a native
// form; it is a normal citation rendered with the paren-stripped CSL.
#let _native-form(form) = if form == none or form == "bare" { "normal" } else { form }

// Rendering API

// The native citations for a citation group (from a marker's `group`
// metadata). A multi-item cluster renders as one grouped CSL run.
#let cite-group(group) = group.map(item => cite(
  label(item.key),
  form: _native-form(item.form),
  style: if item.form == "bare" { bytes(bare-citation-xml) } else { auto },
  supplement: _supplement(item.locator, item.locator-label),
)).join()

// The per-page bibliography. Native `bibliography` with the default
// `target: auto` scopes each page's bibliography to the citations in that
// page's document (including citations pulled in through transclusion).
#let references(bib) = bibliography(
  bib,
  style: bytes(citation-style-xml),
  title: none,
)
