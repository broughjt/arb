#import "node.typ": node-href

// Citation rendering uses Typst's native `cite`/`bibliography` (hayagriva is
// compiled into Typst), not an external plugin. The authoring helpers below
// only emit Weibian link markers so the node graph records citation edges; the
// visible citation text and the per-page bibliography are produced natively in
// `render-citation`/`render-references`.

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

// Render a citation group (from a marker's `group` metadata) as native Typst
// citations. A lone citation links to its reference node's page (the native
// in-page bibliography link is flattened away so there is no nested <a>); a
// multi-item cluster renders as one grouped CSL run (plain text, matching the
// previous behavior — graph edges to each reference still exist).
#let render-citation(group) = {
  let render-item(item) = cite(
    label(item.key),
    form: _native-form(item.form),
    style: if item.form == "bare" { bytes(bare-citation-xml) } else { auto },
    supplement: _supplement(item.locator, item.locator-label),
  )

  if group.len() == 1 {
    let item = group.first()
    // Emit the citation as an `a.node-link` (not a bare `link(...)`) so it
    // picks up the link color + hover wash from 06-links.css, matching other
    // node links. The inner `show link` flattens the native in-page
    // bibliography link so there is no nested <a>.
    html.elem("a", attrs: (href: node-href(item.key), class: "node-link"))[
      #show link: inner => inner.body
      #render-item(item)
    ]
  } else {
    show link: inner => inner.body
    group.map(render-item).join()
  }
}

// The per-page "References" bibliography. Native `bibliography` with the default
// `target: auto` scopes each page's bibliography to the citations in that page's
// document (including citations pulled in through transclusion). `references` is
// accepted for signature compatibility / the emit gate in `render-page`; the
// native bibliography detects its own entries.
#let render-references(references, bib) = {
  html.section(class: "backmatter-section")[
    #html.h3(class: "backmatter-section-title")[References]
    #bibliography(
      bib,
      style: bytes(citation-style-xml),
      title: none,
    )
  ]
}
