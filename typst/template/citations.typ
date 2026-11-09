#let citation-style-xml = read("chicago-author-date.csl")

// Hack: we want to avoid rendering extra parentheses in contexts like "Theorem
// ((Rijke 2025))", so we patch the CSL manually here.
#let bare-citation-xml = {
  let layout = "<layout delimiter=\"; \" prefix=\"(\" suffix=\")\">"
  let bare = citation-style-xml.replace(layout, "<layout delimiter=\"; \">")
  if bare == citation-style-xml {
    panic("bare CSL derivation failed: citation layout not found in chicago-author-date-csl input")
  }
  bare
}

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

#let _native-form(form) = if form == none or form == "bare" { "normal" } else { form }

// Rendering API

#let cite-group(group) = group.map(item => cite(
  label(item.key),
  form: _native-form(item.form),
  style: if item.form == "bare" { bytes(bare-citation-xml) } else { auto },
  supplement: _supplement(item.locator, item.locator-label),
)).join()

#let references(bib) = bibliography(
  bib,
  style: bytes(citation-style-xml),
  title: none,
)
