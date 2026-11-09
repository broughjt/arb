#import "@local/arb-core:0.1.0": node
#import "@local/notes:0.1.0": locator-abbreviations
#import "@preview/citegeist:0.3.1": load-bibliography

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

#let _supplement(locator, locator-label) = {
  if locator == none {
    return none
  }
  let abbreviation = locator-abbreviations.at(locator-label)
  if abbreviation == none {
    locator
  } else if abbreviation == "§" {
    [§#locator]
  } else {
    [#abbreviation #locator]
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

// Instead of source files with nodes for each reference, we inject them
// synthetically here from entries in the bibliography.
#let reference-sources(bib) = {
  let sources = ()
  for (key, entry) in load-bibliography(read(bib)) {
    sources.push((
      path: "references.bib",
      content: node(
        key,
        entry.fields.at("shorttitle", default: entry.fields.title),
        [],
        taxon: "reference",
        tags: (entry.entry_type,),
      ),
    ))
  }
  sources
}
