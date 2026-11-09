#import "@local/arb-core:0.1.0": link-node

// Normalize one citation item -- a reference key string, or a `(key: ..)` dict
// optionally carrying `form` / `locator` / `locator-label` -- into the canonical
// item dict the rest of the citation code consumes. Shared by `cite-nodes`
// (prose) and `library/math.typ`'s theorem-head `citation-items`, so the two
// surfaces cannot drift in how a citation spec is read.
#let normalize-citation-item(argument) = {
  let item = if type(argument) == str {
    (key: argument)
  } else if type(argument) == dictionary and "key" in argument {
    argument
  } else {
    panic("citation items must be reference keys or dictionaries with a `key`")
  }

  (
    key: item.key,
    form: item.at("form", default: none),
    locator: item.at("locator", default: none),
    locator-label: item.at("locator-label", default: "page"),
  )
}

// Authoring API ---------------------------------------------------------------

// A citation cluster. Emits one Weibian link marker per cited key (so each
// reference gets a graph edge / backlink) carrying the ordered group so that
// `render-citation` can render the whole cluster natively at the first marker.
#let cite-nodes(..arguments) = {
  if arguments.named().len() > 0 {
    panic("cite-nodes accepts only positional citation items")
  }

  let items = arguments.pos().map(normalize-citation-item)

  items
    .enumerate()
    .map(((index, item)) => link-node(
      item.key,
      citation: true,
      group: items,
      group-index: index,
    ))
    .join()
}

// A single citation. One-item wrapper over `cite-nodes`.
//
// The locator-spec is the optional second positional argument: a plain string
// (a bare page locator), or a dict from the `at` namespace
// (`at.definition("1.1.9")`), or omitted. This is unambiguous because clusters
// live on `cite-nodes` and a single citation has no body, so nothing else
// competes for the slot. The `form` is filled in by the `cite-node-<form>`
// family below.
#let cite-node-with(id, ..rest, form: none) = {
  if rest.named().len() > 0 {
    panic("cite-node accepts only an id and an optional locator-spec positionally")
  }
  let positional = rest.pos()
  if positional.len() > 1 {
    panic("cite-node accepts at most one locator-spec after the id")
  }

  let spec = positional.at(0, default: none)
  let loc = if spec == none {
    (:)
  } else if type(spec) == str {
    (locator: spec, locator-label: "page")
  } else {
    spec
  }

  cite-nodes((key: id, form: form, ..loc))
}

// The form family. The abbreviation is the canonical name with the form word
// truncated to its first letter; the hyphen keeps it clear of the plural
// `cite-nodes`. `form` values are Typst's native `CitationForm` (normal / prose
// / author / year / full); `bare` is repo-local (normal with paren-stripped CSL).
#let cite-node = cite-node-with                          // normal / parenthetical
#let cite-node-prose = cite-node-with.with(form: "prose")
#let cite-node-author = cite-node-with.with(form: "author")
#let cite-node-bare = cite-node-with.with(form: "bare")
#let cite-node-year = cite-node-with.with(form: "year")
#let cite-node-full = cite-node-with.with(form: "full")

#let cite-node-p = cite-node-prose
#let cite-node-a = cite-node-author
#let cite-node-b = cite-node-bare
#let cite-node-y = cite-node-year
#let cite-node-f = cite-node-full
