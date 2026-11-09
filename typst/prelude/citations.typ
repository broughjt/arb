#import "@local/arb-core:0.1.0": link-node

#let locator-labels = (
  "page",
  "theorem",
  "proposition",
  "lemma",
  "corollary",
  "definition",
  "example",
  "remark",
  "exercise",
  "appendix",
  "chapter",
  "section",
  "equation",
  "figure",
  "table",
)

#let normalize-citation-item(argument) = {
  let item = if type(argument) == str {
    (key: argument)
  } else if type(argument) == dictionary and "key" in argument {
    argument
  } else {
    panic("citation items must be reference keys or dictionaries with a `key`")
  }

  let labels = ()
  for field in item.keys() {
    if field in ("key", "form") {
      continue
    }
    if field not in locator-labels {
      panic(
        "unknown citation field `" + field + "`; the locators are "
          + locator-labels.join(", "),
      )
    }
    labels.push(field)
  }
  if labels.len() > 1 {
    panic("a citation takes at most one locator, not " + labels.join(" and "))
  }
  let label = labels.at(0, default: none)

  (
    key: item.key,
    form: item.at("form", default: none),
    locator: if label == none { none } else { item.at(label) },
    locator-label: label,
  )
}

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

// Takes an optional position argument for the page, otherwise locators are
// specified with a keyword argument.
#let cite-node-with(id, ..rest, form: none) = {
  let positional = rest.pos()
  if positional.len() > 1 {
    panic("cite-node accepts at most one page after the id")
  }

  let item = (key: id, form: form, ..rest.named())
  if positional.len() == 1 {
    if "page" in item {
      panic("cite-node accepts a page positionally or as `page:`, not both")
    }
    item.insert("page", positional.first())
  }

  cite-nodes(item)
}

#let cite-node = cite-node-with
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
