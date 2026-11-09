// TODO: A lot of this stuff (most of it?) seems like template stuff, not
// something the authoring prelude needs

#let TAXA = (
  "axiom",
  "corollary",
  "definition",
  "example",
  "lemma",
  "list",
  "module",
  "note",
  "outline",
  "question",
  "reference",
  "theorem",
)

#let assert-taxon(taxon, label: "taxon") = {
  if type(taxon) != str {
    panic(label + " must be a string, got " + repr(taxon))
  }

  if taxon not in TAXA {
    panic("unknown " + label + " `" + taxon + "`; known taxa: " + TAXA.join(", "))
  }

  taxon
}

#let taxon-class(taxon) = "taxon-" + assert-taxon(taxon)
#let target-taxon-class(taxon) = "target-taxon-" + assert-taxon(taxon)

#let node-taxon(node) = {
  // TODO: Handle this by wrapping node and subnode from weibian to pass taxon:
  // note by default.
  // Then go back to old behavior of panicing if missing
  let taxon = node.metadata.at("taxon", default: "note")

  assert-taxon(taxon, label: "taxon for node " + node.id)
}

#let node-taxon-class(node) = taxon-class(node-taxon(node))
#let node-target-taxon-class(node) = target-taxon-class(node-taxon(node))
#let node-is-reference(node) = node-taxon(node) == "reference"

#let node-tags(node) = {
  let tags = node.metadata.at("tags", default: ())

  assert(type(tags) == array)
  for tag in tags {
    assert(type(tag) == str)
  }

  tags
}

// TODO: Remove if there is ever anything in Typst standard library content that
// does this better.

// Flatten title content to a plain string. Needed for contexts that require
// character data and reject child elements, e.g. the `<title>` raw-text element.
#let content-to-text(value) = {
  if value == none {
    ""
  } else if type(value) == str {
    value
  } else if type(value) != content {
    str(value)
  } else {
    let f = value.func()
    if f == text or f == raw {
      value.text
    } else if f == [ ].func() or f == linebreak {
      " "
    } else if value.has("children") {
      value.children.map(content-to-text).join()
    } else if value.has("body") {
      content-to-text(value.body)
    } else if value.has("text") {
      value.text
    } else {
      ""
    }
  }
}

// TODO: This seems like template stuff, not something the authoring prelude needs
#let node-title-text(node) = content-to-text(node.title)

#let node-href(id) = id + ".html"

#let node-link(target, body: none, extra-classes: ()) = {
  html.elem("a", attrs: (
    href: node-href(target.id),
    class: ((
      "node-link",
      "target-taxon",
      node-target-taxon-class(target),
    ) + extra-classes).join(" "),
  ))[#if body == none { target.title } else { body }]
}
