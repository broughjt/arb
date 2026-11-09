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

#let node-title-text(node) = content-to-text(node.title)

#let node-href(id) = id + ".html"

// `target` is a template node.
#let node-link(target, body: none, extra-classes: ()) = {
  html.elem("a", attrs: (
    href: node-href(target.id),
    class: ((
      "node-link",
      "target-taxon",
      "target-taxon-" + target.taxon,
    ) + extra-classes).join(" "),
  ))[#if body == none { target.title } else { body }]
}
