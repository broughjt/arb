// Formal-result references: how body-less `link-node` and `link-nodes` markers
// to theorem-like nodes (lemma/theorem/corollary) render. A lone reference
// renders as "Lemma 0079" (one link); a `link-nodes` group renders as
// "Lemmas 0079 and 008j", sharing one pluralized kind word with each address its
// own link. `template.typ`'s `render-link` routes here.

#import "node.typ": node-taxon, node-link

// Theorem-like taxa carry a "kind word" that a body-less reference renders in
// front of the node's monospace address (e.g. "Lemma 0079"). The plural form is
// used when `link-nodes` groups several same-taxon references under one word.
#let _theorem-like = (
  "lemma": (singular: "Lemma", plural: "Lemmas"),
  "theorem": (singular: "Theorem", plural: "Theorems"),
  "corollary": (singular: "Corollary", plural: "Corollaries"),
)

// Whether a node is a formal result that body-less references render specially.
#let is-theorem-like(node) = node-taxon(node) in _theorem-like

// One linked, monospace formal-result address: `<a class="node-link node-ref">0079</a>`.
#let _render-ref-address(target-node) = node-link(
  target-node,
  body: html.span(class: "node-ref-address")[#target-node.id],
  extra-classes: ("node-ref",),
)

// A singular formal-result reference, "Lemma 0079" as a single link (kind word
// and address both clickable).
#let render-node-ref(target-node) = node-link(
  target-node,
  body: [
    #html.span(class: "node-ref-kind")[#_theorem-like.at(node-taxon(target-node)).singular]
    #html.span(class: "node-ref-address")[#target-node.id]
  ],
  extra-classes: ("node-ref",),
)

// Join rendered items with prose "and"/Oxford commas: "A", "A and B",
// "A, B, and C".
#let _join-and(items) = {
  let n = items.len()
  if n == 1 {
    items.first()
  } else if n == 2 {
    items.at(0) + [ and ] + items.at(1)
  } else {
    items.slice(0, n - 1).join([, ]) + [, and ] + items.last()
  }
}

// A `link-nodes(...)` group: several theorem-like references sharing one
// pluralized kind word, e.g. "Lemmas 0079 and 008j". The shared word is prose;
// each address is its own link. A one-id group renders as the singular form.
// Mixed taxa are not yet supported (MVP panic). `nodes` are the group's node
// records, in order; the caller resolves them, so that `site` is not passed in.
#let render-node-ref-group(nodes) = {
  let ids = nodes.map(node => node.id)
  let taxa = nodes.map(node-taxon)

  for (id, taxon) in ids.zip(taxa) {
    if taxon not in _theorem-like {
      panic(
        "link-nodes target " + id + " has taxon `" + taxon
          + "`; only lemma/theorem/corollary may be grouped",
      )
    }
  }

  if nodes.len() == 1 {
    render-node-ref(nodes.first())
  } else if taxa.dedup().len() == 1 {
    let kind = _theorem-like.at(taxa.first()).plural
    [#html.span(class: "node-ref-kind")[#kind] #_join-and(nodes.map(_render-ref-address))]
  } else {
    panic("link-nodes mixed taxa not yet supported: " + taxa.join(", "))
  }
}
