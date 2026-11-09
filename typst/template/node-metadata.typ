#import "node.typ": (
  node-link,
  node-tags,
  node-taxon,
  taxon-class,
)

#let render-node-meta(id, node, class: "node-meta", aria-label: "Node metadata") = {
  let taxon = node-taxon(node)
  let tags = node-tags(node)

  // Deterministic hue for a tag chip (Round F3): a small polynomial hash of the
  // tag text picks one of the six Modus chip hues, so the same tag gets the same
  // pastel on every page and every build. Emitted as a `hue-*` class on `.node-tag`.
  let tag-hue(tag) = {
    let s = if type(tag) == str { tag } else { repr(tag) }
    let hash = 0
    for c in s.codepoints() {
      hash = calc.rem(hash * 31 + c.to-unicode(), 997)
    }
    ("red", "yellow", "green", "cyan", "blue", "magenta").at(calc.rem(hash, 6))
  }

  html.dl(class: class, aria-label: aria-label)[
    #html.div(class: "node-meta-item node-meta-taxon")[
      #html.dt(class: "node-meta-label")[taxon]
      #html.dd(class: "node-meta-value")[
        #html.elem("span", attrs: (class: ("node-taxon", taxon-class(taxon)).join(" ")))[#taxon]
      ]
    ]
    #html.div(class: "node-meta-item node-meta-address")[
      #html.dt(class: "node-meta-label")[id]
      #html.dd(class: "node-meta-value")[
        #node-link(node, body: id, extra-classes: ("node-address",))
      ]
    ]
    #if tags.len() > 0 {
      html.div(class: "node-meta-item node-meta-tags")[
        #html.dt(class: "node-meta-label")[tags]
        #html.dd(class: "node-meta-value node-tags")[
          #tags.map(tag => html.elem("span", attrs: (class: ("node-tag", "hue-" + tag-hue(tag)).join(" ")))[#tag]).join()
        ]
      ]
    }
  ]
}
