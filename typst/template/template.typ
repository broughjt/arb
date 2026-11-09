#import "model.typ": node-href, theorem-kinds
#import "citations.typ": cite-group, references
#import "agda.typ": render-agda-block

// Links

// `target` is a template node
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

// TODO: inline
#let is-theorem-like(node) = node.taxon in theorem-kinds

#let _render-ref-address(target-node) = node-link(
  target-node,
  body: html.span(class: "node-ref-address")[#target-node.id],
  extra-classes: ("node-ref",),
)

#let render-node-ref(target-node) = node-link(
  target-node,
  body: [
    #html.span(class: "node-ref-kind")[#theorem-kinds.at(target-node.taxon).singular]
    #html.span(class: "node-ref-address")[#target-node.id]
  ],
  extra-classes: ("node-ref",),
)

// Render Oxford commas, e.g. "A, B, and C".
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

#let render-node-ref-group(nodes) = {
  let ids = nodes.map(node => node.id)
  let taxa = nodes.map(node => node.taxon)

  for (id, taxon) in ids.zip(taxa) {
    if taxon not in theorem-kinds {
      panic(
        "link-nodes target " + id + " has taxon `" + taxon
          + "`; only lemma/theorem/corollary may be grouped",
      )
    }
  }

  if nodes.len() == 1 {
    render-node-ref(nodes.first())
  } else if taxa.dedup().len() == 1 {
    let kind = theorem-kinds.at(taxa.first()).plural
    [#html.span(class: "node-ref-kind")[#kind] #_join-and(nodes.map(_render-ref-address))]
  } else {
    panic("link-nodes mixed taxa not yet supported: " + taxa.join(", "))
  }
}

#let render-citation(group) = {
  let citations = {
    show link: inner => inner.body
    cite-group(group)
  }
  if group.len() == 1 {
    html.elem("a", attrs: (href: node-href(group.first().key), class: "node-link"), citations)
  } else {
    citations
  }
}

// Node header

// `node` is a template node
#let render-node-meta(node, class: "node-meta", aria-label: "Node metadata") = {
  let taxon = node.taxon
  let tags = node.tags

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
        #html.elem("span", attrs: (class: ("node-taxon", "taxon-" + taxon).join(" ")))[#taxon]
      ]
    ]
    #html.div(class: "node-meta-item node-meta-address")[
      #html.dt(class: "node-meta-label")[id]
      #html.dd(class: "node-meta-value")[
        #node-link(node, body: node.id, extra-classes: ("node-address",))
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

#let version-label(version, is-latest) = (
  "(version " + str(version) + if is-latest { "; latest" } else { "" } + ")"
)

#let render-version-tag(version, class: "revision-version-tag") = {
  if version != none {
    [ #html.span(class: class)[#version-label(version.number, version.is-latest)]]
  }
}

// Versions callout

#let render-revision-callout(node, versions) = {
  let is-latest = node.version.is-latest

  let version-tag(version, is-latest) = html.span(class: "revision-version-tag")[#version-label(version, is-latest)]

  let body = if versions.len() == 2 {
    if is-latest {
      [This note supersedes an #node-link(versions.first(), body: [earlier version]).]
    } else {
      [This note has been superseded by the #node-link(versions.last(), body: [current version]).]
    }
  } else {
    let item(index, member) = {
      let version = index + 1
      let is-self = version == node.version.number
      let is-latest = member.version.is-latest
      let item-class = "revision-version-item" + if is-self { " current" } else { "" }
      html.li(class: item-class)[
        #html.span(class: "revision-version-body")[#if is-self {
          html.span(class: "revision-version-title")[#member.title]
        } else {
          node-link(member)
        }
        #version-tag(version, is-latest)]
        #if is-self [#html.span(class: "revision-version-here")[current]]
      ]
    }
    [
      This note has several versions:
      #html.ol(class: "revision-version-list")[
        #versions.enumerate().map(((index, member)) => item(index, member)).join()
      ]
    ]
  }

  let state-class = if is-latest { "is-latest" } else { "is-superseded" }
  html.aside(class: "revision-callout " + state-class, role: "note")[#body]
}

// Mainmatter

#let render-theorem(theorem) = {
  let note-parts = ()
  if theorem.name != none {
    note-parts.push(html.span(class: "theorem-name")[#theorem.name])
  }
  if theorem.citation != none {
    note-parts.push(html.span(class: "theorem-citation")[#theorem.citation])
  }

  html.article(class: ("theorem", "theorem-" + theorem.kind))[
    #html.header(class: "theorem-header")[
      #html.span(class: "theorem-label")[#theorem-kinds.at(theorem.kind).singular]
      #if note-parts.len() > 0 {
        [ #html.span(class: "theorem-note")[#note-parts.join()]]
      }
    ]
    #html.div(class: "theorem-body")[#theorem.body]
  ]
}

#let render-proof(body) = {
  html.section(class: "proof")[
    #html.header(class: "proof-header")[Proof.]
    #html.div(class: "proof-body")[#body]
    #html.span(class: "qed", aria-hidden: true)
  ]
}

// Backmatter

// `nodes` are template nodes in display order.
#let render-backmatter-section(title, kind, nodes) = {
  if nodes.len() > 0 {
    html.section(class: ("backmatter-section", kind))[
      #html.h3(class: "backmatter-section-title")[#title]
      #html.ul(class: "node-list")[
        #nodes.map(node => {
          html.li(class: "node-list-item")[
            #node-link(node, body: [#node.title#render-version-tag(node.version)])
          ]
        }).join()
      ]
    ]
  }
}

#let render-references(bib) = {
  html.section(class: "backmatter-section")[
    #html.h3(class: "backmatter-section-title")[References]
    #references(bib)
  ]
}

// Page

// TODO: Remove if there is ever anything in Typst standard library content that
// does this better.
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

#let stylesheet-path = "style.css"
#let script-path = "main.js"

// Render callbacks

// `group` holds the template nodes of a `link-nodes` group's members, and is
// empty otherwise.
#let render-link(target, body, metadata, group) = {
  if metadata.at("citation", default: false) {
    if metadata.group-index == 0 {
      render-citation(metadata.group)
    } else {
      none
    }
  } else if "group" in metadata {
    if metadata.group-index == 0 {
      render-node-ref-group(group)
    } else {
      none
    }
  } else if body == none and is-theorem-like(target) {
    render-node-ref(target)
  } else {
    node-link(target, body: body)
  }
}

#let render-page(node-page, body, bib) = {
  let node = node-page.node
  let id = node.id
  let backmatter = node-page.backmatter
  let has-sections = (
    backmatter.contexts.len() > 0 or backmatter.backlinks.len() > 0 or backmatter.related.len() > 0
  )

  let article = html.article(class: "node")[
    // TODO: Update this comment
    // A hidden carrier for the search index's facet/meta values. Placed as a
    // direct child of `article.node` (the pagefind `--root-selector`) and
    // *outside* the excluded regions (`.node-meta` is excluded, so its
    // taxon/id/tag text never reaches the index) so pagefind still reads these
    // attributes. taxon and each tag become `data-pagefind-filter` values
    // (accumulated per key -> `filters.taxon` / `filters.tag`); the id rides as
    // `data-pagefind-meta` (title is auto-captured from the `<h1>`). The
    // element carries no text, so it adds nothing to the indexed content, and
    // `hidden` keeps it out of the rendered page and the a11y tree. See
    // .scratch/search-design.md -> "Facets".
    #html.elem("div", attrs: (
      "class": "pagefind-attrs",
      "hidden": "",
      "data-pagefind-meta": "id:" + id,
      "data-pagefind-filter": "taxon:" + node.taxon,
    ))[#node.tags.map(tag => html.elem(
      "span",
      attrs: ("data-pagefind-filter": "tag:" + tag),
    )).join()]
    #html.header(class: "node-header")[
      #render-node-meta(node)
      #html.h1(
        id: "node-" + id,
        class: "node-title",
      )[#node.title#render-version-tag(node.version, class: "node-version")]
    ]
    #if node-page.versions != none {
      render-revision-callout(node, node-page.versions)
    }
    #html.section(class: "node-mainmatter")[
      // Also covers transcluded bodies, which render inside the mainmatter.
      #show metadata: it => {
        let value = it.value
        if type(value) == dictionary and "theorem" in value {
          render-theorem(value.theorem)
        } else if type(value) == dictionary and "proof" in value {
          render-proof(value.proof)
        } else {
          it
        }
      }
      #show figure: it => html.elem("figure", attrs: (class: "typst"), html.frame(it.body))
      #show raw.where(lang: "agda"): it => render-agda-block(node-page.agda-fences, it)

      #body
    ]
    #if backmatter.references.len() > 0 {
      html.section(
        class: "backmatter",
        aria-label: "References",
      )[
        #render-references(bib)
      ]
    }
    #if has-sections {
      html.aside(
        class: "node-rail",
        aria-label: "Node context",
      )[
        #render-backmatter-section([Contexts], "contexts", backmatter.contexts)
        #render-backmatter-section([Backlinks], "backlinks", backmatter.backlinks)
        #render-backmatter-section([Related], "related", backmatter.related)
      ]
    }
  ]

  html.html(lang: "en")[
    // TODO: Explain why we aren't just Typst's default head generation.
    //
    // Keep this `html.head` call in sync with Typst's default head generation (pinned rev):
    //   https://github.com/typst/typst/blob/c93cf32f6f08b6085efafe2217efd746f830663d/crates/typst-html/src/document.rs#L314
    // and the `<html lang>` attribute set alongside it in `finalize_dom`:
    //   https://github.com/typst/typst/blob/c93cf32f6f08b6085efafe2217efd746f830663d/crates/typst-html/src/document.rs#L306
    #html.head[
      #html.meta(charset: "utf-8")
      #html.meta(name: "viewport", content: "width=device-width, initial-scale=1")
      #html.title(node-title-text(node))
      #html.link(rel: "stylesheet", href: stylesheet-path)
      #html.script(type: "module", src: script-path)
    ]
    #html.elem("body", attrs: (
      "data-node-id": id,
    ))[
      #html.main[
        #article
      ]
    ]
  ]
}

#let render-transclusion(target-page, metadata, body) = {
  let expanded = metadata.at("expanded", default: true)
  let target-node = target-page.node
  let title-body = node-link(
    target-node,
    body: html.span(class: "node-title-text")[
      #target-node.title#render-version-tag(target-node.version, class: "node-version")
    ],
  )

  // TODO: Handle heading offsets
  // TODO: Is this still relevant?

  html.section(class: "transclusion block")[
    #html.details(open: expanded, class: "transclusion-details")[
      #html.summary(class: "transclusion-summary")[#title-body]
      #render-node-meta(
        target-node,
        class: "transclusion-meta",
        aria-label: "Transcluded node metadata",
      )
      #html.div(class: "transclusion-body")[
        #show figure: it => html.elem("figure", attrs: (class: "typst"), html.frame(it.body))
        #show raw.where(lang: "agda"): it => render-agda-block(target-page.agda-fences, it)

        #body
      ]
    ]
  ]
}
