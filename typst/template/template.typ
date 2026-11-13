#import "model.typ": node-href, theorem-kinds
#import "citations.typ": cite-group, references
#import "agda.typ": render-agda-block

// Links

// `target` is a template node
#let node-link(target, body: none, extra-classes: ()) = {
  html.a(
    href: node-href(target.id),
    class: ("node-link", "target-taxon", "target-taxon-" + target.taxon) + extra-classes,
  )[#if body == none { target.title } else { body }]
}

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

// One member of a `link-nodes` group, which renders as "Lemmas 0079 and 008j"
// across its members' markers: the first carries the plural kind word, and each
// later one its separator.
#let render-node-ref-group-member(target, index, count) = {
  if count == 1 {
    render-node-ref(target)
  } else {
    let address = _render-ref-address(target)
    if index == 0 {
      [#html.span(class: "node-ref-kind")[#theorem-kinds.at(target.taxon).plural] #address]
    } else if count == 2 {
      [ and #address]
    } else if index == count - 1 {
      [, and #address]
    } else {
      [, #address]
    }
  }
}

#let render-citation(group) = {
  let citations = {
    show link: inner => inner.body
    cite-group(group)
  }
  if group.len() == 1 {
    html.a(href: node-href(group.first().key), class: "node-link", citations)
  } else {
    citations
  }
}

// Node header

#let tag-hue(tag) = {
  let hash = 0
  for c in tag.codepoints() {
    hash = calc.rem(hash * 31 + c.to-unicode(), 997)
  }
  ("red", "yellow", "green", "cyan", "blue", "magenta").at(calc.rem(hash, 6))
}

// `node` is a template node
#let render-node-meta(node, class: "node-meta", aria-label: "Node metadata") = {
  let taxon = node.taxon
  let tags = node.tags

  html.dl(class: class, aria-label: aria-label)[
    #html.div(class: "node-meta-item node-meta-taxon")[
      #html.dt(class: "node-meta-label")[taxon]
      #html.dd(class: "node-meta-value")[
        #html.span(class: ("node-taxon", "taxon-" + taxon))[#taxon]
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
          #tags.map(tag => html.span(class: ("node-tag", "hue-" + tag-hue(tag)))[#tag]).join()
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

  let body = if versions.len() == 2 {
    if is-latest {
      [This note supersedes an #node-link(versions.first(), body: [earlier version]).]
    } else {
      [This note has been superseded by the #node-link(versions.last(), body: [current version]).]
    }
  } else {
    let item(member) = {
      let is-self = member.id == node.id
      html.li(class: ("revision-version-item",) + if is-self { ("current",) } else { () })[
        #html.span(class: "revision-version-body")[#if is-self {
          html.span(class: "revision-version-title")[#member.title]
        } else {
          node-link(member)
        }#render-version-tag(member.version)]
        #if is-self [#html.span(class: "revision-version-here")[current]]
      ]
    }
    [
      This note has several versions:
      #html.ol(class: "revision-version-list")[
        #versions.map(item).join()
      ]
    ]
  }

  let state-class = if is-latest { "is-latest" } else { "is-superseded" }
  html.aside(class: ("revision-callout", state-class), role: "note")[#body]
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

// Render callbacks

#let render-link(target, body, metadata) = {
  let kind = metadata.at("kind", default: "node")
  if kind == "citation" {
    if metadata.group-index == 0 {
      render-citation(metadata.group)
    }
  } else if kind == "group" {
    render-node-ref-group-member(target, metadata.group-index, metadata.group.len())
  } else if body == none and target.taxon in theorem-kinds {
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
    // Pagefind indexes `article.node` but skips `.node-meta`, so the taxon,
    // tags and id it filters and labels results by ride on this hidden element,
    // which has no text of its own. A tag's filter is named after its hue, so
    // that search results can color it without hashing it again.
    #html.elem("div", attrs: (
      "class": "pagefind-attrs",
      "hidden": "",
      "data-pagefind-meta": "id:" + id,
      "data-pagefind-filter": "taxon:" + node.taxon,
    ))[#node.tags.map(tag => html.elem(
      "span",
      attrs: ("data-pagefind-filter": "tag-" + tag-hue(tag) + ":" + tag),
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
      #show figure: it => html.figure(class: "typst", html.frame(it.body))
      #show raw.where(lang: "agda"): it => render-agda-block(node-page.agda-blocks, it)

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
    // Typst generates a default <head> only when the document has no <html> of
    // its own, and offers no way to add to it. We need our own head for the
    // stylesheet and script and our own <body> for `data-node-id`, so we
    // replicate the defaults here, as of:
    //   https://github.com/typst/typst/blob/c93cf32f6f08b6085efafe2217efd746f830663d/crates/typst-html/src/document.rs#L314
    //   https://github.com/typst/typst/blob/c93cf32f6f08b6085efafe2217efd746f830663d/crates/typst-html/src/document.rs#L306
    #html.head[
      #html.meta(charset: "utf-8")
      #html.meta(name: "viewport", content: "width=device-width, initial-scale=1")
      #html.title(node-title-text(node))
      #html.link(rel: "stylesheet", href: "style.css")
      #html.script(type: "module", src: "main.js")
      // `arb watch` passes `--input dev=true`, and its server sends a message
      // on `/__events` after each publish.
      #if sys.inputs.at("dev", default: none) == "true" {
        html.script("new EventSource(\"/__events\").onmessage = () => location.reload();")
      }
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

  // TODO: Handle heading offsets. The current corpus doesn't have them
  // (11-09-2026), but remember working on that a while ago? It should supported
  // eventually?

  html.section(class: "transclusion block")[
    #html.details(open: expanded, class: "transclusion-details")[
      #html.summary(class: "transclusion-summary")[#title-body]
      #render-node-meta(
        target-node,
        class: "transclusion-meta",
        aria-label: "Transcluded node metadata",
      )
      #html.div(class: "transclusion-body")[
        #show raw.where(lang: "agda"): it => render-agda-block(target-page.agda-blocks, it)

        #body
      ]
    ]
  ]
}
