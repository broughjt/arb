#import "backmatter.typ": (
  render-backmatter-section,
  render-version-tag,
  render-revision-callout,
)
#import "citations.typ": (
  render-citation,
  render-references,
)
#import "node.typ": node-link, node-title-text
#import "node-metadata.typ": render-node-meta
#import "agda.typ": render-agda-block
#import "theorems.typ": render-theorem, render-proof
#import "link.typ": (
  is-theorem-like,
  render-node-ref,
  render-node-ref-group,
)

#let stylesheet-path = "style.css"
#let script-path = "main.js"

// The render callbacks for this site, as named arguments for arb-core's
// `make-renderer`. The node pages from `collect-node-pages` reach the callbacks by
// closure capture. Do not rewrite this with `.with(...)`: a closure's captured
// values are hashed once, whereas a `.with` function re-hashes its bound
// arguments on every call. For the same reason, the callbacks never pass
// `node-pages` to a helper function, only a node page or a template node.
// arb-core's own node records are ignored (`_`) in favor of template nodes.
#let site-callbacks(node-pages, bib) = {
  let render-link(target-id, _, body, metadata) = {
    let target-node = node-pages.at(target-id).node
    if metadata.at("citation", default: false) {
      if metadata.group-index == 0 {
        render-citation(metadata.group)
      } else {
        none
      }
    } else if "group" in metadata {
      if metadata.group-index == 0 {
        let nodes = ()
        for group-id in metadata.group {
          let group-page = node-pages.at(group-id, default: none)
          if group-page == none {
            panic("unknown node: " + group-id)
          }
          nodes.push(group-page.node)
        }
        render-node-ref-group(nodes)
      } else {
        none
      }
    } else if body == none and is-theorem-like(target-node) {
      render-node-ref(target-node)
    } else {
      node-link(target-node, body: body)
    }
  }

  let render-page(id, _, body) = {
    let node-page = node-pages.at(id)
    let node = node-page.node
    let backmatter = node-page.backmatter
    let has-sections = (
      backmatter.contexts.len() > 0 or backmatter.backlinks.len() > 0 or backmatter.related.len() > 0
    )

    let article = html.article(class: "node")[
      // A hidden carrier for the search index's facet/meta values. Placed as a
      // direct child of `article.node` (the pagefind `--root-selector`) and
      // *outside* the excluded regions (`.node-meta` is excluded, so its
      // taxon/id/tag text never reaches the index) so pagefind still reads these
      // attributes. taxon and each tag become `data-pagefind-filter` values
      // (accumulated per key -> `filters.taxon` / `filters.tag`); the id rides as
      // `data-pagefind-meta` (title is auto-captured from the `<h1>`). The element
      // carries no text, so it adds nothing to the indexed content, and `hidden`
      // keeps it out of the rendered page and the a11y tree. See
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

  let render-transclusion(target-id, _, metadata, resolve-body) = {
    let expanded = metadata.at("expanded", default: true)
    let target-page = node-pages.at(target-id)
    let target-node = target-page.node
    let title-body = node-link(
      target-node,
      body: html.span(class: "node-title-text")[
        #target-node.title#render-version-tag(target-node.version, class: "node-version")
      ],
    )

    // TODO: Handle heading offsets

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

          #resolve-body()
        ]
      ]
    ]
  }

  (
    render-page: render-page,
    render-link: render-link,
    render-transclusion: render-transclusion,
  )
}
