#import "backmatter.typ": (
  render-backmatter-section,
  render-version-tag,
  render-revision-callout,
)
#import "citations.typ": (
  render-citation,
  render-references,
)
#import "node.typ": (
  node-link,
  node-taxon,
  node-tags,
  node-title-text,
)
#import "node-metadata.typ": render-node-meta
#import "agda.typ": agda-fences, render-agda-block
#import "theorems.typ": render-theorem, render-proof
#import "link.typ": (
  is-theorem-like,
  render-node-ref,
  render-node-ref-group,
)

#let stylesheet-path = "style.css"
#let script-path = "main.js"

// The render callbacks for this site, as named arguments for arb-core's
// `make-renderer`. The bodyless `site` and the template data computed from it
// (`backmatter` from `collect-backmatter`, `agda` from `agda-context`) reach
// the callbacks by closure capture, never by being added to `site`. Do not
// rewrite this with `.with(...)`: a closure's captured values are hashed once,
// whereas a `.with` function re-hashes its bound arguments on every call.
//
// For the same reason, the callbacks never pass `site`, `backmatter` or `agda`
// to a helper function: every call hashes its arguments, and these are large
// dictionaries. They resolve what a helper needs with loops and lookups,
// which are not calls, and pass it the small result.
#let site-callbacks(site, backmatter, agda, bib) = {
  let render-link(target-id, target-node, body, metadata) = {
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
          let group-node = site.nodes.at(group-id, default: none)
          if group-node == none {
            panic("unknown node: " + group-id)
          }
          nodes.push(group-node)
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

  let render-page(id, node, body) = {
    let node-backmatter = backmatter.at(id)
    let revisions = node-backmatter.revisions
    let has-sections = (
      node-backmatter.contexts.keys().len() > 0
        or node-backmatter.backlinks.keys().len() > 0
        or node-backmatter.related.keys().len() > 0
    )
    let fences = agda-fences(agda, id)

    // Each rail section's entries, resolved here rather than in
    // `render-backmatter-section` (see above).
    let sections = (:)
    for kind in ("contexts", "backlinks", "related") {
      let entries = ()
      for target in node-backmatter.at(kind).keys().sorted() {
        let target-node = site.nodes.at(target, default: none)
        if target-node == none {
          panic("Node id didn't exist when rendering backmatter section")
        }
        entries.push((node: target-node, revisions: backmatter.at(target).revisions))
      }
      sections.insert(kind, entries)
    }

    let article = html.article(class: "node")[
      // A hidden carrier for the search index's facet/meta values. Placed as a
      // direct child of `article.node` (the pagefind `--root-selector`) and
      // *outside* the excluded regions (`.node-meta` is excluded, so its
      // taxon/id/tag text never reaches the index) so pagefind still reads these
      // attributes. taxon and each tag become `data-pagefind-filter` values
      // (accumulated per key → `filters.taxon` / `filters.tag`); the id rides as
      // `data-pagefind-meta` (title is auto-captured from the `<h1>`). The element
      // carries no text, so it adds nothing to the indexed content, and `hidden`
      // keeps it out of the rendered page and the a11y tree. See
      // .scratch/search-design.md → "Facets".
      #html.elem("div", attrs: (
        "class": "pagefind-attrs",
        "hidden": "",
        "data-pagefind-meta": "id:" + id,
        "data-pagefind-filter": "taxon:" + node-taxon(node),
      ))[#node-tags(node).map(tag => html.elem(
        "span",
        attrs: ("data-pagefind-filter": "tag:" + tag),
      )).join()]
      #html.header(class: "node-header")[
        #render-node-meta(id, node)
        #html.h1(
          id: "node-" + id,
          class: "node-title",
        )[#node.title#render-version-tag(revisions, class: "node-version")]
      ]
      #if revisions.in-chain {
        let member-nodes = (:)
        for member in revisions.members {
          member-nodes.insert(member, site.nodes.at(member))
        }
        render-revision-callout(id, revisions, member-nodes)
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
        #show raw.where(lang: "agda"): it => render-agda-block(fences, it)

        #body
      ]
      #if node-backmatter.references.keys().len() > 0 {
        html.section(
          class: "backmatter",
          aria-label: "References",
        )[
          #render-references(node-backmatter.references, bib)
        ]
      }
      #if has-sections {
        html.aside(
          class: "node-rail",
          aria-label: "Node context",
        )[
          #render-backmatter-section([Contexts], "contexts", sections.contexts)
          #render-backmatter-section([Backlinks], "backlinks", sections.backlinks)
          #render-backmatter-section([Related], "related", sections.related)
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

  let render-transclusion(target-id, target-node, metadata, resolve-body) = {
    let expanded = metadata.at("expanded", default: true)
    let revisions = backmatter.at(target-id).revisions
    let fences = agda-fences(agda, target-id)
    let title-body = node-link(
      target-node,
      body: html.span(class: "node-title-text")[
        #target-node.title#render-version-tag(revisions, class: "node-version")
      ],
    )

    // TODO: Handle heading offsets

    html.section(class: "transclusion block")[
      #html.details(open: expanded, class: "transclusion-details")[
        #html.summary(class: "transclusion-summary")[#title-body]
        #render-node-meta(
          target-id,
          target-node,
          class: "transclusion-meta",
          aria-label: "Transcluded node metadata",
        )
        #html.div(class: "transclusion-body")[
          #show figure: it => html.elem("figure", attrs: (class: "typst"), html.frame(it.body))
          #show raw.where(lang: "agda"): it => render-agda-block(fences, it)

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
