#import "@local/arb-core:0.1.0": collect-site, render-pages
#import "backmatter.typ": collect-backmatter
#import "node.typ": node-href
#import "agda.typ": agda-context
#import "template.typ": site-callbacks, render-document

// The whole site. `sources` is an array of `(path:, content:)`, one per
// source file, included by the entry file because a package cannot include
// project files. `bib` is a `path` to the BibTeX bibliography, and
// `agda-annotations` a `path` to agda-annotate's output, or `none`.
#let site(sources, bib: none, agda-annotations: none) = {
  let (site, bodies) = collect-site(sources)
  let backmatter = collect-backmatter(site)
  // Agda token/type annotations (none when the input is absent), consumed by
  // template.typ's raw show rule.
  let agda = agda-context(site, agda-annotations)
  let callbacks = site-callbacks(site, backmatter, agda, bib)

  let pages = render-pages(site, bodies, ..callbacks)
  for (id, page) in pages [
    #let node = site.nodes.at(id)
    #document(node-href(id), title: node.title)[#render-document(node, page)]
    #label(id)
  ]

  asset("site.json", json.encode(
    (
      nodes: site.nodes.keys().sorted(),
      links: site.links.map(target-set => target-set.keys().sorted()),
      transclusions: site.transclusions.map(target-set => target-set.keys().sorted())
    ),
    pretty: false
  ))
}
