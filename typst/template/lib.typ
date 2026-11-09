#import "@local/arb-core:0.1.0": collect-site, make-renderer
#import "backmatter.typ": collect-backmatter
#import "node.typ": node-href
#import "agda.typ": agda-context
#import "template.typ": site-callbacks

// `sources` is an array of `(path:, content:)` dictionaries.
#let site(sources, bib: none, agda-annotations: none) = {
  let (site, bodies) = collect-site(sources)

  let backmatter = collect-backmatter(site)
  let agda = agda-context(site, agda-annotations)

  let render = make-renderer(
    site,
    bodies,
    ..site-callbacks(site, backmatter, agda, bib)
  )

  for (id, node) in site.nodes [
    #document(node-href(id), title: node.title)[#render(id)]
    #label(id)
  ]
  asset("site.json", json.encode(
    (
      nodes: site.nodes.keys().sorted(),
      links: site.links.map(neighbors => neighbors.keys().sorted()),
      transclusions: site.transclusions.map(neighbors => neighbors.keys().sorted())
    ),
    pretty: false
  ))
}
