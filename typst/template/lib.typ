#import "@local/arb-core:0.1.0": collect-site, make-renderer
#import "model.typ": collect-node-pages, node-href
#import "agda.typ": agda-context
#import "template.typ": render-link, render-page, render-transclusion

// `sources` is an array of `(path:, content:)` dictionaries.
#let site(sources, bib: none, agda-annotations: none) = {
  let (site, bodies) = collect-site(sources)

  let agda = agda-context(site, agda-annotations)
  let node-pages = collect-node-pages(site, agda)

  // The callbacks look up node pages and pass the render functions only what
  // they read. They are closures, not `.with` partial applications, so that
  // the captured `node-pages` is hashed once rather than on every call.
  // arb-core's own node records are ignored (`_`) in favor of template nodes.
  let render = make-renderer(
    site,
    bodies,
    render-page: (id, _, body) => render-page(node-pages.at(id), body, bib),
    render-link: (target-id, _, body, metadata) => {
      let group = ()
      if "group" in metadata and not metadata.at("citation", default: false) {
        for id in metadata.group {
          group.push(node-pages.at(id).node)
        }
      }
      render-link(node-pages.at(target-id).node, body, metadata, group)
    },
    render-transclusion: (target-id, _, metadata, resolve-body) => {
      render-transclusion(node-pages.at(target-id), metadata, resolve-body())
    },
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
