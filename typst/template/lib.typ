#import "@local/arb-core:0.1.0": collect-site, make-renderer
#import "model.typ": collect-node-pages, node-href
#import "agda.typ": collect-agda-blocks
#import "citations.typ": reference-sources
#import "template.typ": render-link, render-page, render-transclusion

// `sources` is an array of `(path:, content:)` dictionaries.
#let site(sources, bib: none, agda-annotations: none) = {
  let references = if bib != none { reference-sources(bib) } else { () }
  let (site, bodies) = collect-site(sources + references)

  let agda-blocks = collect-agda-blocks(site, agda-annotations)
  let node-pages = collect-node-pages(site, agda-blocks)

  // The callbacks look up node pages and pass the render functions only what
  // they read. They are closures, not `.with` partial applications, so that
  // the captured `node-pages` is hashed once rather than on every call.
  let render = make-renderer(
    site,
    bodies,
    render-page: (id, _, body) => render-page(node-pages.at(id), body, bib),
    render-link: (target-id, _, body, metadata) => {
      render-link(node-pages.at(target-id).node, body, metadata)
    },
    render-transclusion: (target-id, _, metadata, resolve-body) => {
      render-transclusion(node-pages.at(target-id), metadata, resolve-body())
    },
  )

  for (id, node) in site.nodes [
    #document(node-href(id), title: node.title)[#render(id)]
    #label(id)
  ]
  for name in (
    "style.css",
    "main.js",
    "main.js.map",
    "EBGaramond-Variable.ttf",
    "EBGaramond-Italic-Variable.ttf",
    "SourceSans3-Variable.ttf",
    "JuliaMono-Regular.ttf",
    "JuliaMono-Bold.ttf",
    "LatinModernMath-Regular.otf",
  ) {
    asset(name, read("assets/" + name, encoding: none))
  }
}
