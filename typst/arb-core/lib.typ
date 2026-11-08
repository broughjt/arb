// Inspiration
// - Forester: https://www.jonmsterling.com/index/
// - Org-roam: https://www.orgroam.com/
// - Andy Matuschak's Evergreen Notes: https://notes.andymatuschak.org/About_these_notes

// Why are we doing it this way? Why does it need to be so complicated? Why
// recursive crawling?
//
// If we were just doing a single node per file, we could just call `#document`
// in each file and be done with it. The first bit of complexity comes from the
// ability to transclude other nodes. Nested document calls result in an
// compilation error, because inside a call to `#document`, Typst compiles the
// inner content with a PDF or HTML target, not a bundle target. We could handle
// this by `#include`ing each file in a central location, which would get us a
// dictionary with `content`s entries for each node, which we could then output
// by calling `#document` in a for loop.
//
// The second piece of complexity comes from the desire to support arbitrarily
// nested subnodes. I want this because I write literate Agda code inside Typst,
// and I want the ability to have many (10s, 100s) of nodes inside a single Agda
// module, which is necessarily contained in one file. I decided that it was
// better to support the general case of arbitrarily nested subnodes instead of
// only supporting my special case of many top-level nodes in Agda files.
//
// I tried using Typst's query and state mechanisms to support both the
// transclusion and nested subnode functionality, but I couldn't get it to
// work. The (somewhat unsatisfying) solution is to treat nodes, links, and
// transclusions as Typst metadata and separate into two phases. In Phase 1, we
// recursively crawl the Typst content to build the node graph, and in Phase 2,
// we render each node (resolving transclusions) to its own HTML document.

#let _marker(kind, fields) = metadata((..fields, weibian: true, kind: kind))

#let _marker-value(value) = {
  if type(value) == content and value.func() == metadata {
    let data = value.fields().value

    if type(data) == dictionary and data.at("weibian", default: false) == true {
      data
    } else {
      none
    }
  } else {
    none
  }
}

#let _walk(value) = {
  let result = (links: (), transclusions: (), entries: ())

  if type(value) == content {
    let marker = _marker-value(value)

    if marker == none {
      let kind = value.func()
      if kind == math.equation or kind == raw {
        result
      } else {
        _walk(value.fields())
      }
    } else if marker.kind == "node" {
      let found = _walk(marker.body)
      let entry = (
        node: (id: marker.id, title: marker.title, metadata: marker.metadata),
        body: marker.body,
        links: found.links,
        transclusions: found.transclusions,
      )
      result.entries = (entry,) + found.entries
      result
    } else if marker.kind == "link" {
      result.links.push((target: marker.target, metadata: marker.metadata))
      result
    } else if marker.kind == "transclusion" {
      result.transclusions.push((target: marker.target, metadata: marker.metadata))
      result
    } else {
      result
    }
  } else if type(value) == array or type(value) == dictionary {
    let children = if type(value) == dictionary {
      value.values()
    } else {
      value
    }
    for child in children {
      let found = _walk(child)
      result.links += found.links
      result.transclusions += found.transclusions
      result.entries += found.entries
    }
    result
  } else {
    result
  }
}

// Memoized on the file's content, whose hash is cached, so that an unchanged
// file costs one cache lookup.
#let _collect-file(content) = _walk(content)

#let _build-edge-graph(edge-metadata) = {
  let graph = (:)

  for (source, entries) in edge-metadata {
    let outgoing = (:)
    for entry in entries {
      outgoing.insert(entry.target, true)
    }
    graph.insert(source, outgoing)
  }

  graph
}

// Return `(site, bodies)`. `site` consists of:
//
// - `nodes`: a map from id to (id, title, metadata, path)
// - `links`/`transclusions`: adjacency list for the graph of
//   links/transclusions (map from id to set of neighbors, where a set is
//   represented as a dictionary with `true` as the values)
// - `link-metadata`/`transclusion-metadata`: map from id to ordered list of
//   (target, metadata) pairs. We do this because there might be several
//   links/transclusions with the same target but potentially different metadata.
// 
// `bodies` maps from id to node body.
//
// The `sources` argument is an array of (path, content) pairs.
#let collect-site(sources) = {
  let nodes = (:)
  let bodies = (:)
  let links = (:)
  let transclusions = (:)
  for source in sources {
    let collected = _collect-file(source.content)

    let edges-outside-node = collected.links + collected.transclusions
    if edges-outside-node.len() > 0 {
      panic(
        source.path + ": link or transclusion outside any node, to " +
        edges-outside-node.map(edge => edge.target).join(", ")
      )
    }

    for entry in collected.entries {
      let id = entry.node.id
      let node = (
        id: id,
        title: entry.node.title,
        metadata: entry.node.metadata,
        path: source.path,
      )
      let existing-node = nodes.at(id, default: none)

      if existing-node == none {
        nodes.insert(id, node)
        bodies.insert(id, entry.body)
        links.insert(id, entry.links)
        transclusions.insert(id, entry.transclusions)
      } else if existing-node != node or bodies.at(id) != entry.body {
        panic("conflicting node definitions for ID: " + id)
      }

    }
  }

  // Dictionaries iterate in insertion order, so we sort by id to keep the order
  // of sources and subnodes from leaking into the output.
  //
  // We use a loop instead of sorted to avoid a key closure that would add a
  // bunch of memoized calls per node we don't need.
  let sorted-nodes = (:)
  let sorted-bodies = (:)
  let link-metadata = (:)
  let transclusion-metadata = (:)
  for id in nodes.keys().sorted() {
    sorted-nodes.insert(id, nodes.at(id))
    sorted-bodies.insert(id, bodies.at(id))
    link-metadata.insert(id, links.at(id))
    transclusion-metadata.insert(id, transclusions.at(id))
  }

  (
    site: (
      nodes: sorted-nodes,
      links: _build-edge-graph(link-metadata),
      transclusions: _build-edge-graph(transclusion-metadata),
      link-metadata: link-metadata,
      transclusion-metadata: transclusion-metadata,
    ),
    bodies: sorted-bodies,
  )
}

// Rendering
//
// The `render-pages` function renders one page per node, returning a
// dictionary from id to the rendered content (in sorted id order). User
// templates supply three callbacks:
//
// - `render-node(id, node, body)`
// - `render-link(target-id, target-node, body, metadata)`
// - `render-transclusion(target-id, target-node, metadata, render-body)`
//
// Caching rules
//
// Typst memoizes every call of a user-defined function by hashing its
// arguments, and it reuses the result during a rebuild only if every argument
// hashes equal to that of a previous call. Content and closures cache their
// hashes, but dictionaries and arrays do not, and functions created with
// partial application through `.with` do not. Therefore, templates should:
//
// - not let a rendering callback close over `bodies`, since any edit will leave
//   it modified, and therefore all pages rendered using the callback will need
//   to be rerendered again on every edit;
// - avoid the use of `.with` for building rendering callbacks, since the
//   captured arguments will be re-hashed on every call;
// - not pass `site` or another large dictionary as an argument to a function
//   called once per page (or link or transclusion), since each call hashes the
//   whole dictionary.

// For each node, the sorted ids of every node it transcludes reflexive-
// transitively.
#let _transclusion-closures(transclusions) = {
  let closures = (:)
  for id in transclusions.keys() {
    let reachable = (:)
    let pending = (id,)
    while pending.len() > 0 {
      let current = pending.pop()
      if current in reachable or current not in transclusions {
        continue
      }
      reachable.insert(current, true)
      pending += transclusions.at(current).keys()
    }
    closures.insert(id, reachable.keys().sorted())
  }
  closures
}

#let render-pages(
  site,
  bodies,
  render-node: none,
  render-link: none,
  render-transclusion: none,
) = {
  for (name, callback) in (
    render-node: render-node,
    render-link: render-link,
    render-transclusion: render-transclusion,
  ) {
    if callback == none {
      panic("render-pages requires " + name)
    }
  }

  let render-page(id, page-bodies) = {
    // Created once per page, capturing `site` and `page-bodies`, so that they
    // are hashed once per page rather than at every transclusion.
    let render-content(body, stack) = [
      #show metadata: it => {
        let marker = _marker-value(it)

        if marker == none {
          it
        } else if marker.kind == "node" {
          none
        } else if marker.kind == "link" {
          let target-id = marker.target
          let target-node = site.nodes.at(target-id, default: none)
          if target-node == none {
            panic("unknown node: " + target-id)
          }
          render-link(target-id, target-node, marker.body, marker.metadata)
        } else if marker.kind == "transclusion" {
          let target-id = marker.target

          if stack.contains(target-id) {
            panic("transclusion cycle: " + stack.join(" -> ") + " -> " + target-id)
          }

          let target-node = site.nodes.at(target-id, default: none)
          if target-node == none {
            panic("unknown node: " + target-id)
          }
          let render-body = () => render-content(page-bodies.at(target-id), stack + (target-id,))
          render-transclusion(target-id, target-node, marker.metadata, render-body)
        } else {
          none
        }
      }

      #body
    ]

    render-node(id, site.nodes.at(id), render-content(page-bodies.at(id), (id,)))
  }

  let closures = _transclusion-closures(site.transclusions)
  let pages = (:)
  for id in site.nodes.keys() {
    // A plain loop rather than `.map(reached => ...)`, since that closure would
    // capture `bodies` and hash all of it once per page.
    let page-bodies = (:)
    for reachable-id in closures.at(id) {
      page-bodies.insert(reachable-id, bodies.at(reachable-id))
    }

    pages.insert(id, render-page(id, page-bodies))
  }

  pages
}

// Public authoring API

#let node(id, title, body, ..arguments) = _marker("node", (
  id: id,
  title: title,
  metadata: arguments.named(),
  body: body,
))

// Named `link-node` because `link` is already taken for more general Typst
// links.
#let link-node(id, ..arguments) = {
  let positional = arguments.pos()

  if positional.len() > 1 {
    panic("link-node accepts at most one body argument")
  }

  _marker("link", (
    target: id,
    body: positional.at(0, default: none),
    metadata: arguments.named(),
  ))
}

#let transclude(id, ..arguments) = {
  if arguments.pos().len() > 0 {
    panic("transclude accepts no positional arguments")
  }

  _marker("transclusion", (
    target: id,
    metadata: arguments.named(),
  ))
}

#let subnode(id, title, body, should-transclude: true, ..arguments) = [
  #node(id, title, body, ..arguments)
  #if should-transclude {
    transclude(id, ..arguments)
  }
]
