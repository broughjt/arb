#let TAXA = (
  "axiom",
  "corollary",
  "definition",
  "example",
  "lemma",
  "list",
  "module",
  "note",
  "outline",
  "question",
  "reference",
  "theorem",
)

#let theorem-kinds = (
  "lemma": (singular: "Lemma", plural: "Lemmas"),
  "theorem": (singular: "Theorem", plural: "Theorems"),
  "corollary": (singular: "Corollary", plural: "Corollaries"),
)

#let node-href(id) = id + ".html"

#let node-taxon(node) = {
  let taxon = node.metadata.at("taxon", default: "note")
  let label = "taxon for node " + node.id

  if type(taxon) != str {
    panic(label + " must be a string, got " + repr(taxon))
  }
  if taxon not in TAXA {
    panic("unknown " + label + " `" + taxon + "`; known taxa: " + TAXA.join(", "))
  }

  taxon
}

#let _node-tags(node) = {
  let tags = node.metadata.at("tags", default: ())

  assert(type(tags) == array)
  for tag in tags {
    assert(type(tag) == str)
  }

  tags
}

// We provide a versioning mechanism for nodes. For example, we can store
// `supersedes` in a node's metadata, as in `node("002x", ..., supersedes:
// "00ah")`. We think of `supersedes` as a binary relation on nodes and require
// each connected component of the reflexive-transitive closure of
// `supersedes` to be a finite chain. In other words, every node should
// supersede at most one node, should be superseded by at most one node, and
// should never transitively supersede itself.

// Build a map from the id of every node that has other versions to the list of
// ids of its versions (oldest first).
#let _collect-versions(site) = {
  let successors = (:)
  for (id, node) in site.nodes {
    let previous = node.metadata.at("supersedes", default: none)
    if previous == none {
      continue
    }
    if type(previous) != str or previous not in site.nodes {
      panic("supersedes for node " + id + " must be a node id, got " + repr(previous))
    }
    if previous in successors {
      panic("nodes " + successors.at(previous) + " and " + id + " both supersede " + previous)
    }
    successors.insert(previous, id)
  }

  let versions = (:)
  for (id, node) in site.nodes {
    if id in successors and node.metadata.at("supersedes", default: none) == none {
      let chain = (id,)
      while chain.last() in successors {
        chain.push(successors.at(chain.last()))
      }
      for member in chain {
        versions.insert(member, chain)
      }
    }
  }

  for id in successors.keys() {
    if id not in versions {
      panic("supersedes cycle through node " + id)
    }
  }

  versions
}

#let _reverse-graph(nodes, graph) = {
  let reversed = (:)

  for id in nodes.keys().sorted() {
    reversed.insert(id, (:))
  }

  for (source, targets) in graph {
    for target in targets.keys() {
      if target in reversed {
        let incoming = reversed.at(target)
        incoming.insert(source, true)
        reversed.insert(target, incoming)
      }
    }
  }

  reversed
}

// Build a map from every node id to the ids in its backmatter, `(contexts,
// backlinks, related, references)`, each sorted. The `nodes` parameter is the
// dictionary of template nodes, which we read for their taxa.
#let _collect-backmatter(site, nodes) = {
  let is-reference(id) = nodes.at(id, default: (taxon: none)).taxon == "reference"

  let contexts = _reverse-graph(site.nodes, site.transclusions)
  let backlinks = _reverse-graph(site.nodes, site.links)

  let backmatter = (:)
  for id in site.nodes.keys() {
    let related = ()
    for target in site.links.at(id).keys() {
      if not is-reference(target) {
        related.push(target)
      }
    }

    let references = (:)
    for source in site.transclusion-closures.at(id) {
      for target in site.links.at(source).keys() {
        if is-reference(target) {
          references.insert(target, true)
        }
      }
    }

    backmatter.insert(id, (
      contexts: contexts.at(id).keys().sorted(),
      backlinks: backlinks.at(id).keys().sorted(),
      related: related.sorted(),
      references: references.keys().sorted(),
    ))
  }

  backmatter
}

// Every node linked in a `link-nodes` group must share the same theorem-like taxon.
#let _check-link-groups(site, nodes) = {
  for (id, links) in site.link-metadata {
    for link in links {
      let metadata = link.metadata
      if metadata.at("kind", default: none) != "group" or metadata.group-index != 0 {
        continue
      }
      let taxa = ()
      for member in metadata.group {
        let taxon = nodes.at(member).taxon
        if taxon not in theorem-kinds {
          panic(
            "link-nodes target " + member + " has taxon `" + taxon
              + "`; only lemma/theorem/corollary may be grouped",
          )
        }
        taxa.push(taxon)
      }
      if taxa.dedup().len() > 1 {
        panic("link-nodes mixed taxa not yet supported: " + taxa.join(", "))
      }
    }
  }
}

// A *template node* is the structured representation of a node in this
// template. We use arb-core's node metadata support to keep track of a taxon,
// tags, and versioning for each node. We store versions as `(number,
// is-latest)` or `none` when there are no other versions.
//
// A *node page* is the data needed to render the page for a node, besides its
// body which we keep separately. It includes:
//
// - `node`: the template node
// - `versions`: the template nodes of the node's versions, oldest first, or
//   `none` when there are no other versions
// - `backmatter`: `(contexts, backlinks, related, references)`, each a list of
//   template nodes sorted by id. Related nodes are direct link targets that are
//   not references. References are the reference nodes linked from the node or
//   anything it reflexive-transitively transcludes.
// - `agda-blocks`: the rendered Agda blocks of the node's module, by their text
#let collect-node-pages(site, agda-blocks) = {
  let versions = _collect-versions(site)

  let nodes = (:)
  for (id, node) in site.nodes {
    let chain = versions.at(id, default: none)
    nodes.insert(id, (
      id: id,
      title: node.title,
      taxon: node-taxon(node),
      tags: _node-tags(node),
      version: if chain != none {
        (number: chain.position(member => member == id) + 1, is-latest: chain.last() == id)
      },
    ))
  }

  let template-nodes(ids) = {
    let result = ()
    for id in ids {
      let node = nodes.at(id, default: none)
      if node == none {
        panic("unknown node: " + id)
      }
      result.push(node)
    }
    result
  }

  _check-link-groups(site, nodes)
  let backmatter = _collect-backmatter(site, nodes)

  let pages = (:)
  for (id, node) in nodes {
    let ids = backmatter.at(id)
    let chain = versions.at(id, default: none)

    pages.insert(id, (
      node: node,
      versions: if chain != none { template-nodes(chain) },
      backmatter: (
        contexts: template-nodes(ids.contexts),
        backlinks: template-nodes(ids.backlinks),
        related: template-nodes(ids.related),
        references: template-nodes(ids.references),
      ),
      agda-blocks: agda-blocks.at(id, default: (:)),
    ))
  }

  pages
}
