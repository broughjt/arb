#import "node.typ": node-is-reference, node-link

#let _set-union(left, right) = {
  let result = left

  for key in right.keys() {
    result.insert(key, true)
  }

  result
}

#let _reverse-graph(nodes, graph) = {
  let reversed = (:)

  for id in nodes.keys().sorted() {
    reversed.insert(id, (:))
  }

  for (source, targets) in graph {
    for target in targets.keys() {
      if reversed.keys().contains(target) {
        let incoming = reversed.at(target)
        incoming.insert(source, true)
        reversed.insert(target, incoming)
      }
    }
  }

  reversed
}

// Forester "related": direct outgoing links to non-reference nodes only. This
// deliberately does not follow transclusions, unlike references below.
#let _collect-related(site, id) = {
  let related = (:)

  for target in site.links.at(id, default: (:)).keys() {
    let node = site.nodes.at(target, default: none)
    if node == none or not node-is-reference(node) {
      related.insert(target, true)
    }
  }

  related
}

// Forester "references": reference-taxon targets reachable through this node's
// reflexive-transitive transclusion closure.
#let _collect-references(site, id, stack) = {
  if stack.contains(id) {
    panic("transclusion cycle while collecting backmatter references: " + stack.join(" -> ") + " -> " + id)
  }

  let references = (:)

  for target in site.links.at(id, default: (:)).keys() {
    let node = site.nodes.at(target, default: none)
    if node != none and node-is-reference(node) {
      references.insert(target, true)
    }
  }

  for target in site.transclusions.at(id, default: (:)).keys().sorted() {
    if site.nodes.keys().contains(target) {
      references = _set-union(
        references,
        _collect-references(site, target, stack + (id,)),
      )
    }
  }

  references
}

// Revision lineage from the `supersedes` node metadata. `node("002x", …,
// supersedes: "00ah")` declares that 002x replaces 00ah. The relation must be a
// *chain* (a total order on each lineage): every note supersedes at most one
// predecessor (it is a scalar field) and is superseded by at most one successor
// (asserted here). That invariant is what makes "version N" and "latest"
// well-defined; a branch ("two version 2s") is a hard error. Returns per-node
// `(previous, next, latest, version, is-latest, in-chain)`.
#let _collect-revisions(site) = {
  // id -> the predecessor it supersedes (or none).
  let supersedes = (:)
  for (id, node) in site.nodes {
    let prev = node.metadata.at("supersedes", default: none)
    if prev != none {
      if type(prev) != str {
        panic("supersedes for node " + id + " must be a node id string, got " + repr(prev))
      }
      if prev == id {
        panic("node " + id + " supersedes itself")
      }
      if not site.nodes.keys().contains(prev) {
        panic("node " + id + " supersedes unknown node " + prev)
      }
    }
    supersedes.insert(id, prev)
  }

  // id -> the successor that supersedes it (or none). At most one: a second note
  // superseding the same predecessor would branch the lineage.
  let superseded-by = (:)
  for id in site.nodes.keys() {
    superseded-by.insert(id, none)
  }
  for id in site.nodes.keys().sorted() {
    let prev = supersedes.at(id)
    if prev != none {
      let existing = superseded-by.at(prev)
      if existing != none {
        panic(
          "revision lineage must be a chain: nodes " + existing + " and " + id
            + " both supersede " + prev,
        )
      }
      superseded-by.insert(prev, id)
    }
  }

  let revisions = (:)
  for id in site.nodes.keys().sorted() {
    // Walk back to the lineage root, detecting cycles. `seen` ends as
    // (id, previous, …, root), so its last entry is the root.
    let seen = (id,)
    let cursor = supersedes.at(id)
    while cursor != none {
      if seen.contains(cursor) {
        panic("supersedes cycle: " + (seen + (cursor,)).join(" -> "))
      }
      seen.push(cursor)
      cursor = supersedes.at(cursor)
    }
    let root = seen.last()

    // Walk forward from the root to list the lineage in version order. Safe
    // without a cycle check: the back-walk above proved the lineage acyclic.
    let members = (root,)
    let forward = superseded-by.at(root)
    while forward != none {
      members.push(forward)
      forward = superseded-by.at(forward)
    }

    let latest = members.last()
    revisions.insert(id, (
      previous: supersedes.at(id),
      next: superseded-by.at(id),
      latest: latest,
      // The whole lineage in version order (version 1 … latest), including this
      // node. Drives the in-body callout.
      members: members,
      version: members.position(m => m == id) + 1,
      is-latest: id == latest,
      in-chain: members.len() > 1,
    ))
  }

  revisions
}

// The backmatter of every node, as id -> (contexts, backlinks, related,
// references, revisions), computed from the bodyless `site`. The template
// captures the result in its rendering closures rather than adding it to
// `site`.
#let collect-backmatter(site) = {
  let contexts = _reverse-graph(site.nodes, site.transclusions)
  let backlinks = _reverse-graph(site.nodes, site.links)
  let revisions = _collect-revisions(site)
  let backmatter = (:)

  for id in site.nodes.keys().sorted() {
    backmatter.insert(id, (
      contexts: contexts.at(id, default: (:)),
      backlinks: backlinks.at(id, default: (:)),
      related: _collect-related(site, id),
      references: _collect-references(site, id, ()),
      revisions: revisions.at(id),
    ))
  }

  backmatter
}

// Rendering helpers

// The "(version n; latest)" label text — shared so the title badge, the in-body
// callout, and backmatter links never drift in wording.
#let version-label(version, is-latest) = (
  "(version " + str(version) + if is-latest { "; latest" } else { "" } + ")"
)

// The version postfix span appended after a node's title wherever it is linked,
// so a versioned note is always legible as such. Nothing for a note outside a
// chain. The caller picks the span class for its visual context.
#let render-version-tag(revisions, class: "revision-version-tag") = {
  if revisions.in-chain {
    [ #html.span(class: class)[#version-label(revisions.version, revisions.is-latest)]]
  }
}

// In-body revision callout: the prominent lineage display for a note that is one
// of several versions, shown at the top of every version. Two versions render as
// directional prose; three or more as a list (one row per version, the viewed
// one flagged "current", the newest tagged "latest"). The `is-latest` /
// `is-superseded` class drives the state styling (info vs caution).
//
// `member-nodes` maps each id in `revisions.members` to its node record; the
// caller resolves them, so that `site` is not passed in.
#let render-revision-callout(id, revisions, member-nodes) = {
  let members = revisions.members

  let version-tag(version, is-latest) = html.span(class: "revision-version-tag")[#version-label(version, is-latest)]

  let body = if members.len() == 2 {
    if revisions.is-latest {
      let previous-node = member-nodes.at(revisions.previous)
      [This note supersedes an #node-link(previous-node, body: [earlier version]).]
    } else {
      let latest-node = member-nodes.at(revisions.latest)
      [This note has been superseded by the #node-link(latest-node, body: [current version]).]
    }
  } else {
    let item(index, member) = {
      let version = index + 1
      let member-node = member-nodes.at(member)
      let is-self = member == id
      let is-latest = member == revisions.latest
      let item-class = "revision-version-item" + if is-self { " current" } else { "" }
      html.li(class: item-class)[
        #html.span(class: "revision-version-body")[#if is-self {
          html.span(class: "revision-version-title")[#member-node.title]
        } else {
          node-link(member-node)
        }
        #version-tag(version, is-latest)]
        #if is-self [#html.span(class: "revision-version-here")[current]]
      ]
    }
    [
      This note has several versions:
      #html.ol(class: "revision-version-list")[
        #members.enumerate().map(((index, member)) => item(index, member)).join()
      ]
    ]
  }

  let state-class = if revisions.is-latest { "is-latest" } else { "is-superseded" }
  html.aside(class: "revision-callout " + state-class, role: "note")[#body]
}

// `entries` is an array of `(node, revisions)` records in display order; the
// caller resolves them from `site` and the backmatter, so that neither is
// passed in (each call would re-hash them).
#let render-backmatter-section(title, kind, entries) = {
  if entries.len() > 0 {
    html.section(class: ("backmatter-section", kind))[
      #html.h3(class: "backmatter-section-title")[#title]
      #html.ul(class: "node-list")[
        #entries.map(entry => {
          html.li(class: "node-list-item")[
            #node-link(entry.node, body: [#entry.node.title#render-version-tag(entry.revisions)])
          ]
        }).join()
      ]
    ]
  }
}
