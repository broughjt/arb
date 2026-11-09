// Agda token annotations: syntax highlighting, definition links, and the
// anchors the hover client keys off (see .scratch/agda-annotations.md).
//
// This is user-layer presentation code: weibian-core knows nothing about code
// blocks. All resolution is static -- a pure function of the `agda-annotations`
// input (produced by agda-annotate during the weibian build) and the site
// graph -- so focus and full builds render identically and no introspection is
// involved.
//
// - Fences match their token lists by *text within their module*: the show
//   rule knows the node it renders inside, node -> module comes from the
//   node's core-recorded source `path` (annotation entries carry their
//   module's source path; weibian invokes agda-annotate from the repository
//   root, so the paths coincide with sources.json entries), and the fence's
//   trimmed `raw.text` is looked up in that module's table. Matching is per
//   module because identical fences legitimately recur across modules (a
//   Scratch predecessor and its successor share code). A fence with no entry
//   (annotations stale after an edit, a fence outside any annotated module,
//   or the input absent entirely) falls back to today's plain rendering.
// - Definition links resolve through *module nodes* (each literate source's
//   unique taxon-"module" node): a definition site `[module, offset]` becomes
//   `<module-node-id>.html#<module>-<offset>`. The module node's page
//   transcludes every fence of its module in source order, so the anchor is
//   always present there; this matches the navigation semantics of
//   `agda --html` and 1lab. Anchors are emitted on every rendered copy of a
//   fence (each page contains a given fence at most once in practice).
// - Definition sites in modules outside the corpus (builtins such as
//   Agda.Primitive) render as unlinked spans; their link policy is an open
//   decision.
//
// Annotation offsets are 1-based character offsets into the literate source
// file, matching the anchors Agda's own HTML backend emits.

#import "node.typ": node-href, node-taxon

// The anchor id / URL fragment for a definition site. Module-prefixed because
// one page can carry fences from several modules (via transclusion), and
// offsets are only unique within a module.
#let agda-anchor(module, position) = module + "-" + str(position)

// One token: plain text for unannotated runs (whitespace between tokens); an
// anchored/linked <a> for identifiers whose definition site has a page in the
// corpus; a <span data-agda-def> for identifiers defined outside it (builtins
// such as Agda.Primitive -- no page to link, but the hover client resolves
// the attribute against the module's types file); a plain classed <span>
// otherwise. Definition occurrences (`def`) carry the anchor id and, like
// Agda's own backend, still link to themselves. `modules` is the module
// name -> module-node id table.
//
// Bound variables are left unlinked entirely (settled 2026-07-03): they
// render in plain ink, binder offsets have no types entry so hovering shows
// nothing, and same-page binder anchors would dangle on subnode pages that
// transclude only part of a module. If per-occurrence types ever arrive
// (Verso-level; see .scratch/agda-annotations.md), links and binder ids can
// return with actual payload.
#let render-agda-token(modules, token) = {
  let classes = token.at("a", default: ())
  let definition-site = token.at("d", default: none)
  if classes.len() == 0 and definition-site == none { return token.s }

  let attrs = (:)
  if classes.len() > 0 {
    attrs.insert("class", classes.join(" "))
  }
  if definition-site != none and not classes.contains("Bound") {
    let (module, position) = (definition-site.at(0), definition-site.at(1))
    if token.at("def", default: false) {
      attrs.insert("id", agda-anchor(module, position))
    }
    let node-id = modules.at(module, default: none)
    if node-id != none {
      // Positions <= 1 name the module itself: link to the page, no fragment.
      let href = node-href(node-id)
      if position > 1 { href = href + "#" + agda-anchor(module, position) }
      attrs.insert("href", href)
      return html.elem("a", attrs: attrs, token.s)
    }
    // Positions <= 1 name the module itself, which has no type to show.
    if position > 1 {
      attrs.insert("data-agda-def", agda-anchor(module, position))
    }
  }
  html.elem("span", attrs: attrs, token.s)
}

#let render-agda-fence(modules, tokens) = html.elem(
  "pre",
  attrs: (class: "Agda"),
  html.elem(
    "code",
    tokens.map(token => render-agda-token(modules, token)).join(),
  ),
)

// module name -> (fence text -> *rendered content*), for the annotations at
// `input-path` and the module name -> module-node id table `modules`.
//
// A separate function from `agda-context` so that its call is memoized on
// these two small arguments alone. `agda-context` depends on the whole site
// and reruns whenever the graph changes (a new link, say), but `modules`
// rarely changes then, so this call, the expensive part, is reused. It takes
// the input's path rather than the decoded annotations, which are far too
// large to hash at every compile; the `read` is tracked, so a changed file
// still invalidates it.
#let _agda-fences(input-path, modules) = {
  // module name -> (fence text -> *rendered content*). Keys are trimmed on
  // both sides of the join: Agda's block text keeps the trailing newline,
  // `raw.text` does not.
  //
  // Rendering happens here, once per fence, not in the show rule: this table
  // rides along every document render, and typst content is
  // pre-hashed while plain token dicts are structurally re-hashed at each
  // memoization boundary -- threading the raw ~250k-value token tables
  // through the renders stalled the build (measured 6.4s -> 76s). Rendering once
  // also means a transcluded fence is rendered once, not once per copy.
  let annotations = json(read(input-path, encoding: none))
  let fences = (:)
  for (name, entry) in annotations {
    let table = (:)
    for tokens in entry.blocks {
      let text = tokens.map(token => token.s).join()
      if text == none { continue }
      text = text.trim()
      if text in table {
        panic(
          "identical Agda fence text twice in module `" + name
            + "`; fence-to-annotation matching is by text within a module"
            + " and needs it unique",
        )
      }
      table.insert(text, render-agda-fence(modules, tokens))
    }
    fences.insert(name, table)
  }

  fences
}

// Build the rendering context from the `agda-annotations` input and the site
// graph, or `none` when the input is absent (plain rendering everywhere).
// Called once in site.typ; template.typ's rendering closures capture the
// result (see `site-callbacks`). Node-to-module
// mapping comes straight off each node's core-recorded source `path`.
#let agda-context(site, input-path) = {
  if input-path == none { return none }
  let annotations = json(read(input-path, encoding: none))

  // source path -> module name.
  let path-modules = (:)
  for (name, entry) in annotations {
    path-modules.insert(entry.path, name)
  }

  // node id -> module name, and module name -> module-node id (the module
  // source's unique taxon-"module" node).
  let node-modules = (:)
  let modules = (:)
  for (id, node) in site.nodes {
    let module = path-modules.at(node.path, default: none)
    if module == none { continue }
    node-modules.insert(id, module)
    if node-taxon(node) == "module" {
      if module in modules {
        panic(
          "two module-taxon nodes in " + node.path + ": "
            + modules.at(module) + " and " + id,
        )
      }
      modules.insert(module, id)
    }
  }
  for (name, _entry) in annotations {
    if name not in modules {
      panic("no module node found for Agda module `" + name + "`")
    }
  }

  let fences = _agda-fences(input-path, modules)

  (node-modules: node-modules, fences: fences)
}

// The fence table for the module containing node `node-id`, or `none` when
// there is no Agda context or the node is not in an Agda module. Call it once
// per page, not per fence: each call hashes the whole context.
#let agda-fences(agda, node-id) = {
  if agda == none { return none }
  let module = agda.node-modules.at(node-id, default: none)
  if module == none { return none }
  agda.fences.at(module)
}

// The `show raw.where(lang: "agda")` body. template.typ installs it with the
// fence table of the enclosing node's module (`agda-fences`), not the whole
// context, which would be re-hashed at every fence. Pure lookup: the content
// was rendered at context construction.
#let render-agda-block(fences, it) = {
  if fences == none { return it }
  fences.at(it.text.trim(), default: it)
}
