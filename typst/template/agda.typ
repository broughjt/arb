#import "model.typ": node-href, node-taxon

// A token is plain text, a span, or a link to the definition of the name it
// refers to. `module-nodes` maps a module's name to the id of its module node,
// whose page transcludes every block of the module, so that each definition
// has an anchor there. A definition in a module without a node (a builtin, say)
// gets a `data-agda-def` attribute instead, from which the popover finds its
// type.
#let render-agda-token(module-nodes, token) = {
  let classes = token.at("a", default: ())
  let attrs = if classes.len() > 0 { (class: classes.join(" ")) } else { (:) }
  let definition = token.at("d", default: none)

  // Bound variables are not linked: they have no type to show, and a node that
  // transcludes only part of a module might not contain their binder.
  if definition == none or "Bound" in classes {
    return if attrs.len() == 0 { token.s } else {
      html.elem("span", attrs: attrs, token.s)
    }
  }

  // Positions are unique only within a module, and a page can hold blocks of
  // several modules. A position of 1 refers to the module itself.
  let (module, position) = definition
  let anchor = module + "-" + str(position)
  if token.at("def", default: false) {
    attrs.insert("id", anchor)
  }

  let node-id = module-nodes.at(module, default: none)
  if node-id != none {
    let fragment = if position > 1 { "#" + anchor }
    attrs.insert("href", node-href(node-id) + fragment)
    html.elem("a", attrs: attrs, token.s)
  } else {
    if position > 1 {
      attrs.insert("data-agda-def", anchor)
    }
    html.elem("span", attrs: attrs, token.s)
  }
}

// Returns a dictionary from module name to a dictionary from the trimmed text
// of each of the module's blocks to its rendered content.
//
// Blocks are rendered here, once each, and not in the show rule, because the
// result is passed to every page's render: Typst hashes content cheaply, but
// would hash the token dictionaries structurally at each call.
//
// This is a separate function from `collect-agda-blocks` so that it is
// memoized on its two small arguments. That function reruns whenever the site
// changes, but `module-nodes` rarely changes with it. It takes the path, not
// the decoded annotations, because those are too large to hash.
#let _render-agda-blocks(annotations-path, module-nodes) = {
  let modules = (:)
  for (name, annotations) in json(annotations-path) {
    let blocks = (:)
    for tokens in annotations.blocks {
      let text = tokens.map(token => token.s).join().trim()
      if text in blocks {
        panic(
          "the Agda module " + name + " has two blocks with the same text, "
            + "and blocks are matched with their annotations by text",
        )
      }
      blocks.insert(text, html.pre(class: "Agda", html.code(
        tokens.map(token => render-agda-token(module-nodes, token)).join(),
      )))
    }
    modules.insert(name, blocks)
  }
  modules
}

// Returns a dictionary from the id of each node in an Agda module to the
// rendered blocks of that module, keyed by their text. A node belongs to the
// module whose source file it is in.
#let collect-agda-blocks(site, annotations-path) = {
  let path-modules = (:)
  for (name, annotations) in json(annotations-path) {
    path-modules.insert(annotations.path, name)
  }

  let node-modules = (:)
  let module-nodes = (:)
  for (id, node) in site.nodes {
    let module = path-modules.at(node.path, default: none)
    if module == none { continue }
    node-modules.insert(id, module)
    if node-taxon(node) == "module" {
      if module in module-nodes {
        panic(
          "two module nodes in " + node.path + ": "
            + module-nodes.at(module) + " and " + id,
        )
      }
      module-nodes.insert(module, id)
    }
  }
  for name in path-modules.values() {
    if name not in module-nodes {
      panic("no module node for the Agda module " + name)
    }
  }

  let modules = _render-agda-blocks(annotations-path, module-nodes)
  node-modules.pairs().map(((id, module)) => (id, modules.at(module))).to-dict()
}

// The body of the show rule for Agda raw blocks, given the blocks of the
// enclosing node. A block without annotations is left as it is, which happens
// when the annotations are older than the source.
#let render-agda-block(blocks, it) = blocks.at(it.text.trim(), default: it)
