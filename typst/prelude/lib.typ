// Public authoring API for arb notes.

#import "@local/arb-core:0.1.0": (
  node,
  subnode,
  link-node,
  transclude,
)

#import "citations.typ": (
  cite-node,
  cite-nodes,
  cite-node-with,
  cite-node-prose,
  cite-node-author,
  cite-node-bare,
  cite-node-year,
  cite-node-full,
  cite-node-p,
  cite-node-a,
  cite-node-b,
  cite-node-y,
  cite-node-f,
)

#import "theorems.typ": (
  lemma,
  theorem,
  corollary,
  proof,
)

// Group several theorem-like references together. Example: `#link-nodes("0079",
// "008j")` renders as "Lemmas 0079 and 008j".
#let link-nodes(..arguments) = {
  if arguments.named().len() > 0 {
    panic("link-nodes accepts only positional node ids")
  }

  let ids = arguments.pos()
  if ids.len() == 0 {
    panic("link-nodes requires at least one node id")
  }

  ids
    .enumerate()
    .map(((index, id)) => link-node(
      id,
      kind: "group",
      group: ids,
      group-index: index,
    ))
    .join()
}
