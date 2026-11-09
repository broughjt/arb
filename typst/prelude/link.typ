#import "@local/arb-core:0.1.0": link-node

// Group several theorem-like references under one shared (pluralized) kind word,
// e.g. `#link-nodes("0079", "008j")` -> "Lemmas 0079 and 008j". Like `cite-nodes`,
// it emits one link marker per id (so each target keeps its own graph
// edge/backlink) carrying the ordered group, and `render-link` coordinates the
// display at `group-index == 0`, through `render-node-ref-group` below. A one-id
// group renders as the singular reference form.
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
      ref-group: true,
      group: ids,
      group-index: index,
    ))
    .join()
}
