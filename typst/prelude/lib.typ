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

#import "math.typ": (
  diagram,
  edge,
  fletchereq,
  piType,
  sigmaType,
  constant,
  induction,
  recursion,
  inductionNN,
  recursionNN,
  unitType,
  inductionUnit,
  induction1,
  recursionUnit,
  recursion1,
  emptyType,
  inductionEmpty,
  induction0,
  recursionEmpty,
  recursion0,
  inject,
  inject1,
  inject2,
  pair,
  project,
  project1,
  project2,
  Bool,
  btrue,
  bfalse,
  refl,
  concat,
  ap,
  tr,
  apd,
  Decision,
  decide,
  IsEquivalence,
  HasInverse,
  resolve,
  resolve1,
  resolve2,
  swap,
  Equal,
  EqualSigma,
  IsContractible,
  Contractible,
  IsProposition,
  Proposition,
  IsSet,
  Set,
  IsTruncated,
  Truncated,
  evaluate,
  Lift,
  lift,
  Fiber,
  total,
  uncurry,
  curry,
  distance,
  IsEmbedding,
  proof,
  lemma,
  corollary,
  theorem,
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
      group: ids,
      group-index: index,
    ))
    .join()
}
