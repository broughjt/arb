// Public authoring prelude for bundle notes.
//
// Note source files import this module to get the small, stable surface for
// defining nodes, links, transclusions, and citations. Rendering code lives
// in `template.typ` instead.

#import "@local/arb-core:0.1.0": (
  node,
  subnode,
  link-node,
  transclude,
)

#import "link.typ": link-nodes

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

// The `at` locator namespace: `at.definition("1.1.9")`, `at.exercise("9.7(a)")`,
// etc. Re-exported as a module so authors get `at.<label>(..)` call syntax.
#import "locators.typ" as at

#import "math.typ": *
