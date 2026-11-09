// TODO: Should we consider other short syntax that doesn't play this weird game with modules? Like `at-page` or something?

// The `at` namespace: locator constructors for citations.
//
// Imported and re-exported as `at` by `note.typ`, so authors write
// `at.definition("1.1.9")`, `at.exercise("9.7(a)")`, etc. Each returns a
// `(locator:, locator-label:)` dict consumed by the `cite-node*` family (as the
// positional locator-spec) and by theorem-head `citation:` dicts (spread:
// `(key: "k", ..at.exercise("9.2.9"))`).
//
// This must be a *module* rather than a dictionary: Typst forbids calling a
// value stored under a dictionary key as a function (`d.foo(..)` errors), but
// module field access (`at.foo(..)`) is allowed. Module members are static
// `#let`s, so they can't be generated from `library/citations.typ`'s
// `_locator-labels`; `citations.typ` asserts the two key sets match to catch
// drift.

#let page(loc) = (locator: loc, locator-label: "page")
#let theorem(loc) = (locator: loc, locator-label: "theorem")
#let proposition(loc) = (locator: loc, locator-label: "proposition")
#let lemma(loc) = (locator: loc, locator-label: "lemma")
#let corollary(loc) = (locator: loc, locator-label: "corollary")
#let definition(loc) = (locator: loc, locator-label: "definition")
#let example(loc) = (locator: loc, locator-label: "example")
#let remark(loc) = (locator: loc, locator-label: "remark")
#let exercise(loc) = (locator: loc, locator-label: "exercise")
#let appendix(loc) = (locator: loc, locator-label: "appendix")
#let chapter(loc) = (locator: loc, locator-label: "chapter")
#let section(loc) = (locator: loc, locator-label: "section")
#let equation(loc) = (locator: loc, locator-label: "equation")
#let figure(loc) = (locator: loc, locator-label: "figure")
#let table(loc) = (locator: loc, locator-label: "table")
