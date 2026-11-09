// Rendering of the prelude's theorem frames and proofs, which reach the
// template as `metadata` (see the prelude's theorems.typ).

// The theorem-like kinds and their words, used in theorem headings and in
// body-less references to theorem-like nodes ("Lemma 0079", "Lemmas 0079 and
// 008j").
#let theorem-kinds = (
  "lemma": (singular: "Lemma", plural: "Lemmas"),
  "theorem": (singular: "Theorem", plural: "Theorems"),
  "corollary": (singular: "Corollary", plural: "Corollaries"),
)

// A theorem frame: a heading with the kind word and an optional parenthetical
// note, `(name; citation)`. The parentheses and the "; " are CSS-generated
// (09-theorems.css).
#let render-theorem(theorem) = {
  let note-parts = ()
  if theorem.name != none {
    note-parts.push(html.span(class: "theorem-name")[#theorem.name])
  }
  if theorem.citation != none {
    note-parts.push(html.span(class: "theorem-citation")[#theorem.citation])
  }

  html.article(class: ("theorem", "theorem-" + theorem.kind))[
    #html.header(class: "theorem-header")[
      #html.span(class: "theorem-label")[#theorem-kinds.at(theorem.kind).singular]
      #if note-parts.len() > 0 {
        [ #html.span(class: "theorem-note")[#note-parts.join()]]
      }
    ]
    #html.div(class: "theorem-body")[#theorem.body]
  ]
}

#let render-proof(body) = {
  html.section(class: "proof")[
    #html.header(class: "proof-header")[Proof.]
    #html.div(class: "proof-body")[#body]
    #html.span(class: "qed", aria-hidden: true)
  ]
}
