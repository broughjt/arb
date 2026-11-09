#import "citations.typ": cite-nodes

// The citations in a theorem's heading are bare, since the heading supplies its
// own parentheses.
#let citation-items(citation) = {
  let items = if type(citation) == array {
    citation
  } else {
    (citation,)
  }
  items.map(item => {
    let item = if type(item) == str { (key: item) } else { item }
    item + (form: "bare")
  })
}

#let frame(kind) = (name: none, citation: none, body) => metadata((
  theorem: (
    kind: kind,
    name: name,
    citation: if citation == none {
      none
    } else {
      cite-nodes(..citation-items(citation))
    },
    body: body,
  ),
))

#let lemma = frame("lemma")
#let theorem = frame("theorem")
#let corollary = frame("corollary")

#let proof(body) = metadata((proof: body))
