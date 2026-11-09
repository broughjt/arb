// Small native-math/theorem helper layer for notes.

#import "@preview/fletcher:0.5.8" as fletcher
#import "citations.typ": cite-nodes, normalize-citation-item

#let diagram = fletcher.diagram
#let edge = fletcher.edge
// TODO: Find the notes that use it and fix them
#let fletchereq = (..arguments) => edge(..arguments, stroke: 0.5pt, extrude: (-1.5pt, 1.5pt))

#let piType(..arguments) = {
    let inputs = arguments.pos()
    let type = inputs.last()
    let variables = inputs.slice(0, -1).join($, $)

    $scripts(product)_((#variables : #type))$
}

#let sigmaType(..arguments) = {
    let inputs = arguments.pos()
    let type = inputs.last()
    let variables = inputs.slice(0, -1).join($, $)

    $scripts(sum)_((#variables : #type))$
}

#let constant = math.op("const")

#let induction = math.class("normal", math.sans("induction"))
#let recursion = math.class("normal", math.sans("recursion"))

#let inductionNN = $attach(induction, br: NN)$
#let recursionNN = $attach(recursion, br: NN)$

#let unitType = $bold(1)$
#let inductionUnit = $attach(induction, br: unitType)$
#let induction1 = inductionUnit
#let recursionUnit = $attach(recursion, br: unitType)$
#let recursion1 = recursionUnit

#let emptyType = $bold(0)$
#let inductionEmpty = $attach(induction, br: emptyType)$
#let induction0 = inductionEmpty
#let recursionEmpty = $attach(recursion, br: emptyType)$
#let recursion0 = recursionEmpty

#let inject = math.class("normal", math.sans("inject"))
#let inject1 = $inject_(1)$
#let inject2 = $inject_(2)$

#let pair = math.class("normal", math.sans("pair"))
#let project = math.class("normal", math.sans("project"))
#let project1 = $project_(1)$
#let project2 = $project_(2)$

#let Bool = $bold(2)$
#let btrue = math.class("normal", math.sans("true"))
#let bfalse = math.class("normal", math.sans("false"))

#let refl = math.class("normal", math.sans("refl"))

#let concat = math.class("normal", math.sans("concat"))

#let ap = math.class("normal", math.sans("ap"))
#let tr = math.class("normal", math.sans("tr"))
#let apd = math.class("normal", math.sans("apd"))

#let Decision = math.class("normal", math.sans("Decision"))
#let decide = math.class("normal", math.sans("decide"))

#let IsEquivalence = math.class("normal", math.sans("IsEquivalence"))
#let HasInverse = math.class("normal", math.sans("HasInverse"))

#let resolve = math.class("normal", math.sans("resolve"))
#let resolve1 = $resolve_(1)$
#let resolve2 = $resolve_(2)$

#let swap = math.class("normal", math.sans("swap"))

#let Equal = math.class("normal", math.sans("Equal"))
#let EqualSigma = $Equal_(Sigma)$

#let IsContractible = math.class("normal", math.sans("IsContractible"))
#let Contractible = math.class("normal", math.sans("Contractible"))
#let IsProposition = math.class("normal", math.sans("IsProposition"))
#let Proposition = math.class("normal", math.sans("Proposition"))
#let IsSet = math.class("normal", math.sans("IsSet"))
#let Set = math.class("normal", math.sans("Set"))
#let IsTruncated = math.class("normal", math.sans("IsTruncated"))
#let Truncated = math.class("normal", math.sans("Truncated"))

#let evaluate = math.class("normal", math.sans("evaluate"))

#let Lift = math.class("normal", math.sans("Lift"))
#let lift = math.class("normal", math.sans("lift"))

#let Fiber = math.class("normal", math.sans("Fiber"))

#let total = math.class("normal", math.sans("total"))

#let uncurry = math.class("normal", math.sans("uncurry"))
#let curry = math.class("normal", math.sans("curry"))

#let distance = math.class("normal", math.sans("distance"))

#let IsEmbedding = math.class("normal", math.sans("IsEmbedding"))

#let proof(body) = {
  html.section(class: "proof")[
    #html.header(class: "proof-header")[Proof.]
    #html.div(class: "proof-body")[#body]
    #html.span(class: "qed", aria-hidden: true)
  ]
}

// Normalize a frame `citation:` argument -- a key string, a citation dict, or an
// array of either -- into bare-form citation items. Bareness is forced here: the
// theorem head supplies its own parentheses, so the citations must not bring
// Chicago's.
#let citation-items(citation) = {
  let items = if type(citation) == array { citation } else { (citation,) }
  items.map(item => normalize-citation-item(item) + (form: "bare"))
}

#let make-frame(identifier, kind) = {
  // The parenthetical note after the head: `(name; citation)`, either part
  // optional. `citation` cites where the stated result comes from;
  // `cite-prefix` (e.g. "see", "cf.") conventionally marks an adapted or
  // paraphrased statement rather than a verbatim one. The parentheses and the "; "
  // separator are presentation (CSS-generated, see 09-theorems.css); the
  // prefix is content because it changes meaning.
  let frame(label: none, name: none, citation: none, cite-prefix: none, body) = {
    let prefix = if label != none { kind + " " + label } else { kind }
    let cited-items = if citation != none { citation-items(citation) } else { () }
    let note-parts = ()

    if name != none {
      note-parts.push(html.span(class: "theorem-name")[#name])
    }
    if cited-items.len() > 0 {
      let cluster = cite-nodes(..cited-items)
      let cited = if cite-prefix != none { [#cite-prefix #cluster] } else { cluster }
      note-parts.push(html.span(class: "theorem-citation")[#cited])
    }

    html.article(class: ("theorem", "theorem-" + identifier))[
      #html.header(class: "theorem-header")[
        #html.span(class: "theorem-label")[#prefix]
        #if note-parts.len() > 0 {
          [ #html.span(class: "theorem-note")[#note-parts.join()]]
        }
      ]
      #html.div(class: "theorem-body")[#body]
    ]
  }

  frame
}

#let lemma = make-frame("lemma", "Lemma")
#let corollary = make-frame("corollary", "Corollary")
#let theorem = make-frame("theorem", "Theorem")
