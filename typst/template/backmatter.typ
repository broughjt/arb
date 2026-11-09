#import "node.typ": node-link

// The "(version n; latest)" label text, shared so the title badge, the in-body
// callout, and backmatter links never drift in wording.
#let version-label(version, is-latest) = (
  "(version " + str(version) + if is-latest { "; latest" } else { "" } + ")"
)

// The version postfix span appended after a node's title wherever it is linked,
// so a versioned note is always legible as such. `version` is a template node's
// `version`, and nothing renders for `none`. The caller picks the span class
// for its visual context.
#let render-version-tag(version, class: "revision-version-tag") = {
  if version != none {
    [ #html.span(class: class)[#version-label(version.number, version.is-latest)]]
  }
}

// In-body revision callout: the prominent display of a note's versions, shown
// at the top of every version. Two versions render as
// directional prose; three or more as a list (one row per version, the viewed
// one flagged "current", the newest tagged "latest"). The `is-latest` /
// `is-superseded` class drives the state styling (info vs caution). `node` is
// the template node of the page, and `versions` is its node page's `versions`.
#let render-revision-callout(node, versions) = {
  let is-latest = node.version.is-latest

  let version-tag(version, is-latest) = html.span(class: "revision-version-tag")[#version-label(version, is-latest)]

  let body = if versions.len() == 2 {
    if is-latest {
      [This note supersedes an #node-link(versions.first(), body: [earlier version]).]
    } else {
      [This note has been superseded by the #node-link(versions.last(), body: [current version]).]
    }
  } else {
    let item(index, member) = {
      let version = index + 1
      let is-self = version == node.version.number
      let is-latest = member.version.is-latest
      let item-class = "revision-version-item" + if is-self { " current" } else { "" }
      html.li(class: item-class)[
        #html.span(class: "revision-version-body")[#if is-self {
          html.span(class: "revision-version-title")[#member.title]
        } else {
          node-link(member)
        }
        #version-tag(version, is-latest)]
        #if is-self [#html.span(class: "revision-version-here")[current]]
      ]
    }
    [
      This note has several versions:
      #html.ol(class: "revision-version-list")[
        #versions.enumerate().map(((index, member)) => item(index, member)).join()
      ]
    ]
  }

  let state-class = if is-latest { "is-latest" } else { "is-superseded" }
  html.aside(class: "revision-callout " + state-class, role: "note")[#body]
}

// `nodes` are template nodes in display order.
#let render-backmatter-section(title, kind, nodes) = {
  if nodes.len() > 0 {
    html.section(class: ("backmatter-section", kind))[
      #html.h3(class: "backmatter-section-title")[#title]
      #html.ul(class: "node-list")[
        #nodes.map(node => {
          html.li(class: "node-list-item")[
            #node-link(node, body: [#node.title#render-version-tag(node.version)])
          ]
        }).join()
      ]
    ]
  }
}
