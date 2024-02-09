import nakai/html.{Element, LeafElement, Node}
import nakai/html/attrs.{Attr}

pub fn slot(attrs: List(Attr(a))) -> Node(a) {
  LeafElement(tag: "slot", attrs: attrs)
}

pub fn template(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "template", attrs: attrs, children: children)
}
