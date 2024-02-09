import nakai/html/attrs.{Attr}

pub fn click(script: String) -> Attr(a) {
  Attr(name: "onclick", value: script)
}
