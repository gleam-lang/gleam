





// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// * THIS FILE IS GENERATED. DO NOT EDIT IT.                                             *
// * You're probably looking for ./codegen/attrs_prelude.gleam, or ./codegen/attrs.json. *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *





pub type Attr(a) {
  Attr(name: String, value: String)
  Event(name: String, action: a)
}

pub fn data(name: String, value: String) -> Attr(a) {
  Attr(name: "data-" <> name, value: value)
}

pub fn accept(value: String) -> Attr(a) {
  Attr(name: "accept", value: value)
}

pub fn accept_charset(value: String) -> Attr(a) {
  Attr(name: "accept-charset", value: value)
}

pub fn action(value: String) -> Attr(a) {
  Attr(name: "action", value: value)
}

pub fn alt(value: String) -> Attr(a) {
  Attr(name: "alt", value: value)
}

pub fn async() -> Attr(a) {
  Attr(name: "async", value: "true")
}

pub fn autocapitalize(value: String) -> Attr(a) {
  Attr(name: "autocapitalize", value: value)
}

pub fn autocomplete(value: String) -> Attr(a) {
  Attr(name: "autocomplete", value: value)
}

pub fn autofocus() -> Attr(a) {
  Attr(name: "autofocus", value: "true")
}

pub fn autoplay() -> Attr(a) {
  Attr(name: "autoplay", value: "true")
}

pub fn capture(value: String) -> Attr(a) {
  Attr(name: "capture", value: value)
}

pub fn charset(value: String) -> Attr(a) {
  Attr(name: "charset", value: value)
}

pub fn checked() -> Attr(a) {
  Attr(name: "checked", value: "true")
}

pub fn cite(value: String) -> Attr(a) {
  Attr(name: "cite", value: value)
}

pub fn class(value: String) -> Attr(a) {
  Attr(name: "class", value: value)
}

pub fn content(value: String) -> Attr(a) {
  Attr(name: "content", value: value)
}

pub fn contenteditable() -> Attr(a) {
  Attr(name: "contenteditable", value: "true")
}

pub fn crossorigin() -> Attr(a) {
  Attr(name: "crossorigin", value: "true")
}

pub fn defer() -> Attr(a) {
  Attr(name: "defer", value: "true")
}

pub fn disabled() -> Attr(a) {
  Attr(name: "disabled", value: "true")
}

pub fn draggable() -> Attr(a) {
  Attr(name: "draggable", value: "true")
}

pub fn for(value: String) -> Attr(a) {
  Attr(name: "for", value: value)
}

pub fn formaction(value: String) -> Attr(a) {
  Attr(name: "formaction", value: value)
}

pub fn height(value: String) -> Attr(a) {
  Attr(name: "height", value: value)
}

pub fn href(value: String) -> Attr(a) {
  Attr(name: "href", value: value)
}

pub fn http_equiv(value: String) -> Attr(a) {
  Attr(name: "http-equiv", value: value)
}

pub fn id(value: String) -> Attr(a) {
  Attr(name: "id", value: value)
}

pub fn integrity(value: String) -> Attr(a) {
  Attr(name: "integrity", value: value)
}

pub fn lang(value: String) -> Attr(a) {
  Attr(name: "lang", value: value)
}

pub fn loop() -> Attr(a) {
  Attr(name: "loop", value: "true")
}

pub fn method(value: String) -> Attr(a) {
  Attr(name: "method", value: value)
}

pub fn name(value: String) -> Attr(a) {
  Attr(name: "name", value: value)
}

pub fn placeholder(value: String) -> Attr(a) {
  Attr(name: "placeholder", value: value)
}

pub fn preload() -> Attr(a) {
  Attr(name: "preload", value: "true")
}

pub fn property(value: String) -> Attr(a) {
  Attr(name: "property", value: value)
}

pub fn readonly() -> Attr(a) {
  Attr(name: "readonly", value: "true")
}

pub fn rel(value: String) -> Attr(a) {
  Attr(name: "rel", value: value)
}

pub fn selected() -> Attr(a) {
  Attr(name: "selected", value: "true")
}

pub fn src(value: String) -> Attr(a) {
  Attr(name: "src", value: value)
}

pub fn style(value: String) -> Attr(a) {
  Attr(name: "style", value: value)
}

pub fn tabindex(value: String) -> Attr(a) {
  Attr(name: "tabindex", value: value)
}

pub fn target(value: String) -> Attr(a) {
  Attr(name: "target", value: value)
}

pub fn title(value: String) -> Attr(a) {
  Attr(name: "title", value: value)
}

pub fn type_(value: String) -> Attr(a) {
  Attr(name: "type", value: value)
}

pub fn value(value: String) -> Attr(a) {
  Attr(name: "value", value: value)
}

pub fn width(value: String) -> Attr(a) {
  Attr(name: "width", value: value)
}
