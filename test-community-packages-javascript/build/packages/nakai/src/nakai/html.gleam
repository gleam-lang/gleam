





// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// * THIS FILE IS GENERATED. DO NOT EDIT IT.                                             *
// * You're probably looking for ./codegen/html_prelude.gleam, or ./codegen/html.json.   *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *





import nakai/html/attrs.{Attr}

pub type Node(a) {
  /// Can be used anywhere in the document, and will set the doctype of the document
  /// being rendered. Usually not necessary, as documents have a default of `<!DOCTYPE html>`.
  /// ## Example
  /// ```gleam
  /// html.Doctype("html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\"")
  /// ```
  Doctype(content: String)
  /// Used for setting attributes on the root `<html>` element of the document. Children
  /// will be rendered in-place, equivalent to using `html.Fragment(children)`.
  /// ## Example
  /// ```gleam
  /// html.Html([attrs.lang("en-US")], [
  ///   ...
  /// ])
  /// ```
  Html(attrs: List(Attr(a)), children: List(Node(a)))
  /// Used for placing content in the `<head>` of the document. Useful for elements like
  /// `<meta>`, `<title>`, `<link>`, etc.
  /// ## Example
  /// ```gleam
  /// html.Fragment([
  ///   html.Head([
  ///     html.title("List of puppies")
  ///   ]),
  ///   html.div([], [
  ///     ...
  ///   ])
  /// ])
  /// ```
  Head(children: List(Node(a)))
  /// Used for setting attributes on the `<body>` element of the document. Children
  /// will be rendered in-place, equivalent to using `html.Fragment(children)`.
  /// ## Example
  /// ```gleam
  /// html.Body([attrs.class("dark-mode")], [
  ///   ...
  /// ])
  /// ```
  Body(attrs: List(Attr(a)), children: List(Node(a)))
  /// An "transparent" container that will render it's children, but does not add anything
  /// itself to the document. If you've ever used `React.Fragment` or `<>` and `</>` in
  /// JSX/React, this is that.
  /// ## Example
  /// ```gleam
  /// html.ul([], [
  ///   // some puppies are hard-coded
  ///   html.li_text([], "August"),
  ///   // some are loaded from a server
  ///   html.Fragment(puppies_fetched_from_api |> list.map(html.li_text([], _)))
  /// ])
  /// // <ul>
  /// //   <li>August</li>
  /// //   <li>Dot</li>
  /// //   <li>Mody</li>
  /// //   <li>Spot</li>
  /// //   <li>Toby</li>
  /// // </ul>
  /// ```
  Fragment(children: List(Node(a)))
  /// An HTML element. You shouldn't need to reach for this very often, but it can be a
  /// handy escape hatch if there isn't a shorthand function for the element type you need.
  /// ## Example
  /// ```gleam
  /// // bad example, pls use `html.div`
  /// html.Element("div", [], [html.Text("hello, lucy!")])
  /// ```
  Element(tag: String, attrs: List(Attr(a)), children: List(Node(a)))
  /// An HTML element, but that does not have any children, and should be self closing.
  /// Similarly to `Element`, you shouldn't really need this, except as an escape hatch
  /// if there isn't a shorthand function for the element type you need.
  /// ## Example
  /// ```gleam
  /// // bad example, pls use `html.link`
  /// html.LeafElement("link", [attrs.rel("stylesheet"), attrs.href(...)])
  /// ```
  LeafElement(tag: String, attrs: List(Attr(a)))
  /// An HTML comment, which will be included in the document.
  /// ## Example
  /// ```gleam
  /// html.Comment("You've uncovered my secrets!")
  /// // <!-- You've uncovered my secrets! -->
  /// ```
  Comment(content: String)
  /// Some plain text to include in the document. The provided text will be escaped, to
  /// make it safe to include in the document.
  /// ## Example
  /// ```gleam
  /// html.Text("hello, lucy!")
  /// // hello, lucy!
  /// ```
  /// ```gleam
  /// // Time to trust some unvalidated user input! :^)
  /// html.div_text([], "<script>alert('pwned');</script>")
  /// // <div>&lt;script&gt;alert('pwned');&lt;/script&gt;</div>
  /// ```
  Text(content: String)
  /// The dangerous cousin of `Text`. This will render the provided text as-is, without
  /// any santization. Good for things like including some HTML you just generated from
  /// a Markdown file. Bad for things like `$_GET['search']`.
  /// ## Example
  /// ```gleam
  /// html.Text("hello, lucy!")
  /// // hello, lucy!
  /// ```
  /// ```gleam
  /// // Time to trust some unvalidated user input! :^)
  /// html.div([], [html.UnsafeText("<script>alert('pwned');</script>")])
  /// // <div><script>alert('pwned');</script></div>
  /// // Oh no, we just got got! D:
  /// ```
  UnsafeText(content: String)
  /// Add some JavaScript to your page! Scripts will always be inserted at the end of the
  /// page, regardless of where in the document the `Script` node is, so that your content
  /// loads first.
  /// ## Example
  /// ```gleam
  /// html.Script("alert('hello, lucy!')")
  /// ```
  Script(script: String)
  /// Renders absolutely nothing. For when you may or may not have something to render,
  /// and need a way to say "I've got nothing."
  /// ## Example
  /// ```gleam
  /// html.div([], [
  ///   case my_cool_feature {
  ///     Enabled -> super_cool_stuff()
  ///     Disabled -> html.Nothing
  ///   }
  /// ])
  Nothing
}

/// The HTML [`<title>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/title) element
pub fn title(text: String) -> Node(a) {
  Element("title", [], [Text(text)])
}

/// The [HTML `<a>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a)
pub fn a(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "a", attrs: attrs, children: children)
}

/// Shorthand for `html.a(attrs, children: [html.Text(text)])`
pub fn a_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "a", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<abbr>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr)
pub fn abbr(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "abbr", attrs: attrs, children: children)
}

/// Shorthand for `html.abbr(attrs, children: [html.Text(text)])`
pub fn abbr_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "abbr", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<address>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address)
pub fn address(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "address", attrs: attrs, children: children)
}

/// Shorthand for `html.address(attrs, children: [html.Text(text)])`
pub fn address_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "address", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<area />` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/area)
pub fn area(attrs: List(Attr(a))) -> Node(a) {
  LeafElement(tag: "area", attrs: attrs)
}

/// The [HTML `<article>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article)
pub fn article(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "article", attrs: attrs, children: children)
}

/// Shorthand for `html.article(attrs, children: [html.Text(text)])`
pub fn article_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "article", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<aside>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside)
pub fn aside(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "aside", attrs: attrs, children: children)
}

/// Shorthand for `html.aside(attrs, children: [html.Text(text)])`
pub fn aside_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "aside", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<audio>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio)
pub fn audio(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "audio", attrs: attrs, children: children)
}

/// Shorthand for `html.audio(attrs, children: [html.Text(text)])`
pub fn audio_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "audio", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<b>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b)
pub fn b(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "b", attrs: attrs, children: children)
}

/// Shorthand for `html.b(attrs, children: [html.Text(text)])`
pub fn b_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "b", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<base />` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/base)
pub fn base(attrs: List(Attr(a))) -> Node(a) {
  LeafElement(tag: "base", attrs: attrs)
}

/// The [HTML `<bdi>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi)
pub fn bdi(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "bdi", attrs: attrs, children: children)
}

/// Shorthand for `html.bdi(attrs, children: [html.Text(text)])`
pub fn bdi_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "bdi", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<bdo>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo)
pub fn bdo(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "bdo", attrs: attrs, children: children)
}

/// Shorthand for `html.bdo(attrs, children: [html.Text(text)])`
pub fn bdo_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "bdo", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<blockquote>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote)
pub fn blockquote(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "blockquote", attrs: attrs, children: children)
}

/// Shorthand for `html.blockquote(attrs, children: [html.Text(text)])`
pub fn blockquote_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "blockquote", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<br />` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br)
pub fn br(attrs: List(Attr(a))) -> Node(a) {
  LeafElement(tag: "br", attrs: attrs)
}

/// The [HTML `<button>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button)
pub fn button(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "button", attrs: attrs, children: children)
}

/// Shorthand for `html.button(attrs, children: [html.Text(text)])`
pub fn button_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "button", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<canvas>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas)
pub fn canvas(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "canvas", attrs: attrs, children: children)
}

/// Shorthand for `html.canvas(attrs, children: [html.Text(text)])`
pub fn canvas_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "canvas", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<caption>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption)
pub fn caption(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "caption", attrs: attrs, children: children)
}

/// Shorthand for `html.caption(attrs, children: [html.Text(text)])`
pub fn caption_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "caption", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<cite>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite)
pub fn cite(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "cite", attrs: attrs, children: children)
}

/// Shorthand for `html.cite(attrs, children: [html.Text(text)])`
pub fn cite_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "cite", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<code>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code)
pub fn code(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "code", attrs: attrs, children: children)
}

/// Shorthand for `html.code(attrs, children: [html.Text(text)])`
pub fn code_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "code", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<col>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col)
pub fn col(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "col", attrs: attrs, children: children)
}

/// Shorthand for `html.col(attrs, children: [html.Text(text)])`
pub fn col_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "col", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<colgroup>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup)
pub fn colgroup(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "colgroup", attrs: attrs, children: children)
}

/// Shorthand for `html.colgroup(attrs, children: [html.Text(text)])`
pub fn colgroup_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "colgroup", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<data>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/data)
pub fn data(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "data", attrs: attrs, children: children)
}

/// Shorthand for `html.data(attrs, children: [html.Text(text)])`
pub fn data_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "data", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<datalist>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist)
pub fn datalist(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "datalist", attrs: attrs, children: children)
}

/// Shorthand for `html.datalist(attrs, children: [html.Text(text)])`
pub fn datalist_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "datalist", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<dd>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd)
pub fn dd(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "dd", attrs: attrs, children: children)
}

/// Shorthand for `html.dd(attrs, children: [html.Text(text)])`
pub fn dd_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "dd", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<del>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del)
pub fn del(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "del", attrs: attrs, children: children)
}

/// Shorthand for `html.del(attrs, children: [html.Text(text)])`
pub fn del_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "del", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<details>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details)
pub fn details(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "details", attrs: attrs, children: children)
}

/// Shorthand for `html.details(attrs, children: [html.Text(text)])`
pub fn details_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "details", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<dfn>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn)
pub fn dfn(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "dfn", attrs: attrs, children: children)
}

/// Shorthand for `html.dfn(attrs, children: [html.Text(text)])`
pub fn dfn_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "dfn", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<dialog>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dialog)
pub fn dialog(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "dialog", attrs: attrs, children: children)
}

/// Shorthand for `html.dialog(attrs, children: [html.Text(text)])`
pub fn dialog_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "dialog", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<div>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div)
pub fn div(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "div", attrs: attrs, children: children)
}

/// Shorthand for `html.div(attrs, children: [html.Text(text)])`
pub fn div_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "div", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<dl>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl)
pub fn dl(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "dl", attrs: attrs, children: children)
}

/// Shorthand for `html.dl(attrs, children: [html.Text(text)])`
pub fn dl_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "dl", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<dt>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt)
pub fn dt(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "dt", attrs: attrs, children: children)
}

/// Shorthand for `html.dt(attrs, children: [html.Text(text)])`
pub fn dt_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "dt", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<em>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em)
pub fn em(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "em", attrs: attrs, children: children)
}

/// Shorthand for `html.em(attrs, children: [html.Text(text)])`
pub fn em_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "em", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<embed>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed)
pub fn embed(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "embed", attrs: attrs, children: children)
}

/// Shorthand for `html.embed(attrs, children: [html.Text(text)])`
pub fn embed_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "embed", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<fieldset>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset)
pub fn fieldset(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "fieldset", attrs: attrs, children: children)
}

/// Shorthand for `html.fieldset(attrs, children: [html.Text(text)])`
pub fn fieldset_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "fieldset", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<figcaption>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption)
pub fn figcaption(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "figcaption", attrs: attrs, children: children)
}

/// Shorthand for `html.figcaption(attrs, children: [html.Text(text)])`
pub fn figcaption_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "figcaption", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<figure>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure)
pub fn figure(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "figure", attrs: attrs, children: children)
}

/// Shorthand for `html.figure(attrs, children: [html.Text(text)])`
pub fn figure_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "figure", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<footer>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer)
pub fn footer(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "footer", attrs: attrs, children: children)
}

/// Shorthand for `html.footer(attrs, children: [html.Text(text)])`
pub fn footer_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "footer", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<form>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form)
pub fn form(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "form", attrs: attrs, children: children)
}

/// Shorthand for `html.form(attrs, children: [html.Text(text)])`
pub fn form_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "form", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<h1>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1)
pub fn h1(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "h1", attrs: attrs, children: children)
}

/// Shorthand for `html.h1(attrs, children: [html.Text(text)])`
pub fn h1_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "h1", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<h2>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2)
pub fn h2(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "h2", attrs: attrs, children: children)
}

/// Shorthand for `html.h2(attrs, children: [html.Text(text)])`
pub fn h2_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "h2", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<h3>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3)
pub fn h3(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "h3", attrs: attrs, children: children)
}

/// Shorthand for `html.h3(attrs, children: [html.Text(text)])`
pub fn h3_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "h3", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<h4>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4)
pub fn h4(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "h4", attrs: attrs, children: children)
}

/// Shorthand for `html.h4(attrs, children: [html.Text(text)])`
pub fn h4_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "h4", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<h5>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5)
pub fn h5(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "h5", attrs: attrs, children: children)
}

/// Shorthand for `html.h5(attrs, children: [html.Text(text)])`
pub fn h5_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "h5", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<h6>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6)
pub fn h6(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "h6", attrs: attrs, children: children)
}

/// Shorthand for `html.h6(attrs, children: [html.Text(text)])`
pub fn h6_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "h6", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<header>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header)
pub fn header(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "header", attrs: attrs, children: children)
}

/// Shorthand for `html.header(attrs, children: [html.Text(text)])`
pub fn header_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "header", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<hr />` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr)
pub fn hr(attrs: List(Attr(a))) -> Node(a) {
  LeafElement(tag: "hr", attrs: attrs)
}

/// The [HTML `<i>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i)
pub fn i(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "i", attrs: attrs, children: children)
}

/// Shorthand for `html.i(attrs, children: [html.Text(text)])`
pub fn i_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "i", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<iframe>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe)
pub fn iframe(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "iframe", attrs: attrs, children: children)
}

/// Shorthand for `html.iframe(attrs, children: [html.Text(text)])`
pub fn iframe_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "iframe", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<img />` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img)
pub fn img(attrs: List(Attr(a))) -> Node(a) {
  LeafElement(tag: "img", attrs: attrs)
}

/// The [HTML `<input />` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input)
pub fn input(attrs: List(Attr(a))) -> Node(a) {
  LeafElement(tag: "input", attrs: attrs)
}

/// The [HTML `<ins>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins)
pub fn ins(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "ins", attrs: attrs, children: children)
}

/// Shorthand for `html.ins(attrs, children: [html.Text(text)])`
pub fn ins_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "ins", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<kbd>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd)
pub fn kbd(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "kbd", attrs: attrs, children: children)
}

/// Shorthand for `html.kbd(attrs, children: [html.Text(text)])`
pub fn kbd_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "kbd", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<label>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label)
pub fn label(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "label", attrs: attrs, children: children)
}

/// Shorthand for `html.label(attrs, children: [html.Text(text)])`
pub fn label_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "label", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<legend>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend)
pub fn legend(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "legend", attrs: attrs, children: children)
}

/// Shorthand for `html.legend(attrs, children: [html.Text(text)])`
pub fn legend_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "legend", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<li>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li)
pub fn li(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "li", attrs: attrs, children: children)
}

/// Shorthand for `html.li(attrs, children: [html.Text(text)])`
pub fn li_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "li", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<link />` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link)
pub fn link(attrs: List(Attr(a))) -> Node(a) {
  LeafElement(tag: "link", attrs: attrs)
}

/// The [HTML `<main>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main)
pub fn main(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "main", attrs: attrs, children: children)
}

/// Shorthand for `html.main(attrs, children: [html.Text(text)])`
pub fn main_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "main", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<map>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/map)
pub fn map(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "map", attrs: attrs, children: children)
}

/// Shorthand for `html.map(attrs, children: [html.Text(text)])`
pub fn map_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "map", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<mark>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark)
pub fn mark(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "mark", attrs: attrs, children: children)
}

/// Shorthand for `html.mark(attrs, children: [html.Text(text)])`
pub fn mark_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "mark", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<math>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math)
pub fn math(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "math", attrs: attrs, children: children)
}

/// Shorthand for `html.math(attrs, children: [html.Text(text)])`
pub fn math_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "math", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<menu>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu)
pub fn menu(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "menu", attrs: attrs, children: children)
}

/// Shorthand for `html.menu(attrs, children: [html.Text(text)])`
pub fn menu_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "menu", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<menuitem>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem)
pub fn menuitem(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "menuitem", attrs: attrs, children: children)
}

/// Shorthand for `html.menuitem(attrs, children: [html.Text(text)])`
pub fn menuitem_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "menuitem", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<meta />` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meta)
pub fn meta(attrs: List(Attr(a))) -> Node(a) {
  LeafElement(tag: "meta", attrs: attrs)
}

/// The [HTML `<meter>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter)
pub fn meter(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "meter", attrs: attrs, children: children)
}

/// Shorthand for `html.meter(attrs, children: [html.Text(text)])`
pub fn meter_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "meter", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<nav>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav)
pub fn nav(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "nav", attrs: attrs, children: children)
}

/// Shorthand for `html.nav(attrs, children: [html.Text(text)])`
pub fn nav_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "nav", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<noscript>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/noscript)
pub fn noscript(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "noscript", attrs: attrs, children: children)
}

/// Shorthand for `html.noscript(attrs, children: [html.Text(text)])`
pub fn noscript_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "noscript", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<object>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object)
pub fn object(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "object", attrs: attrs, children: children)
}

/// Shorthand for `html.object(attrs, children: [html.Text(text)])`
pub fn object_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "object", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<ol>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol)
pub fn ol(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "ol", attrs: attrs, children: children)
}

/// Shorthand for `html.ol(attrs, children: [html.Text(text)])`
pub fn ol_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "ol", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<optgroup>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup)
pub fn optgroup(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "optgroup", attrs: attrs, children: children)
}

/// Shorthand for `html.optgroup(attrs, children: [html.Text(text)])`
pub fn optgroup_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "optgroup", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<option>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option)
pub fn option(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "option", attrs: attrs, children: children)
}

/// Shorthand for `html.option(attrs, children: [html.Text(text)])`
pub fn option_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "option", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<output>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output)
pub fn output(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "output", attrs: attrs, children: children)
}

/// Shorthand for `html.output(attrs, children: [html.Text(text)])`
pub fn output_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "output", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<p>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p)
pub fn p(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "p", attrs: attrs, children: children)
}

/// Shorthand for `html.p(attrs, children: [html.Text(text)])`
pub fn p_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "p", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<param>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param)
pub fn param(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "param", attrs: attrs, children: children)
}

/// Shorthand for `html.param(attrs, children: [html.Text(text)])`
pub fn param_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "param", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<picture>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/picture)
pub fn picture(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "picture", attrs: attrs, children: children)
}

/// Shorthand for `html.picture(attrs, children: [html.Text(text)])`
pub fn picture_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "picture", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<pre>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre)
pub fn pre(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "pre", attrs: attrs, children: children)
}

/// Shorthand for `html.pre(attrs, children: [html.Text(text)])`
pub fn pre_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "pre", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<progress>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress)
pub fn progress(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "progress", attrs: attrs, children: children)
}

/// Shorthand for `html.progress(attrs, children: [html.Text(text)])`
pub fn progress_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "progress", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<q>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q)
pub fn q(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "q", attrs: attrs, children: children)
}

/// Shorthand for `html.q(attrs, children: [html.Text(text)])`
pub fn q_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "q", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<rp>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp)
pub fn rp(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "rp", attrs: attrs, children: children)
}

/// Shorthand for `html.rp(attrs, children: [html.Text(text)])`
pub fn rp_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "rp", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<rt>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt)
pub fn rt(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "rt", attrs: attrs, children: children)
}

/// Shorthand for `html.rt(attrs, children: [html.Text(text)])`
pub fn rt_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "rt", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<ruby>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby)
pub fn ruby(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "ruby", attrs: attrs, children: children)
}

/// Shorthand for `html.ruby(attrs, children: [html.Text(text)])`
pub fn ruby_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "ruby", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<s>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s)
pub fn s(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "s", attrs: attrs, children: children)
}

/// Shorthand for `html.s(attrs, children: [html.Text(text)])`
pub fn s_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "s", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<samp>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp)
pub fn samp(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "samp", attrs: attrs, children: children)
}

/// Shorthand for `html.samp(attrs, children: [html.Text(text)])`
pub fn samp_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "samp", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<section>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section)
pub fn section(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "section", attrs: attrs, children: children)
}

/// Shorthand for `html.section(attrs, children: [html.Text(text)])`
pub fn section_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "section", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<select>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select)
pub fn select(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "select", attrs: attrs, children: children)
}

/// Shorthand for `html.select(attrs, children: [html.Text(text)])`
pub fn select_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "select", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<small>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small)
pub fn small(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "small", attrs: attrs, children: children)
}

/// Shorthand for `html.small(attrs, children: [html.Text(text)])`
pub fn small_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "small", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<source />` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source)
pub fn source(attrs: List(Attr(a))) -> Node(a) {
  LeafElement(tag: "source", attrs: attrs)
}

/// The [HTML `<span>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span)
pub fn span(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "span", attrs: attrs, children: children)
}

/// Shorthand for `html.span(attrs, children: [html.Text(text)])`
pub fn span_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "span", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<strong>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong)
pub fn strong(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "strong", attrs: attrs, children: children)
}

/// Shorthand for `html.strong(attrs, children: [html.Text(text)])`
pub fn strong_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "strong", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<sub>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub)
pub fn sub(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "sub", attrs: attrs, children: children)
}

/// Shorthand for `html.sub(attrs, children: [html.Text(text)])`
pub fn sub_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "sub", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<summary>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary)
pub fn summary(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "summary", attrs: attrs, children: children)
}

/// Shorthand for `html.summary(attrs, children: [html.Text(text)])`
pub fn summary_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "summary", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<sup>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup)
pub fn sup(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "sup", attrs: attrs, children: children)
}

/// Shorthand for `html.sup(attrs, children: [html.Text(text)])`
pub fn sup_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "sup", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<svg>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/svg)
pub fn svg(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "svg", attrs: attrs, children: children)
}

/// Shorthand for `html.svg(attrs, children: [html.Text(text)])`
pub fn svg_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "svg", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<table>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)
pub fn table(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "table", attrs: attrs, children: children)
}

/// Shorthand for `html.table(attrs, children: [html.Text(text)])`
pub fn table_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "table", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<tbody>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody)
pub fn tbody(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "tbody", attrs: attrs, children: children)
}

/// Shorthand for `html.tbody(attrs, children: [html.Text(text)])`
pub fn tbody_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "tbody", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<td>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td)
pub fn td(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "td", attrs: attrs, children: children)
}

/// Shorthand for `html.td(attrs, children: [html.Text(text)])`
pub fn td_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "td", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<textarea>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea)
pub fn textarea(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "textarea", attrs: attrs, children: children)
}

/// Shorthand for `html.textarea(attrs, children: [html.Text(text)])`
pub fn textarea_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "textarea", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<tfoot>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot)
pub fn tfoot(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "tfoot", attrs: attrs, children: children)
}

/// Shorthand for `html.tfoot(attrs, children: [html.Text(text)])`
pub fn tfoot_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "tfoot", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<th>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th)
pub fn th(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "th", attrs: attrs, children: children)
}

/// Shorthand for `html.th(attrs, children: [html.Text(text)])`
pub fn th_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "th", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<thead>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead)
pub fn thead(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "thead", attrs: attrs, children: children)
}

/// Shorthand for `html.thead(attrs, children: [html.Text(text)])`
pub fn thead_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "thead", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<time>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time)
pub fn time(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "time", attrs: attrs, children: children)
}

/// Shorthand for `html.time(attrs, children: [html.Text(text)])`
pub fn time_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "time", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<tr>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr)
pub fn tr(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "tr", attrs: attrs, children: children)
}

/// Shorthand for `html.tr(attrs, children: [html.Text(text)])`
pub fn tr_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "tr", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<track />` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track)
pub fn track(attrs: List(Attr(a))) -> Node(a) {
  LeafElement(tag: "track", attrs: attrs)
}

/// The [HTML `<u>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u)
pub fn u(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "u", attrs: attrs, children: children)
}

/// Shorthand for `html.u(attrs, children: [html.Text(text)])`
pub fn u_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "u", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<ul>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul)
pub fn ul(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "ul", attrs: attrs, children: children)
}

/// Shorthand for `html.ul(attrs, children: [html.Text(text)])`
pub fn ul_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "ul", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<var>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var)
pub fn var(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "var", attrs: attrs, children: children)
}

/// Shorthand for `html.var(attrs, children: [html.Text(text)])`
pub fn var_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "var", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<video>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video)
pub fn video(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "video", attrs: attrs, children: children)
}

/// Shorthand for `html.video(attrs, children: [html.Text(text)])`
pub fn video_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "video", attrs: attrs, children: [Text(text)])
}

/// The [HTML `<wbr>` element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr)
pub fn wbr(attrs: List(Attr(a)), children: List(Node(a))) -> Node(a) {
  Element(tag: "wbr", attrs: attrs, children: children)
}

/// Shorthand for `html.wbr(attrs, children: [html.Text(text)])`
pub fn wbr_text(attrs: List(Attr(a)), text: String) -> Node(a) {
  Element(tag: "wbr", attrs: attrs, children: [Text(text)])
}
