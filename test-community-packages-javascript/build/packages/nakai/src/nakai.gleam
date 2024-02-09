//// Nakai has several "builders" that can be used.
//// - A `document` builder (the recommend one) that renders a full HTML document,
////   does a little magic to dedepulicate `<head>` elements, and some other things
////   that generally fit the theme of "rendering a full, valid, HTML document"
//// - An `inline` builder that should mostly be used for snippets, and partial bits of
////   HTML that will be inlined into a full document; hence the name. It renders things
////   much more literally. If you tell it to give you a `<head>` element inside a
////   `<p>`, it will, as an example.
//// - A future experimental DOM renderer (meant for use in the browser) that isn't
////   actually done yet.

import gleam/string_builder.{StringBuilder}
import nakai/html.{Node}
import nakai/internal/render

/// Renders a full HTML document from the given tree, into a `StringBuilder`.
/// ## Examples
/// ```gleam
/// html.div_text([], "hello, lucy!")
/// |> nakai.to_string_builder()
/// ```
pub fn to_string_builder(tree: Node(a)) -> StringBuilder {
  render.render_document(tree)
}

/// Renders a full HTML document from the given tree, into a `String`.
/// ## Examples
/// ```gleam
/// html.div_text([], "hello, lucy!")
/// |> nakai.to_string()
/// ```
pub fn to_string(tree: Node(a)) -> String {
  render.render_document(tree)
  |> string_builder.to_string()
}

/// Renders only the provided HTML, exactly as provided (disables `<head>`
/// deduplication, etc.), into a `StringBuilder`. Useful for generating snippets
/// instead of whole pages.
/// ## Examples
/// ```gleam
/// html.div_text([], "hello, lucy!")
/// |> nakai.to_inline_string_builder()
/// ```
pub fn to_inline_string_builder(tree: Node(a)) -> StringBuilder {
  render.render_inline(tree)
}

/// Renders only the provided HTML, exactly as provided (disables `<head>`
/// deduplication, etc.), into a `String`. Useful for generating snippets instead
/// of whole pages.
/// ## Examples
/// ```gleam
/// html.div_text([], "hello, lucy!")
/// |> nakai.to_inline_string()
/// ```
pub fn to_inline_string(tree: Node(a)) -> String {
  render.render_inline(tree)
  |> string_builder.to_string()
}
