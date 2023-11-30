import htmb.{h, text}
import gleam/string_builder

pub fn page(words words: String, code code: String) -> String {
  h("html", [#("lang", "en-gb")], [
    h("head", [], [
      h("meta", [#("charset", "utf-8")], []),
      h(
        "meta",
        [
          #("name", "viewport"),
          #("content", "width=device-width, initial-scale=1"),
        ],
        [],
      ),
      h("title", [], [text("Gleam Playground")]),
      h("link", [#("rel", "stylesheet"), #("href", "/style.css")], []),
    ]),
    h("body", [], [
      h("nav", [#("class", "navbar")], [text("Gleam Playground")]),
      h("article", [#("class", "playground")], [
        h("section", [#("id", "text")], [
          htmb.dangerous_unescaped_fragment(string_builder.from_string(words)),
        ]),
        h("section", [#("id", "editor")], [
          h("pre", [#("id", "editor-target")], [
            htmb.dangerous_unescaped_fragment(string_builder.from_string(code)),
          ]),
        ]),
        h("aside", [#("id", "output")], []),
      ]),
      h("script", [#("type", "module"), #("src", "/playground.js")], []),
    ]),
  ])
  |> htmb.render_page("html")
  |> string_builder.to_string
}
