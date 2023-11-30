import htmb.{h, text}
import gleam/string_builder

pub fn page() -> String {
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
          h("h2", [], [text("Hello, friend!")]),
          h("p", [], [text("Welcome to the Gleam playground.")]),
          h("p", [], [
            text(
              "The Gleam code on the left is editable and will be compiled and evaluated as you type. Anything you print using ",
            ),
            h("code", [], [text("io.println")]),
            text(" or "),
            h("code", [], [text("io.debug")]),
            text(
              " will be shown in the lower right, along with any compile errors and warnings.",
            ),
          ]),
          h("p", [], [
            text(
              "To compile your code, this playground has a WASM version of the Gleam compiler, which is used to compile Gleam to JavaScript code. It is then evaluated in the browser.",
            ),
          ]),
          h("p", [], [
            text("The "),
            h("a", [#("href", "https://hexdocs.pm/gleam_stdlib")], [
              text("Gleam standard library"),
            ]),
            text(
              " is loaded into the playground, allowing you to import any of the modules it contains. ",
            ),
            h(
              "a",
              [#("href", "https://hexdocs.pm/gleam_stdlib/gleam/list.html")],
              [h("code", [], [text("gleam/list")])],
            ),
            text(" and "),
            h(
              "a",
              [#("href", "https://hexdocs.pm/gleam_stdlib/gleam/result.html")],
              [h("code", [], [text("gleam/result")])],
            ),
            text(" are Gleamlin favorites."),
          ]),
          h("p", [], [text("Happy hacking!")]),
        ]),
        h("section", [#("id", "editor")], [
          h("div", [#("id", "editor-target")], []),
        ]),
        h("aside", [#("id", "output")], []),
      ]),
      h("script", [#("type", "module"), #("src", "/playground.js")], []),
    ]),
  ])
  |> htmb.render_page("html")
  |> string_builder.to_string
}
