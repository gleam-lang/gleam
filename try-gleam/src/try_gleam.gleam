import gleam/io
import gleam/list
import htmb.{h, text}
import gleam/string_builder
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/string
import gleam/result
import simplifile
import snag

const static = "static"

const public = "public"

const public_precompiled = "public/precompiled"

const prelude = "../compiler-core/templates/prelude.mjs"

const stdlib_compiled = "build/dev/javascript/gleam_stdlib/gleam"

const stdlib_sources = "build/packages/gleam_stdlib/src/gleam"

const stdlib_external = "build/packages/gleam_stdlib/src"

const compiler_wasm = "../compiler-wasm/pkg"

const lessons_src = "lessons/src"

const hello_joe = "import gleam/io

pub fn main() {
  io.println(\"Hello, Joe!\")
}
"

// Don't include deprecated stdlib modules
const skipped_stdlib_modules = [
  "bit_string.gleam", "bit_builder.gleam", "map.gleam",
]

pub fn main() {
  let result = {
    use _ <- result.try(reset_output())
    use _ <- result.try(make_prelude_available())
    use _ <- result.try(make_stdlib_available())
    use _ <- result.try(copy_wasm_compiler())
    use p <- result.try(load_pages())
    use _ <- result.try(write_pages(p))
    Ok(Nil)
  }

  case result {
    Ok(_) -> Nil
    Error(snag) -> {
      io.println(snag.pretty_print(snag))
      panic
    }
  }
}

type Page {
  Page(
    name: String,
    text: String,
    code: String,
    path: String,
    previous: Option(String),
    next: Option(String),
  )
}

fn load_pages() -> snag.Result(List(Page)) {
  use lessons <- result.try(
    simplifile.read_directory(lessons_src)
    |> file_error("Failed to read lessons directory"),
  )

  let lessons =
    lessons
    |> list.sort(by: string.compare)
    |> list.index_map(pair.new)

  use pages <- result.try(
    list.try_map(lessons, fn(pair) {
      let #(index, lesson) = pair
      let path = lessons_src <> "/" <> lesson
      let name =
        lesson
        |> string.split("_")
        |> list.drop(1)
        |> string.join("-")

      use code <- result.try(
        simplifile.read(path <> "/code.gleam")
        |> file_error("Failed to read code.gleam"),
      )

      use text <- result.try(
        simplifile.read(path <> "/text.html")
        |> file_error("Failed to read text.html"),
      )

      let path = case index {
        0 -> "/"
        _ -> "/" <> name
      }

      Ok(Page(
        name: name,
        text: text,
        code: code,
        path: path,
        previous: None,
        next: None,
      ))
    }),
  )

  Ok(add_previous_next(pages, [], None))
}

fn write_pages(pages: List(Page)) -> snag.Result(Nil) {
  use _ <- result.try(list.try_each(pages, write_page))

  let render = fn(h) { string_builder.to_string(htmb.render(h)) }
  let html =
    string.concat([
      render(h("h2", [], [text("Table of contents")])),
      render(h(
        "ul",
        [],
        list.map(pages, fn(page) {
          h("li", [], [
            h("a", [#("href", page.path)], [
              page.name
              |> string.replace("-", " ")
              |> string.capitalise
              |> text,
            ]),
          ])
        }),
      )),
    ])

  let page =
    Page(
      name: "Index",
      text: html,
      code: hello_joe,
      path: "/index",
      previous: None,
      next: None,
    )
  write_page(page)
}

fn write_page(page: Page) -> snag.Result(Nil) {
  let path = public <> page.path
  use _ <- result.try(
    simplifile.create_directory_all(path)
    |> file_error("Failed to make " <> path),
  )

  let path = path <> "/index.html"
  simplifile.write(to: path, contents: page_html(page))
  |> file_error("Failed to write page " <> path)
}

fn add_previous_next(
  rest: List(Page),
  acc: List(Page),
  previous: Option(String),
) -> List(Page) {
  case rest {
    [] -> list.reverse(acc)
    [page, next, ..rest] -> {
      let page = Page(..page, previous: previous, next: Some(next.path))
      add_previous_next([next, ..rest], [page, ..acc], Some(page.path))
    }
    [page, ..rest] -> {
      let page = Page(..page, previous: previous, next: None)
      add_previous_next(rest, [page, ..acc], Some(page.path))
    }
  }
}

fn copy_wasm_compiler() -> snag.Result(Nil) {
  use <- require(
    simplifile.is_directory(compiler_wasm),
    "compiler-wasm/pkg must have been compiled",
  )

  simplifile.copy_directory(compiler_wasm, public <> "/compiler")
  |> file_error("Failed to copy compiler-wasm")
}

fn make_prelude_available() -> snag.Result(Nil) {
  use _ <- result.try(
    simplifile.create_directory_all(public_precompiled)
    |> file_error("Failed to make " <> public_precompiled),
  )

  simplifile.copy_file(prelude, public_precompiled <> "/gleam.mjs")
  |> file_error("Failed to copy prelude.mjs")
}

fn make_stdlib_available() -> snag.Result(Nil) {
  use files <- result.try(
    simplifile.read_directory(stdlib_sources)
    |> file_error("Failed to read stdlib directory"),
  )

  let modules =
    files
    |> list.filter(fn(file) { string.ends_with(file, ".gleam") })
    |> list.filter(fn(file) { !list.contains(skipped_stdlib_modules, file) })
    |> list.map(string.replace(_, ".gleam", ""))

  use _ <- result.try(
    generate_stdlib_bundle(modules)
    |> snag.context("Failed to generate stdlib.js bundle"),
  )

  use _ <- result.try(
    copy_compiled_stdlib(modules)
    |> snag.context("Failed to copy precompiled stdlib modules"),
  )

  use _ <- result.try(
    copy_stdlib_externals()
    |> snag.context("Failed to copy stdlib external files"),
  )

  Ok(Nil)
}

fn copy_stdlib_externals() -> snag.Result(Nil) {
  use files <- result.try(
    simplifile.read_directory(stdlib_external)
    |> file_error("Failed to read stdlib external directory"),
  )
  let files = list.filter(files, string.ends_with(_, ".mjs"))

  list.try_each(files, fn(file) {
    let from = stdlib_external <> "/" <> file
    let to = public_precompiled <> "/" <> file
    simplifile.copy_file(from, to)
    |> file_error("Failed to copy stdlib external file " <> from)
  })
}

fn copy_compiled_stdlib(modules: List(String)) -> snag.Result(Nil) {
  use <- require(
    simplifile.is_directory(stdlib_compiled),
    "Project must have been compiled for JavaScript",
  )

  let dest = public_precompiled <> "/gleam"
  use _ <- result.try(
    simplifile.create_directory_all(dest)
    |> file_error("Failed to make " <> dest),
  )

  use _ <- result.try(
    list.try_each(modules, fn(name) {
      let from = stdlib_compiled <> "/" <> name <> ".mjs"
      let to = dest <> "/" <> name <> ".mjs"
      simplifile.copy_file(from, to)
      |> file_error("Failed to copy stdlib module " <> from)
    }),
  )

  Ok(Nil)
}

fn generate_stdlib_bundle(modules: List(String)) -> snag.Result(Nil) {
  use entries <- result.try(
    list.try_map(modules, fn(name) {
      let path = stdlib_sources <> "/" <> name <> ".gleam"
      use code <- result.try(
        simplifile.read(path)
        |> file_error("Failed to read stdlib module " <> path),
      )
      let name = string.replace(name, ".gleam", "")
      let code =
        code
        |> string.replace("\\", "\\\\")
        |> string.replace("`", "\\`")
        |> string.split("\n")
        |> list.filter(fn(line) { !string.starts_with(string.trim(line), "//") })
        |> list.filter(fn(line) {
          !string.starts_with(line, "@external(erlang")
        })
        |> list.filter(fn(line) { line != "" })
        |> string.join("\n")

      Ok("  \"gleam/" <> name <> "\": `" <> code <> "`")
    }),
  )

  entries
  |> string.join(",\n")
  |> string.append("export default {\n", _)
  |> string.append("\n}\n")
  |> simplifile.write(public <> "/stdlib.js", _)
  |> file_error("Failed to write stdlib.js")
}

fn reset_output() -> snag.Result(Nil) {
  use _ <- result.try(
    simplifile.create_directory_all(public)
    |> file_error("Failed to delete public directory"),
  )

  use files <- result.try(
    simplifile.read_directory(public)
    |> file_error("Failed to read public directory"),
  )

  use _ <- result.try(
    files
    |> list.map(string.append(public <> "/", _))
    |> simplifile.delete_all
    |> file_error("Failed to delete public directory"),
  )

  simplifile.copy_directory(static, public)
  |> file_error("Failed to copy static directory")
}

fn require(
  that condition: Bool,
  because reason: String,
  then next: fn() -> snag.Result(t),
) -> snag.Result(t) {
  case condition {
    True -> next()
    False -> Error(snag.new(reason))
  }
}

fn file_error(
  result: Result(t, simplifile.FileError),
  context: String,
) -> snag.Result(t) {
  case result {
    Ok(value) -> Ok(value)
    Error(error) ->
      snag.error("File error: " <> string.inspect(error))
      |> snag.context(context)
  }
}

fn page_html(page: Page) -> String {
  let navlink = fn(name, link) {
    case link {
      None -> h("span", [], [text(name)])
      Some(path) -> h("a", [#("href", path)], [text(name)])
    }
  }

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
      h("title", [], [text("Try Gleam")]),
      h("link", [#("rel", "stylesheet"), #("href", "/style.css")], []),
    ]),
    h("body", [], [
      h("nav", [#("class", "navbar")], [
        h("a", [#("href", "/")], [text("Try Gleam")]),
      ]),
      h("article", [#("class", "playground")], [
        h("section", [#("id", "text")], [
          htmb.dangerous_unescaped_fragment(string_builder.from_string(page.text,
          )),
          h("nav", [#("class", "prev-next")], [
            navlink("Back", page.previous),
            text(" — "),
            h("a", [#("href", "/index")], [text("Index")]),
            text(" — "),
            navlink("Next", page.next),
          ]),
        ]),
        h("section", [#("id", "editor")], [
          h("div", [#("id", "editor-target")], []),
        ]),
        h("aside", [#("id", "output")], []),
      ]),
      h("script", [#("type", "gleam"), #("id", "code")], [
        htmb.dangerous_unescaped_fragment(string_builder.from_string(page.code)),
      ]),
      h("script", [#("type", "module"), #("src", "/index.js")], []),
    ]),
  ])
  |> htmb.render_page("html")
  |> string_builder.to_string
}
