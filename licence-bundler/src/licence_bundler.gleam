import gleam/bool
import gleam/io
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import gleam/string_tree
import htmb
import shellout
import simplifile
import tom

pub fn main() -> Nil {
  let gleam_crates = determine_gleam_crates()
  let active_licences = determine_licences()

  let assert Ok(output) =
    shellout.command(
      run: "cargo",
      with: [
        "tree",
        "--format",
        "{p} {l}",
        "--edges",
        "no-dev,no-build,no-proc-macro",
      ],
      in: "..",
      opt: [],
    )

  let rust_dependencies =
    output
    |> string.split("\n")
    |> list.filter(fn(line) { line != "" })
    |> list.map(trim_line_prefix)
    |> list.filter_map(dependency_information(_, gleam_crates, active_licences))
    |> set.from_list
    |> set.to_list
    |> list.sort(fn(a, b) {
      string.compare(
        a.name <> a.licence_identifier,
        b.name <> b.licence_identifier,
      )
    })

  let assert Ok(licences) =
    active_licences
    |> set.to_list
    |> list.sort(string.compare)
    |> list.try_map(fn(identifier) {
      simplifile.read("../licences/" <> identifier <> ".txt")
      |> result.map(Licence(identifier:, text: _))
    })

  let h = htmb.h
  let t = htmb.text

  let licence_slug = fn(identifier) { string.replace(identifier, " ", "-") }
  let licence_link = fn(licence_identifier) {
    let slug = licence_slug(licence_identifier)
    h("a", [#("href", "#" <> slug)], [t(licence_identifier)])
  }

  let html =
    h("html", [#("lang", "en")], [
      h("head", [], []),
      h("body", [], [
        h("h1", [], [t("Gleam dependency licences")]),
        h("section", [], [
          h("h2", [], [t("Rust dependency packages")]),
          h_ul(rust_dependencies, fn(package) {
            assert licences != [] as { package.name <> " missing licences" }
            let licences =
              list.map(package.used_licences, licence_link)
              |> list.intersperse(t(" or "))
            let url =
              "https://crates.io/crates/"
              <> package.name
              <> "/"
              <> package.version
            [
              h("a", [#("href", url)], [
                t(package.name <> "@" <> package.version),
              ]),
              t(" using "),
              ..licences
            ]
          }),
        ]),
        h("section", [], [
          h("h2", [], [t("Licences")]),
          h_ul(licences, fn(licence) {
            let id = licence.identifier
            [
              h("h3", [#("id", licence_slug(id))], [t(id)]),
              h("pre", [], [t(licence.text)]),
            ]
          }),
        ]),
      ]),
    ])
    |> htmb.render_page
    |> string_tree.to_string

  let assert Ok(_) = simplifile.write("../gleam-licences.html", html)
  io.println("Written ../gleam-licences.html")

  Nil
}

fn h_ul(items: List(a), mapper: fn(a) -> List(htmb.Html)) -> htmb.Html {
  htmb.h("ul", [], list.map(items, fn(item) { htmb.h("li", [], mapper(item)) }))
}

fn determine_licences() -> Set(String) {
  let assert Ok(files) = simplifile.read_directory("../licences")
  files
  |> list.map(string.remove_suffix(_, ".txt"))
  |> set.from_list
}

fn determine_gleam_crates() -> Set(String) {
  let assert Ok(toml) = simplifile.read("../Cargo.toml")
  let assert Ok(config) = tom.parse(toml)
  let assert Ok(crates) = tom.get_array(config, ["workspace", "members"])
  let assert Ok(paths) = list.try_map(crates, tom.as_string)
  let crates =
    list.map(paths, fn(path) {
      let assert Ok(toml) = simplifile.read("../" <> path <> "/Cargo.toml")
      let assert Ok(config) = tom.parse(toml)
      let assert Ok(name) = tom.get_string(config, ["package", "name"])
      name
    })
  set.from_list(crates)
}

type Crate {
  Crate(
    name: String,
    version: String,
    licence_identifier: String,
    used_licences: List(String),
  )
}

type Licence {
  Licence(identifier: String, text: String)
}

fn dependency_information(
  line: String,
  gleam_crates: Set(String),
  active_licences: Set(String),
) -> Result(Crate, Nil) {
  let line = string.remove_suffix(line, " (*)")
  let assert [name, version, ..licence_parts] = string.split(line, " ")
  use <- bool.guard(set.contains(gleam_crates, name), return: Error(Nil))
  let version = string.remove_prefix(version, "v")
  let licence_identifier = string.join(licence_parts, " ")
  let used_licences =
    licence_identifier
    |> string.split(" OR ")
    |> list.flat_map(string.split(_, " AND "))
    |> list.flat_map(string.split(_, "/"))
    |> list.map(string.remove_prefix(_, "("))
    |> list.map(string.remove_suffix(_, ")"))
    |> list.map(string.remove_suffix(_, "+"))
    |> list.filter(set.contains(active_licences, _))
  assert used_licences != []
    as { string.inspect(line) <> " parsed to no used licences" }
  Ok(Crate(name:, version:, licence_identifier:, used_licences:))
}

/// Convert lines like these:
///
/// ```txt
/// │   └── wasm-bindgen v0.2.106 MIT OR Apache-2.0
/// ```
///
/// into lines like these
///
/// ```txt
/// wasm-bindgen v0.2.106 MIT OR Apache-2.0
/// ```
///
fn trim_line_prefix(line: String) -> String {
  case line {
    " " <> line | "│" <> line | "─" <> line | "├" <> line | "└" <> line ->
      trim_line_prefix(line)

    ""
    | "a" <> _
    | "b" <> _
    | "c" <> _
    | "d" <> _
    | "e" <> _
    | "f" <> _
    | "g" <> _
    | "h" <> _
    | "i" <> _
    | "j" <> _
    | "k" <> _
    | "l" <> _
    | "m" <> _
    | "n" <> _
    | "o" <> _
    | "p" <> _
    | "q" <> _
    | "r" <> _
    | "s" <> _
    | "t" <> _
    | "u" <> _
    | "v" <> _
    | "w" <> _
    | "x" <> _
    | "y" <> _
    | "z" <> _ -> line
    _ -> panic as { "Unexpected line prefix: " <> string.inspect(line) }
  }
}
