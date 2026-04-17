import gleam/io
import gleam/list
import gleam/set.{type Set}
import gleam/string
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

  let lines =
    output
    |> string.split("\n")
    |> list.filter(fn(line) { line != "" })
    |> list.map(trim_line_prefix)
    |> list.map(crate_information(_, active_licences))
    |> list.filter(is_dependency(_, gleam_crates))
    |> set.from_list
    |> set.to_list
    |> list.map(string.inspect)

  io.println(string.join(lines, "\n"))
  Nil
}

fn determine_licences() -> Set(String) {
  let assert Ok(files) = simplifile.read_directory("../licences")
  files
  |> list.map(string.remove_suffix(_, ".txt"))
  |> set.from_list
}

fn is_dependency(crate: Crate, gleam_crates: Set(String)) -> Bool {
  !set.contains(gleam_crates, crate.name)
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

fn crate_information(line: String, active_licences: Set(String)) -> Crate {
  let line = string.remove_suffix(line, " (*)")
  let assert [name, version, ..licence_parts] = string.split(line, " ")
  let licence_identifier = string.join(licence_parts, " ")
  let used_licences =
    licence_identifier
    |> string.split(" OR ")
    |> list.flat_map(string.split(_, " AND "))
    |> list.flat_map(string.split(_, "/"))
    |> list.map(string.remove_prefix(_, "("))
    |> list.map(string.remove_suffix(_, ")"))
    |> list.filter(set.contains(active_licences, _))
  Crate(name:, version:, licence_identifier:, used_licences:)
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
