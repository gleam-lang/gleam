import gleam/list
import gleam/string
import simplifile

pub fn main() {
  let assert Ok(files) = simplifile.read_directory("../../vendor/gleam")
  let assert Ok(_) =
    files
    |> list.filter(fn(file) { string.ends_with(file, ".gleam") })
    |> list.map(read_file)
    |> string.join(",\n")
    |> string.append("export default {\n", _)
    |> string.append("\n}\n")
    |> simplifile.write("../../stdlib.js", _)
}

fn read_file(name) {
  let assert Ok(code) = simplifile.read("../../vendor/gleam/" <> name)
  let name = string.replace(name, ".gleam", "")
  let code =
    code
    |> string.replace("\\", "\\\\")
    |> string.replace("`", "\\`")
    |> string.split("\n")
    |> list.filter(fn(line) { !string.starts_with(string.trim(line), "//") })
    |> list.filter(fn(line) { !string.starts_with(line, "@external(erlang") })
    |> list.filter(fn(line) { line != "" })
    |> string.join("\n")

  "  \"gleam/" <> name <> "\": `" <> code <> "`"
}
