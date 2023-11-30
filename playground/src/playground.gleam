import gleam/io
import gleam/list
import gleam/string
import gleam/result
import simplifile
import playground/html
import snag

const static = "static"

const public = "public"

const public_precompiled = "public/precompiled"

const prelude = "../compiler-core/templates/prelude.mjs"

const stdlib_compiled = "build/dev/javascript/gleam_stdlib/gleam"

const stdlib_sources = "build/packages/gleam_stdlib/src/gleam"

const stdlib_external = "build/packages/gleam_stdlib/src"

const compiler_wasm = "../compiler-wasm/pkg"

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
    use _ <- result.try(write_pages())
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

fn write_pages() -> snag.Result(Nil) {
  let html = html.page()
  simplifile.write(public <> "/index.html", html)
  |> file_error("Failed to write index.html")
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

  use _ <- result.try(list.try_each(modules, fn(name) {
    let from = stdlib_compiled <> "/" <> name <> ".mjs"
    let to = dest <> "/" <> name <> ".mjs"
    simplifile.copy_file(from, to)
    |> file_error("Failed to copy stdlib module " <> from)
  }))

  Ok(Nil)
}

fn generate_stdlib_bundle(modules: List(String)) -> snag.Result(Nil) {
  use entries <- result.try(list.try_map(modules, fn(name) {
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
      |> list.filter(fn(line) { !string.starts_with(line, "@external(erlang") })
      |> list.filter(fn(line) { line != "" })
      |> string.join("\n")

    Ok("  \"gleam/" <> name <> "\": `" <> code <> "`")
  }))

  entries
  |> string.join(",\n")
  |> string.append("export default {\n", _)
  |> string.append("\n}\n")
  |> simplifile.write(public <> "/stdlib.js", _)
  |> file_error("Failed to write stdlib.js")
}

fn reset_output() -> snag.Result(Nil) {
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
