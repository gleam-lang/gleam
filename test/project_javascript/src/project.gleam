pub fn main() {
  println("Hello, from Gleam compiled to JavaScript!")
}

external fn println(String) -> Nil =
  "" "globalThis.console.log"
