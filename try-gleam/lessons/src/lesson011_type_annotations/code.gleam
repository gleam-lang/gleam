import gleam/io

pub fn main() {
  let name: String = "Gleam"
  io.debug(name)

  let is_cool: Bool = True
  io.debug(is_cool)

  let version: Int = 1
  io.debug(version)
}
