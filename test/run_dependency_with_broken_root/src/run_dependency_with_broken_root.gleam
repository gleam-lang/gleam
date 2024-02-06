import gleam/io

pub fn main() {
  let this_is_intentionally_broken: Int = "Broken!"
  io.println("Hello from run_dependency_with_broken_root!")
}
