import gleam/io

/// This tests that a project with an empty readme doesn't get published.
///
pub fn main() -> Nil {
  greeting()
  first_line()
  second_line()
}

fn greeting() {
  io.println("Hello from empty_readme!")
}

fn first_line() {
  io.println("Here we have some additional code so that this is not mistaken")
}

fn second_line() {
  io.println("for a default main project, that would be rejected as well!")
}
