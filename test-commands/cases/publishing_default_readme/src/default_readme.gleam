import gleam/io

/// This tests that a project with a default readme doesn't get published.
///
pub fn main() -> Nil {
  greeting()
  first_line()
  second_line()
}

fn greeting() {
  io.println("Hello from default_readme!")
}

fn first_line() {
  io.println("Here we have some additional code so that this is not mistaken")
}

fn second_line() {
  io.println("for a default main project, that would be rejected as well!")
}
