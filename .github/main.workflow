workflow "Build and test" {
  on = "push"
  resolves = ["Test Gleam compiler"]
}

action "Test Gleam compiler" {
  uses = "./.github/actions/rust"
  args = "cargo test"
}
