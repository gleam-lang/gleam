workflow "Build and test" {
  on = "push"
  resolves = ["Test compiler"]
}

action "Build compiler deps" {
  uses = "./.github/actions/rust"
  args = "cd gleam && cp src/main.rs{,.bak} && echo '' > src/main.rs && cargo build && cp src/main.rs{.bak,}"
}

action "Test compiler" {
  uses = "./.github/actions/rust"
  needs = ["Build compiler deps"]
  args = "cd gleam && cargo test"
}
