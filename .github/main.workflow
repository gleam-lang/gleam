workflow "Build and test" {
  on = "push"
  resolves = ["Build compiler deps"]
}

action "Build compiler deps" {
  uses = "./.github/actions/rust"
  args = "cd gleam && cp src/main.rs{,.bak} && echo '' > src/main.rs && cargo build && cp src/main.rs{.bak,}"
}
