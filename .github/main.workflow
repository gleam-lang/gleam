workflow "Build and test" {
  on = "push"
  resolves = ["Test compiler"]
}

action "Build compiler deps" {
  uses = "./.github/actions/rust"
  args = "cd gleam && cp src/main.rs{,.bak} && echo '' > src/main.rs && cargo build && cp src/main.rs{.bak,} && echo done"
}

action "Test compiler" {
  uses = "./.github/actions/rust"
  needs = ["Build compiler deps"]
  args = "cd gleam && cargo test && echo done"
}

action "Test" {
  uses = "./.github/actions/rust"
  needs = ["Test compiler"]
  args = "ls gleam && ls gleam/target"
}
