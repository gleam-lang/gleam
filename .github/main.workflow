workflow "Build and test" {
  on = "push"
  resolves = ["Build compiler deps"]
}

action "Build compiler deps" {
  uses = "./.github/actions/rust"
  #args = "action-bin/compile-deps.sh"
  args = "ls"
}

# action "Test compiler" {
#   uses = "./.github/actions/rust"
#   needs = ["Build compiler deps"]
#   args = "cd gleam && cargo test && echo done"
# }
