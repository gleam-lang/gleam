workflow "Build and test" {
  on = "push"
  resolves = ["Build compiler deps"]
}

action "Build compiler deps" {
  uses = "./.github/actions/rust"
  args = "/bin/compile-deps.sh"
}

# action "Test compiler" {
#   uses = "./.github/actions/rust"
#   needs = ["Build compiler deps"]
#   args = "cd gleam && cargo test && echo done"
# }
