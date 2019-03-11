workflow "Build and test" {
  on = "push"
  resolves = ["Test compile"]
}

action "Build compiler deps" {
  uses = "./.github/actions/rust"
  args = "/bin/build-compiler-deps.sh"
}

action "Test compiler" {
  uses = "./.github/actions/rust"
  needs = ["Build compiler deps"]
  args = "/bin/test-compiler.sh"
}
