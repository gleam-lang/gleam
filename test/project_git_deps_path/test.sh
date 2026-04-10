#!/bin/sh

set -eu

GLEAM_COMMAND=${GLEAM_COMMAND:-"cargo run --quiet --"}

g() {
  echo "Running: $GLEAM_COMMAND $@"
  $GLEAM_COMMAND "$@"
}

# Create a monorepo with two sibling packages in a temp directory
REPO_DIR=$(mktemp -d)
trap 'rm -rf "$REPO_DIR"' EXIT

mkdir -p "$REPO_DIR/package_a/src" "$REPO_DIR/package_b/src"

cat > "$REPO_DIR/package_b/gleam.toml" << 'TOML'
name = "package_b"
version = "0.1.0"

[dependencies]
TOML

cat > "$REPO_DIR/package_b/src/package_b.gleam" << 'GLEAM'
pub fn greeting() -> String {
  "hello from package_b"
}
GLEAM

cat > "$REPO_DIR/package_a/gleam.toml" << 'TOML'
name = "package_a"
version = "0.1.0"

[dependencies]
package_b = { path = "../package_b" }
TOML

cat > "$REPO_DIR/package_a/src/package_a.gleam" << 'GLEAM'
import package_b

pub fn hello() -> String {
  package_b.greeting()
}
GLEAM

cd "$REPO_DIR"
git init -q
git add .
git commit -q -m "Initial commit"
REF=$(git rev-parse HEAD)
cd "$OLDPWD"

echo Resetting the build directory to get to a known state
rm -fr build

# Write gleam.toml with the local repo URL
cat > gleam.toml << EOF
name = "git_deps_path"
version = "0.1.0"

[dependencies]
package_a = { git = "file://${REPO_DIR}", ref = "${REF}", path = "package_a" }
EOF

g update
g check

echo
echo Success! 💖
echo
