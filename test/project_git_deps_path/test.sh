# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2026 The Gleam contributors

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
git -c user.name="Test" -c user.email="test@example.com" commit -q -m "Initial commit"
git branch -M main
REF=$(git rev-parse HEAD)
cd "$OLDPWD"

echo Resetting the build directory to get to a known state
rm -fr build

cat > gleam.toml << EOF
name = "git_deps_path"
version = "0.1.0"

[dependencies]
package_a = { git = "file://${REPO_DIR}", ref = "${REF}", path = "package_a" }
EOF

g update
g check

echo
echo Testing with a branch ref, two packages from one repo, and a clean rebuild
rm -fr build

cat > gleam.toml << EOF
name = "git_deps_path"
version = "0.1.0"

[dependencies]
package_a = { git = "file://${REPO_DIR}", ref = "main", path = "package_a" }
package_b = { git = "file://${REPO_DIR}", ref = "main", path = "package_b" }
EOF

g update

echo Checking that the hard-linked packages do not contain a .git entry
if [ -e "build/packages/package_a/.git" ]; then
  echo "build/packages/package_a/.git should not exist"
  exit 1
fi
if [ -e "build/packages/package_b/.git" ]; then
  echo "build/packages/package_b/.git should not exist"
  exit 1
fi

EXPECTED_PACKAGE_B="  { name = \"package_b\", version = \"0.1.0\", build_tools = [\"gleam\"], requirements = [], source = \"git\", repo = \"file://${REPO_DIR}\", commit = \"${REF}\", path = \"package_b\" },"
if ! grep -qFx "$EXPECTED_PACKAGE_B" manifest.toml; then
  echo "manifest.toml does not lock package_b as a git source. Expected:"
  echo "$EXPECTED_PACKAGE_B"
  echo "Got:"
  cat manifest.toml
  exit 1
fi

rm -fr build
g check

echo
echo Testing that a new commit on a branch refreshes a transitive path dependency
rm -fr build

cat > gleam.toml << EOF
name = "git_deps_path"
version = "0.1.0"

[dependencies]
package_a = { git = "file://${REPO_DIR}", ref = "main", path = "package_a" }
EOF

g update

echo Checking that the transitive package_b starts at the original commit
if ! grep -qF "hello from package_b" build/packages/package_b/src/package_b.gleam; then
  echo "build/packages/package_b should contain the original greeting"
  cat build/packages/package_b/src/package_b.gleam
  exit 1
fi

echo Making a new commit that changes package_b without changing its version
cat > "$REPO_DIR/package_b/src/package_b.gleam" << 'GLEAM'
pub fn greeting() -> String {
  "hello from package_b v2"
}
GLEAM
git -C "$REPO_DIR" add .
git -C "$REPO_DIR" -c user.name="Test" -c user.email="test@example.com" commit -q -m "Update package_b"
NEW_REF=$(git -C "$REPO_DIR" rev-parse HEAD)

g update

echo Checking that build/packages/package_b was refreshed to the new commit
if ! grep -qF "hello from package_b v2" build/packages/package_b/src/package_b.gleam; then
  echo "build/packages/package_b was not refreshed to the new commit"
  cat build/packages/package_b/src/package_b.gleam
  exit 1
fi

EXPECTED_PACKAGE_B="  { name = \"package_b\", version = \"0.1.0\", build_tools = [\"gleam\"], requirements = [], source = \"git\", repo = \"file://${REPO_DIR}\", commit = \"${NEW_REF}\", path = \"package_b\" },"
if ! grep -qFx "$EXPECTED_PACKAGE_B" manifest.toml; then
  echo "manifest.toml does not lock package_b at the new commit. Expected:"
  echo "$EXPECTED_PACKAGE_B"
  echo "Got:"
  cat manifest.toml
  exit 1
fi

echo
echo Testing that an unresolvable ref fails
rm -fr build

cat > gleam.toml << EOF
name = "git_deps_path"
version = "0.1.0"

[dependencies]
package_a = { git = "file://${REPO_DIR}", ref = "no-such-ref", path = "package_a" }
EOF

if g update; then
  echo "g update should have failed for an unresolvable ref"
  exit 1
fi

echo
echo Success! 💖
echo
