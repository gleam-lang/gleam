# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2026 The Gleam contributors

#!/bin/sh

set -eu

GLEAM_COMMAND=${GLEAM_COMMAND:-"cargo run --quiet --"}

g() {
  echo "Running: $GLEAM_COMMAND $@"
  $GLEAM_COMMAND "$@"
}

fail() {
  echo "$1" >&2
  exit 1
}

file=priv/symlink.toml
target="../../hextarball/gleam.toml"

echo "Checking that the src file is a symlink"

if [ ! -L "$file" ]; then
  fail "Expected $file to be a symlink"
fi

if [ ! -e "$file" ]; then
  fail "Expected $file to resolve"
fi

actual=$(readlink "$file")
if [ "$actual" != "$target" ]; then
  fail "Expected $file to resolve to $target, got $actual"
fi

echo Resetting the build directory to get to a known state
rm -fr build

echo Running publish should not publish anything
if yes "n" | g publish; then
  echo "Expected publish to fail, but it succeeded"
  exit 1
fi

echo
echo Success! 💖
echo
