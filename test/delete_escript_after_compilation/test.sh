#!/bin/sh

set -eu

GLEAM_COMMAND=${GLEAM_COMMAND:-"cargo run --quiet --"}

g() {
  echo "Running: $GLEAM_COMMAND $@"
  $GLEAM_COMMAND "$@"
}

echo Resetting the build directory to get to a known state
rm -rf build

echo Building the project
g build

echo Searching for leftover gleam@@compile.erl files
if find build -name "gleam@@compile.erl" | grep -q .; then
  echo "ERROR: gleam@@compile.erl file(s) still exist after build"
  exit 1
fi

echo
echo None found! Success!
echo
