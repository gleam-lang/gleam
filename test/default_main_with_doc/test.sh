#!/bin/sh

set -eu

GLEAM_COMMAND=${GLEAM_COMMAND:-"cargo run --quiet --"}

g() {
  echo "Running: $GLEAM_COMMAND $@"
  $GLEAM_COMMAND "$@"
}

echo Resetting the build directory to get to a known state
rm -fr build

echo Running publish should publish
if ! yes "n" | g publish; then
  echo "Expected publish to succeed, but it failed"
  exit 1
fi

echo
echo Success! 💖
echo
