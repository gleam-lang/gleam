#!/bin/sh

# https://github.com/gleam-lang/gleam/pull/4445

set -eu

GLEAM_COMMAND=${GLEAM_COMMAND:-"cargo run --quiet --"}

g() {
	echo "Running: $GLEAM_COMMAND $@"
	$GLEAM_COMMAND "$@"
}

echo Resetting the build directory to get to a known state
rm -fr build

echo Running publish should not print the warning
output=$(yes "n" | g publish)
if echo "$output" | grep -q "Your package defines multiple top-level modules"; then
    echo "Expected warning to be printed"
    exit 1
fi

echo
echo Success! 💖
echo
