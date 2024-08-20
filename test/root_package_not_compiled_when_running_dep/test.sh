#!/bin/sh

set -eu

GLEAM_COMMAND=${GLEAM_COMMAND:-"cargo run --quiet --"}

g() {
	echo "Running: $GLEAM_COMMAND $@"
	$GLEAM_COMMAND "$@"
}

echo Resetting the build directory to get to a known state
rm -fr build

echo This should succeed regardless of root package compilation errors as it is a dependency module
g run --module=hello_joe
g run --module=hello_joe --target=erlang
g run --module=hello_joe --target=javascript

echo Running for Erlang should fail, even if previously a Erlang dependency was built
if g run --target=erlang; then
	echo "Expected run to fail"
	exit 1
fi

echo Running for JavaScript should fail, even if previously a JavaScript dependency was built
if g run --target=javascript; then
	echo "Expected run to fail"
	exit 1
fi

echo
echo Success! 💖
echo
