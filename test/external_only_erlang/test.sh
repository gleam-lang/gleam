#!/bin/sh

set -eu

g() {
	echo "Running: cargo run --quiet -- $@"
	cargo run --quiet -- "$@"
}

echo Resetting the build directory to get to a known state
g clean

echo This should succeed regardless of target as it is a dependency module
g run --module=hello_joe
g run --module=hello_joe --target=erlang
g run --module=hello_joe --target=javascript

echo Building and running for Erlang should succeed
g build --target=erlang
g run --target=erlang

# TODO: FIXME: Re-enable this one the compiler understand that the previous build
#       cache cannot be used here.
#
# echo Building for JavaScript should fail, even if previously a JavaScript dependency was built
# if g build --target=javascript; then
# 	echo "Expected build to fail"
# 	exit 1
# fi

echo Running for JavaScript should fail, even if previously a JavaScript dependency was built
if g run --target=javascript; then
	echo "Expected run to fail"
	exit 1
fi

echo
echo Success! 💖
echo
