#!/bin/sh

set -eu

g() {
	echo "Running: gleam $@"
	cargo run --quiet -- "$@"
}

g clean

echo This should succeed regardless of target as it is a dependency module
g run --module=hello_joe --target=erlang
g run --module=hello_joe --target=javascript

echo This should succeed as JavaScript is supported
g build --target=javascript
g run --target=javascript

echo This should fail as Erlang is not supported
if g build --target=erlang; then
	echo "Expected build to fail"
	exit 1
fi
if g run --target=erlang; then
	echo "Expected run to fail"
	exit 1
fi
