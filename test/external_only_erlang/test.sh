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

echo This should succeed as Erlang is supported
g build --target=erlang
g run --target=erlang

echo This should fail as JavaScript is not supported
if g build --target=javascript; then
	echo "Expected build to fail"
	exit 1
fi
if g run --target=javascript; then
	echo "Expected run to fail"
	exit 1
fi
