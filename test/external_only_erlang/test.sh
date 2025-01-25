#!/bin/sh

set -eu

GLEAM_COMMAND=${GLEAM_COMMAND:-"cargo run --quiet --"}

g() {
  echo "Running: $GLEAM_COMMAND $@"
  $GLEAM_COMMAND "$@"
}

echo Resetting the build directory to get to a known state
rm -fr build

echo This should succeed regardless of target as it is a dependency module
g run --module=hello_joe
g run --module=hello_joe --target=erlang
g run --module=hello_joe --target=javascript

echo Building and running for Erlang should succeed
g build --target=erlang
g run --target=erlang

echo Building for JavaScript should fail, even if previously a JavaScript dependency was built
if g build --target=javascript; then
  echo "Expected build to fail"
  exit 1
fi

echo Running for JavaScript should fail, even if previously a JavaScript dependency was built
if g run --target=javascript; then
  echo "Expected run to fail"
  exit 1
fi

echo Running erlang shipment should succeed
g export erlang-shipment
grep "external_only_erlang_ffi" "build/erlang-shipment/external_only_erlang/ebin/external_only_erlang.app"

echo
echo Success! 💖
echo
