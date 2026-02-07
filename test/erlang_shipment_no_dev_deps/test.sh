#!/bin/sh

set -eu

GLEAM_COMMAND=${GLEAM_COMMAND:-"cargo run --quiet --"}

g() {
  echo "Running: $GLEAM_COMMAND $@"
  $GLEAM_COMMAND "$@"
}

echo Resetting the build directory to get to a known state
rm -fr build

echo Building erlang shipment
g export erlang-shipment

echo Checking that gleam_stdlib IS in the shipment
if [ ! -d "build/erlang-shipment/gleam_stdlib" ]; then
  echo "ERROR: gleam_stdlib should be in the shipment but was not found"
  exit 1
fi
echo "gleam_stdlib found in shipment"

echo Checking that hpack IS in the shipment
echo "(hpack_erl package has otp_app=hpack, so directory is named hpack)"
if [ ! -d "build/erlang-shipment/hpack" ]; then
  echo "ERROR: hpack (from hpack_erl package) should be in the shipment but was not found"
  exit 1
fi
echo "hpack found in shipment"

echo Checking that gleeunit is NOT in the shipment
if [ -d "build/erlang-shipment/gleeunit" ]; then
  echo "ERROR: gleeunit is a dev dependency and should NOT be in the shipment"
  exit 1
fi
echo "gleeunit correctly excluded from shipment"

echo Checking that the root package IS in the shipment
if [ ! -d "build/erlang-shipment/shipment_test" ]; then
  echo "ERROR: shipment_test (root package) should be in the shipment but was not found"
  exit 1
fi
echo "shipment_test found in shipment"

echo
echo Success!
echo
