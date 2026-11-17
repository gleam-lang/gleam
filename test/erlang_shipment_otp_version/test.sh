#!/bin/sh

# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2026 The Gleam contributors

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

ENTRYPOINT=build/erlang-shipment/entrypoint.sh

echo Checking that there is no warning when the OTP version matches
sh "$ENTRYPOINT" run 2> build/stderr.txt
if [ -s build/stderr.txt ]; then
  echo "ERROR: expected no output on stderr, got:"
  cat build/stderr.txt
  exit 1
fi
echo "No warning printed"

echo Checking that there is a warning when the OTP version differs
sed 's/^OTP_VERSION=.*/OTP_VERSION=0/' "$ENTRYPOINT" > build/entrypoint.sh
mv build/entrypoint.sh "$ENTRYPOINT"
sh "$ENTRYPOINT" run 2> build/stderr.txt
if ! grep -qF "built with Erlang/OTP 0" build/stderr.txt; then
  echo "ERROR: expected an OTP version warning on stderr, got:"
  cat build/stderr.txt
  exit 1
fi
echo "Warning printed"

echo
echo Success!
echo
