#!/bin/sh

set -eu

echo
echo Testing compiler
echo

cd gleam

cargo test

echo
echo Done
echo
