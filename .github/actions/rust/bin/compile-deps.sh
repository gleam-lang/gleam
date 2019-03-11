#!/bin/sh
set -eu

echo
echo Compiling Rust deps
echo

cd gleam
cp -v src/main.rs src/main.rs.bak
cargo build
cp -v src/main.rs.bak src/main.rs
cd ..

echo
echo Done
echo
