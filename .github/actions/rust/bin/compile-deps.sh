#!/bin/sh
set -eu

echo
echo Compiling Rust deps
echo

cd gleam

# Create an empty main to avoid compiling the entire application
mv -v src/main.rs src/main.rs.bak
echo "fn main() {}" > src/main.rs

cargo build

# Put back the application main
mv -v src/main.rs.bak src/main.rs
cd ..

echo
echo Done
echo
