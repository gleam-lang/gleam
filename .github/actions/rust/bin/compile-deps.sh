#!/bin/sh

set -eu

cd gleam
cp -v src/main.rs{,.bak}
cargo build
cp -v src/main.rs{.bak,}
cd ..
