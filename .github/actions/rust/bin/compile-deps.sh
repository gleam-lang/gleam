#!/bin/sh

set -eu

cd -v gleam
cp -v src/main.rs{,.bak}
cargo build
cp -v src/main.rs{.bak,}
