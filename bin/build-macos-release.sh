#!/bin/sh

set -eu

GLEAM_ROOT=$(pwd)
TAG=$(git tag --points-at HEAD)
ARCHIVE=gleam-$TAG-macos.tar.gz

cd gleam
cargo build --release

TMP=$(mktemp -d)
cd $TMP
cp -v $GLEAM_ROOT/gleam/target/release/gleam gleam
tar -czvf $ARCHIVE gleam

cd $GLEAM_ROOT
hub release edit --attach $TMP/$ARCHIVE --message $TAG $TAG
