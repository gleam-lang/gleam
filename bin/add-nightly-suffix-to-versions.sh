#!/bin/sh

#
# Add the `-nightly-YYYYMMDD` suffix the version of all Rust crates in the
# workspace. Used by the nightly build.
#

set -e

find . -name Cargo.toml -exec \
  sed -i "" -e "s/^version = \"\([^\"]*\)\"$/version = \"\1-nightly-$(date +%Y%m%d)\"/" {} \;
