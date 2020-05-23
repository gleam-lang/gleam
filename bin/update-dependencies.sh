#!/usr/bin/env bash
set -euo pipefail

function replace-version () {
    pkg=$1
    version=$2
    major_minor=$(echo $version | awk -F"." '{print $1"."$2}')
    echo pkg $pkg version $major_minor
    sed -i "s/$pkg = .*/$pkg = \"$major_minor\"/g" Cargo.toml
}

## so parallel can pick it up
export -f replace-version

function run () {
    cargo outdated | awk 'NR > 2 && $4 != "Removed" {n=split($1,a,/->/); if (n == 2) print a[2] " " $4; else print a[1] " " $4;}' | parallel --colsep ' ' replace-version {1} {2}
}

run
