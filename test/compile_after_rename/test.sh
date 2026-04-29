#/bin/sh

set -eu

GLEAM_COMMAND=${GLEAM_COMMAND:-"cargo run --quiet --"}

g() {
  echo "Running: $GLEAM_COMMAND $@"
  $GLEAM_COMMAND "$@"
}

cd $(dirname $0)

rm -rf project project_renamed

g new project
cd project
g add lustre
g build
cd ..
mv project project_renamed
cd project_renamed
g build
