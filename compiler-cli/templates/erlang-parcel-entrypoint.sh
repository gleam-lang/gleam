#!/bin/sh
set -eu

PROJECT=$PROJECT_NAME_FROM_GLEAM
BASE=$(dirname $0)
COMMAND="${1-default}"

# -mode embedded \

run() {
  erl \
    -pa "$BASE"/*/ebin \
    -eval "gleam@@main:run($PROJECT)" \
    -noshell \
    -extra "$@"
}

case "$COMMAND" in
  run)
    shift
    run "$@"
  ;;

  *)
    echo "usage:" >&2
    echo "  entrypoint.sh run" >&2
    exit 1
esac
