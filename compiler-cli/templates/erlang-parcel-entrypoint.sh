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

shell() {
  erl -pa "$BASE"/*/ebin
}

case "$COMMAND" in
  run)
    shift
    run "$@"
  ;;

  shell)
    shell
  ;;

  *)
    echo "usage:" >&2
    echo "  entrypoint.sh \$COMMAND" >&2
    echo "" >&2
    echo "commands:" >&2
    echo "  run    Run the project main function" >&2
    echo "  shell  Run an Erlang shell" >&2
    exit 1
esac
