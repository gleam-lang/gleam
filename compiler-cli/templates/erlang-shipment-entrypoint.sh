#!/bin/sh

set -eu

PACKAGE=$PACKAGE_NAME_FROM_GLEAM
BASE=$(dirname "$0")
COMMAND="${1-default}"
OTP_VERSION=$OTP_VERSION_FROM_GLEAM
OTP_VERSION_CHECK="case list_to_atom(erlang:system_info(otp_release)) of '$OTP_VERSION' -> ok; V -> io:format(standard_error, 'warning: This Erlang shipment was built with Erlang/OTP $OTP_VERSION but you are running Erlang/OTP ~s.~nThe shipment may fail to run. Please use Erlang/OTP $OTP_VERSION or rebuild the shipment with your current version.~n', [V]) end"

run() {
  exec erl \
    -pa "$BASE"/*/ebin \
    -eval "$OTP_VERSION_CHECK" \
    -eval "$PACKAGE@@main:run($PACKAGE)" \
    -noshell \
    -extra "$@"
}

shell() {
  erl \
    -pa "$BASE"/*/ebin \
    -eval "$OTP_VERSION_CHECK"
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
  ;;
esac
