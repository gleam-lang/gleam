#!/bin/sh
set -eu

PACKAGE=$PACKAGE_NAME_FROM_GLEAM
BASE=$(dirname "$0")
COMMAND="${1-default}"

run() {
  exec erl \
    -pa "$BASE"/*/ebin \
    -eval 'case erlang:system_info(otp_release) of "$OTP_VERSION_FROM_GLEAM" -> ok; V -> io:format(standard_error, "warning: This Erlang shipment was built with Erlang/OTP $OTP_VERSION_FROM_GLEAM but you are running Erlang/OTP ~s.~nThe shipment may fail to run. Please use Erlang/OTP $OTP_VERSION_FROM_GLEAM or rebuild the shipment with your current version.~n", [V]) end' \
    -eval "$PACKAGE@@main:run($PACKAGE)" \
    -noshell \
    -extra "$@"
}

shell() {
  erl \
    -pa "$BASE"/*/ebin \
    -eval 'case erlang:system_info(otp_release) of "$OTP_VERSION_FROM_GLEAM" -> ok; V -> io:format(standard_error, "warning: This Erlang shipment was built with Erlang/OTP $OTP_VERSION_FROM_GLEAM but you are running Erlang/OTP ~s.~nThe shipment may fail to run. Please use Erlang/OTP $OTP_VERSION_FROM_GLEAM or rebuild the shipment with your current version.~n", [V]) end'
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
