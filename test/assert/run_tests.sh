#/usr/bin/env sh

set -eu

should_succeed() {
    echo
    echo Running: "$@"

    EXIT_CODE=0
    cargo run -- $@ > /dev/null 2>&1 || EXIT_CODE=$?
    if [ $EXIT_CODE -ne 0 ]
    then
        echo ERROR: Command should have succeeded
        exit 1
    else
        echo Test Passed '(command run successfully)'
    fi
}

should_fail() {
    echo
    echo Running: "$@"

    EXIT_CODE=0
    cargo run -- $@ > /dev/null 2>&1 || EXIT_CODE=$?

    if [ $EXIT_CODE -eq 0 ]
    then
        echo ERROR: Command should have failed
        exit 1
    else
        echo Test Passed '(command errored as expected)'
    fi
}

all_targets() {
  $@ --target erlang
  $@ --target javascript --runtime nodejs
  $@ --target javascript --runtime deno
  $@ --target javascript --runtime bun
}

# Ensure the project builds correctly
should_succeed build --target erlang
should_succeed build --target javascript

all_targets should_succeed run --module passing

# Since a single failing `assert` will exit immediately, we must test each
# failing case individually as separate modules.
all_targets should_fail run --module failing1
all_targets should_fail run --module failing2
all_targets should_fail run --module failing3
all_targets should_fail run --module failing4
