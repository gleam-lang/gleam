#/usr/bin/env sh

set -eu

should_succeed() {
    echo
    echo Running: "$@"
    cargo run -- $@ > /dev/null 2>&1
    if [ $? -ne 0 ]
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
    cargo run -- $@ > /dev/null 2>&1
    if [ $? -eq 0 ]
    then
        echo ERROR: Command should have failed
        exit 1
    else
        echo Test Passed '(command errored as expected)'
    fi
}

# No module
should_succeed run --target erlang
should_succeed run --target javascript --runtime nodejs
should_succeed run --target javascript --runtime deno

# Module from same package
should_succeed run --module module --target erlang
should_succeed run --module module --target javascript --runtime nodejs
should_succeed run --module module --target javascript --runtime deno

# Nested module from same package
should_succeed run --module module/sub_module --target erlang
should_succeed run --module module/sub_module --target javascript --runtime nodejs
should_succeed run --module module/sub_module --target javascript --runtime deno

# Dependency package
should_succeed run --module gleeunit --target erlang
should_succeed run --module gleeunit --target javascript --runtime nodejs
should_succeed run --module gleeunit --target javascript --runtime deno

# Unknown module
should_fail run --module doesnt_exist

# No main function
should_fail run --module module/no_main_function

# Main function with wrong arity
should_fail run --module module/wrong_arity
