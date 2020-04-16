# Gleam Core Language Tests

This is a collection of basic tests and sanity checks of the core language written in gleam.

# Running

From this directory run `rebar3 eunit`.

This will first build gleam from source, then compile the gleam modules in this project
and then run the tests. You must have `rust`, `rebar3`, and `erlang` installed.

# Adding Tests

Any function ending in `_test` in a module in the `test` directory will get run as a test by
eunit.
