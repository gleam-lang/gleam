# Changelog

## Unreleased

### Compiler

The compiler will now raise a warning for `let assert` assignments where the
assertion is redundant.
([Giacomo Cavalieri](https://github.com/giacomocavalieri))

Empty case expressions are no longer parse errors and will instead be
exhaustiveness errors, resulting in a better error message that shows what the
missing patterns are. ([Race Williams](https://github.com/raquentin))

### Formatter

Now the formatter removes redundant alias names from imported modules.
([Giacomo Cavalieri](https://github.com/giacomocavalieri))

#### Bug Fixes

Fixed [RUSTSEC-2021-0145](https://rustsec.org/advisories/RUSTSEC-2021-0145) by
using Rust's `std::io::IsTerminal` instead of the `atty` library.
([Pi-Cla](https://github.com/Pi-Cla))

Fixed the generated `mod` property in the Erlang application file when using the
`application_start_module` property in `gleam.toml`.
([Alex Manning](https://github.com/rawhat))

Fixed a bug where using some reserved keywords would result in confusing error
messages. ([Giacomo Cavalieri](https://github.com/giacomocavalieri))