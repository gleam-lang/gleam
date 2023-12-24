# Annoyances

This document contains a list of issues and annoyances that we have writing
Gleam code today, so that we can devise solutions to them in future.

There are other annoyances that have known solutions that are yet to be
implemented. These are tracked in the Gleam issue tracker instead.

## Cannot infer what targets a package supports

## Dynamic decoding boilerplate

## JSON (de|en)coding boilerplate

## Bundling compiled JavaScript is non-obvious

## Cannot shift repeated work to compile or initialise time

For example, regex compilation.

## No good story for creating CLI programs

The Erlang virtual machine is not typically installed on a user's computer, and
bytecode compiled for it is not easy to distribute.

JavaScript runtimes do better, but it is still not as good an experience as
languages that compile to a single binary, and forcing Gleam programmers to use
promises and a single thread is not ideal.

## Runtime debugging is basic

Rust's `dbg!` and Elixir's `dbg` were mentioned as good additions.

## Last of if/else makes boolean logic less pretty and more alien

With case:

```gleam
case str == "+" {
  True -> Keep(Plus)
  False ->
    case all(str, is_ascii_letter) {
      True -> Keep(Ident(str))
      False ->
        case all(str, is_digit) {
          True -> {
            let assert Ok(int) = int.parse(str)
            Keep(Number(int)
          }
          False -> Next
    }
}
```

With use:

```gleam
use <- bool.guard(
  when: str == "+",
  return: Keep(Plus),
)
use <- bool.guard(
  when: all(str, is_ascii_letter),
  return: Keep(Ident(str),
)
use <- bool.guard(
  when: !all(str, is_digit),
  return: Next,
)
let assert Ok(int) = int.parse(str)
Keep(Number(int))
```
