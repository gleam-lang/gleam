# Changelog

## Unreleased

### Compiler

- The compiler now performs function inlining optimisations for a specific set
  of standard library functions, which can allow functions which were previously
  not tail-recursive on the JavaScript target to become tail-recursive. For
  example, the following code:

  ```gleam
  pub fn count(from: Int, to: Int) -> Int {
    use <- bool.guard(when: from >= to, return: from)
    io.println(int.to_string())
    count(from + 1, to)
  }
  ```

  Would previously cause a stack overflow on the JavaScript target for large
  values. Now it is rewritten to:

  ```gleam
  pub fn count(from: Int, to: Int) -> Int {
    case from >= to {
      True -> from
      False -> {
        io.println(int.to_string())
        count(from + 1, to)
      }
    }
  }
  ```

  Which allows tail-call optimisation to occur.

  ([Surya Rose](https://github.com/GearsDatapacks))

### Build tool

- New projects are generated using OTP28 on GitHub Actions.
  ([Louis Pilfold](https://github.com/lpil))

- `gleam add` now adds `dependencies` and `dev-dependencies` as tables instead
  of inline tables if they are missing.
  ([Andrey Kozhev](https://github.com/ankddev))

### Language server

### Formatter

### Bug fixes

- Fixed a bug where `echo` could crash on JavaScript if the module contains
  record variants with the same name as some built-in JavaScript objects.
  ([Louis Pilfold](https://github.com/lpil))
