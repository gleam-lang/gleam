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

- The compiler now emits a better error message for private types marked as
  opaque. For example, the following piece of code:

  ```gleam
  opaque type Wibble {
    Wobble
  }
  ```

  Would result in the following error:

  ```
  error: Private opaque type
    ┌─ /src/one/two.gleam:2:1
    │
  2 │ opaque type Wibble {
    │ ^^^^^^ You can safely remove this.

  Only a public type can be opaque.
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The parsing of opaque private types is now fault tolerant: having a private
  opaque type in a module no longer stops the compiler from highlighting other
  errors.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now emits a single warning for multiple negations in a row, while
  previously it would emit multiple comments highlighting increasingly longer
  spans.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Redundant `_ as x` patterns are now deprecated in favour of `x`.
  ([eutampieri](https://github.com/eutampieri))

- The compiler will now raise warning for inefficient use of `list.length()`
  when trying to check is list empty via `0 < list.length(list)` or
`list.length(list) > 0` as well as in other cases. For example, the following
  code:

  ```gleam
  import gleam/list

  pub fn main() {
    let numbers = [1, 46]
    let _ = 0 < list.length(numbers)
    let _ = list.length(numbers) > 0
  }
  ```

  Would result in following warnings:

  ```
  warning: Inefficient use of `list.length`
    ┌─ /data/data/com.termux/files/home/test_gleam/src/test_gleam.gleam:5:13
    │
  5 │     let _ = 0 < list.length(numbers)
    │             ^^^^^^^^^^^^^^^^^^^^^^^^

  The `list.length` function has to iterate across the whole
  list to calculate the length, which is wasteful if you only
  need to know if the list is empty or not.

  Hint: You can use `the_list != []` instead.

  warning: Inefficient use of `list.length`
    ┌─ /data/data/com.termux/files/home/test_gleam/src/test_gleam.gleam:6:13
    │
  6 │     let _ = list.length(numbers) > 0
    │             ^^^^^^^^^^^^^^^^^^^^^^^^

  The `list.length` function has to iterate across the whole
  list to calculate the length, which is wasteful if you only
  need to know if the list is empty or not.

  Hint: You can use `the_list != []` instead.
  ```

  ([Andrey Kozhev](https://github.com/ankddev))

- The compiler now provides an improved error message for when trying to define
  a constant inside a function. For example, the following code:

  ```gleam
  pub fn deep_thought() {
    const the_answer = 42
    the_answer
  }
  ```

  Will produce this error message:

  ```txt
    error: Syntax error
    ┌─ /src/file.gleam:2:3
    │
  3 │   const the_answer = 43
    │   ^^^^^ Constants are not allowed inside functions

  All variables are immutable in Gleam, so constants inside functions are not
  necessary.
  Hint: Either move this into the global scope or use `let` binding instead.
  ```

  ([Surya Rose](https://github.com/GearsDatapacks))

### Build tool

- New projects are generated using OTP28 on GitHub Actions.
  ([Louis Pilfold](https://github.com/lpil))

- `gleam add` now adds `dependencies` and `dev-dependencies` as tables instead
  of inline tables if they are missing.
  ([Andrey Kozhev](https://github.com/ankddev))

### Language server

- The "pattern match on variable" can now be triggered on lists. For example:

  ```gleam
  pub fn is_empty(list: List(a)) -> Bool {
    //            ^^^^ Triggering the action over here
  }
  ```

  Triggering the action over the `list` argument would result in the following
  code:

  ```gleam
  pub fn is_empty(list: List(a)) -> Bool {
    case list {
      [] -> todo
      [first, ..rest] -> todo
    }
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The "pattern match on variable" can now be triggered on variables introduced
  by other patterns. For example:

  ```gleam
  pub fn main() {
    let User(name:, role:) = find_user("lucy")
    //              ^^^^ Triggering the action over
  }
  ```

  Triggering the action over another variable like `role` would result in the
  following code:

  ```gleam
  pub fn main() {
    let User(name:, role:) = find_user("lucy")
    case role {
      Admin -> todo
      Member -> todo
    }
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now offers a quick fix to remove `opaque` from a private
  type:

  ```gleam
  opaque type Wibble {
  // ^^^ This is an error!
    Wobble
  }
  ```

  If you hover over the type and trigger the quick fix, the language server will
  automatically remove the `opaque` keyword:

  ```gleam
  type Wibble {
    Wobble
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The "inline variable" code action is now only suggested when hovering over the
  relevant variable.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- When hovering over a record field in a record access expression, the language
  sever will now show the documentation for that field, if present.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Renaming a variable from a label shorthand (`name:`) no longer includes the
  colon in the rename dialog (`name:` -> `name`)
  ([fruno](https://github.com/frunobulax-the-poodle))

### Formatter

- The formatter now removes needless multiple negations that are safe to remove.
  For example, this snippet of code:

  ```gleam
  pub fn useless_negations() {
    let lucky_number = --11
    let lucy_is_a_star = !!!False
  }
  ```

  Is rewritten as:

  ```gleam
  pub fn useless_negations() {
    let lucky_number = 11
    let lucy_is_a_star = !False
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Redundant `_ as x` patterns are rewritten to `x`.
  ([eutampieri](https://github.com/eutampieri))

- The formatter no longer removes blocks from case clause guards.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Bug fixes

- Fixed a bug where `echo` could crash on JavaScript if the module contains
  record variants with the same name as some built-in JavaScript objects.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where the compiler would highlight an entire double negation
  expression as safe to remove, instead of just highlighting the double
  negation.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would crash if there was an invalid version
  requirement in a project's `gleam.toml`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the compiler would suggest a discouraged project name as an
  alternative to the reserved `gleam` name.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where Forgejo source URLs in the HTML documentation could be
  incorrectly structured.
  ([Louis Pilfold](https://github.com/lpil))

- The compiler now emits an error when a module in the `src` directory imports
  a dev dependency, while previously it would incorrectly let these
  dependencies to be imported.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Erroneous extra fields in `gleam.toml` dependency specifications will no
  longer be siltently ignored. An error is now returned highlighting the
  problem instead.
  ([Louis Pilfold](https://github.com/lpil))

- Fixed a bug where renaming a constant which is referenced in another module
  inside a guard would generate invalid code.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where `echo .. as ..` message will be omitted in browser target.
  ([Andrey Kozhev](https://github.com/ankddev))

- Fixed a bug where renaming a variable used in a record update would produce
  invalid code in certain situations.
  ([Surya Rose](https://github.com/GearsDatapacks))
