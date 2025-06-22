# Changelog

## Unreleased

### Compiler

- Generated JavaScript functions, constants, and custom type constructors now
  include any doc comment as a JSDoc comment, making it easier to use the
  generated code and browse its documentation from JavaScript.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The code generated for a `case` expression on the JavaScript target is now
  reduced in size in many cases.
  ([Surya Rose](https://github.com/GearsDatapacks))

- The code generators now perform usage-based dead code elimination. Unused
  definitions are not longer generated.
  ([Louis Pilfold](https://github.com/lpil))

- `echo` now has better support for character lists, JavaScript errors, and
  JavaScript circular references.
  ([Louis Pilfold](https://github.com/lpil))

- The look of errors and warnings has been improved. Additional labels providing
  context for the error message are no longer highlighted with the same style as
  the source of the problem.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Gleam will now emit a helpful message when attempting to import modules using
  `.` instead of `/`.

  ```gleam
  error: Syntax error
    ┌─ /src/parse/error.gleam:1:11
    │
  1 │ import one.two.three
    │           ^ I was expecting either `/` or `.{` here.

  Perhaps you meant one of:

      import one/two
      import one.{item}
  ```

  ([Zij-IT](https://github.com/zij-it))

- The compiler now emits a warning when a top-level constant or function
  declaration shadows an imported name in the current module.
  ([Aayush Tripathi](https://github.com/aayush-tripathi))

- The compiler can now tell when an unknown variable might be referring to an
  ignored variable and provide an helpful error message highlighting it. For
  example, this piece of code:

  ```gleam
  pub fn go() {
    let _x = 1
    x + 1
  }
  ```

  Now results in the following error:

  ```
  error: Unknown variable
    ┌─ /src/one/two.gleam:4:3
    │
  3 │   let _x = 1
    │       -- This value is discarded
  4 │   x + 1
    │   ^ So it is not in scope here.

  Hint: Change `_x` to `x` or reference another variable
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- When using two spreads, trying to concatenate lists, the compiler will now
  show a nicer error message. For example, this snippet of code:

  ```gleam
  pub fn main() -> Nil {
    let xs = [1, 2, 3]
    let ys = [5, 6, 7]
    [1, ..xs, ..ys]
  }
  ```

  Would result in the following error:

  ```
  error: Syntax error
    ┌─ /src/parse/error.gleam:5:13
    │
  5 │   [1, ..xs, ..ys]
    │       --    ^^ I wasn't expecting a second spread here
    │       │
    │       You're using a spread here

  Lists are immutable and singly-linked, so to join two or more lists
  all the elements of the lists would need to be copied into a new list.
  This would be slow, so there is no built-in syntax for it.
  ```

  ([Carl Bordum Hansen](https://github.com/carlbordum)) and
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

### Build tool

- `gleam update`, `gleam deps update`, and `gleam deps download` will now print
  a message when there are new major versions of packages available.

  ```text
   $ gleam update
    Resolving versions

  The following dependencies have new major versions available:

  gleam_http 1.7.0 -> 4.0.0
  gleam_json 1.0.1 -> 3.0.1
  lustre     3.1.4 -> 5.1.1
  ```

  ([Amjad Mohamed](https://github.com/andho))

- Docs generator now strips trailing slashes from Gitea/Forgejo hosts so
  sidebar "Repository" and "View Source" links never include `//`, and
  single-line “View Source” anchors emit `#Lx` instead of `#Lx-x`.
  ([Aayush Tripathi](https://github.com/aayush-tripathi))

### Language server

### Formatter

### Bug fixes

- Fixed a bug where the language server would not show type-related code action
  for record fields in custom type definitions.
  ([cysabi](https://github.com/cysabi))

- Fixed a bug where the "Inline variable" code action would be offered for
  function parameters and other invalid cases.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the "Inline variable" code action would not be applied
  correctly to variables using label shorthand syntax.
  ([Surya Rose](https://github.com/GearsDatapacks))

- Fixed a bug where the compiler would emit the same error twice for patterns
  with the wrong number of labels.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the language server would generate invalid code when the
  "Extract variable" code action was used on a `use` expression.
  ([Surya Rose](https://github.com/GearsDatapacks))

## v1.11.1 - 2025-06-05

### Compiler

- The displaying of internal types in HTML documentation has been improved.
  ([Louis Pilfold](https://github.com/lpil))

- A warning is now emitted when the same module is imported
  multiple times into the same module with different aliases.
  ([Louis Pilfold](https://github.com/lpil))

### Bug fixes

- Fixed a bug where a bit array segment matching on a floating point number
  would match with `NaN` or `Infinity` on the JavaScript target.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
