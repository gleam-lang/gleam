# Changelog

## Unreleased

### Compiler

- Prepending to lists in JavaScript (`[x, ..xs]` syntax) has been optimised.
- Function stubs are no longer generated for functions that do not have an
  implementation for the current targeting being compiled for.
- Fixed a bug where some functions would not result in a compile error when
  compiled for a target that they do not support.
- Fixed a bug where sometimes a warning would not be emitted when a result is
  discarded.
- Fixed a bug with JavaScript code generation of pattern matching guards.
- URLs in error messages have been updated for the new language tour.
- Improved error message when erroneously trying to append items to a list using
  the spread syntax (like `[..rest, last]`).
- Generate [type references](https://www.typescriptlang.org/docs/handbook/triple-slash-directives.html#-reference-types-)
  when compiling to JavaScript with TypeScript definitions enabled.
- Fix a bug where JavaScript code generation would not properly return the result of nested blocks.


### Formatter

- The formatting of case expressions with multiple subjects has been improved.
- Fixed a bug where the formatter would move comments from the end of bounded
  expressions like lists, tuples, case expressions or function calls.
- Fixed a bug where a record update's arguments would not be indented correctly.

### Build tool

- A warning is now emitted if there is a `.gleam` file with a path that would be
  invalid as a module name.
- New projects are created with the GitHub `actions/checkout` v4 action.
- Fixed a bug where bit arrays would break syntax highlighting in the generated
  HTML documentation.
- Dependencies that use Erlang-only bit options can now compile on JavaScript,
  though the functions that use them will not be available for use in the root
  package.
- Generated documentation no longer exposes the constructors of opaque types,
  no longer exposes the values of constants, and indicates which types are
  opaque.

### Language Server

- The `Compiling Gleam` message is no longer emitted each time code is compiled.
  This is to reduce noise in editors that show this message prominently such as
  Neovim.
- Fixed a bug where hovering over an expression in the middle of a pipe would
  give the wrong node.


## v1.0.0 - 2024-03-04

### Language changes

- Comments have been added to the JavaScript prelude to indicate which members
  are in the public API and which are internal.

### Build tool

- Fixed a bug where the exported package interface would not have a module's
  documentation.
- Fixed a bug where the `export package interface` command would always
  recompile the project ignoring the cache.

## v1.0.0-rc2 - 2024-02-14

### Bug fixes

- Fixed a bug where the exhaustiveness checker could crash for some generic
  types.

### Formatter

- The format used by the formatter has been improved in some niche cases.
- Improved the formatting of long case guards.
- The formatter can now format groups of imports alphabetically.


## v1.0.0-rc1 - 2024-02-10

### Language changes

- Using a reserved word is now a compile error, not a warning.
- Inexhaustive matches are now compile errors, not warnings.
- The warning for an unused module alias now shows how to not assign a name to
  the module.
- Type aliases with unused type parameters now emit an error.
- Type definitions with duplicate type parameters now emit an error.

### Formatter

- Now the formatter will nest pipelines and binary operators that are used as
  function arguments, list items or as tuple items.
- The format function literals used as the last argument in a function call
  on long lines has been improved.

### Build tool

- If a package contains a `todo` expression then the build tool will now refuse
  to publish it to Hex.
- The search bar in generated docs now has a darker background color.
- `gleam export` now takes a `package-interface` option to export a json file
  containing metadata about the root package.
- `gleam docs build` now creates a json file containing metadata about the root
  package.
- The order of dependencies in `manifest.toml` is now in alphabetical order.
- The search bar in generated docs now has a darker background color.
- The generated docs no longer shows whether an argument is discarded or
  not in a function signature.
- It is now possible to use `gleam run -m` to run a dependency module even if
  that dependency uses a compile target that your project does not support.

### Bug fixes

- Fixed a bug the build tool could be make to attempt to run a main function
  that does not support the current target in some circumstances.
- Fixed a bug where the exhaustiveness checker could crash when checking nested
  values inserted into the parent type using type parameters.
- Fixed a bug where `functionname(_name)` would incorrectly parse as a function
  capture instead of a syntax error.
- Fixed a bug where external only functions would "successfully" compile for a
  target they do not support, leading to a runtime error.


## v0.34.1 - 2023-01-17

### Build tool changes

- Support has been added for using SourceHut as a repository.

### Bug fixes

- Fixed a bug where long function headers with external implementations could
  format incorrectly.
- The `@deprecated` attribute can now be used to annotate module constants.
  This will cause a warning to be emitted when the constant is used.


## v0.34.0 - 2023-01-16

## v0.34.0-rc3 - 2023-01-12

### Language changes

- "echo" is now a reserved word.
- A warning is no longer emitted when a function has a Gleam implementation as
  well as external implementations for both targets. This is because having a
  default Gleam implementation means the code is future-proof and continues to
  be cross platform even if a new target is added.

### Bug fixes

- Fixed a bug where function heads would go over the line limit in the
  formatter.


## v0.34.0-rc2 - 2023-01-11

### Bug fixes

- Fixed a bug where `gleam run` would fail when the current directory is not
  the root of the project and using the JavaScript target.
- Fixed a bug where the compiler would in some cases fail to error when an
  application uses functions that do not support the current compilation
  target.


## v0.34.0-rc1 - 2024-01-07

### Language changes

- Warn about function body not being used, because it already has external
  implementations for all targets.
- It's now possible to compile a project with external functions in dependency
  packages that are not supported by the compilation target so long as they are
  not used on the current target.
- The error message for when one imports a constructor instead of an homonymous
  type has been improved.

### Language Server Changes

- Added a `View on HexDocs` link on function hover.

### Formatter

- Fixed some quirk with the formatting of binary operators.
- Fixed a bug where the formatter would move a function call's closed
  parentheses on a new line instead of splitting the function's arguments.
- Now the formatter will format tuples as if they were functions, trying to
  first split just the last element before splitting the whole tuple.
- Improved the formatting of multiline strings in string concatenation.

### Build tool changes

- The `gleam new` command now accepts any existing path, as long as there are
  no conflicts with already existing files. Examples: `gleam new .`, `gleam new
  ..`, `gleam new ~/projects/test`.
- The format for the README created by `gleam new` has been altered.
- The `gleam.toml` created by `gleam new` now has a link to the full reference
  for its available options.
- The `gleam` binary is now statically linked on Windows.
- New projects are created requiring between versions of v0.34.0 inclusive and
  exclusive v2.0.0.
- The `repository` section now supports additional VCS types in the form of
  codeberg, forgejo and gitea allowing a `user`, `repo` and additionally a
  `host` url.
- TypeScript declaration for the prelude exports previously missing functions
  and classes. Additionally, swaps interfaces for classes and adds missing
  attributes to classes.
- `gleam` commands now look in parent directories for a `gleam.toml` file.

### Bug fixes

- Fixed a bug where `gleam add` would not update `manifest.toml` correctly.
- Fixed a bug where `fn() { Nil }()` could generate invalid JavaScript code.
- Fixed a bug where the build tool would make unnecessary calls to the Hex API
  when path dependencies are used.
- Fixed a bug where `gleam new` would generate a gitignore with `build` rather
  than `/build`.
- Fixed where the types of generic constants could be incorrecly inferred.
- `Utf8Codepoint` has been renamed to `UtfCodepoint` in `prelude.d.mts`.
- Fixed a bug where `gleam deps list` would look in filesystem root instead of
  the current directory.
- Fixed a bug with the `isEqual` function in `prelude.js` where RegExps were
  being incorrectly structurally compared and being falsely reported as being
  equal.
- JavaScript: export from `prelude.d.mts` in `gleam.d.mts` to fix the error:
  "Type 'Result' is not generic".
- Not providing a definition after some attributes is now a parse error.


## v0.33.0 - 2023-12-18

## v0.33.0-rc4 - 2023-12-17

- The deprecated bit array options `binary` and `bit_string` have been removed.
- The deprecated ambiguous type import syntax has been removed.
- The deprecated `BitString` type has been removed.
- The deprecated `inspect` functions and `BitString` type has been removed from
  the JavaScript prelude.


## v0.33.0-rc3 - 2023-12-17

### Formatter

- The formatter now tries to split long chains of binary operations around the
  operator itself, rather than around other elements like lists or function
  calls.

### Bug fixes

- Fixed a bug where string prefix aliases defined in alternative case branches
  would all be bound to the same constant.


## v0.33.0-rc2 - 2023-12-07

### Language changes

- The `\e` string escape sequence has been removed. Use `\u{001b}` instead.
- Generated Erlang now disabled redundant case clause warnings as these are now
  redundant due to exhaustiveness checking.

### Bug fixes

- Fixed a bug where the `\u` string escape sequence would not work with
  on Erlang on the right hand side of a string concatenation.


## v0.33.0-rc1 - 2023-12-06

### Formatter

- The formatter now tries to keep a function body and its arguments on a single
  line by first trying to split only its last argument on multiple lines.
- Fixed a bug where the formatter would move comments out of blocks.
- `gleam format` now ignores the Gleam build directory by default, even when not
  in a git repository.

### Language changes

- Gleam now has full exhaustiveness checking. Exhaustiveness issues have been
  downgraded from errors to warnings so that existing Gleam code can be
  upgraded to be exhaustive without breaking existing code. In a future version
  they will be upgraded to errors.
- The `!` operator can now be used in clause guards.
- The words `auto`, `delegate`, `derive`, `else`, `implement`, `macro`, and
  `test` are now reserved for future use. If used they will emit a warning. In
  a future version this may be upgraded to an error.
- The `\u{...}` syntax can be used in strings to specify unicode codepoints via a
  hexadecimal number with 1 to 6 digits.
- The `todo as` and `panic as` syntaxes now accept an expression that evaluates
  to a string rather than just a string literal.

### Build tool changes

- The `gleam run` and `gleam test` commands gain the `-t` flag, which is an
  alias of the `--target` flag.
- The `gleam build`, `gleam check`, `gleam run` and `gleam test` commands now
  also accept `js` and `erl` as values for the `--target` flag.
- The `gleam new` command now creates packages at version 1.0.0.
- The `gleam publish` command now asks for confirmation if the package being
  published is not yet version 1.0.0.
- The `gleam publish` command now asks for confirmation if the package name is
  one that implies the package is maintained by the Gleam core team.
- The error messages shown when dependency resolution fails have been improved.

### Compiler WASM API

- The WASM API for the compiler has been rewritten to be simpler.
- The WASM API for the compiler now exposes warnings.

### HTML documentation generator

- Searching in rendered HTML documentation now also matches words that do not
  start with the input but do contain it.

### Bug fixes

- Fixed a bug where the JavaScript code generator could generate invalid code
  when pretty printing a zero arity function call when the line is over 80
  columns wide.
- Fixed a bug where the build directory could be left in an invalid state if
  there is Elixir code to compile and running on Windows without permission to
  create symlinks.
- Fixed a bug where numbers with preceeding zeros could generate incorrect
  JavaScript.
- The Erlang code generated by the `/.` operator no longer generates a warning
  for the upcoming negative zero float change in Erlang OTP 27.
- Fixed a bug where using only types from an aliased import, wouldn't stop the
  compiler from emitting an unused alias warning for that import.
- Fixed a bug where the formatter would remove the ` as name` from string prefix
  patterns.
- Fixed a bug where the formatter would misplace comments at the start of a
  block.
- Fixed a bug where using a string prefix pattern in `let assert` would generate
  incorrect JavaScript.


## v0.32.4 - 2023-11-09

### Build tool changes

- The build tool now supports rebar3 and mix hex packages where the package name
  differs from the otp application name.

### bug fixes

- Fixed a bug where invalid javascript code could be generated when a module
  function calls another function that was passed as an argument and the
  argument has the same name as the module function.
- Fixed the `target` property of `gleam.toml` being ignored for local path
  dependencies by `gleam run -m module/name`


## v0.32.3 - 2023-11-07

### Language changes

- Imported modules can now be discarded by giving them an alias starting with `_`.

### Build tool changes

- New projects are now generated with the call to `gleam format` coming last in
  the GitHub Actions workflow. This is so that feedback from tests is presented
  even if formatting is incorrect.
- Added Windows support for the `gleam export erlang-shipment` command.

### Bug fixes

- Fixed a bug where some nested pipelines could fail to type check.


## v0.32.2 - 2023-11-03

### Build tool changes

- New Gleam projects are created with `gleam_stdlib` v0.32 and `gleeunit` v1.0.

### Bug fixes

- Fixed a bug where `gleam fix` would not produce correct results for code that
  shadowed a prelude name with an import of the same name but a different kind.
- Fixed a bug where documentation would not publish to Hexdocs for packages with
  a number in the name.
- Fixed a bug where aliased unqualified types and values of the same name could
  produce an incorrect error.


## v0.32.1 - 2023-11-02

### Bug fixes

- Fixed a bug where `gleam fix` would not produce correct results for code that
  shadowed a prelude name with an import of the same name but a different kind.
- Fixed a bug where incorrect JavaScript could be generated due to backwards
  compatibility with the deprecated import syntax.


## v0.32.0 - 2023-11-01

### Bug fixes

- Fixed a bug where running `gleam fix` multiple times could produce incorrect
  results.


## v0.32.0-rc3 - 2023-10-26

### Bug fixes

- Fixed a bug where `gleam fix` would fail to update the deprecated type import
  syntax for aliased unqualified types.


## v0.32.0-rc2 - 2023-10-26

### Bug fixes

- Fixed a bug where the backward compatibility for the deprecated import syntax
  could result in an import error with some valid imports.


## v0.32.0-rc1 - 2023-10-25

### Language changes

- Using `import module.{TypeName}` to import a type has been deprecated,
  replaced by `import module.{type TypeName}`. In a future version of Gleam the
  old syntax will only import the value of the same name. Run `gleam fix` to
  update your code.
- The `BitString` type has been renamed to `BitArray`. Run `gleam fix` to update
  your code.
- The `binary` and `bit_string` bit array modifier have been deprecated in favour
  of `bytes` and `bits`.
- The error message for when one element in a list doesn't match the others has
  been improved.
- The error message for when the elements of a list's tail don't match the
  previous ones has been improved.
- The error message for when one tries to access an unknown field has been
  improved.
- The `__gleam_prelude_variant__` property has been removed from the classes
  defined in the JavaScript prelude.
- The deprecated `todo("...")` syntax has been removed.
- Module access can now be used in case clause guards.
- The JS target now supports bit syntax for module constants.
- The Erlang compiler will no longer emit a duplicate warning for unused
  functions.
- The `@deprecated` attribute can now be used with type definitions.
- A warning is now emitted if a module alias is unused.

### Language server changes

- The language server now has a code action for removing unused items.
- The language server now shows the type of variables defined using `use` on
  hover.

### Build tool changes

- The `gleam check` command supports the `target` flag.
- The `gleam fix` command updates code to use `BitArray` rather than `BitString`.
- The `gleam fix` command updates code to use the new import type syntax.
- `gleam fix` sets the `gleam` version constraint in `gleam.toml` to `>= 0.32.0`.
- The `gleam` version constraint field in `gleam.toml` now disregards pre and
  build components when checking for compatibility.
- The prelude is no longer rendered once per package when compiling to
  JavaScript, instead one copy is rendered for the entire project. If you are
  using the `gleam compile-package` API you now need to give a path to the
  prelude using the `--javascript-prelude` flag.
- The `gleam export javascript-prelude` and `gleam export typescript-prelude`
  commands have been added to export a copy of the prelude. This command may be
  useful for build tools that use the compiler via the `gleam compile-package`
  API.
- Fixed a bug where some deprecation messages would not be printed.
- The content has been made wider in rendered HTML documentation.
- Dependencies that can be built with both `mix` and `rebar3` are now built
  with `mix` if it exists on the system, and with `rebar3` if it doesn't.

### Bug fixes

- "Compiling $package" is now only printed when a package has new changes to
  compile.
- The main process started with `gleam run` no longer traps exits on Erlang.
- The formatting of code in rendered HTML documentation has been improved.
- The formatter no longer moves trailing comments out of custom type definitions.
- Fixed a bug where some hexidecimal numbers would generate incorrect Erlang.
- Fixed a bug where markdown tables would not render correctly in HTML
  documentation.
- The float 0.0 is now rendered in Erlang as `+0.0` to silence warnings in
  Erlang/OTP 27.

## v0.31.0 - 2023-09-25

- New Gleam projects are created with `gleam_stdlib` v0.31, `actions/checkout`
  v3.\*, and `erlef/setup-beam` v1.\*.
- A note is included in the generated HTML documentation if a function is
  deprecated.

## v0.31.0-rc1 - 2023-09-18

- The `@deprecated("...")` attribute can be used to mark a function as
  deprecated. This will cause a warning to be emitted when the function is used.
- A warning is now emitted if a module from a transitive dependency is imported.
- Record access can now be used in case clause guards.
- Fixed a bug where `manifest.toml` could contain absolute paths for path
  dependencies.
- The `as` keyword can now be used to assign the literal prefix to a variable
  when pattern matching on a string.
- The `if` conditional compilation, `external fn`, and `external type` syntaxes
  have been removed.
- The `description` flag for the `gleam new` command has been removed.
- The highlight.js grammar included with generated HTML documentation has been
  updated for the latest syntax.
- Packages are no longer precompiled to Erlang when publishing to Hex if the
  package target is set to JavaScript.
- An exception is now raised if JavaScript code uses the `BitString` class
  constructor and passes in the incorrect argument type.
- Fixed a bug where mutually recursive functions could be incorrectly inferred
  as having an overly general type.
- Fixed a bug where recursive type constructors could incorrectly infer a type
  error.
- Fixed a bug where some mutually recursive functions would be inferred as
  having too general a type.
- Fixed a bug where constants where not being correctly inlined when used in the
  size option of a bit string pattern match.
- Fixed a bug where anonymous functions could parse successfully when missing a
  body.
- Fixed a bug where incorrect unused variable warnings could be emitted for code
  that doesn't type check.
- Fixed a bug where packages defaulting to the JavaScript target could have
  modules missing from their HTML documentation when published.
- Corrected some outdated links in error messages.
- Hovering over a function definition will now display the function signature,
  or the type of the hovered argument.
- Use `import type` for importing types from typescript declarations.
- Use `.d.mts` extension for typescript declarations to match `.mjs`.
- Prefix module names with dollar sign in typescript to avoid name collisions.

## v0.30.4 - 2023-07-26

- External implementations are always referenced directly in generated code, to
  avoid the overhead of an extra function call.
- Fixed a bug where the compiler could infer incorrect generic type parameters
  when analysing a module without type annotations with self recursive
  functions that reference themselves multiple times.

## v0.30.3 - 2023-07-23

- Fixed a bug where JavaScript module path such as `node:fs` would be rejected.
- New Gleam projects are created with `gleam_stdlib` v0.30, Erlang OTP v26.0.2,
  Elixir v1.15.4, actions/checkout v3.5.1, and erlef/setup-beam v1.16.0.

## v0.30.2 - 2023-07-20

- Fixed a bug where the compiler could infer incorrect generic type parameters
  when analysing a module without type annotations with self recursive
  functions.
- Fixed a bug where the formatter would incorrectly format external functions
  by breaking the return annotation instead of the function arguments.

## v0.30.1 - 2023-07-13

- Fixed a bug where the language server could fail to import path dependencies
  in monorepos.

## v0.30.0 - 2023-07-12

- A warning is now emitted for the deprecated external fn syntax.

## v0.30.0-rc4 - 2023-07-10

- An error is now emitted for invalid JavaScript external implementations.
- Fixed a bug where Erlang external implementations could generate invalid code.

## v0.30.0-rc3 - 2023-07-05

- Fixed a bug where `gleam fix` would fail to parse command line flags.

## v0.30.0-rc2 - 2023-07-03

- Fixed a bug where `gleam fix` would merge external functions of the same name
  but incompatible types.
- Fixed a bug where external function arguments would incorrectly be marked as
  unused.

## v0.30.0-rc1 - 2023-06-29

- The new `@target(erlang)` and `@target(javascript)` attribute syntax has been
  added for conditional compilation. The existing `if` conditional compilation
  syntax has been deprecated. Run `gleam fix` to update your code.
- The new `type TypeName` syntax syntax replaces the `external type TypeName`
  syntax. The existing external type syntax has been deprecated. Run `gleam format`
  to update your code.
- Adding a new dependency now unlocks the target package. This helps avoid
  failing to find a suitable version for the package due to already being
  locked.
- A custom message can now be specified for `panic` with `panic as "..."`.
- The syntax for specifying a custom message for `todo` is now `todo as "..."`.
- The Erlang error raised by `let assert` is now tagged `let_assert`.
- Types named `Dynamic` are now called `dynamic_` in Erlang to avoid a clash
  with the new Erlang `dynamic` type introduced in OTP26.
- Dependencies can now be loaded from paths using the
  `packagename = { path = "..." }` syntax in `gleam.toml`.
- The `javascript.deno.unstable` field in `gleam.toml` can now be used to
  enable Deno's unstable APIs when targeting JavaScript.
- Blockquotes are now styled in rendered HTML documentation.
- The `gleam` property can be set in `gleam.toml` can be set to a version
  requirement to specify the version of Gleam required to build the project.
- Type aliases can now refer to type aliases defined later in the same module.
- Fixed a bug where unapplied record constructors in constant expressions would
  generate invalid Erlang.
- Fixed a bug where the prescedence of `<>` and `|>` would clash.
- Fixed a bug where `gleam docs build` would print an incorrect path upon
  completion.
- Warnings from dependency packages are no longer surfaced in the language
  server.
- A warning is now emitted when a Gleam file is found with an invalid name.
- A warning is now emitted when using `list.length` to check for the empty list,
  which is slow compared to checking for equality or pattern matching (#2180).
- The new `gleam remove <package_name>` can be used to remove dependencies
  from a Gleam project.
- Fixed a bug where the formatter could crash.
- Fixed a bug where invalid Erlang would be generated when piping into `panic`.
- The `gleam docs build` command gains the `--open` flag to open the docs after
  they are generated (#2188).
- Fixed a bug where type annotations for constants could not be written with
  type annotations.
- Updated font loading in generated HTML documentation to fix an issue with
  fonts not loading properly in some browsers (#2209).

## v0.29.0 - 2023-05-23

- New projects now require `gleam_stdlib` v0.29.

## v0.29.0-rc2 - 2023-05-22

- The `gleam lsp` command is no longer hidden from the help output.
- Fixed a bug where some language server clients would show autocompletion
  suggestions too eagerly.

## v0.29.0-rc1 - 2023-05-16

- The language server will now provide autocomplete suggestions for types and
  values either imported or defined at the top level of the current module.
- Fixed a bug where record patterns using the spread operator (`..`) to discard
  unwanted arguments would not type check correctly when the record had no
  labelled fields.
- Add support for using sized binary segments in pattern matches when targeting
  JavaScript.
- A warning is now emitted for double unary negation on ints (`--`) and bools
  (`!!`) as this does nothing but return the original value.
- Previously the build tool would discard the entire build directory when dependencies
  were changed. Now it will only discard the build artefacts for removed
  dependencies.
- The errors emitted when a name is reused in a module have been made clearer.
- Fixed an incorrect URL in the error message for failing to parse a let binding
  with a type annotation.
- Fixed a bug where shadowing a prelude type name could result in incorrect
  errors in exhaustiveness checking.
- Fixed a bug where the language server would in some scenarios not remove an
  error diagnostic after it becomes outdated.
- Fixed a bug where the formatter would incorrectly format blocks with a comment
  before them that were the only argument to a function call.
- Fixed a bug where the language server would not reset the build directory when
  it was created by a different version of Gleam.
- New Gleam projects are created with `erlef/setup-beam@v1.15.4` in their GitHub
  actions CI configuration.
- Running a module now uses the dependency's target and runtime in its `gleam.toml`.

## v0.28.3 - 2023-04-17

- Fixed a bug where the language server would show outdated error diagnostics
  when a new one was emitted in a different module.
- Fixed a bug where the language server would attempt to analyse Gleam modules
  that were outside of the `src` or `test` directories.
- New Gleam projects are created with `actions/checkout@v3.5.1` and
  `erlef/setup-beam@1.15.3` in their GitHub actions CI configuration.

## v0.28.2 - 2023-04-10

- Fixed a bug where comments above a `use` expression would be formatted
  incorrectly.
- Fixed a bug where the formatter would fail to preserve empty lines after a
  block.
- Fixed a bug where the formatter would fail to preserve empty lines after an
  anonymous function with a return annotation.

## v0.28.1 - 2023-04-05

- Fixed a bug where the language server would unset too many error diagnostics
  when multiple projects are open, more than one have errors, and one of them is
  successfully compiled.
- Fixed a bug where the language server would unset error diagnostics when
  displaying information on hover.
- Added support for type annotations in use statements.

## v0.28.0 - 2023-04-03

- New projects now require `gleam_stdlib` v0.28.

## v0.28.0-rc3 - 2023-03-31

- Fixed a bug where source links would be incorrect in HTML documentation.

## v0.28.0-rc2 - 2023-03-27

- Fixed a bug where single statement blocks inside binary operators could
  generate invalid JavaScript.
- Fixed a bug where the formatter could incorrectly place comments.
- Fixed a bug where the language server would show outdated diagnostics when a
  file with an error reverts to the previous valid version, causing the compiler
  to use the cached version of the file.

## v0.28.0-rc1 - 2023-03-26

- The language server now analyzes files on edit rather than on save, providing
  feedback faster.
- The language server now supports editor sessions that span multiple projects.
  This is useful for mono-repos and projects with both a frontend and backend in
  Gleam.
- The language server now also shows documentation on hover for expressions.
- The language server now shows types and documentation on hover for patterns.
- Added support for negation of integers with the new `-` unary operator.
- Variable assignments are now only permitted within a function or a block, not
  anywhere that an expression is permitted.
- The deprecated `try` expression has been removed.
- The deprecated `assert ... = ...` syntax has been removed.
- Semicolons are no longer whitespace. An error will be emitted if one is
  encountered.
- Warnings are now immediately emitted rather than being buffered until the end
  of the compilation.
- The `--warnings-as-errors` flag is now supported by `gleam build`.
- Blocks are now preserved by the formatter when they only have a single
  expression within them.
- Generated docs now export more meta data to improve the developer experience,
  accessibility and search engine discoverability.
- Files are now only recompiled if they have changed since the last compilation,
  detected by file hash and modification time. Previously only the modification
  time was used.
- Autocompletion of module imports was removed due to a buggy implementation.
- Fixed a bug where the formatter would incorrectly remove `{ ... }` from bit
  string segment value expressions.
- Fixed a bug where TypeScript type definitions files could include incorrect
  type names.
- Fixed a bug where the compiler used VSCode specific behaviour in the language
  server which was incompatible with Helix.
- Fixed a bug where string concatenation patterns on strings with escape
  characters would generate javascript code with wrong slice index.
- Fixed a bug where blocks could parse incorrectly.
- Allow modules to be run with the `gleam run --module` command.

## v0.27.0 - 2023-03-01

- Fixed a bug where `panic` could generate incorrect JavaScript code.
- New projects now require `gleam_stdlib` v0.27.

## v0.27.0-rc1 - 2023-02-26

- The new `panic` keyword can be used to crash the program. This may be useful
  for situations in which a program has got into an unrecoverable invalid state.
- `try` expressions are now deprecated and will be removed in a future version.
- The new `gleam fix` command can be used to automatically convert `try`
  expressions to `use` expressions.
- `let assert ... = ...` is now the syntax for assertion assignments. The
  `assert ... = ...` syntax is deprecated and will be removed in a future
  version. Run `gleam format` to automatically update your code.
- `gleam export hex-tarball` can be used to create a tarball suitable for
  uploading to a Hex compatible package repository.
- The unused private type and constructor detection has been improved.
- The argument `--runtime` now accepts `nodejs` as the name for that runtime.
  The previous name `node` is still accepted.
- Patterns can now be used in `use` expressions.
- Fixed a bug where string concatenation patterns could generate javascript
  code with wrong slice index due to ut8/ut16 length mismatch.
- The Erlang compiler will no longer emit a duplicate warning for unused
  variables.
- Fixed a bug where typescript type definitions for types with unlabelled
  arguments where generated with an invalid identifier and unlabelled fields
  were generated with a name that didn't match the javascript implementation.
- Fixed a bug in the type inferrer were unannotated functions that were
  used before they were defined in a module could in rare cased be inferred
  with a more general type than is correct.
- Fixed a bug where the LSP would fail to show type information on hover for
  expressions after a use expression.
- Fixed a bug where imported constants could generated incorrect JavaScript
  code.
- Fixed a bug where the LSP would perform codegen for dependencies.
- Fixed a bug where the LSP would compile native dependencies needlessly.
- Fixed a bug where integer division with large numbers on JavaScript could
  produce incorrect results.
- Fixed a bug where pattern matches on custom types with mixed labelled and
  unlabelled arguments could not be compiled when targeting JavaScript.
- Fixed a bug where local variables in case guard constant expressions caused
  the compiler to panic.
- The formatter now truncates meaningless zeroes of floats' fractional parts.
- Anonymous functions may now have an empty body. The compiler will emit a
  warning for functions without a body, and these functions will crash at
  runtime if executed.
- Fixed bug where raised errors on JS would have an extra stack frame recorded
  in them.

## v0.26.2 - 2023-02-03

- The formatter now wraps long `|` patterns in case clauses over multiple lines.
- Fixed a bug where unlabelled function arguments could be declared after
  labelled ones.
- A broken link was removed from the error messages.
- Fixed a bug where using a qualified imported record constructor function as a
  value would produce invalid Erlang code if the name of the record variant was
  an Erlang reserved word.

## v0.26.1 - 2023-01-22

- New projects now require `gleeunit` v0.10.
- Rebar3 dependency projects are now compiled in-place. This fixes an issue
  where some NIF using projects would fail to boot due to some paths not being
  copied to the `build` directory.
- An error is now emitted if a list spread expression is written without a tail
  value.
- An error is now emitted when a function is defined with multiple arguments
  with the same name.
- The error message emitted when a `let` does not match all possible values has
  been improved.
- Fixed a bug where the language server wouldn't analyse test code.
- Fixed a bug where `assert` expressions can generate invalid Erlang.
  warning.
- Fixed a bug where arguments would be passed incorrectly to Deno.
- Fixed a bug where defining variables that shadow external functions could
  generate invalid JavaScript.

## v0.26.0 - 2023-01-19

[Release blog post](https://gleam.run/news/v0.26-incremental-compilation-and-deno/)

- New projects require `gleam_stdlib` v0.26 and `gleeunit` v0.9.
- Fixed a bug where JavaScript default projects would fail to publish to Hex.

## v0.26.0-rc1 - 2023-01-12

- Added support for Deno runtime for JavaScript target.
- Scientific notation is now available for float literals.
- The compiler now supports incremental compilation at the module level. If a
  module or its dependencies have not been changed then it will not be
  recompiled.
- The format used by the formatter has been improved.
- 4 digit integers are now always formatted without underscores.
- Running `gleam new` will skip `git init` if the new project directory is
  already part of a git work tree.
- Generated HTML documentation now includes all static assets, including web
  fonts, so that it can be accessed offline and in future once CDNs would 404.
- Generated HTML documentation now supports TypeScript syntax highlighting.
- New Gleam projects are created using GitHub actions erlef/setup-beam@v1.15.2.
- Some modules can now be hidden from the docs by specifying a list of glob
  patterns in `internal_modules` in `gleam.toml`. The default value for this
  list is `["$package_name/internal", "$package_name/internal/*"]`.
- The `gleam new` command gains the `--skip-git` flag to skip creation of
  `.git/*`, `.gitignore` and `.github/*` files.
- The `gleam new` command gains the `--skip-github` flag to skip creation of
  `.github/*` files.
- Fixed a bug where no error would be emitted if a `src` module imported a
  `test` module.
- Fixed a bug where comments in list prepending expressions could be formatted
  incorrectly.
- Fixed a bug where comments in record update expressions could be formatted
  incorrectly.
- Fixed a bug where long `use` expressions could be formatted incorrectly.
- Fixed a bug integer multiplication would overflow large integers when
  compiling to JavaScript.
- Fixed `int` and `float` formatting in `const`s and patterns.
- Fixed a bug where piping into a function capture expression with a pipe as one
  of the arguments would produce invalid Erlang code.
- Formatter no longer removes new lines in expression blocks within case branches

## v0.25.3 - 2022-12-16

- 4 digit integers are no longer automatically formatted with underscores.

## v0.25.2 - 2022-12-16

- Updated `actions/checkout` from `actions/checkout@v3.0.0` to `@v3.2.0` for
  projects created via `gleam new`.
- Fixed a bug where `gleam new` would set a `Rebar3` version to `25.1`
  instead of the latest stable `3`.
- Updated following runtime versions set via `gleam new`: `Erlang/OTP`
  to `25.2`, and `Elixir` to `1.14.2`.
- The formatter now inserts underscores into larger `Int`s and the larger
  integer parts of `Float`s.
- Added support for top level TypeScript file inclusion in builds.
- The build tool will now favour using rebar3 over Mix for packages that support
  both. This fixes an issue where some packages could not be compiled without
  Elixir installed even though it is not strictly required.

## v0.25.1 - 2022-12-11

- New Gleam projects are now configured to explicitly install rebar3 using
  GitHub actions erlef/setup-beam.
- A better error message is now shown when attempting to use a function within a
  constant expression.
- Changed float size limit in bit string expressions to 16, 32 or 64, when static.
  Also allowed dynamic size.
- New Gleam projects are created using GitHub actions erlef/setup-beam@v1.15.0.
- Fixed a bug where returning an anonymous function from a pipeline and calling
  it immediately without assigning it to a variable would produce invalid Erlang
  code.
- Fixed a bug where the formatter would remove the braces from negating boolean
  expressions.

## v0.25.0 - 2022-11-24

[Release blog post](https://gleam.run/news/v0.25-introducing-use-expressions/)

## v0.25.0-rc2 - 2022-11-23

- Fixed a bug where Gleam dependency packages with a `priv` directory could fail
  to build.
- Fixed a regression where Elixir and Erlang Markdown code blocks in generated
  documentation would not be highlighted.

## v0.25.0-rc1 - 2022-11-19

- Generated HTML documentation now includes the `theme-color` HTML meta tag.
- The `use` expression has been introduced. This is a new syntactic sugar that
  permits callback using code to be written without indentation.
- Nightly builds are now also published as OCI container images hosted on
  GitHub.
- Fixed a bug where the build tool would not hook up stdin for Gleam programs it
  starts.
- Fixed a bug where using a record constructor as a value could generate a
  warning in Erlang.
- Fixed a bug where the build tool would use precompiled code from Hex packages
  rather than the latest version, which could result in incorrect external
  function usage in some cases.
- Fixed a bug where the warning for `todo` would not print the type of the code
  to complete.
- Fixed a bug where `try` expressions inside blocks could generate incorrect
  JavaScript.
- Generated HTML documentation now includes all static assets (but the web
  fonts), so that it can be accessed offline or in far future once CDNs would 404.
- New Gleam projects are created using GitHub actions erlef/setup-beam@v1.14.0
- The `javascript.typescript_declarations` field in `gleam.toml` now applies to
  the entire project rather than just the top level package.
- The formatter now adds a `0` to floats ending with `.` (ie `1.` => `1.0`).
- New projects require `gleam_stdlib` v0.25.

## 0.24.0 - 2022-10-25

[Release blog post](https://gleam.run/news/gleam-v0.24-released/)

## 0.24.0-rc4 - 2022-10-23

- Fixed a bug where the string concatenate operator could produce invalid Erlang
  code when working with pipe expressions.

## 0.24.0-rc3 - 2022-10-20

- Fixed a bug where the OOP method call error hint would be shown on too many
  errors.
- Fixed a bug where the string concatenate operator could produce invalid Erlang
  code when working with constant values.

## 0.24.0-rc2 - 2022-10-18

- Fixed a bug where imported and qualified record constructors used in constant
  expressions could fail to resolve.

## 0.24.0-rc1 - 2022-10-15

- Gleam can now compile Elixir files within a project's `src` directory.
- The `<>` operator can now be used for string concatenation and for string
  prefix pattern matching.
- Fixed a bug where TypeScript definitions may have incorrect type parameters.
- New projects depend on `gleam_stdlib` v0.24.
- New projects' GitHub Actions config specifies Erlang/OTP 25.1 and suggest
  Elixir 1.14.1.
- If you attempt to use the method call syntax (`thing.method()`) on a value
  without that field the error message will now include a hint explaining that
  Gleam is not object oriented and does not have methods.
- Fixed a bug in the formatter where multiple line documentation comments for
  custom type constructor fields could be formatted incorrectly.
- Fixed a bug where tail call optimisation could be incorrectly applied when
  compiling to JavaScript in some situations.
- Fixed a bug where the remainder operator would return NaN results when the
  right hand side was zero when compiling to JavaScript.
- Fixed a bug where Elixir dependencies would fail to compile on Windows.
- Fixed a bug where images added to HTML documentation via documentation
  comments would not have a max width.

## v0.23.0 - 2022-09-15

[Release Blog Post](https://gleam.run/news/gleam-v0.23-released/)

## v0.23.0-rc2 - 2022-09-15

- New Gleam projects are created using GitHub actions erlef/setup-beam@v1.13.0
  and actions/checkout@v3.0.0.
- New Gleam projects are created using version v0.23.0 of the stdlib.
- Fixed a bug where LSP hovering would fail to locate the expression.

## v0.23.0-rc1 - 2022-09-01

- Gleam can now build dependency packages that are managed using Mix.
- Compiler performance has been improved by buffering disc writing and by lazily
  loading TLS certs. In testing this doubles performance when compiling the
  standard library.
- The `gleam publish` command now adds the `priv` directory and any `NOTICE`
  file to the tarball.
- The `gleam update` command can now be used to update dependency packages to
  their latest versions.
- Module functions with empty bodies are no longer syntax errors.
- The format used by the formatter has been improved.
- OpenSSL swapped out for RustTLS.
- Generated HTML documentation now includes a search bar.
- The LSP will now provide autocompletion for imports.
- A helpful error message is now returned when assignments are missing either a
  keyword or a value.
- Qualifiers are now used when multiple types have the same name in an error
  message.
- In JavaScript, if an object has defined an `equals` method in its prototype,
  Gleam will now use this method when checking for equality.
- Functions can now be defined and referenced in constant expressions.
- An error is now raised if the record update syntax is used with a custom type
  that has multiple constructors.
- An error is now raised if a module is imported multiple times.
- Fixed a bug where defining a type named `CustomeType` would product invalid
  JavaScript.
- Fixed a bug where defining a variable with the same name as an unqualified
  import would produce invalid JavaScript.
- Fixed a bug where piping to `todo` would generate invalid Erlang code.
- Fixed a bug where inspecting a JavaScript object with a null prototype would
  crash.
- Fixed a bug where the formatter could crash if source code contained 3 or more
  empty lines in a row.
- Fixed a bug where the formatter would remove braces from blocks used as the
  subject of a case expression.
- Fixed a bug alternative patterns with a clause containing a pipe with a pipe
  after the case expresson could render incorrect Erlang.
- Fixed a bug where formatter would strip curly braces around case guards even
  when they are required to specify boolean precedence.
- Fixed a bug where `gleam new` would in some situations not validate the
  target directory correctly.
- Fixed a bug where pipes inside record update subjects could generate invalid
  Erlang.
- Fixed a bug where pipes inside record access could generate invalid Erlang.

## v0.22.1 - 2022-06-27

- The `gleam publish` confirmation prompt now accepts both "Y" and "y".
- Fixed a bug where `todo` would not emit the correct line number to the LSP while.

## v0.22.0 - 2022-06-12

[Release Blog Post](https://gleam.run/news/gleam-v0.22-released/)

- New projects are created with `gleam_stdlib` v0.22.

## v0.22.0-rc1 - 2022-06-12

- Fixed a bug where doc comments would dissociate from their statements when
  generating html documentation.
- You are now allowed to use named accessors on types with multiple constructors if the
  accessor's name, position and type match (among the constructors) (#1610).
- Added the ability to replace a release up to one hour after it is published
  using `gleam publish --replace`.
- `gleam publish`, `gleam docs publish`, `gleam docs remove`, `gleam hex retire`,
  and `gleam hex unretire` now have access to environment variables for
  username (default key `HEXPM_USER`) and password (default key `HEXPM_PASS`)
- The `gleam publish` command gains the `-y/--yes` flag to disable the "are you
  sure" prompt.
- Clear outdated files from the build directory after compilation.
- Fixed a bug where immediately calling the value that a case expression
  evaluates to could generate invalid JavaScript.
- Fixed a bug where the default project target is set to JavaScript,
  but the project would run on target Erlang instead.
- The compiler is now able to generate TypeScript declaration files on target
  JavaScript (#1563). To enable this edit `gleam.toml` like so:

  ```toml
  [javascript]
  typescript_declarations = true
  ```

- Fixed a bug where argument labels were allowed for anonymous functions.
- Fixed a bug where JavaScript code could be invalid if a variable is defined
  inside an anonymous function with a parameter with the same name as the
  variable.
- Fixed a bug where importing a JavaScript function named "then" could produce
  invalid code.
- Fixed a bug where constants that reference locally defined custom types could
  render invalid JavaScript.
- The project generator will no longer permit use of the reserved `gleam_`
  prefix.
- Generated HTML docs easter egg updated.
- `gleam export erlang-shipment` can be used to create a directory of compiled
  Erlang bytecode that can be used as a deployment artefact to get your
  application live.
- `gleam format` will now preserve (up to one) empty lines between consecutive
  comments, as well as between comments and any following expression
- The deprecated rebar3 integration has been removed.
- Fixed a bug where `gleam format` would output an unwanted newline at the top
  of documents that only contain simple `//` comments.
- No longer add `dev-dependencies` to generated `.app` Erlang files unless
  we're compiling the root project (#1569).
- Fixed a bug where the formatter could render a syntax error with lists on long
  unbreakable lines.
- Fixed a bug where JavaScript variable names could be incorrectly reused.
- Fixed a bug where `gleam format` would remove the braces around a tuple index
  access when accessing a field of the returned element.
- Fixed a bug case clause guards could render incorrect JavaScript if a variable
  name was rebinded in the clause body.
- The `gleam compile-package` command no longer generates a `.app` file. This
  should now be done by the build tool that calls this command as it is
  responsible for handling dependencies.
- Fixed a bug where piping a list tail would create invalid Erlang code (#1656).

## v0.21.0 - 2022-04-24

[Release Blog Post](https://gleam.run/news/v0.21-introducing-the-gleam-language-server/)

- New projects are created with `gleam_stdlib` v0.21.

## v0.21.0-rc2 - 2022-04-20

- Added the ability to replace a release up to one hour after it is published
  using `gleam publish --replace`.
- The language server will now enter a degraded mode that only performs
  formatting if running in a directory that is not a Gleam project with a
  `gleam.toml`.

## v0.21.0-rc1 - 2022-04-16

- The Gleam language server is here! This will provide IDE like features for
  code editors that support LSP, including but not limited to VSCode, Neovim,
  Emacs, Eclipse, Visual Studio, and Atom. This first version includes these
  features:
  - Project compilation.
  - Inline errors and warnings.
  - Type information on hover.
  - Go-to definition.
  - Code formatting.
- Fixed a bug in generated JavaScript code where functions named `then` would
  cause errors when dynamically imported.
- Initialize `git` repo when creating a new project.
- Log messages controlled with `GLEAM_LOG` now print to standard error.
- Log message colours can be disabled by setting the `GLEAM_LOG_NOCOLOUR`
  environment variable.
- You can now specify multiple packages when using `gleam add`.
- Bools can now be negated with the `!` unary operator.
- If the compiler version changes we now rebuild the project from scratch on
  next build command to avoid issues arising from reading metadata in an old
  format (#1547).
- Updated the "Unknown label" error message to match other error messages
  (#1548).
- Type holes are now permitted in function arguments and return annotations
  (#1519).
- Unused module imports now emit a warning (#1553).
- The error message for failing to parse a multiline clauses without curly
  braces has been improved with a hint on how to fix the issue (#1555).
- The error messages for when rebar3 or Erlang are missing from the machine has
  been improved with a tip on how to install them (#1567).
- Corrected the hint given with certain int and float binary operator type
  errors.
- Add support for `int` and `float` bit string type when compiling to JavaScript.
- Add support for specifying size of integers in a bit string. Supports only exact binaries,
  i.e. length is a multiple of 8.
- Fixed compilation of rebar3 based dependencies on Windows.

## v0.20.1 - 2022-02-24

- The type checker has been improved to enable use of the record access syntax
  (`record.field`) in anonymous functions passed into higher order functions
  without additional annotations.

## v0.20.0 - 2022-02-23

[Release Blog Post](https://gleam.run/news/gleam-v0.20-released/)

- New projects are created with `gleam_stdlib` v0.20.

## v0.20.0-rc1 - 2022-02-20

- Type unification errors involving user annotated types now refer to the names
  specified by the user instead of internal rigid-type ids.
- The build tool now validates that listed licenses are valid SPDX expressions.
- A WebAssembly version of the compile is now available for use in JavaScript
  and other WebAssembly environments.
- New projects include Hex badges and a link to Hexdocs.
- Enhance type mismatch errors in the presence of try.
- Enhance type mismatch error for an inconsistent try.
- Enhance type mismatch error for pipe expressions to show the whole pipeline
  and not only its first line.
- Fixed a bug where sometimes type variable could be reused result in incorrect
  non-deterministic type errors.
- Built in support for the Mix build tool has been removed. The `mix_gleam`
  plugin is to be used instead.
- Introduce a limited form of exhaustiveness checking for pattern matching
  of custom types, which only checks that all constructor tags are covered
  at the top level of patterns.
- The `ebin` directory is now copied to the build directory for rebar3 managed
  dependencies if present before compilation.
- The format used by the formatter has been improved.
- Package names in `gleam.toml` are validated when the config is read.
- The `priv` directory is linked into the build directory for Gleam projects
  managed by the build tool.
- Fixed a bug where type errors from pipes could show incorrect information.
- Fixed a bug where types could not be imported if they had the same name as a
  value in the prelude.

## v0.19.0 - 2022-01-12

Dedicated to the memory of Muhammad Shaheer, a good and caring man.

[Release Blog Post](https://gleam.run/news/gleam-v0.19-released/)

## v0.19.0-rc4 - 2022-01-10

- New projects are created with `gleam_stdlib` v0.19 and `gleeunit` v0.6.
- Fixed a bug where external functions could be specified with the wrong module
  name in generated Erlang when imported from a nested module in another
  package.
- Fixed a bug where warnings wouldn't get printed.

## v0.19.0-rc3 - 2022-01-07

- Fixed a bug where precompiled packages would fail to compile due to Erlang
  files being compiled twice concurrently.

## v0.19.0-rc2 - 2022-01-06

- Erlang modules are now compiled in a multi-core fashion.
- New projects are created with `erlef/setup-beam` v1.9.0 instead of
  `gleam-lang/setup-erlang` and `gleam-lang/setup-gleam`.
- Fixed a bug where tail call optimisation could generate incorrect code when
  the function has argument names that are JavaScript keywords.
- Fixed a bug where the build would continue when dependency packages failed to
  compile.
- Fixed a bug where `include` directories would not be accessible by the Erlang
  compiler during Gleam compilation.

## v0.19.0-rc1 - 2022-01-03

- The build tool now supports the JavaScript target. The target can be specified
  in either `gleam.toml` or using the `--target` flag.
- The `gleam check` command has been introduced for rapidly verifying the types
  of Gleam code without performing codegen.
- `true` and `false` can no longer be used as pattern matching variables, to
  avoid accidental uses of incorrect syntax that is popular in other languages.
  An error will hint about using Gleam's `True` and `False` values instead.
- You can now remove build artifacts using the new `gleam clean` command.
- The `compile-package` can now generate `package.app` files and compile source
  modules to `.beam` bytecode files.
- The flags that `compile-package` accepts have changed.
- Published Hex packages now include precompiled Erlang files.
- Erlang record headers are now written to the `include` directory within the
  package build directory.
- The format used by the formatter has been improved.
- Fixed a bug where tail recursion could sometimes generated incorrect
  JavaScript code.
- Performance of code generators has been slightly improved.
- Allow the record in a record expansion to be an expression that returns a
  record.
- Fixed a bug where external function module names would not be escaped
  correctly if they contained special characters and were assigned to a
  variable.
- A helpful error message is shown if Erlang is not installed.

## v0.18.2 - 2021-12-12

- Erlang applications are now automatically started when the VM is started by
  `gleam run` and `gleam test`.

## v0.18.1 - 2021-12-12

- Fixed a bug where pipe expressions in record updates and operator expressions
  could generate incorrect Erlang code.
- The `priv` directory is now copied to the output directory for rebar3 packages
  prior to compilation. This is required for some packages to compile.
- Fixed a bug where deps that fail to compile would be skipped when compilation
  would next be attempted, resulting the project being in an invalid state.

## v0.18.0 - 2021-12-06

[Release Blog Post](https://gleam.run/news/gleam-v0.18-released/)

- New projects now include `gleeunit`.

## v0.18.0-rc3 - 2021-12-05

- URL format in gleam.toml is now validated.
- The `gleam deps list` command has been added.
- Fixed a bug where changing requirements in `gleam.toml` would not cause deps
  to be re-resolved.
- Fixed a bug where locked deps would cause incompatible package requirements to
  be discarded.
- Development dependencies are now included in the applications listed in the
  generated OTP `.app` file.
- `gleam.toml` now includes an `erlang.extra_applications` key to specify extra
  OTP applications that need to be started.

## v0.18.0-rc2 - 2021-11-26

- Fixed a bug where OTP .app files would be generated with invalid syntax.
- Removed extra whitespace from newly generated projects.

## v0.18.0-rc1 - 2021-11-25

- Gleam can now compile Gleam projects.
- Gleam can now run tests with the `gleam eunit` command.
- Gleam can now run programs with the `gleam run` command.
- Gleam can now run an Erlang shell with the `gleam shell` command.
- Gleam can now resolve package versions for a Gleam project's dependency tree.
- Gleam can now download Hex packages.
- Gleam can now build dependency packages that are managed using Gleam or
  rebar3.
- Gleam is now the default build tool for new projects.
- The template names for `gleam new` have been changed.
- Fixed a bug where the error message for a record update with an unknown field
  would point to all the fields rather than the unknown one.
- Improved styling for inline code in generated documentation.
- New projects use v0.18 of the stdlib.

## v0.17.0 - 2021-09-20

[Release Blog Post](https://gleam.run/news/gleam-v0.17-released/)

- Functions now get special handling when being printed from JavaScript.

## v0.17.0-rc2 - 2021-09-19

- Errors thrown when no case clause or assignment pattern matches the subject
  value now include more debugging information when targeting JavaScript.
- New projects are generated using `gleam_stdlib` v0.17.1.

## v0.17.0-rc1 - 2021-09-11

- Redesigned the Gleam prelude to be a module of core classes when compiling to
  JavaScript. This improves the resulting generated code and makes debugging and
  interop easier.
- Projects without rebar3 can be generated using the `gleam-lib` template.
- JavaScript modules are imported using a camel case variable name to avoid name
  collisions with variables.
- Pipelines now use assignments in the generated code in order to preserve the
  order of any side effects.
- Fixed a bug where the compiler would crash rather than raise an error if a
  project contained a single module and attempted to import another.
- Special variable naming has been made more consistent in rendered Erlang and
  JavaScript.
- Conditional compilation can now be used to have different code within a module
  when compiling to a specific target.
- Fixed a bug where `todo` caused values not to be returned in JavaScript.
- Fixed a bug where multiple discarded function arguments generated invalid
  JavaScript.
- Fixed a bug where using JavaScript reserved words as function argument names
  caused generated invalid JavaScript.
- Fixed a bug where a case expression of just a catch-all pattern generated
  invalid JavaScript.
- Fixed a bug where the formatter would incorrectly render extra newlines below
  try expressions.
- Fixed a bug where tail recursive functions with arguments with the same name
  as JavaScript reserved words generated the wrong JavaScript.
- Fixed a bug where list equality would be incorrectly reported in JavaScript.
- Multiple subjects are now supported for case expressions in JavaScript.
- Fixed a bug where matching using a Bool or Nil literal as the subject for a
  case expression would produce invalid code when compiling to JavaScript.
- Unsupported feature error messages now include file path and line numbers for
  debugging.
- Bit string literals with no segment options or just the `bit_string`, `utf8`
  or `utf8_codepoint` options can be constructed when compiling to JavaScript.
- The format of generated JavaScript has been improved.
- Fixed a bug where rendered JavaScript incorrectly incremented variables when
  reassigned in patterns.
- Added `eval` and `arguments` to JavaScript reserved words.
- Support for the deprecated `tuple(x, y, ...)` syntax has been removed in favor
  of the more concise (`#(x, y, ...)`). Use `gleam format` with the previous
  version of the compiler to auto-migrate.
- New OTP projects are generated using `gleam_otp` v0.1.6.
- Fixed a bug where the equality operators could return the incorrect value for
  records when compiling to JavaScript.
- Fixed a bug where `todo` could sometimes render invalid JavaScript when used
  as an expression in the generated code.
- An error is now emitted if the list spread syntax is used with no prepended
  elements `[..xs]`.
- Fixed a bug where type errors inside piped expressions would be incorrectly be
  reported as being an incorrect usage of the pipe operator.
- Gleam modules with no public exports no longer render private members in
  Erlang.
- Fixed a bug where discard variables used in assert assignments would generate
  invalid Erlang code.
- Fixed a bug where some expressions as case subjects would generate invalid
  JavaScript code.
- Fixed a bug where some assignments as the final expression in a function would
  not return the correct value in JavaScript.
- Gleam packages imported in JavaScript now have the path prefix
  `gleam-packages`. This can be served from your web server or aliased in your
  `package.json` for NodeJS projects.
- Fixed a bug where the type checker would fail to generalise some type
  variables, causing module metadata writing to fail.
- Fixed a bug where tail call optimisation when compiling to JavaScript could
  result in incorrect code.
- Fixed a bug where variable names could be rendered incorrectly in closures.
- An error is now emitted if alternative patterns fail to define all the
  variables defined by the first pattern.
- New projects are generated using `gleam_stdlib` v0.17.0.
- New projects are generated using `gleam_otp` v0.2.0.

## v0.16.1 - 2021-06-21

- Values which are being imported more than once in an unqualified fashion now
  cause an error to be reported.
- Argument docs for custom type constructors are now rendered in the HTML
  documentation.
- Patterns can be used with `try` expressions when compiling to JavaScript.
- Types and record constructors can now be aliased with an uppercase name when
  imported. Aliasing them with a lowercase name is no longer permitted.
- Fixed a bug where nested import paths could be rendered incorrectly in
  JavaScript.

## v0.16.0 - 2021-06-17

[Release Blog Post](https://gleam.run/news/gleam-v0.16-released/)

## v0.16.0-rc4 - 2021-06-17

- Fixed a bug where if a JavaScript global function was imported as an external
  function with the same name the generated code would diverge.

## v0.16.0-rc3 - 2021-06-17

- New projects are generated using `gleam_stdlib` v0.16.0.

## v0.16.0-rc2 - 2021-06-08

- Gleam now supports alternative patterns in case expressions for the JavaScript target.
- The `gleam` prelude module can now be imported when compiling to JavaScript.
- Fixed a bug where the prelude module could not be imported when using the old
  build compiler API.
- Fixed a bug where if a JavaScript global function was imported as an external
  function with the same name the generated code would diverge.
- Type error messages coming from pipe usage have been improved.

## v0.16.0-rc1 - 2021-06-04

- Gleam can now compile to JavaScript! Specify the `--target javascript` flag to
  `gleam compile-package` to use it today.
- A compile time error is now raised when multiple module level constants with
  the same name are defined.
- Fixed a bug where declaring a type constructor using reserved erlang keyword
  in its fields results in invalid erlang code being generated.
- Fixed a bug where calling a function with discarded labelled arguments
  incorrectly results in a compile error.
- Fixed a bug where assert statements return the wrong value.
- The `gleam new` command requires a root folder param, project name is
  optional and if not provided the project name will be inferred from
  the folder name.
- Generated Erlang record header files now contain Erlang type information.
- New OTP application projects depend on `gleam_otp` v0.1.5.
- The output of the formatter has been improved.

## v0.15.1 - 2021-05-07

- Fixed a bug where blocks that contained try expressions could be formatted
  incorrectly.

## v0.15.0 - 2021-05-06

[Release Blog Post](https://gleam.run/news/gleam-v0.15-released/)

## v0.15.0-rc1 - 2021-05-05

- Syntax highlighting of Gleam code in generated HTML documentation has been
  improved.
- Fixed a bug where markdown tables in rendered HTML documentation would have
  the incorrect background colour on every other row.
- Tuples now have a new, concise syntax variant: `#(x, y, ...)`. Existing code
  can be auto-migrated to the new syntax by running `gleam format`.
- Fixed a bug where customt type constructors with Erlang keywords as names
  would generate invalid Erlang code.
- Gleam now supports `\e` string escapes.
- Values and types from the prelude can now be used in a qualified fashion by
  importing the `gleam` module.
- Empty lists can now be used in constants.
- Compiler performance has been improved when working with lists.
- Compiler performance has been improved when working with sequences of
  expressions.
- Assignments using `let` and `assert` are now expressions and no longer require
  a following expression in their containing block. They are now themselves
  expressions.
- Fixed a bug where tuple indexing could incorrectly claim a tuple is not of
  type tuple in some circumstances.
- Glean `new` command now checks if target folder exists, if so it returns
  an error.
- A compile time error is now raised if a module is defined with the name `gleam`.
- A compile time error is now raised if a module is defined with the a keyword
  in the name.
- New projects are generated using `gleam_stdlib` v0.15.0.
- New projects are generated at v0.1.0.

## v0.14.4 - 2021-03-27

- The Gleam compiler has been updated to compile with the new Rust v1.51.0.
- New project's `gleam.toml` has a comment that shows how to add a
  `repository` field.
- New projects no longer include a licence field in `src/$APP.app.src` by
  default.

## v0.14.3 - 2021-03-20

- Added an error hint when joining string using the `+` or `+.` operator.
- New projects are created with `setup-erlang` v1.1.2 and Erlang/OTP v23.2.
- Fixed a bug where the compiler would be unable to locate an imported module
  if a value from a nested module is used in a qualified fashion.

## v0.14.2 - 2021-03-02

- Project names can now contain numbers.

## v0.14.1 - 2021-02-27

- The error message for binary operators has been given more detail and
  hints.
- Fixed a bug where alternative patterns would incorrectly report unused
  variables.
- Fixed a bug where private types shadowed shadowed by values would
  incorrectly report unused variables.

## v0.14.0 - 2021-02-18

[Release Blog Post](https://gleam.run/news/gleam-v0.14-released/)

## v0.14.0-rc2 - 2021-02-18

- New projects are created with `gleam_stdlib` v0.14.0.

## v0.14.0-rc1 - 2021-02-14

- Gleam now generates Erlang typespecs.
- New projects no longer include a licence file by default.
- New projects can be created using the new `escript` template to generate a
  command line tool style program.
- A warning is emitted when a literal value is constructed but not used.
- Automatically generate a link to repository in docs if available.
- Code in HTML documentation is has highlighted syntax.
- Gleam now only supports `\r`, `\n`, `\t`, `\"`, and `\\` string escapes.
- A set of OCI container images are built automatically for each release.
- New compile time checks for invalid bit string literals and patterns have
  been added.
- The error messages for syntax errors in names have been improved.
- Fixed a bug where the repo URL would render incorrectly in HTML docs.
- Fixed a bug where piping a block can render invalid Erlang.
- New compile time warnings on unused types, functions and variables.
- The runtime error emitted by the `todo` keyword now carries additional
  information.
- The runtime error emitted by the `assert` keyword now carries additional
  information.
- Fixed a bug where bit string patterns would not correctly unify with the
  subject being pattern matches on.
- Documentation dark mode.
- Fixed a bug where some app.src properties were incorrectly named.
- `--warnings-as-errors` flag added to `gleam build` command.

## v0.13.2 - 2021-01-14

- `ring` dep upgraded to enable compilation on Apple M1 ARM processors.

## v0.13.1 - 2021-01-13

- Fix off-by-one error in message messages.

## v0.13.0 - 2021-01-13

[Release Blog Post](https://gleam.run/news/gleam-v0.13-released/)

- New Gleam projects use stdlib v0.13.0.

## v0.13.0-rc2 - 2021-01-12

- The `version` property in `gleam.toml` is now optional again.

## v0.13.0-rc1 - 2021-01-09

- Variable names now only have 1st letter capitalized when converted to erlang.
- Records defined in other modules can now be used in module constants.
- Documentation can link from functions, types & constants to their source
  code definitions on popular project hosting sites.
- Documentation hosted on HexDocs now has a version selector.
- Fixed a bug where the `app` project template rendered invalid code.
- Newly generated projects use stdlib v0.12.0.
- Named subexpressions in patterns now render correct Erlang.
- The anonymous function syntax now successfully parses with whitespace
  between `fn` and `(`.
- Fixed a bug where the formatter would incorrectly remove blocks around some
  binary operators.
- Constants can now be defined after they are used in functions
- The parser has been rewritten from scratch, dramatically improving error
  messages and compilation times.
- `1-1` and `a-1` are now parsed as `1 - 1` and `a - 1`
- Further information has been added to the error messages when a function
  returns the wrong type.
- Further information has been added to the error messages when case clauses
  return different types.
- Fixed a bug where imported record constructors without labels used as an
  anonymous function generates incorrect Erlang.

## v0.12.1 - 2020-11-15

- The compiler can now discriminate between record access and module access
  for shadowed names
- The `new` command will no longer permit projects to be made with names that
  clash with Erlang standard library modules.
- The formatter now correctly treats lines of only whitespace as empty.
- The styling of tables in rendered HTML documentation has been improved.
- Rendered HTML documentation has regained its max-width styling.

## v0.12.0 - 2020-10-31

[Release Blog Post](https://gleam.run/news/gleam-v0.12-and-gleam-otp-v0.1-released/)

## v0.12.0-rc4 - 2020-10-31

- The rendered module documentation sidebar can now scroll independently to
  the page.
- Application projects now have the correct `mod` value in the generated
  `.app.src`.
- Records without fields can now be used in module constants.
- New application projects are now created used Gleam's type safe OTP pulled
  from Hex.

## v0.12.0-rc3 - 2020-10-24

## v0.12.0-rc2 - 2020-10-24

## v0.12.0-rc1 - 2020-10-24

- The utf8, utf16, and utf32 type specifiers are now only available in bit
  string construction, matching must be done with the codepoint versions.
- Functions may now be called before they are defined in a module. This
  enabled mutually recursive functions!
- Discarded variable names may now include numbers.
- Fixed a bug where discarded variables might generate incorrect Erlang.
- Added support tuple access in clause guards.
- New projects are created with version 1.0.2 of the setup-gleam GitHub
  action.
- New application projects are now created used Gleam's type safe OTP.
- Comments are now correctly handled on platforms that use \r\n line endings,
  such as Windows.

## v0.11.2 - 2020-09-01

- Fixed a bug where an imported constructor would emit an unused constructor
  warning when only used in pattern matching.

## v0.11.1 - 2020-08-31

- The formatter style has been improved to render function type arguments on
  a single line when possible, even if the return type will not fit on a
  single line.
- The format for printed types in error messages has been improved.
- Fixed a bug where the formatter would strip a constructor pattern spread
  when no fields are given.
- Fixed a bug where assigning the result of a block to a variable would
  generate incorrect Erlang.
- The formatter style has been improved for function calls that take a single
  block as an argument.
- Reserved words are no longer incorrectly permitted as project names.

## v0.11.0 - 2020-08-28

[Release Blog Post](https://lpil.uk/blog/gleam-v0.11-released/)

## v0.11.0-rc3 - 2020-08-27

- Bit strings now support non-literal strings as segment values.
- Fixed a bug where Erlang variables could be generated with incorrect names
  when defining an anonymous function.

## v0.11.0-rc2 - 2020-08-24

- The formatter style has been improved to render some single argument calls
  in a more compact style.

## v0.11.0-rc1 - 2020-08-22

- Field access now works before the custom type is defined.
- The error message returned by the compiler when the user tries to use unknown
  labelled arguments now handles multiple labels at once, and does not suggest
  labels they have already supplied.
- The formatter style has been improved to use a trailing comma on imports
  broken over multiple lines.
- The formatter style has been improved to wrap lists and bit strings over as
  few lines as possible if the elements are Ints, Floats, or Strings.
- The formatter style has been improved to preserve comments on labelled
  call arguments.
- The formatter style has been improved to preserve empty lines in assignments.
- The performance of the formatter has been improved.
- Records can be updated using the spread syntax. A warning is emitted if no
  fields are updated when using this syntax.
- Fixed a bug where type parameters can leak between different type
  definitions in a module.
- Markdown tables, footnotes, strikethroughs, and tasklists are now supported
  in documentation.
- Fixed a bug where generic types may be incorrectly unified.
- Ints and floats can now be written with underscores for clarity.
- The warning for a `todo` now includes the required type of the
  not-yet-implemented expression.
- Holes can be used in type annotations to specify part of a type, leaving the
  rest for inference.
- The incorrect arity error now prints any missing labelled arguments.
- Fixed a bug where Erlang variables could be generated with incorrect names
  when directly calling an anonymous function.
- A warning is emitted when a type is imported or created but not used.
- Fixed a bug where Erlang variables names could clash when rebinding
  variables while similarly named variables ending in a number are in scope.
- Fixed a bug in the pretty printer which prevented the formatter from
  rendering sub-expressions in a single line when later code would not fit on
  the same line.
- The formatter style has been improved to render some single argument calls
  in a more compact style.
- Gleam now supports hex, octal, and binary literals.
- Rebar3 hex packages now include `gleam.toml` and `gen`.
- Newly generated projects use stdlib v0.11.0.

## v0.10.1 - 2020-07-15

- Fixed a bug where the compiler failed to return an error when type checking
  a tuple with the wrong arity in a pattern.
- The error message for a duplicate module member now shows the location of
  both definitions.
- Fix compiler bug where labelled arguments were being reordered incorrectly.

## v0.10.0 - 2020-07-01

[Release Blog Post](https://lpil.uk/blog/gleam-v0.10-released/)

- Newly generated projects use stdlib v0.10.1.
- Fixed a bug where discards inside bit string patterns generated invalid
  code.

## v0.10.0-rc2 - 2020-06-30

- Fixed a bug where variables names would be incorrectly generated when using
  alternative patterns.

## v0.10.0-rc1 - 2020-06-29

- Single letter module names are now permitted.
- Added support for bit string syntax.
- Support for the deprecated list prepend syntax has been removed.
- Added module level constants that are inlined at compile time.
- Public module level constants generate documentation.
- The formatter style has been improved to wrap and sort imports.
- The formatter now permits comments at the end of module function bodies.
- The formatter now skips files that match patterns defined in ignore files
  such as .gitignore and .ignore.
- Error message diagnostic code previews for type errors when using the the
  pipe operator have been made more accurate.
- Added support for list literals in clause guards.
- Fixed bug when reassigning a variable inside a case clause with alternative
  patterns.
- Todos can now take an optional label.

## v0.9.1 - 2020-06-12

- Fixed a bug where binary operators may lose required `{ }`s when formatted.

## v0.9.0 - 2020-06-01

[Release Blog Post](https://lpil.uk/blog/gleam-v0.9-released/)

- Newly generated projects use stdlib v0.9.0.
- Additional information is printed to the console when generating HTML
  documentation from Gleam code.
- Fixed a bug where blocks on either side of a binary operator would be
  rendered without `{ }`.

## v0.9.0-rc1 - 2020-05-26

- The formatter style has been improved.
- Numbers are now permitted in module names.
- Emitted Erlang code correctly adds parentheses around binary subexpressions
  to preserve precedence.
- Record names and fields are now escaped in `.hrl` files if they conflict
  with Erlang reserved words
- Annotations are now supported on `let` and `assert` expressions
- Formatter now accepts comments for the fields of a custom type's constructors
- Added opaque custom types, which have constructors that cannot be accessed
  from outside their own modules.
- Additional (arbitrary) markdown documentation pages can now be added and
  built with `docs build`.
- Fix code generation when calling functions returned through either record
  or tuple access
- Add lookup for Gleam source code in Mix's `deps` directory.
- Newly generated Gleam projects use the GitHub action
  `gleam-lang/setup-erlang` v1.1.0.
- Added support for custom type record literals in guards.
- Type variables are now correctly preserved within nested scopes.

## v0.8.1 - 2020-05-19

- The formatter now correctly handles unicode comments.

## v0.8.0 - 2020-05-07

[Release Blog Post](https://lpil.uk/blog/gleam-v0.8-released/)

- The `docs build`, `docs publish`, and `docs remove` commands can be used to
  compile HTML documentation locally, publish them to HexDocs, and remove them
  from HexDocs respectively.
- Type error reporting has been improved when using the pipe operator.
- Newly generated projects use stdlib v0.8.0.
- The compiler can now emit warnings. Currently there are warnings for using
  the old '|' syntax in lists and for todos.
- Will give a clearer error when a function given as an argument to another
  function doesn't match the type of the parameter.
- Fixed bug where imported type constructors had the incorrect arity.
- Fixed bug where a doing an unqualified import of a type constructor and
  giving it an alias would use the wrong name if it contained any values.
- Fixed a bug trying to access an imported constructor which contained values.
- Fixed a compiler crash that occurred when trying to unify a tuple with something
  other than another tuple or a variable.
- Added support for tuple literals in guards.

## v0.8.0-rc1 - 2020-04-28

- Strings are now encoded as utf8 binaries in the generated Erlang.
- HTML documentation can now be generated from Gleam code by running `gleam build --doc`.
- Gleam code can be formatted using the `gleam format` command.
- The pipe operator `|>` will now attempt to insert the left hand side as the
  first argument to the right hand side if the right hand side is a call,
  removing the need for function capture boilerplate.
- A `record.label` syntax can now be used to access the fields of a custom
  type that have a single record variant.
- Anonymous functions can now have return type annotations.
- There is a `todo` keyword for type checking functions that have not yet been
  implemented.
- Tuples can be indexed into using the `var.1` syntax.
- `>`, `>=`, `<`, and `<=` operators are now supported in case clause guards
  and can be used to check the ordering of integers.
- `>.`, `>=.`, `<.`, and `<=.` operators are now supported in case clause
  guards and can be used to check the ordering of floats.
- The list prepend syntax is now `[x, ..y]`. The old `[x | y]` syntax is
  deprecated but will continue to work for now. The formatter will rewrite the
  old syntax to the new.
- Add new assert syntax for binding variables `assert Ok(x) = result`. In the
  future this will allow you to use a pattern that does not match all values.
- Added support for int and float literals in guards.
- Color codes are now only emitted in error output for interactive terminal
  sessions.
- Added a new `..` syntax for discarding the remaining fields of a record.
- Using the same variable name multiple times in the same pattern will now
  raise an error.
- Discard can now be omitted in list tails in patterns, ie `[x, ..]` is the
  same as `[x, .._]`. The former is the preferred version and is emitted by the
  formatter.

## v0.7.1 - 2020-03-03

- Projects generated with `gleam new` use `stdlib` version 0.7.0.

## v0.7.0 - 2020-03-01

[Release Blog Post](https://lpil.uk/blog/gleam-v0.7-released/)

## v0.7.0-rc1 - 2020-02-28

- Type aliases can be defined to give concise names to frequently used types.
- Case expression clauses may have guards which can be used to require
  equality between specified variables in order for the clause to match.
- Case expression clauses may have alternative patterns, enabling one clause
  to match for multiple different possible patterns.
- Types may now be used before they are defined within their defining module.
- Fixed a bug where import paths would not be correctly resolved on Windows.
- Added job to create precompiled binary for 64-bit Windows when releasing.
- `gleam new` now creates a project that uses `actions/checkout@v2.0.0` in its
  GitHub actions workflow.
- Labelled argument in functions may now be discarded by prefixing the name
  with an underscore, like unlabelled arguments.
- Sub-patterns can have names assigned to them within a pattern using the `as`
  keyword.
- The format of compiler error messages printed to the console has been
  improved by upgrading to a newer version of the codespan-reporting library.
- Type variables in the given and expected types will now be printed with the
  same name in type error messages if they are equivalent.
- A friendly error message is rendered when a case expression clause has the
  incorrect number of patterns for the subjects.
- A friendly error message is rendered when a .gleam file cannot be read.
- A friendly error message is rendered when the `gleam new` command fails to
  write the new project to the file system.
- A friendly error message is rendered when there is a cycle formed by module
  imports.
- Top level types are now printed in error messages for type parameter mismatches.
- The `gen` directory is now deleted before each compilation.
- `gleam new` now includes installation instructions for Hex packages in the
  generated README.
- `gleam new` now accepts a `--description` flag for including a description of
  the project in the README and `.app.src` file.
- Fixed a bug where variable names would be incorrectly generated in some
  situations when variable names are reused during and after a case
  expression.
- Performance of the Erlang code generator has been improved by removing some
  vector allocations.
- An error is emitted when multiple types with the same name are defined in or
  imported into a module.

## v0.6.0 - 2019-12-25 🎄

[Release Blog Post](https://lpil.uk/blog/gleam-v0.6-released/)

- Function capture syntax now supports labelled arguments.

## v0.6.0-rc1 - 2019-12-23

- Syntax for defining structs and enums have been unified into a singular
  custom type definition statement. Instances of these custom types are called
  records.
- Anonymous structs have been renamed tuples.
- Values and types can be given a new name when imported in the unqualified
  fashion using the `import mod.{value as name}` syntax.
- An error will be emitted if multiple values constructors are defined with
  the same name in a module.

## v0.5.1 - 2019-12-23

- Fixed a bug where invalid Erlang would be generated when using a local
  private function as a value.

## v0.5.0 - 2019-12-16

[Release Blog Post](https://lpil.uk/blog/gleam-v0.5-released/)

- Enum constructor arguments can now be labelled, allowing arguments to be
  given by name at the call site.
- An Erlang header file with a record definition is generated for each Gleam
  struct defined.
- `gleam new` creates a project at v1.0.0.
- Function calls are now properly escaped when the function name conflicts
  with an Erlang keyword.
- References to unqualified imported functions now generate correct Erlang
  code.
- Fixed a bug where variable rebinding would generate incorrect code in some
  case expressions.
- Fixed a bug where variable rebinding of function arguments would generate
  incorrect code.

## v0.5.0-rc1 - 2019-11-26

- Function arguments can be labelled, allowing arguments to be given by name
  at the call site.
- `case` expressions now accept multiple subjects, enabling pattern matching
  on multiple values simultaneously.
- Values and types can be imported from modules and references in an
  unqualified fashion.
- Named structs now have their name as the first element in the generated
  Erlang code. This enabled easier use from Erlang by defining records for
  them, as well as slightly clearer printf debugging.
- Anonymous structs have been introduced, serving as a quick and generic
  alternative to declared structs and as a format for interop with Erlang
  tuples.
- `gleam new` now accepts a `--template` flag to generate different styles of
  project. An OTP application template has been added alongside the existing
  OTP library template.
- `gleam new` now creates configuration for GitHub Actions, making Gleam
  projects ready for continuous integration out of the box.
- The syntax for defining enums, case expressions, and blocks has been changed
  to a syntax closer to that found in the C family of languages.
- The source code preview for functions that return a type incompatible with
  the functions annotations has been improved to be more precise.
- A helpful error message is rendered if an enum field contains a generic type
  that has not been declared.
- A bug has been fixed in which type mismatch errors originating from pattern
  matching would sometimes display the incorrect expected type.

## v0.4.2 - 2019-10-22

- Fixed a crash when an incorrect number of labelled struct arguments are
  given.
- Fixed a struct labelled argument being incorrect reported as already given.

## v0.4.1 - 2019-09-29

- Struct types with parameterised fields are now registered with the correct
  number of type parameters.

## v0.4.0 - 2019-09-19

[Release Blog Post](https://lpil.uk/blog/gleam-v0.4-released/)

- The struct data type has be introduced. Structs are pre-declared user
  defined data types with named fields and constant access time.
- The map and tuple data types has been removed, replaced by the struct data
  type.
- The generated code no longer contains export statements if no functions are
  exported from a module.
- Comparison operators have been specialised to operate only on Ints.
- The `>.` `>=.` `<.` and `<=.` comparison operators have been added for
  comparing Floats.
- It is now an error to export an enum which has a constructor that takes a
  private type as an argument.
- The error messages for defining multiple modules with the same name and for
  importing test modules into application code have been improved.
- Numbers are now permitted in type names and constructors.
- The `Nil` constructor will no longer erroneously be of type `Int`.

## v0.3.0 - 2019-08-08

[Release Blog Post](https://lpil.uk/blog/gleam-v0.3-released/)

- New project structure can be generated with the `gleam new` command.
- Functions can be annotated with their argument and return types. This may be
  used to restrict the function to a less general type than inferred by the
  compiler, or purely for documentation purposes.
- External function names and their target functions are now escaped in the
  generated code if they collide with Erlang keywords such as `catch` or `or`.
- Type error arising from the arguments of function calls have more accurate
  error diagnostics.
- Precompiled Gleam binaries are now available on the GitHub release page.
- Precompiled Docker images containing the Gleam binary are now available on
  DockerHub.
- The formatting of the Erlang code rendered by the compiler has been altered
  to improve legibility.
- A helpful error message is now rendered if the shorthand anonymous function
  syntax is used with too many underscores.
- A helpful error message is now rendered when attempting to import an unknown
  module.

## v0.2.0 - 2019-06-25

- Modules can now live within namespaces such as `my_app/user/profile`.
- The name of the variable created can be specified when importing a module
  using the `import my_mod as name` syntax.
- Function names and atoms are now escaped in the generated code if they
  collide with Erlang keywords such as `catch` or `or`.
- There is a shorthand syntax for prepending multiple elements to a list.
  `[1, 2, 3 | my_list]`

## v0.1.2 - 2019-05-12

- Types containing more than 26 type variables will no longer render with
  invalid type variable names.
- Types in error messages no longer have extra indentation that increases as
  the type gets larger.
- There is a new type `Nil` which is occupied by a single value (`Nil`). This
  type is used to represent the absence of a value and is commonly used with
  `Result` to model a value that is either present (`Ok(value)`) or absent
  (`Error(Nil)`).
- Zero arity enum constructors now generate the correct Erlang when used in
  modules other than the one they are defined in.

## v0.1.1 - 2019-04-28

- Error messages now display the path of the file containing the problem.
- Maps and modules with erroneous extra fields now have a custom error
  message.
- Rows with tails that are unbound type variables are now correctly unified in
  the type system. This fixes a bug in which maps and modules may sometimes
  fail to type check when there is no error.

## v0.1.0 - 2019-04-15

[Release Blog Post](https://lpil.uk/blog/hello-gleam/)

- Initial release!
