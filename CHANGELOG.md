# Changelog

## Unreleased

- Added the ability to replace a release up to one hour after it is published
  using `gleam publish --replace`
- All commands that authenticate using the `ApiKeyCommand` trait (currently
  `gleam publish`, `gleam docs publish`, `gleam docs remove`, `gleam hex retire`,
  and `gleam hex unretire`) and now have access to environment variables for
  username (default key `HEXPM_USER`) and password (default key `HEXPM_PASS`)
- The `gleam publish` command gains the `-y/--yes` flag to disable the "are you
  sure" prompt
- Clear outdated files from the build directory after compilation.
- Fixed a bug where immediately calling the value that a case expression
  evaluates to could generate invalid JavaScript.
- Fixed a bug where running a project on the Erlang target when the default
  project target is set to JavaScript.

## v0.21.0 - 2022-04-24

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
  format. (#1547)
- Updated the "Unknown label" error message to match other error messages
  (#1548)
- Type holes are now permitted in function arguments and return annotations.
  (#1519)
- Unused module imports now emit a warning. (#1553)
- The error message for failing to parse a multiline clauses without curly
  braces has been improved with a hint on how to fix the issue. (#1555)
- The error messages for when rebar3 or Erlang are missing from the machine has
  been improved with a tip on how to install them. (#1567)
- Corrected the hint given with certain int and float binary operator type
  errors.
- Add support for `int` and `float` bitstring type when compiling to JavaScript.
- Add support for specifying size of integers in a bitstring. Supports only exact binaries,
  i.e. length is a multiple of 8.
- Fixed compilation of rebar3 based dependencies on Windows.

## v0.20.1 - 2022-02-24

- The type checker has been improved to enable use of the record access syntax
  (`record.field`) in anonymous functions passed into higher order functions
  without additional annotations.

## v0.20.0 - 2022-02-23

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
  could geneate incorrect Erlang code.
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
  value now include more debugging information when targetting JavaScript.
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
- Patterns can be used with `try` expresssions when compiling to JavaScript.
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
  expessions.
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
- The parser has been rewitten from scratch, dramatically improving error
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
  not-yet-implented expression.
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

# v0.10.0 - 2020-07-01

[Release Blog Post](https://lpil.uk/blog/gleam-v0.10-released/)

- Newly generated projects use stdlib v0.10.1.
- Fixed a bug where discards inside bit string patterns generated invalid
  code.

# v0.10.0-rc2 - 2020-06-30

- Fixed a bug where variables names would be incorrectly generated when using
  alternative patterns.

# v0.10.0-rc1 - 2020-06-29

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
- Fixed a compiler crash that occured when trying to unify a tuple with something
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
  same as `[x, .._]`. The former is the prefered version and is emitted by the
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
  same name in type error messages if they are equivilent.
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
