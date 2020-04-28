# Changelog

## v0.8.0-rc1 - 2020-04-28

- Strings are now encoded as utf8 binaries in the generated Erlang.
- HTML documentation can now be generated from Gleam code by running `gleam
  build --doc`.
- Gleam code can be formatted the `gleam format` command.
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
- `>.`, `>=.`, `<.`, and `<=.` operators are now supported in case clause guards
  and can be used to check the ordering of floats.
- The list prepend syntax is now [x, ..y]. The old [x | y] syntax is deprecated
  but will continue to work for now. The formatter will output the new syntax.
- Add new assert syntx for binding variables `assert Ok(x) = result`. In the future
  this will allow you to use a pattern that does not match all values.
- Added support for int and float literals in guards.
- Color codes are now only emitted in error output for interactive terminal sessions.
- Added a new `..` syntax for discarding the remaining fields of a record.
- Using the same variable name multiple times in the same pattern will now raise
  an error.
- Discard can now be omitted in list tails in patterns, ie `[x, ..]` is the
  same as `[x, .._]`. The former is the prefered version and is emitted by the
  formatter.

## v0.7.1 - 2020-03-03

- Projects generated with `gleam new` use `stdlib` version 0.7.0.

## v0.7.0 - 2020-03-01

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

- Initial release!
