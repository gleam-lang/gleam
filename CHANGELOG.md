# Changelog

## Unreleased

- External function arguments can be labelled, allowing arguments to be given
  by name at the call site.
- `gleam new` now accepts a `--template` flag to generate different styles of
  project. An OTP application template has been added alongside the existing
  OTP library template.
- The source code preview for functions that return a type incompatible with
  the functions annotations has been improved to be more precise.
- A helpful error message is rendered if an enum field contains a generic type
  that has not been declared.

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
