# Gleam `dev`

Currently Gleam code has to reside in one of two directories.

- `src`: The directory for production code that is part of the application or
  library itself.
- `test`: The directory for non-production code. This is likely mostly test
  code, but it could also include development helpers, code generation, etc.

The non-production directory being called `test` has been a repeat source of
confusion. The name implies it is only for test code, and so some folks have
taken to adding development code to their production application or library by
putting it in `src`.

It's also unclear how to add development specific code to your application and
it is not uncommon to have development-only code such as environment management
live within the `src` directory.

## The proposal

I think renaming the `test` directory to `dev` would resolve this problem,
setting clearer expectations about how to structure Gleam projects.

The `test` directory would continue to be loaded if `dev` is absent, but the
name would be deprecated and would emit a warning if used to house Gleam code.

`gleam fix` could rename `test` to `dev` if it contains Gleam code.

Further a `gleam dev` command could be added, which would be similar to `gleam
test`, serving as a shorthand for `gleam -m "$PACKAGE"_dev`, which would be an
optional development entrypoint for the project. This new entrypoint function
would be the recommended place to setup or inject development specific behaviour
into the application defined in `src`.
