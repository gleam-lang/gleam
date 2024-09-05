# Runtime errors

There are several runtime errors that Gleam code can throw. This documentation
lists them and their runtime properties.

On Erlang runtime errors are Erlang maps thrown with `erlang:error/1`, having at
least these properties:

| Key         | Type   | Value                                |
| ---         | ----   | -----                                |
| gleam_error | Atom   | See individual errors                |
| message     | String | See individual errors                |
| module      | String | The module the error occured in      |
| function    | String | The function the error occured in    |
| line        | Int    | The line number the error occured on |

On JavaScript runtime errors are instances of the JavaScript `Error` class,
having at least these properties added to them:

| Key         | Type   | Value                                |
| ---         | ----   | -----                                |
| gleam_error | String | See individual errors                |
| message     | String | See individual errors                |
| module      | String | The module the error occured in      |
| function    | String | The function the error occured in    |
| line        | Number | The line number the error occured on |

## Todo

A panic that indicates that the code has not yet been completed, intended for
use in development.

```gleam
todo
todo as "some message"
```
| Key         | Erlang Value      | JavaScript Value  |
| ---         | ------------      | ----------------  |
| gleam_error | `todo`            | `"todo"`         |
| message     | The given message | The given message |

## Panic

An explicit panic to unconditionally error.

```gleam
panic
panic as "some message"
```
| Key         | Erlang Value      | JavaScript Value  |
| ---         | ------------      | ----------------  |
| gleam_error | `panic`           | `"panic"`         |
| message     | The given message | The given message |

## Let assert

An inexhaustive pattern match, erroring if the pattern does not match.

```gleam
let assert Ok(x) = something()

```
| Key         | Erlang Value        | JavaScript Value    |
| ---         | ------------        | ----------------    |
| gleam_error | `let_assert`        | `"let_assert"`      |
| message     | The given message   | The given message   |
| value       | The unmatched value | The unmatched value |
