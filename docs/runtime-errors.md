# Runtime errors

There are several runtime errors that Gleam code can throw. This documentation
lists them and their runtime properties.

On Erlang runtime errors are Erlang maps thrown with `erlang:error/1`, having at
least these properties:

| Key         | Type   | Value                                 |
| ---         | ----   | -----                                 |
| gleam_error | Atom   | See individual errors                 |
| message     | String | See individual errors                 |
| module      | String | The module the error occurred in      |
| function    | String | The function the error occurred in    |
| line        | Int    | The line number the error occurred on |

On JavaScript runtime errors are instances of the JavaScript `Error` class,
having at least these properties added to them:

| Key         | Type   | Value                                 |
| ---         | ----   | -----                                 |
| gleam_error | String | See individual errors                 |
| message     | String | See individual errors                 |
| module      | String | The module the error occurred in      |
| function    | String | The function the error occurred in    |
| line        | Number | The line number the error occurred on |

## Todo

A panic that indicates that the code has not yet been completed, intended for
use in development.

```gleam
todo
todo as "some message"
```
| Key         | Erlang Value      | JavaScript Value  |
| ---         | ------------      | ----------------  |
| gleam_error | `todo`            | `"todo"`          |
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
let assert Error(e) = something() as "This should fail"
```
| Key                  | Erlang Value                                               | JavaScript Value                                          |
| ---                  | ------------                                               | ----------------                                          |
| gleam_error          | `let_assert`                                               | `"let_assert"`                                            |
| message              | The given message                                          | The given message                                         |
| value                | The unmatched value                                        | The unmatched value                                       |
| start                | The byte-index of the start of the `let assert` statement  | The byte-index of the start of the `let assert` statement |
| pattern_start        | The byte-index of the start of the asserted pattern        | The byte-index of the start of the asserted pattern       |
| pattern_end          | The byte-index of the end of the asserted pattern          | The byte-index of the end of the asserted pattern         |

## Assert

An assertion of a boolean value.

The error format of `assert` differs based on the expression that is asserted.
It always has these fields:

| Key              | Erlang Value                                           | JavaScript Value                                       |
| ---              | ------------                                           | ----------------                                       |
| gleam_error      | `assert`                                               | `"assert"`                                             |
| message          | The given message                                      | The given message                                      |
| kind             | The kind of asserted expression                        | The kind of asserted expression                        |
| start            | The byte-index of the start of the `assert` statement  | The byte-index of the start of the `assert` statement  |
| expression_start | The byte-index of the start of the asserted expression | The byte-index of the start of the asserted expression |
| expression_end   | The byte-index of the end of the asserted expression   | The byte-index of the end of the asserted expression   |

But, depending on the expression that was asserted, it contains additional
information which can be used to diagnose the error.

### Binary operators

```gleam
assert level >= 30
```

| Key       | Erlang Type  | JavaScript Type  | Value                              |
| ---       | -----------  | ---------------  | -----                              |
| kind      | Atom         | String           | `binary_operator`                  |
| operator  | Atom         | String           | The binary operator that was used  |
| left      | Map          | Object           | The left hand side of the operator |
| right     | Map          | Object           | The left hand side of the operator |

### Function calls

```gleam
assert check_some_property(a, b, c)
```

| Key       | Erlang Type  | JavaScript Type  | Value                                  |
| ---       | -----------  | ---------------  | -----                                  |
| kind      | Atom         | String           | `function_call`                        |
| arguments | List of map  | Array of objects | The arguments of the asserted function |

### Other expressions

```gleam
assert other_expression
```

| Key        | Erlang Type  | JavaScript Type  | Value                                |
| ---        | -----------  | ---------------  | -----                                |
| kind       | Atom         | String           | `expression`                         |
| expression | Map          | Object           | The value of the asserted expression |

The expression maps have this structure:

| Key    | Erlang Type  | JavaScript Type  | Value                                                             |
| ---    | -----------  | ---------------  | -----                                                             |
| value  | any          | any              | the value the expression evaluated to at runtime                  |
| kind   | Atom         | String           | `literal` or `expression` or `unevaluated`                        |
| start  | Int          | Number           | The byte-index of the start of this expression in the source code |
| end    | Int          | Number           | The byte-index of the end of this expression in the source code   |

If the expression is a literal, such as `True` or `15`, it will have the `literal`
kind. This signals that its value is not runtime dependent, and may not need to
be printed.

If the expression is on the right hand side of a short-circuiting operator, like
`||` or `&&`, it might not be evaluated. If the operator short-circuits, the right
hand side expression will have kind `unevaluated`.
