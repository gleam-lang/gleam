# Bool assert

There are times where we wish our Gleam code to deliberately panic:

- In tests when the code has shown incorrect behaviour.
- In production code when the code has entered into an invalid state.

In both cases we want to have sufficient information with the panic so the
programmer can understand what the problem is and correct it as easily as
possible. This might include:

- That the panic is intentional and not from bug in FFI code.
- The path of the file the panic occurred in.
- The line number the panic occurred on within the file.
- A message supplied by the programmer, should they want to include one.
- The values that were checked when deciding whether to panic.

It is challenging to achieve most of this in Gleam today.

The approach that comes closest is to manually construct a message and use it
with the `panic` keyword.

```gleam
case check_some_property(a, b, c) {
  True -> Nil
  False -> {
    let detail = "<Message explaining the problem here>

a: " <> string.inspect(a) <> "
b: " <> string.inspect(b) <> "
c: " <> string.inspect(c)

    panic as detail
  }
}
```

This contains most the information but has some problems.

The information is unstructured so it cannot be used easily by runtime
systems such as test frameworks, exception trackers, and the logger. At best
they can print the message as the programmer formatted it.

It is verbose. The programmer likely doesn't see this code as valuable enough
to invest the time in writing, which results it being rarely done. More commonly
`panic` will be used with no message, or a message without dynamic information
such as the runtime values that caused the problem.

Another approach is to use the `let assert` syntax with a bool pattern.

```gleam
let assert True = check_some_property(a, b, c)
```

This does have the file and line information, but has no message or information
about the values. We are planning to add support for a custom message to `let
assert`, at which point is could have the same amount of information as `case` +
`panic` approach, but with a slightly more concise syntax.

I have seen this pattern used in test code, but it is uncommon.

The last approach is to use runtime functions which internally use the `case` +
`panic` pattern, or some FFI equivalent. This is most commonly done
with assertion functions from test frameworks such as those found in
`gleeunit`'s `gleeunit/should` module.

```gleam
check_some_property(a, b, c)
|> should.be_true

some_computation(abc)
|> should.equal(xyz)
```

When using FFI to panic these can provide the information in a structured way
that can be used by test frameworks etc to provide more useful feedback, but
they can only provide information that a function can know about.

They cannot tell the line number they have been called from. They may be able to
determine the file by using FFI to throw and catch an exception and then parsing
the stacktrace, but it's an ugly and potentially brittle solution.

In `x |> should.equal(y)` the function has a reference to `x` and `y`, so it can
include them in the error information. In `f(abc) |> should.be_true` it only
receives a `True` or a `False` value, so it is unable to include anything about
the expression that evaluated to the bool, in this case `f` and `abc`.

This pattern is very common in test code.

## The proposal

I think Gleam would benefit from having a bool `assert` syntax, as the final way
to `panic`. This syntax would take an expression that evaluates to a `Bool`,
panicking if it is `False`, and including all of the information above in a
structured form in the panic.

```python
assert check_some_property(a, b, c)

assert wibble() == 123

assert level >= 30

assert wobble(a) |> wubble |> woo(b)
```

It could also take an optional message from the programmer.

```python
assert level >= 30 as "frozen orb requires level 30"
```

Gleam [runtime errors](../runtime-errors.md) always have at least the below
properties, and so `assert` would be no different.

| Key         | Type   | Value                                |
| ---         | ----   | -----                                |
| gleam_error | Atom   | See individual errors                |
| message     | String | See individual errors                |
| module      | String | The module the error occured in      |
| function    | String | The function the error occured in    |
| line        | Int    | The line number the error occured on |

`gleam_error` would be `"assert"`, and `message` would be the message supplied
by the programmer, defaulting to `"Assertion failed"` if none is supplied.

Depending on the expression given `assert` can include other information too.

```python
assert level >= 30
```

| Key       | Type   | Value                      |
| ---       | ----   | -----                      |
| kind      | Atom   | `binary_operator`          |
| operator  | Atom   | `>=`                       |
| left      | Map    | See expression table below |
| right     | Map    | See expression table below |

```python
assert check_some_property(a, b, c)
```

| Key       | Type        | Value                      |
| ---       | ----        | -----                      |
| kind      | Atom        | `function_call`            |
| arguments | List of map | See expression table below |


```python
assert other_expression
```

| Key        | Type  | Value                      |
| ---        | ----  | -----                      |
| kind       | Atom  | `expression`               |
| expression | Map   | See expression table below |

Where the expression maps have this structure:

| Key    | Type  | Value                                                |
| ---    | ----  | -----                                                |
| value  | t1    | the value the expression evaluated to at runtime     |
| kind   | Atom  | `literal` or `expression`                            |

With this the programmer would write minimal boilerplate (just the keyword
`assert`) and the runtime error handling tooling would be able to have a high
degree of capability with the error. For example, a test runner could produce
these errors:

```python
assert lock_user(username, 10, role) as "Lock required for user"
```
```
error: Lock required for user
test/myapp/user_test.gleam:45

    assert lock_user(username, 10, role)

1. "lucystarfish"
2. 10
3. Admin
```

```python
assert level >= 30
```
```
error: Assertion failed
test/myapp/user_test.gleam:45

    assert level >= 30

left:  27
right: 30
```
