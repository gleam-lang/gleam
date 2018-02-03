# Gleam

```elm
module Shape

export Quadrilateral(..), from_dimensions/2

type Quadrilateral
  = Square(Int)
  | Rectangle(Int, Int)

let from_dimensions(width, height) =
  match width
  | height => Square(width)
  | _other => Rectangle(width, height)
```

```sh
# Build the project
rebar3 compile

# Run the tests
rebar3 eunit

# Run the test watcher
# (requires rebar3_autotest)
rebar3 as test autotest

# Run the REPL
rebar3 shell
```
