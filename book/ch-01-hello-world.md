# Hello, world!

:)

```elm
module Shape exposing Quadrilateral(..), from_dimensions/2

type Quadrilateral =
  | Square(Int)
  | Rectangle(Int, Int)

fn from_dimensions(width, height) =
  case width == height
  | True => Square(width)
  | False => Rectangle(width, height) |> go(_)

test from_dimensions =
  from_dimensions(100, 100) |> Assert.equal(_, Square(100))
  from_dimensions(100, 120) |> Assert.equal(_, Rectangle(100, 120))
```

```erlang
-module(shape).

-export([from_dimensions/2]).

-record(square, {width :: interger()}).
-record(rectangle, {width :: interger(), height :: interger()}).
-type quadrilateral :: square | rectangle.

from_dimensions(Width, Height) ->
  case width == height of
    true -> #square{width = Width};
    false -> #rectangle{width = Width, height = Height}
  end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

from_dimensions_test() ->
  ?assertEqual(from_dimensions(100, 100),
               #square{width = 100}),
  ?assertEqual(from_dimensions(100, 120),
               #rectangle{width = 100, height = 120}).
-endif.
```

```elixir
defmodule Shape do
  @type quadrilateral
        :: {:square, integer()}
        | {:rectangle, interger(), integer()}

  def from_dimensions(width, height) do
    if width == height do
      {:square, width}
    else
      {:rectangle, width, height}
    end
  end
end

defmodule ShapeTest do
  use ExUnit.Case

  test "from_dimensions/2" do
    assert Shape.from_dimensions(100, 100) == {:square, width}
    assert Shape.from_dimensions(100, 120) == {:rectangle, width, height}
  end
end
```
