# Gleam

```elm
module Shape

pub type Quadrilateral =
  | Square(Int)
  | Rectangle(Int, Int)

pub fn from_dimensions(width, height) {
  case width == height {
  | True => Square(width)
  | False => Rectangle(width, height)
  }
}

test from_dimensions {
  from_dimensions(100, 100)
    |> Assert.equal(_, Square(100))
  from_dimensions(100, 120)
    |> Assert.equal(_, Rectangle(100, 120))
}
```
