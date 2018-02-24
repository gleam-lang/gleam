# Gleam

```elm
module Shape

export Quadrilateral(..), from_dimensions/2

type Quadrilateral
  = Square(Int)
  | Rectangle(Int, Int)

fn from_dimensions(width, height) =
  case width
  | height => Square(width)
  | _other => Rectangle(width, height)
```
