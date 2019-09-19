# Int and Float

Gleam's main number types are Int and Float.


## Ints

Ints are "whole" numbers.

```rust,noplaypen
1
2
-3
4001
```

Gleam has several operators that work with Ints.

```rust,noplaypen
1 + 1 // => 2
5 - 1 // => 4
5 / 2 // => 2
3 * 3 // => 9
5 % 2 // => 1

2 > 1  // => True
2 < 1  // => False
2 >= 1 // => True
2 <= 1 // => False
```

## Floats

Floats are numbers that have a decimal point.

```rust,noplaypen
1.5
2.0
-0.1
```

Floats also have their own set of operators.

```rust,noplaypen
1.0 +. 1.4 // => 2.4
5.0 -. 1.5 // => 3.5
5.0 /. 2.0 // => 2.5
3.0 *. 3.1 // => 9.3

2.0 >. 1.0  // => True
2.0 <. 1.0  // => False
2.0 >=. 1.0 // => True
2.0 <=. 1.0 // => False
```
