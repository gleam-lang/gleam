# Gleam to JS

Working through the Gleam book to see how the language concepts translate to JS.

### Comments, Strings, Bools, Ints and Floats
These should all be trivial to map.

*Integers in JS are not "efficient" but all the Gleam operators should work appropriately and return more integers.*

From Gleam book and translated JS
```rs
1 + 1 // => 2
5 - 1 // => 4
5 / 2 // => 2
3 * 3 // => 9
5 % 2 // => 1
```

```js
1 + 1 // => 2
5 - 1 // => 4
parseInt(5 / 2) // => 2
3 * 3 // => 9
5 % 2 // => 1
```

JS has no way to check if something is a float or integer reliably.
`Number.isInteger(1.0) // => false`

### Let bindings
JS same.

### Lists

*JS lists are Arrays, so they will not have the same performance charachteristics.*
However they match very neatly for syntax

```rs
let [a, ..b] = [1, 2, 3]
let c = [4, ..b]
let d = [4, ..b]
```
```js
let [a, ...b] = [1, 2, 3]
if (a === undefined) throw "match error"
let c = [4, ...b]
let d = [4, ...b]
```

Note the first line is a match error if list length is less than 1.

### Tuple

Probably best covered by a JS array, which can be heterogeneous.

**Is it a problem that the dynamic library can't tell the difference between tuple and list in this case?**

### Case Expressions

JS `switch` statement uses strict equality and is not expressive enough.

```rs
case xs {
  [[]] -> "The only element is an empty list"
  [[], ..] -> "The 1st element is an empty list"
  other -> "Something else"
}
```

```js
function arrayEquals(a, b) {
  return Array.isArray(a) &&
    Array.isArray(b) &&
    a.length === b.length &&
    a.every((val, index) => val === b[index]);
}

(function gleamCase(term) {
  let tmp1, other;
  if (arrayEquals(term, [[]])) {
    return "The only element is an empty list";
  } else if (term.length >= 1 && ([tmp1] = term) && arrayEquals(tmp1, [])) {
    return "The 1st element is an empty list";
  } else if (other = term) {
    return "Something else";
  }
}(xs))
```

The hardest thing here is the fact that JS doesn't have good equality checks for lists objects, however there are so many well tested solutions to writing a deeps equals I'm not sure it's a problem.

### Functions

No difficulties

### modules
Should use es6 syntax for imports, pretty close mapping of concepts.

### Custom types

records can be objects with name as key, see result section below

### Result

```rs
case result {
  Ok(x) if x > 10 -> x + 5
  Ok(x) -> x + 1
  Error(_) -> 0
}
```

```js
(function ({ok, error}) {
  let x, y;
  if (ok !== undefined && ok > 10) {
    let x = ok;
    return x + 5;
  } else if (ok !== undefined) {
    let x = ok;
    return x + 1;
  } else if (error !== undefined) {
    return 0;
  }
})(result);
```

### Try
Early return of error match
```rs
try int_a = parse_int(a)
try int_b = parse_int(b)
try int_c = parse_int(c)
Ok(int_a + int_b + int_c)
```

```js
let {ok: int_a, error} = parse_int(a)
if (error !== undefined) return {error}
let {ok: int_b, error} = parse_int(b)
if (error !== undefined) return {error}
let {ok: int_c, error} = parse_int(c)
if (error !== undefined) return {error}

return {ok: int_a + int_b + int_c}
```

### Assert
```rs
try int_a = parse_int(a)
```

```js
let {ok: int_a, error} = parse_int(a)
if (error !== undefined) throw "Assertion Error"
```

### Binary syntax

```rs
<<3>>
<<"Gleam":utf8>>
```

```js
UInt8Array.from([3])
// => Uint8Array(1) [ 3 ]

((encoder) => { return encoder.encode("Gleam")})(new TextEncoder())
// => Uint8Array(5) [ 71, 108, 101, 97, 109 ]
```

Uint8Array's are deconstrictible like normal arrays. e.g.
```js
let [a, ...rest] = ((encoder) => { return encoder.encode("Gleam")})(new TextEncoder())
a // => 71
```

I don't know if a new text encoder is needed for every binary created, for now we can work with the dumb solution and find efficiencies later.
