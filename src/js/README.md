### Patterns

```rust
let [1, x, ..rest] = list()
```

Need to assign expression to a variable if used more than once, it may have side effects and evaluating twice would change meaning of program

```js
var gleam$tmp = list();
if (!(gleam$tmp.length >= 2));
var [_, x, ...rest] = gleam$tmp;
```

if using `let` would need to assign directly

```js
let gleam$tmp, x, rest;
// They might all previously have been used as gleam allows rebinding.

var gleam$tmp = list();
if (!(gleam$tmp.length >= 2)) throw "Bad match";
x = gleam$tmp[1];
rest = gleam$tmp.slice(2);
```

if using `const` would need to track variables and use `x1` `x2` etc.

use `var` so we don't have to track instantiated variables, or list all used variables.
_Both `let` and `const` cannot be used to rebind a variable._

`var` (or `let` or `const` must be used for destructuring in JS)

### Records

Create with literal or have a constructor function.

As far as I can see the constructor function can't be used for updates, and certainly not destructures so the literal will be used in many places.

### Tuple implementations

```
[a, b]

["gleam$tuple", a, b]
{type: "tuple", 0: a, 1: b}
```

### Case

Done in anonymous function because otherwise need to pass in what to do with last value but this might cause issues with try.

### Web Assembly

```
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
```

https://rustwasm.github.io/wasm-pack/installer/

### Build a project

Docker-compose started twice,
Or docker compose with the CORS file turned off
docker-compose twice with a sirv-cli

Rollup will do, because valid JS
