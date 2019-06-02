# Module

Gleam programs are made up of bundles of functions and types called modules.
Each module has its own namespace and can export types and values to be used
by other modules in the program.

```rust,noplaypen
// inside src/nasa/rocket_ship.gleam

fn count_down() {
  "3... 2... 1..."
}

fn blast_off() {
  "BOOM!"
}

pub fn launch() {
  [
    count_down(),
    blast_off(),
  ]
}
```

Here we can see a module named `nasa/rocket_ship`, the name determined by the
filename `src/nasa/rocket_ship.gleam`. Typically all the modules for one
project would live within a directory with the name of the project, such as
`nasa` in this example.

For the functions `count_down` and `blast_off` we have omitted the `pub`
keyword, so these functions are _private_ module functions. They can only be
called by other functions within the same module.


## Import

To use functions or types from another module we need to import them using the
`import` keyword.

```rust,noplaypen
// inside src/nasa/moon_base.gleam

import nasa/rocket_ship

pub fn explore_space() {
  rocket_ship:launch()
}
```

The statement `import nasa/rocket_ship` creates a new variable with the name
`rocket_ship` and the value of the `rocket_ship` module.

In the `explore_space` function we call the imported module's public `launch`
function using the `:` operator.
If we had attempted to call `count_down` it would result in a compile time
error as this function is private in the `rocket_ship` module.


## Named import

It is also possible to give a module a custom name when importing it using the
`as` keyword.

```rust,noplaypen
import unix/cat
import animal/cat as kitty
```

This may be useful to differentiate between multiple modules that would have
the same default name when imported.


## First class modules

Modules in Gleam are first class values and can be assigned to variables,
passed to functions, or anything else that we can do with regular values.

```rust,noplaypen
import nasa/rocket_ship
import nasa/new_website
import nasa/navy_boat

pub fn perform_launch(some_module) {
  some_module:launch()
}

pub fn run() {
  perform_launch(rocket_ship)
  perform_launch(new_website)
  perform_launch(navy_boat)
}
```

Here we have define a function that takes a module as an argument and then
called it with various different modules.

The `perform_launch` function doesn't care what module it takes, so long as it
has a public function called `launch` that takes no arguments.
