# Changelog

## Unreleased

### Compiler

### Build tool

- When adding a package that does not exist on Hex, the message is a bit friendlier.
  ([Ameen Radwan](https://github.com/Acepie))

### Language server

- The language server now allows extracting the start of a pipeline into a
  variable.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The language server now offers a rename for module when the cursor is placed 
  over its import statement or when placed on its name or alias somewhere. For 
  example,

  ```gleam
  import lustre/element
  import lustre/element/html
  import lustre/event

  fn view(model: Int) -> element.Element(Msg) {
    //                     ^  Renaming module to `el` here
    let count = int.to_string(model)

    html.div([], [
      html.button([event.on_click(Incr)], [element.text(" + ")]),
      html.p([], [element.text(count)]),
      html.button([event.on_click(Decr)], [element.text(" - ")]),
    ])
  }
  ```

  Using renaming when hovering on module name will would result in the
  following code:

  ```gleam
  import lustre/element as el
  import lustre/element/html
  import lustre/event

  fn view(model: Int) -> el.Element(Msg) {
    let count = int.to_string(model)

    html.div([], [
      html.button([event.on_click(Incr)], [el.text(" + ")]),
      html.p([], [el.text(count)]),
      html.button([event.on_click(Decr)], [el.text(" - ")]),
    ])
  }
  ```

  ([Vladislav Shakitskiy](https://github.com/vshakitskiy))

### Formatter

### Bug fixes

- The compiler now correctly tracks the minimum required version for constant
  record updates to be `>= 1.14.0`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The compiler now correctly tracks the minimum required version for expressions
  in `BitArray`s' `size` option to be `>= 1.12.0`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))
