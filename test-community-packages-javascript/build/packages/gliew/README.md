# gliew

[![Package Version](https://img.shields.io/hexpm/v/gliew)](https://hex.pm/packages/gliew)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gliew/)

An **experimental** web framework with server-side rendering and optional live updating UI (à la Phoenix's LiveView) for gleam.

## Quick start

Usage is similar to [mist](https://hexdocs.pm/mist/) except the handler function should return a [nakai](https://hexdocs.pm/nakai/) node tree.

```gleam
import gleam/erlang/process
import gleam/http.{Get}
import gleam/http/request
import nakai/html
import gliew

pub fn main() {
  let assert Ok(_) =
    gliew.new(
      8080,
      fn(req) {
        case req.method, request.path_segments(req) {
          Get, ["hello"] ->
            html.div_text([], "Hello gleam!")
            |> gliew.view(200)
          _, _ ->
            html.div_text([], "Hello world!")
            |> gliew.view(200)
        }
      },
    )
    |> gliew.serve

  process.sleep_forever()
}
```

### Live mounts

In order to have some elements of the HTML live updating you will need to use live mounts.

They can be either used by calling the `gliew.live_mount` function directly:

```gleam
import gleam/int
import gleam/option.{Option}
import gleam/erlang/process.{Subject}
import gleam/http/request
import nakai/html
import gliew

pub fn main() {
  let assert Ok(_) =
    gliew.new(
      8080,
      fn(req) {
        case req.method, request.path_segments(req) {
          _, _ ->
            html.div(
              [],
              [
                html.div_text([], "counter is at:"),
                gliew.live_mount(
                  mount: mount_counter,
                  with: Nil,
                  render: render_counter,
                ),
              ],
            )
            |> gliew.view(200)
        }
      },
    )
    |> gliew.serve

  process.sleep_forever()
}

// The render function is called with `None` on
// initial render but `Some(a)` every time there
// is a new value on the subject returned by the
// mount function.
fn render_counter(assign: Option(Int)) {
  html.div_text(
    [],
    assign
    |> option.unwrap(0)
    |> int.to_string,
  )
}

// This is the mount function.
fn mount_counter(_ctx) {
  let subject = process.new_subject()

  let _ = process.start(fn() { loop(subject, 0) }, True)

  subject
}

// Counter loop for demonstration purposes.
fn loop(subject: Subject(Int), current: Int) {
  process.send(subject, current)
  process.sleep(1000)
  loop(subject, current + 1)
}
```

But a nicer way (IMO) is to use the `use` syntax to turn a function into a live mount by default.

```gleam
import gleam/int
import gleam/option
import gleam/erlang/process.{Subject}
import gleam/http/request
import nakai/html
import gliew

pub fn main() {
  let assert Ok(_) =
    gliew.new(
      8080,
      fn(req) {
        case req.method, request.path_segments(req) {
          _, _ ->
            html.div([], [html.div_text([], "counter is at:"), counter()])
            |> gliew.view(200)
        }
      },
    )
    |> gliew.serve

  process.sleep_forever()
}

// This becomes the render function.
fn counter() {
  // Here we use the `use` syntax as kind of a decorator
  // to turn this function into a live mount.
  use assign <- gliew.live_mount(mount_counter, with: Nil)

  html.div_text(
    [],
    assign
    |> option.unwrap(0)
    |> int.to_string,
  )
}

// This is still the mount function.
fn mount_counter(_ctx) {
  let subject = process.new_subject()

  let _ = process.start(fn() { loop(subject, 0) }, True)

  subject
}

// Counter loop for demonstartion purposes.
fn loop(subject: Subject(Int), current: Int) {
  process.send(subject, current)
  process.sleep(1000)
  loop(subject, current + 1)
}
```

## Example

The following example has a random string generator and a global counter so that every browser session remains in sync when navigating to `http://localhost:8080/mounts`.

```gleam
import gleam/int
import gleam/string
import gleam/base
import gleam/option.{None, Some}
import gleam/http.{Get, Post}
import gleam/http/request
import gleam/erlang/process.{Subject}
import gleam/crypto.{strong_random_bytes}
import nakai/html
import nakai/html/attrs
import gliew
// This is only here for this example.
// It implements an actor that keeps an
// incrementing counter and can be
// subscribed to.
import gliew/integration/counter.{
  CounterMessage, reset, start_counter, subscribe,
}

pub fn main() {
  // Start anything that will be needed by the handlers.
  // In this case we start an actor that simply increments
  // a counter (starting at 0).
  // You can subscribe to the counter to be notified every
  // time it increments.
  let assert Ok(count_actor) = start_counter()

  // Start the gliew server.
  // This is a thin wrapper around mist which handles
  // rendering your views and starting workers for
  // live connections.
  let assert Ok(_) =
    gliew.new(
      8080,
      fn(req) {
        case req.method, request.path_segments(req) {
          Get, ["mounts"] ->
            mounts_page(count_actor)
            |> gliew.view(200)
          Post, ["counter", "reset"] -> reset_counter(count_actor)
          _, _ ->
            home_page()
            |> gliew.view(200)
        }
      },
    )
    |> gliew.serve

  process.sleep_forever()
}

// You can just return a simple nakai node tree for
// a static page.
fn home_page() {
  html.div_text([], "Hello gleam!")
}

fn reset_counter(count_actor: Subject(CounterMessage)) {
  reset(count_actor)

  gliew.response(204)
}

fn mounts_page(count_actor: Subject(CounterMessage)) {
  // Return the HTML for the page.
  html.div(
    [
      attrs.style(
        [
          "display: flex", "flex-direction: column", "justify-content: center",
          "align-items: center", "row-gap: 1em", "height: 100vh",
        ]
        |> string.join(";"),
      ),
    ],
    [
      // Random text
      html.div_text([attrs.style("font-size: x-large")], "Random text for you"),
      random_text(),
      // Counter
      html.div_text([attrs.style("font-size: x-large")], "Counter is at"),
      counter(count_actor),
      html.div(
        [],
        [
          html.button_text([], "Reset")
          |> gliew.on_click(do: Post, to: "/counter/reset"),
        ],
      ),
      // Explanation text
      html.div(
        [
          attrs.style(
            [
              "background-color: #d0ebf4", "padding: 1em", "color: #222",
              "border-radius: 1em", "width: 21em", "margin-top: 0.5em",
            ]
            |> string.join(";"),
          ),
        ],
        [
          html.div_text(
            [attrs.style("font-size: x-large; margin-bottom: 0.5em;")],
            "This page is live rendered.",
          ),
          html.div_text(
            [attrs.style("font-size: large")],
            "Once the browser has made a connection to the server a new random text is generated every 5 seconds and the counter above should auto-increment.",
          ),
        ],
      ),
    ],
  )
}

//                           //
// Global counter live mount //
//                           //

// A live mount that shows a live updating counter.
fn counter(count_actor: Subject(CounterMessage)) {
  // Makes the function a live mount.
  // The second parameter passed to `gliew.live_mount` will
  // be passed to the mount function (first parameter)
  // when the mount is mounted (i.e. the client connects
  // back to the server to get live updates).
  //
  // The returned value is of type `Option(a)` where
  // `a` is the type in the Subject returned by mount.
  use assign <- gliew.live_mount(counter_mount, with: count_actor)

  let text = case assign {
    // View has been mounted and we want to use the
    // "live" value that was setup in the mount function.
    Some(counter) ->
      counter
      |> int.to_string
    // View is being rendered on the server before
    // the client has made a connection back to the server.
    // Here we contact the count_actor to get the current
    // value.
    None -> "Loading.."
  }

  // Return the HTML of the view.
  html.div_text([attrs.style("font-size: xx-large")], text)
}

// A mount function that runs once the client connects through
// a websocket.
// This should be used to subscribe to whatever data that should
// be returned to the render function and return the subject.
fn counter_mount(count_actor: Subject(CounterMessage)) {
  // Create a new subject.
  let subject = process.new_subject()

  // Subscribe to counter.
  subscribe(count_actor, subject)

  // Return the subject.
  subject
}

//                        //
// Random text live mount //
//                        //

// A live mount that renders random text on an interval.
fn random_text() {
  use assign <- gliew.live_mount(text_mount, with: Nil)

  html.div_text(
    [attrs.style("font-size: xx-large")],
    assign
    |> option.unwrap(random_string()),
  )
}

// The mount function used for the random_text mount.
// It will generate random text on an interval and
// send it on the returned subject.
fn text_mount(_) {
  let subject = process.new_subject()

  let _ = process.start(fn() { loop(subject) }, True)

  subject
}

// Random text loop.
fn loop(subject: Subject(String)) {
  process.send(subject, random_string())
  process.sleep(5000)
  loop(subject)
}

// Helper function to generate random string.
fn random_string() {
  strong_random_bytes(10)
  |> base.encode64(False)
}
````

The above example implementation can be found in `test/integration/live_mounts.gleam` and quickly run with:

```sh
gleam run -m gliew/integration/live_mounts
```

And then navigate to `http://localhost:8080/mounts` to see in action.

## Installation

If available on Hex this package can be added to your Gleam project:

```sh
gleam add gliew
```

and its documentation can be found at <https://hexdocs.pm/gliew>.
