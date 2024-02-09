# mist

A glistening Gleam web server.

## Installation

This package can be added to your Gleam project:

```sh
gleam add mist
```

and its documentation can be found at <https://hexdocs.pm/mist>.

## Usage

Right now there are a few options.  Let's say you want a "simple" HTTP server
that you can customize to your heart's content.  In that case, you want:

```gleam
import gleam/bit_builder
import gleam/erlang/process
import gleam/http/response
import mist

pub fn main() {
  let assert Ok(_) =
    mist.run_service(
      8080,
      fn(_req) {
        response.new(200)
        |> response.set_body(bit_builder.from_string("hello, world!"))
      },
      max_body_limit: 4_000_000
    )
  process.sleep_forever()
}
```

Maybe you also want to work with websockets.  Maybe those should only be
upgradable at a certain endpoint.  For that, you can use `mist.handler_func`.
The websocket methods help you build a handler with connect/disconnect handlers.
You can use these to track connected clients, for example.

```gleam
import gleam/bit_builder
import gleam/erlang/process
import gleam/http.{Get, Post}
import gleam/http/request.{Request}
import gleam/http/response
import gleam/result
import mist
import mist/websocket

pub fn main() {
  let assert Ok(_) =
    mist.serve(
      8080,
      mist.handler_func(fn(req) {
        case req.method, request.path_segments(req) {
          Get, ["echo", "test"] -> websocket_echo()
          Post, ["echo", "body"] -> echo_body(req)
          Get, ["home"] ->
            response.new(200)
            |> mist.bit_builder_response(
              bit_builder.from_string("sup home boy")
            )
          _, _ ->
            response.new(200)
            |> mist.bit_builder_response(
              bit_builder.from_string("Hello, world!")
            )
        }
      }),
    )
  process.sleep_forever()
}

fn websocket_echo() {
  websocket.echo_handler
  |> websocket.with_handler
  // Here you can gain access to the `Subject` to send message to
  // with:
  // |> websocket.on_init(fn(subj) { ... })
  // |> websocket.on_close(fn(subj) { ... })
  |> mist.upgrade
}

fn echo_body(req: Request(mist.Body)) {
  req
  |> mist.read_body
  |> result.map(fn(req) {
    response.new(200)
    |> response.prepend_header(
      "content-type",
      request.get_header(req, "content-type")
      |> result.unwrap("application/octet-stream"),
    )
    |> mist.bit_builder_response(bit_builder.from_bit_string(req.body))
  })
  |> result.unwrap(
    response.new(400)
    |> mist.empty_response,
  )
}
```

You might also want to use SSL.  You can do that with the following options.

With `run_service_ssl`:

```gleam
import gleam/bit_builder
import gleam/erlang/process
import gleam/http/response
import mist

pub fn main() {
  let assert Ok(_) =
    mist.run_service_ssl(
      port: 8080,
      certfile: "/path/to/server.crt",
      keyfile: "/path/to/server.key",
      handler: fn(_req) {
        response.new(200)
        |> response.set_body(bit_builder.from_bit_string(<<
          "hello, world!":utf8,
        >>))
      },
      max_body_limit: 4_000_000
    )
  process.sleep_forever()
}
```

With `serve_ssl`:

```gleam
pub fn main() {
  let assert Ok(_) =
    mist.serve_ssl(
      port: 8080,
      certfile: "...",
      keyfile: "...",
      mist.handler_func(fn(req) {
        todo
      }
    )
  // ...
}
```

There is support for sending files as well. This uses the `file:sendfile` erlang
method under the hood.

```gleam
import gleam/bit_builder
import gleam/erlang/process
import gleam/http/request.{Request}
import gleam/http/response
import gleam/string
import mist

pub fn main() {
  let asset_root = "..."
  let assert Ok(_) =
    mist.serve(
      8080,
      mist.handler_func(fn(req: Request(Body)) {
        let not_found =
          response.new(404)
          |> mist.empty_response
        case request.path_segments(req) {
          ["static", ..path] -> {
            // verify, validate, etc
            let file_path =
              path
              |> string.join("/")
              |> string.append("/", _)
            response.new(200)
            |> mist.file_response(asset_root <> file_path)
            |> result.unwrap(not_found)
          }
          _ -> not_found
        }
      }),
    )
  process.sleep_forever()
}
```

You can return chunked responses using the `mist.{chunked_response}` method.
This takes an `Iterator(BitBuilder)` and handles sending the initial
response, and subsequent chunks in the proper format as they are emitted from
the iterator. The iterator must be finite.

If you need something a little more complex or custom, you can always use the
helpers exported by the various `glisten`/`mist` modules.

## Benchmarks

These are currently located [here](https://github.com/rawhat/http-benchmarks)
