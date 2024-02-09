# glisten

See the docs [here](https://hexdocs.pm/glisten/).

It uses the `gleam_otp` library to handle the supervisor and child processes.

`glisten` provides a supervisor which manages a pool of acceptors. Each acceptor
will block on `accept` until a connection is opened.  The acceptor will then
spawn a handler process and then block again on `accept`.

The most obvious entrypoint is `glisten.{serve}` which listens for TCP
connections on a given port.  It also takes a handler function wrapper
`handler.{func}` which you can provide functionality to, and the state which
each TCP connection process will hold.  This takes the shape of:

```gleam
type HandlerFunc(data) =
  fn(BitString, LoopState(data)) -> actor.Next(LoopState(data))
```

SSL is also handled using the `glisten.{serve_ssl}` method.  This requires a
certificate and key file path.  The rest of the handler flow remains unchanged.

`glisten` doesn't provide a public API for connected clients.  In order to hook
into the socket lifecyle, you can establish some functions which are called
for the opening and closing of the socket.  An example is provided below.

## Examples

Here is a basic echo server:

```gleam
import gleam/bit_builder
import gleam/erlang/process
import gleam/otp/actor
import gleam/result
import glisten/acceptor
import glisten/handler
import glisten/tcp
import glisten

pub fn main() {
  handler.func(fn(msg, state) {
    assert Ok(_) = tcp.send(state.socket, bit_builder.from_bit_string(msg))
    actor.Continue(state)
  })
  |> acceptor.new_pool
  |> glisten.serve(8080, _)
  |> result.map(fn(_) { process.sleep_forever() })
}
```

To serve over SSL:

```gleam
// ...
import glisten/ssl

pub fn main() {
  handler.func(fn(msg, state) {
    assert Ok(_) = ssl.send(state.socket, bit_builder.from_bit_string(msg))
    actor.Continue(state)
  })
  |> acceptor.new_pool
  |> glisten.serve_ssl(
    // Passing labeled arguments for clarity
    port: 8080,
    certfile: "/path/to/server.crt",
    keyfile: "/path/to/server.key",
    with_pool: _,
  )
  |> result.map(fn(_) { process.sleep_forever() })
}
```

Managing connected clients can be handled similarly to this simple example.
The `with_init` callback will be called after the SSL handshake, if the
underlying socket is set up to use it.

```gleam
import gleam/bit_builder
import gleam/erlang/process
import glisten/handler
import glisten/tcp
import glisten

pub fn main() {
  // This function is omitted for brevity.  It simply manages a
  // `gleam/set.{Set}` of `Sender(HandlerMessage)`s that "broadcast" the
  // connect/disconnect events to all clients.
  assert Ok(connections) = start_connection_actor()

  handler.func(fn(msg, state) {
    assert Ok(_) = tcp.send(state.socket, bit_builder.from_bit_string(msg))
    actor.Continue(state)
  })
  |> acceptor.new_pool
  |> acceptor.with_init(fn(sender) {
    process.send(connections, Connected(sender))

    Nil
  })
  |> acceptor.with_close(fn(sender) {
    process.send(connections, Disconnected(sender))

    Nil
  })
  |> glisten.serve(8080, _)
  |> result.map(fn(_) { process.sleep_forever() })
}
```

But you can also drop down to the lower level listen/accept flow if you'd prefer
to manage connections yourself, or only handle a small number at a time.

```gleam
import gleam/io
import glisten/socket/options.{ActiveMode, Passive}
import glisten/tcp

pub fn main() {
  try listener = tcp.listen(8000, [ActiveMode(Passive)])
  try socket = tcp.accept(listener)
  try msg = tcp.receive(socket, 0)
  io.debug(#("got a msg", msg))

  Ok(Nil)
}
```

See [mist](https://github.com/rawhat/mist) for HTTP support built on top of
this library.
