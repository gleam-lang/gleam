# Gleam HTTP

Types and functions for HTTP clients and servers!

## HTTP Service Example

```gleam
import gleam/http/elli
import gleam/http/response.{Response}
import gleam/http/request.{Request}
import gleam/bit_builder.{BitBuilder}

// Define a HTTP service
//
pub fn my_service(request: Request(t)) -> Response(BitBuilder) {
  let body = bit_builder.from_string("Hello, world!")

  response.new(200)
  |> response.prepend_header("made-with", "Gleam")
  |> response.set_body(body)
}

// Start it on port 3000 using the Elli web server
//
pub fn main() {
  elli.become(my_service, on_port: 3000)
}
```

## Server adapters

In the example above the Elli Erlang web server is used to run the Gleam HTTP
service. Here's a full list of the server adapters available, sorted
alphabetically.

| Adapter                        | About                                                    |
| ---                            | ---                                                      |
| [gleam_cowboy][cowboy-adapter] | [Cowboy][cowboy] is an Erlang HTTP2 & HTTP1.1 web server |
| [gleam_elli][elli-adapter]     | [Elli][elli] is an Erlang HTTP1.1 web server             |
| [gleam_plug][plug-adapter]     | [Plug][plug] is an Elixir web application interface      |

[cowboy]:https://github.com/ninenines/cowboy
[cowboy-adapter]: https://github.com/gleam-lang/cowboy
[elli]:https://github.com/elli-lib/elli
[elli-adapter]: https://github.com/gleam-lang/elli
[plug]:https://github.com/elixir-plug/plug
[plug-adapter]: https://github.com/gleam-lang/plug

## Client adapters

Client adapters are used to send HTTP requests to services over the network.
Here's a full list of the client adapters available, sorted alphabetically.

| Adapter                          | About                                                    |
| ---                              | ---                                                      |
| [gleam_fetch][fetch-adapter]     | [fetch][fetch] is a HTTP client included with JavaScript |
| [gleam_hackney][hackney-adapter] | [Hackney][hackney] is a simple HTTP client for Erlang    |
| [gleam_httpc][httpc-adapter]     | [httpc][httpc] is a HTTP client included with Erlang     |

[hackney]: https://github.com/benoitc/hackney
[hackney-adapter]: https://github.com/gleam-lang/hackney
[httpc]: https://erlang.org/doc/man/httpc.html
[httpc-adapter]: https://github.com/gleam-lang/httpc
[fetch]: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
[fetch-adapter]: https://github.com/gleam-lang/fetch
