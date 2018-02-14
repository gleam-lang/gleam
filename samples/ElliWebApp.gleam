module ElliWebApp

/// A very simple web application using the Elli web server.
/// https://github.com/elli-lib/elli
///
/// Run the application with the `start_link/0` function.

export start_link/0, handle/2

from Foreign import Foreign

// This would use the Erlang behaviour :elli_handler.
// Can we support this somehow?

type Method
  = GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH

foreign type Request

foreign method :elli_request :method :: Request -> Method

foreign path :elli_request :path :: Request -> List(String)

foreign elli_start_link :elli :start_link :: List((Atom, Foreign)) -> Result(Foreign, Pid)

/// The handle/2 callback is used by Elli to response to requests
///
fn handle(req, _args) =
  method = method(req)
  path = path(req)
  case (method, path)
  | (GET, []) => home()
  | (GET, ["hello", "jane"]) => greet_jane()
  | (GET, ["hello", name]) => greet(name)
  | (DELETE, _) => reject_delete()
  | _ => not_found()

fn home() =
  (200, [], "Hello, world!")

fn greet_jane() =
  (200, [], "Jane, so good to see you!")

fn greet(name) =
  (200, [], "Hello, " <> name <> "!")

fn reject_delete() =
  (405, [], "Sorry, no DELETE requests allowed.")

fn not_found() =
  (404, [], "Not found")

/// The start_link/0 function can be used to create a new
/// Elli web server process running this handler module.
///
fn start_link() =
  elli_opts = [
    // A real module type would be nice.
    (:callback, Foreign.new(:"Gleam.ElliWebApp")),
    (:port, Foreign.new(4000)),
  ]
  elli_start_link(elli_opts)
