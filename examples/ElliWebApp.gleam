module ElliWebApp

// A very simple web application using the Elli web server.
// https://github.com/elli-lib/elli
//
// Run the application with the `start_link/0` function.
//
// This would use the Erlang behaviour :elli_handler.
// Can we support this somehow?

export start_link/0, handle/2

from Foreign import Foreign
from Elli import Request, Response

doc """
The handle/2 callback is used by Elli to response to requests

It serves as the router for our application.
"""
spec |Request, a| -> Response
fn handle(req, _args) =
  method = Elli.method(req)
  path = Elli.path(req)
  case (method, path)
  | (GET, []) => home()
  | (GET, ["hello"]) => greet("world")
  | (GET, ["hello", "jane"]) => greet_jane()
  | (GET, ["hello", name]) => greet(name)
  | (DELETE, _) => reject_delete()
  | _ => not_found()

// Response builder functions

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

// Lastly, a function to start the server.

doc """
The start_link/0 function can be used to create a new
Elli web server process running this handler module.
"""
fn start_link() =
  Elli.start_link({
    callback = :"Gleam.ElliWebApp",
    port = 4000,
  })
