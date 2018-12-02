// An experimental typing of Raxx.SimpleServer

import any

pub enum Method =
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Patch;

pub type Scheme =
  | Http
  | Https;

pub type Header =
  Tuple(String, String);

pub type RequestRec(body) =
  {
    authority: String,
    body: body,
    headers: List(Header),
    method: Method,
    mount: List(String),
    path: List(String),
    // FIXME: This cannot be typed without untagged unions
    // query: binary() | nil,
    query: any:Any,
    raw_path: String,
    scheme: Scheme,
  };

pub type Request =
  RequestRec(String);

pub type HeadRequest =
  RequestRec(Bool);

pub type Response =
  {
    body: String,
    status: Int,
    headers: List(Header)
  };

pub type Next =
  Response;

// In Elixir this uses a lot of metaprogramming to generate the Raxx.Server
// callbacks . It looks like we would need to add all this boilerplate.
// Probably by wrapping it in a behaviour in Erlang?
// https://github.com/CrowdHailer/raxx/blob/master/lib/raxx/simple_server.ex#L48
pub type Server(arg) =
  module {
    fn handle_request(Request, arg) -> Response
  }
