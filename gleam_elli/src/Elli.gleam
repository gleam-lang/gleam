module Elli

import Foreign:Foreign
import Result:Result:*

pub type Method =
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Patch
  | Other(String)

pub type alias Header =
  (String, String)
;

doc """
The Elli request object. Contains all information about the
incoming HTTP request.
"""
pub external type Request

external fn erl_query_string(Request) { String } = :elli_request :query_str

external fn erl_start_link(List((Atom, Foreign))) { Result(Foreign, Pid) }
  = :elli :start_link

doc """
The status code, headers and body to send back to the client.
"""
pub type alias Response =
  (Int, List(Header), String)

doc """
Get the request HTTP method.
"""
pub external fn method(Request) { Method } = :gleam_elli_native :method

doc """
Get the request path segments.
"""
pub external fn path(Request) { List(String) } = :elli_request :path

doc """
Get the request `raw_path', i.e. not split or parsed for query params.
"""
pub external fn raw_path(Request) { String } = :elli_request :raw_path

doc """
Get the request headers.
"""
pub external fn headers(Request) { List((String, String)) } = :elli_request :headers

doc """
Get the request body.
"""
pub external fn body(Request) { String } = :elli_request :body

doc """
Get the query string for the request. Returns `Error` string if
request has no query.
"""
pub fn query_string(req) {
  case erl_query_string(req) {
  | "" => Error(())
  | s => Ok(s)
  }
}

pub type alias StartArguments =
  {
    // A real Module type instead of Atom would be nice.
    callback :: Atom,
    port :: Int,
  }

doc """
Start the Elli web server process tree.
"""
pub fn start_link(args) {
  erl_start_link([
    (:callback, Foreign.new(args.callback)),
    (:port, Foreign.new(args.port)),
  ])
}
