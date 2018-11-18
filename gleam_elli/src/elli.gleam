import any:Any
import result
import gen_server

pub enum Method =
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

pub type Header =
  (String, String)
;

doc """
The Elli request object. Contains all information about the
incoming HTTP request.
"""
pub external type Request
;

doc """
The status code, headers and body to send back to the client.
"""
pub type Response =
  (Int, List(Header), String)
;

pub type Server(arg, r) =
  module { r |
    fn handle(Request, arg) -> Response;

    fn handle_event(Any, Any, Any) -> Atom;
  };

// API

external fn erl_query_string(Request) -> String
  = 'elli_request' 'query_str';

external fn erl_start_link(List((Atom, Any))) -> Result(Any, Pid)
  = 'elli' 'start_link';

doc """
Get the request HTTP method.
"""
pub external fn method(Request) -> Method
  = 'gleam_elli_native' 'method';

doc """
Get the request path segments.
"""
pub external fn path(Request) -> List(String)
  = 'elli_request' 'path';

doc """
Get the request `raw_path', i.e. not split or parsed for query params.
"""
pub external fn raw_path(Request) -> String
  = 'elli_request' 'raw_path';

doc """
Get the request headers.
"""
pub external fn headers(Request) -> List((String, String))
  = 'elli_request' 'headers';

doc """
Get the request body.
"""
pub external fn body(Request) { String }
  = 'elli_request' 'body';

doc """
Get the query string for the request. Returns `Error` string if
request has no query.
"""
pub fn query_string(req) {
  case erl_query_string(req) {
  | "" -> result:Error('none')
  | s -> result:Ok(s)
  }
}

external fn erl_start_link(List(Tuple(Atom, Any))) -> gen_server:StartResult
  = 'elli' 'start_link';

doc """
Start the Elli web server process tree.
"""
pub fn start_link(mod: Server(arg, _), arg: arg, port: Int) {
  erl_start_link([
    {'callback', any:from(mod)},
    {'callback_args', any:from(arg)},
    {'port', any:from(port)},
  ])
}
