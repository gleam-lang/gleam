module Elli

export Method(..), Header, Response, Request

// Request introspection functions
export method/1, path/1, raw_path/1, query_string/1, headers/1, body/1

from Foreign import Foreign

type Method =
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

type alias Header =
  (String, String)

foreign erl_query_string :elli_request :query_str :: |Request| -> String

foreign erl_start_link :elli :start_link ::
  |List((Atom, Foreign))| -> Result(Foreign, Pid)

doc """
The status code, headers and body to send back to the client.
"""
type alias Response =
  (Int, List(Header), String)

doc """
The Elli request object. Contains all information about the
incoming HTTP request.
"""
foreign type Request

doc """
Get the request HTTP method.
"""
foreign method :gleam_elli_native :method :: |Request| -> Method

doc """
Get the request path segments.
"""
foreign path :elli_request :path :: |Request| -> List(String)

doc """
Get the request `raw_path', i.e. not split or parsed for query params.
"""
foreign raw_path :elli_request :raw_path :: |Request| -> String

doc """
Get the request headers.
"""
foreign headers :elli_request :headers :: |Request| -> List((String, String))

doc """
Get the request body.
"""
foreign body :elli_request :body :: |Request| -> String

doc """
Get the query string for the request. Returns empty string if there is none.
"""
fn query_string(req) =
  case erl_query_string(req)
  | "" => Nothing
  | s => Just(s)

type alias StartArguments = {
  // A real Module type instead of Atom would be nice.
  callback :: Atom,
  port :: Int,
}

doc """
Start the Elli web server process tree.
"""
spec StartArguments -> Result(Foreign, Pid)
fn start_link(args) =
  erl_start_link([
    (:callback, Foreign.new(args.callback)),
    (:port, Foreign.new(args.port)),
  ])
