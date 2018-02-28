module Elli
  exposing Method(..), Header, Response, Request, method/1, path/1, raw_path/1,
    query_string/1, headers/1, body/1

import Foreign exposing Foreign

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

external erl_query_string : |Request| -> String = :elli_request.query_str

external erl_start_link
  : |List((Atom, Foreign))| -> Result(Foreign, Pid)
  = :elli.start_link

doc """
The status code, headers and body to send back to the client.
"""
type alias Response =
  (Int, List(Header), String)

doc """
The Elli request object. Contains all information about the
incoming HTTP request.
"""
external type Request

doc """
Get the request HTTP method.
"""
external method : |Request| -> Method = :gleam_elli_native.method

doc """
Get the request path segments.
"""
external path : |Request| -> List(String) = :elli_request.path

doc """
Get the request `raw_path', i.e. not split or parsed for query params.
"""
external raw_path : |Request| -> String = :elli_request.raw_path

doc """
Get the request headers.
"""
external headers : |Request| -> List((String, String)) = :elli_request.headers

doc """
Get the request body.
"""
external body : |Request| -> String = :elli_request.body

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
