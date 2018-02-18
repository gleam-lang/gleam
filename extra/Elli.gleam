module Elli

export Method(..), Header, Response, Request, method/1, path/1

from Foreign import Foreign

type Method =
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH

type alias Header =
  (String, String)

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
foreign method :elli_request :method :: |Request| -> Method

doc """
Get the request path segments as a list of strings.
"""
foreign path :elli_request :path :: |Request| -> List(String)

foreign elli_start_link :elli :start_link ::
  |List((Atom, Foreign))| -> Result(Foreign, Pid)

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
  elli_start_link([
    (:callback, Foreign.new(args.callback)),
    (:port, Foreign.new(args.port)),
  ])
