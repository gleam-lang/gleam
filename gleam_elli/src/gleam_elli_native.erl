-module(gleam_elli_native).

-export([method/1]).

method(Req) ->
  case elli_request:method(Req) of
    'GET' -> get;
    'HEAD' -> head;
    'POST'-> post;
    'PUT' -> put;
    'DELETE' -> delete;
    <<"CONNECT">> -> connect;
    'OPTIONS' -> options;
    'TRACE' -> trace;
    <<"PATCH">> -> patch;
    Method when is_binary(Method) -> {other, Method}
  end.
