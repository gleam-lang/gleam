-module(plug).

-export([method/1, path_info/1, send_resp/3]).

method(A) ->
    'Elixir.GleamPlugFFI':method(A).

path_info(A) ->
    'Elixir.GleamPlugFFI':path_info(A).

send_resp(A, B, C) ->
    'Elixir.Plug.Conn':send_resp(A, B, C).
