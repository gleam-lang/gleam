-module(gleam_plug).

-export([path_info/1, send_resp/3]).

path_info(A) ->
    'Elixir.Plug.Conn':path_info(A).

send_resp(A, B, C) ->
    'Elixir.Plug.Conn':send_resp(A, B, C).
