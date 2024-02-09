-module(ssl_ffi).

-export([controlling_process/2, send/2, set_opts/2, start_ssl/0, shutdown/2, close/1,
         negotiated_protocol/1]).

send(Socket, Packet) ->
  case ssl:send(Socket, Packet) of
    ok ->
      {ok, nil};
    Res ->
      Res
  end.

set_opts(Socket, Options) ->
  case ssl:setopts(Socket, Options) of
    ok ->
      {ok, nil};
    {error, Reason} ->
      {error, Reason}
  end.

controlling_process(Socket, Pid) ->
  case ssl:controlling_process(Socket, Pid) of
    ok ->
      {ok, nil};
    {error, Reason} ->
      {error, Reason}
  end.

start_ssl() ->
  case application:ensure_all_started(ssl) of
    {ok, _} ->
      {ok, nil};
    {error, Reason} ->
      {error, Reason}
  end.

shutdown(Socket, How) ->
  case ssl:shutdown(Socket, How) of
    ok ->
      {ok, nil};
    {error, Reason} ->
      {error, Reason}
  end.

close(Socket) ->
  case ssl:close(Socket) of
    ok ->
      {ok, nil};
    {error, Reason} ->
      {error, Reason}
  end.

negotiated_protocol(Socket) ->
  case ssl:negotiated_protocol(Socket) of
    {error, _} ->
      {error, "Socket not negotiated"};
    Protocol ->
      Protocol
  end.
