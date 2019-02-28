-module(hello_world).

-export([init/1, call/2]).

init(Opts) ->
    Opts.

call(Conn, Opts) ->
    case plug:path_info(Conn) of
        [] ->
            plug:send_resp(Conn, 200, <<"Home page">>);

        [<<"user">>, Name] ->
            Body = string:concat([Name, <<"'s profile page">>]),
            plug:send_resp(Conn, 200, Body);

        Any ->
            plug:send_resp(Conn, 404, <<"Page not found">>)
    end.
