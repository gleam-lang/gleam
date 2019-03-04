-module(app_router).

-export([init/1, call/2]).

concat(A) ->
    erlang:list_to_binary(A).

init(Options) ->
    Options.

call(Conn, _) ->
    Method = plug:method(Conn),
    Path = plug:path_info(Conn),
    case {Method, Path} of
        {get, []} ->
            plug:send_resp(Conn, 200, <<"Welcome home!">>);

        {get, [<<"profile">>, Name]} ->
            Body = concat([Name, <<"'s profile page">>]),
            plug:send_resp(Conn, 200, Body);

        {_, _} ->
            plug:send_resp(Conn, 404, <<"Page not found">>)
    end.
