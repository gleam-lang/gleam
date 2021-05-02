!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa target-erlang

-module(democtl).

main(_) ->
    Print = fun(S) -> 
        io:format("~s", [S]), 
        S 
    end,
    ToString = fun(Term) ->
        List = io_lib:format("~p", [Term]),
        iolist_to_binary(List)
    end,
    Status = main:main(Print, ToString),
    halt(Status).
