!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa target-erlang

-module(democtl).

main(_) ->
    Print = fun(S) -> 
        io:format("~s", [S]), 
        S 
    end,
    Status = main:main(Print),
    halt(Status).
