!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa target-erlang

-module(democtl).

main(_) ->
    halt(main:main()).
