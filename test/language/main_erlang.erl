!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa target-erlang/ebin

-module(democtl).

main(_) ->
    halt(main:main()).
