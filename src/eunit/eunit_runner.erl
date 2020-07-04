#!/usr/bin/env erlang
-mode(compile).

main([Ebin, Args])->
    true = code:add_patha(filename:dirname(escript:script_name())),
    true = code:add_patha(Ebin),
    Strings = string:tokens(Args, ","),
    Modules = lists:map(fun(X) -> list_to_atom(X) end, Strings),
    code:load_file(eunit_progress),
    halt(case eunit:test(Modules, [inparallel, verbose, no_tty, {report, {eunit_progress, [{colored, true}]}}]) of ok -> 0; error -> 1 end).
