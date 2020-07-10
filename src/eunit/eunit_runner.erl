#!/usr/bin/env erlang
-mode(compile).

main([EbinPaths, AllModules])->
    true = code:add_patha(filename:dirname(escript:script_name())),
    SeperatedEbinPaths = string:tokens(EbinPaths, ","),
    ok = code:add_paths(SeperatedEbinPaths),
    SeperatedModules = string:tokens(AllModules, ","),
    Modules = lists:map(fun(X) -> list_to_atom(X) end, SeperatedModules),
    code:load_file(eunit_progress),
    halt(case eunit:test(Modules, [inparallel, verbose, no_tty, {report, {eunit_progress, [{colored, true}]}}]) of ok -> 0; error -> 1 end).
