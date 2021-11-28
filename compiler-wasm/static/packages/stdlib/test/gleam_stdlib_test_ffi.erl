-module(gleam_stdlib_test_ffi).

-export([main/0]).

main() ->
    Options = [
        no_tty, {report, {eunit_progress, [colored]}}
    ],
    Files = filelib:wildcard("test/**/*.{erl,gleam}"),
    Modules = lists:map(fun filepath_to_module/1, Files),
    case eunit:test(Modules, Options) of
        ok -> erlang:halt(0);
        _ -> erlang:halt(1)
    end.

filepath_to_module(Path0) ->
    Path1 = string:replace(Path0, "test/", ""),
    Path2 = string:replace(Path1, ".erl", ""),
    Path3 = string:replace(Path2, ".gleam", ""),
    Path4 = string:replace(Path3, "/", "@", all),
    Path5 = list_to_binary(Path4),
    binary_to_atom(Path5).
