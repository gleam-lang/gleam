-module('{{ application }}@@main').
-export([run/1]).

-define(red, "\e[31;1m").
-define(grey, "\e[90m").
-define(reset_color, "\e[39m").
-define(reset_all, "\e[0m").

run(Module) ->
    io:setopts(standard_io, [binary, {encoding, utf8}]),
    io:setopts(standard_error, [{encoding, utf8}]),
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> run_module(Module) end),
    receive
        {'EXIT', Pid, {Reason, StackTrace}} ->
            print_error(exit, Reason, StackTrace),
            init:stop(1)
    end.

run_module(Module) ->
    try
        {ok, _} = application:ensure_all_started('{{ application }}'),
        erlang:process_flag(trap_exit, false),
        Module:main(),
        init:stop(0)
    catch
        Class:Reason:StackTrace ->
            print_error(Class, Reason, StackTrace),
            init:stop(1)
    end.

print_error(Class, Error, Stacktrace) ->
    Printed = [
        ?red, "runtime error", ?reset_color, ": ", error_class(Class, Error), ?reset_all,
        "\n\n",
        error_message(Error),
        "\n\n",
        error_details(Class, Error),
        "stacktrace:\n",
        [error_frame(Line) || Line <- refine_first(Error, Stacktrace)]
    ],
    io:format(standard_error, "~ts~n", [Printed]).

refine_first(#{gleam_error := _, line := L}, [{M, F, A, [{file, Fi} | _]} | S]) ->
    [{M, F, A, [{file, Fi}, {line, L}]} | S];
refine_first(_, S) ->
    S.

error_class(_, #{gleam_error := panic}) -> "panic";
error_class(_, #{gleam_error := todo}) -> "todo";
error_class(_, #{gleam_error := let_assert}) -> "let assert";
error_class(_, #{gleam_error := assert}) -> "assert";
error_class(Class, _) -> ["Erlang ", atom_to_binary(Class)].

error_message(#{gleam_error := _, message := M}) ->
    M;
error_message(undef) ->
    <<"A function was called but it did not exist."/utf8 >>;
error_message({case_clause, _}) ->
    <<"No pattern matched in an Erlang case expression."/utf8>>;
error_message({badmatch, _}) ->
    <<"An Erlang assignment pattern did not match."/utf8>>;
error_message(function_clause) ->
    <<"No Erlang function clause matched the arguments it was called with."/utf8>>;
error_message(_) ->
    <<"An error occurred outside of Gleam."/utf8>>.

error_details(_, #{gleam_error := let_assert, value := V}) ->
    ["unmatched value:\n  ", print_term(V), $\n, $\n];
error_details(_, {case_clause, V}) ->
    ["unmatched value:\n  ", print_term(V), $\n, $\n];
error_details(_, {badmatch, V}) ->
    ["unmatched value:\n  ", print_term(V), $\n, $\n];
error_details(_, #{gleam_error := _}) ->
    [];
error_details(error, function_clause) ->
    [];
error_details(error, undef) ->
    [];
error_details(C, E) ->
    ["erlang:", atom_to_binary(C), $(, print_term(E), $), $\n, $\n].

print_term(T) ->
    try
        gleam@string:inspect(T)
    catch
        _:_ -> io_lib:format("~p", [T])
    end.

error_frame({?MODULE, _, _, _}) -> [];
error_frame({erl_eval, _, _, _}) -> [];
error_frame({init, _, _, _}) -> [];
error_frame({M, F, _, O}) ->
    M1 = string:replace(atom_to_binary(M), "@", "/", all),
    ["  ", M1, $., atom_to_binary(F), error_frame_end(O), $\n].

error_frame_end([{file, Fi}, {line, L} | _]) ->
    [?grey, $\s, Fi, $:, integer_to_binary(L), ?reset_all];
error_frame_end(_) ->
    [?grey, " unknown source", ?reset_all].
