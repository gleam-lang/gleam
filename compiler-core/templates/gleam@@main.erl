-module('{{ application }}@@main').
-export([run/1]).

-define(red, "\e[31;1m").
-define(grey, "\e[90m").
-define(reset_color, "\e[39m").
-define(reset_all, "\e[0m").

run(Module) ->
    io:setopts(standard_io, [binary, {encoding, utf8}]),
    io:setopts(standard_error, [{encoding, utf8}]),
    try
        {ok, _} = application:ensure_all_started('{{ application }}'),
        erlang:process_flag(trap_exit, false),
        Module:main(),
        erlang:halt(0)
    catch
        Class:Reason:StackTrace ->
            print_error(Class, Reason, StackTrace),
            erlang:halt(127, [{flush, true}])
    end.

print_error(Class, Error, Stacktrace) ->
    Printed = [
        ?red, "runtime error: ", ?reset_color, error_class(Class, Error), ?reset_all,
        "\n\n",
        error_message(Error),
        "\n\n",
        error_details(Error),
        "stacktrace:\n",
        [error_frame(Line) || Line <- Stacktrace]
    ],
    io:format("~ts~n", [Printed]).

error_class(_, #{gleam_error := panic}) -> "panic";
error_class(_, #{gleam_error := todo}) -> "todo";
error_class(_, #{gleam_error := let_assert}) -> "let assert";
error_class(Class, _) -> ["Erlang ", atom_to_binary(Class)].

error_message(#{gleam_error := _, message := M}) -> M;
error_message(_) -> <<"External error"/utf8>>.

error_details(#{gleam_error := let_assert, value := V}) ->
    ["\n\nunmatched value:\n    ", print_term(V)];
error_details(#{gleam_error := _}) ->
    [];
error_details(E) ->
    ["\n\nerlang error:\n    ", print_term(E)].

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
    ["    ", M1, $., atom_to_binary(F), error_frame_end(O), $\n].

error_frame_end([{file, Fi}, {line, L} | _]) ->
    [$\s, ?grey, Fi, $:, integer_to_binary(L), ?reset_all].
