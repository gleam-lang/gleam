-module(gleam@@main).

-export([run/1]).

run(Module) ->
    io:setopts(standard_io, [binary, {encoding, utf8}]),
    io:setopts(standard_error, [{encoding, utf8}]),
    try
        {ok, _} = application:ensure_all_started('{{ application }}'),
        Module:main(),
        erlang:halt(0)
    catch
        Class:Reason:StackTrace ->
            print_error(Class, Reason, StackTrace),
            erlang:halt(127, [{flush, true}])
    end.

print_error(Class, Reason, StackTrace) -> 
    E = erl_error:format_exception(
        1, Class, Reason, StackTrace, fun stack_filter/3, 
        fun print_stack_frame/2, unicode
    ),
    io:put_chars(E).

stack_filter(Module, _F, _A) -> 
    case Module of
        ?MODULE -> true;
        erl_eval -> true;
        init -> true;
        _ -> false
    end.

print_stack_frame(Term, I) ->
    io_lib:format("~." ++ integer_to_list(I) ++ "tP", [Term, 50]).
