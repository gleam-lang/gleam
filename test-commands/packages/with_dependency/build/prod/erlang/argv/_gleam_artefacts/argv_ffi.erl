-module(argv_ffi).

-export([load/0]).

load() ->
    Runtime = <<(stringarg(bindir))/binary, "/", (stringarg(progname))/binary>>,
    PlainArguments = lists:map(
        fun(Arg) -> unicode:characters_to_binary(Arg, utf8) end, init:get_plain_arguments()
    ),
    {Program, Arguments} = case init:get_argument(escript) of
        % We're in an escript, so the first argument is the executable name
        {ok, _} ->
            [P | Rest] = PlainArguments,
            {P, Rest};

        % We're not in a escript. Assume the cwd is the project root.
        _ ->
            {ok, Cwd} = file:get_cwd(),
            {unicode:characters_to_binary(Cwd, utf8), PlainArguments}
    end,
    {Runtime, Program, Arguments}.

stringarg(Name) ->
    case init:get_argument(Name) of
        {ok, [[Value]]} -> list_to_binary(Value);
        _ -> <<>>
    end.
