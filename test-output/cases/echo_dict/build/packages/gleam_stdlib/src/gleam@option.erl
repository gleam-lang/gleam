-module(gleam@option).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([all/1, is_some/1, is_none/1, to_result/2, from_result/1, unwrap/2, lazy_unwrap/2, map/2, flatten/1, then/2, 'or'/2, lazy_or/2, values/1]).
-export_type([option/1]).

-type option(FV) :: {some, FV} | none.

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 24).
-spec do_all(list(option(FW)), list(FW)) -> option(list(FW)).
do_all(List, Acc) ->
    case List of
        [] ->
            {some, Acc};

        [X | Rest] ->
            Accumulate = fun(Acc@1, Item) -> case {Acc@1, Item} of
                    {{some, Values}, {some, Value}} ->
                        {some, [Value | Values]};

                    {_, _} ->
                        none
                end end,
            Accumulate(do_all(Rest, Acc), X)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 55).
-spec all(list(option(GC))) -> option(list(GC)).
all(List) ->
    do_all(List, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 73).
-spec is_some(option(any())) -> boolean().
is_some(Option) ->
    Option /= none.

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 91).
-spec is_none(option(any())) -> boolean().
is_none(Option) ->
    Option =:= none.

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 109).
-spec to_result(option(GL), GO) -> {ok, GL} | {error, GO}.
to_result(Option, E) ->
    case Option of
        {some, A} ->
            {ok, A};

        _ ->
            {error, E}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 130).
-spec from_result({ok, GR} | {error, any()}) -> option(GR).
from_result(Result) ->
    case Result of
        {ok, A} ->
            {some, A};

        _ ->
            none
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 151).
-spec unwrap(option(GW), GW) -> GW.
unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 172).
-spec lazy_unwrap(option(GY), fun(() -> GY)) -> GY.
lazy_unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default()
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 197).
-spec map(option(HA), fun((HA) -> HC)) -> option(HC).
map(Option, Fun) ->
    case Option of
        {some, X} ->
            {some, Fun(X)};

        none ->
            none
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 223).
-spec flatten(option(option(HE))) -> option(HE).
flatten(Option) ->
    case Option of
        {some, X} ->
            X;

        none ->
            none
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 262).
-spec then(option(HI), fun((HI) -> option(HK))) -> option(HK).
then(Option, Fun) ->
    case Option of
        {some, X} ->
            Fun(X);

        none ->
            none
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 293).
-spec 'or'(option(HN), option(HN)) -> option(HN).
'or'(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 324).
-spec lazy_or(option(HR), fun(() -> option(HR))) -> option(HR).
lazy_or(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second()
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 331).
-spec do_values(list(option(HV)), list(HV)) -> list(HV).
do_values(List, Acc) ->
    case List of
        [] ->
            Acc;

        [First | Rest] ->
            Accumulate = fun(Acc@1, Item) -> case Item of
                    {some, Value} ->
                        [Value | Acc@1];

                    none ->
                        Acc@1
                end end,
            Accumulate(do_values(Rest, Acc), First)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/option.gleam", 356).
-spec values(list(option(IA))) -> list(IA).
values(Options) ->
    do_values(Options, []).
