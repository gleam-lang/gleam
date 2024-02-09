-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, values/1, try_recover/2]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-spec map({ok, BIJ} | {error, BIK}, fun((BIJ) -> BIN)) -> {ok, BIN} |
    {error, BIK}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BIQ} | {error, BIR}, fun((BIR) -> BIU)) -> {ok, BIQ} |
    {error, BIU}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BIX} | {error, BIY}} | {error, BIY}) -> {ok, BIX} |
    {error, BIY}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec 'try'({ok, BJF} | {error, BJG}, fun((BJF) -> {ok, BJJ} | {error, BJG})) -> {ok,
        BJJ} |
    {error, BJG}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec then({ok, BJO} | {error, BJP}, fun((BJO) -> {ok, BJS} | {error, BJP})) -> {ok,
        BJS} |
    {error, BJP}.
then(Result, Fun) ->
    'try'(Result, Fun).

-spec unwrap({ok, BJX} | {error, any()}, BJX) -> BJX.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-spec lazy_unwrap({ok, BKB} | {error, any()}, fun(() -> BKB)) -> BKB.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BKG}, BKG) -> BKG.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BKJ} | {error, BKJ}) -> BKJ.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BKM} | {error, any()}) -> {ok, BKM} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BKS} | {error, BKT}, {ok, BKS} | {error, BKT}) -> {ok, BKS} |
    {error, BKT}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-spec lazy_or({ok, BLA} | {error, BLB}, fun(() -> {ok, BLA} | {error, BLB})) -> {ok,
        BLA} |
    {error, BLB}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-spec all(list({ok, BLI} | {error, BLJ})) -> {ok, list(BLI)} | {error, BLJ}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec do_partition(list({ok, BLX} | {error, BLY}), list(BLX), list(BLY)) -> {list(BLX),
    list(BLY)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-spec partition(list({ok, BLQ} | {error, BLR})) -> {list(BLQ), list(BLR)}.
partition(Results) ->
    do_partition(Results, [], []).

-spec replace({ok, any()} | {error, BMG}, BMJ) -> {ok, BMJ} | {error, BMG}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, BMM} | {error, any()}, BMQ) -> {ok, BMM} | {error, BMQ}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-spec values(list({ok, BMT} | {error, any()})) -> list(BMT).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-spec try_recover(
    {ok, BMZ} | {error, BNA},
    fun((BNA) -> {ok, BMZ} | {error, BND})
) -> {ok, BMZ} | {error, BND}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
