-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, replace/2, replace_error/2, values/1, try_recover/2, partition/1]).

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

-spec map({ok, BCT} | {error, BCU}, fun((BCT) -> BCX)) -> {ok, BCX} |
    {error, BCU}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BDA} | {error, BDB}, fun((BDB) -> BDE)) -> {ok, BDA} |
    {error, BDE}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BDH} | {error, BDI}} | {error, BDI}) -> {ok, BDH} |
    {error, BDI}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec 'try'({ok, BDP} | {error, BDQ}, fun((BDP) -> {ok, BDT} | {error, BDQ})) -> {ok,
        BDT} |
    {error, BDQ}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec then({ok, BDY} | {error, BDZ}, fun((BDY) -> {ok, BEC} | {error, BDZ})) -> {ok,
        BEC} |
    {error, BDZ}.
then(Result, Fun) ->
    'try'(Result, Fun).

-spec unwrap({ok, BEH} | {error, any()}, BEH) -> BEH.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-spec lazy_unwrap({ok, BEL} | {error, any()}, fun(() -> BEL)) -> BEL.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BEQ}, BEQ) -> BEQ.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BET} | {error, BET}) -> BET.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BEW} | {error, any()}) -> {ok, BEW} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BFC} | {error, BFD}, {ok, BFC} | {error, BFD}) -> {ok, BFC} |
    {error, BFD}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-spec lazy_or({ok, BFK} | {error, BFL}, fun(() -> {ok, BFK} | {error, BFL})) -> {ok,
        BFK} |
    {error, BFL}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-spec all(list({ok, BFS} | {error, BFT})) -> {ok, list(BFS)} | {error, BFT}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec replace({ok, any()} | {error, BGQ}, BGT) -> {ok, BGT} | {error, BGQ}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, BGW} | {error, any()}, BHA) -> {ok, BGW} | {error, BHA}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-spec values(list({ok, BHD} | {error, any()})) -> list(BHD).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-spec try_recover(
    {ok, BHJ} | {error, BHK},
    fun((BHK) -> {ok, BHJ} | {error, BHN})
) -> {ok, BHJ} | {error, BHN}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.

-spec do_partition(list({ok, BGH} | {error, BGI}), list(BGH), list(BGI)) -> {list(BGH),
    list(BGI)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-spec partition(list({ok, BGA} | {error, BGB})) -> {list(BGA), list(BGB)}.
partition(Results) ->
    do_partition(Results, [], []).
