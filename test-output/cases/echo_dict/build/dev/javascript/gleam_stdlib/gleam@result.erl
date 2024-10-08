-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, values/1, try_recover/2]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 20).
-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 41).
-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 66).
-spec map({ok, BXE} | {error, BXF}, fun((BXE) -> BXI)) -> {ok, BXI} |
    {error, BXF}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 91).
-spec map_error({ok, BXL} | {error, BXM}, fun((BXM) -> BXP)) -> {ok, BXL} |
    {error, BXP}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 120).
-spec flatten({ok, {ok, BXS} | {error, BXT}} | {error, BXT}) -> {ok, BXS} |
    {error, BXT}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 158).
-spec 'try'({ok, BYA} | {error, BYB}, fun((BYA) -> {ok, BYE} | {error, BYB})) -> {ok,
        BYE} |
    {error, BYB}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 170).
-spec then({ok, BYJ} | {error, BYK}, fun((BYJ) -> {ok, BYN} | {error, BYK})) -> {ok,
        BYN} |
    {error, BYK}.
then(Result, Fun) ->
    'try'(Result, Fun).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 192).
-spec unwrap({ok, BYS} | {error, any()}, BYS) -> BYS.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 214).
-spec lazy_unwrap({ok, BYW} | {error, any()}, fun(() -> BYW)) -> BYW.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 236).
-spec unwrap_error({ok, any()} | {error, BZB}, BZB) -> BZB.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 258).
-spec unwrap_both({ok, BZE} | {error, BZE}) -> BZE.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 279).
-spec nil_error({ok, BZH} | {error, any()}) -> {ok, BZH} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 307).
-spec 'or'({ok, BZN} | {error, BZO}, {ok, BZN} | {error, BZO}) -> {ok, BZN} |
    {error, BZO}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 340).
-spec lazy_or({ok, BZV} | {error, BZW}, fun(() -> {ok, BZV} | {error, BZW})) -> {ok,
        BZV} |
    {error, BZW}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 366).
-spec all(list({ok, CAD} | {error, CAE})) -> {ok, list(CAD)} | {error, CAE}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 386).
-spec do_partition(list({ok, CAS} | {error, CAT}), list(CAS), list(CAT)) -> {list(CAS),
    list(CAT)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 382).
-spec partition(list({ok, CAL} | {error, CAM})) -> {list(CAL), list(CAM)}.
partition(Results) ->
    do_partition(Results, [], []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 408).
-spec replace({ok, any()} | {error, CBB}, CBE) -> {ok, CBE} | {error, CBB}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 429).
-spec replace_error({ok, CBH} | {error, any()}, CBL) -> {ok, CBH} | {error, CBL}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 445).
-spec values(list({ok, CBO} | {error, any()})) -> list(CBO).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/result.gleam", 478).
-spec try_recover(
    {ok, CBU} | {error, CBV},
    fun((CBV) -> {ok, CBU} | {error, CBY})
) -> {ok, CBU} | {error, CBY}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
