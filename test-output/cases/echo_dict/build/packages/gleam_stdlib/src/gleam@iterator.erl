-module(gleam@iterator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([unfold/2, repeatedly/1, repeat/1, from_list/1, transform/3, fold/3, run/1, to_list/1, step/1, take/2, drop/2, map/2, map2/3, append/2, flatten/1, concat/1, flat_map/2, filter/2, filter_map/2, cycle/1, find/2, find_map/2, index/1, iterate/2, take_while/2, drop_while/2, scan/3, zip/2, chunk/2, sized_chunk/2, intersperse/2, any/2, all/2, group/2, reduce/2, last/1, empty/0, once/1, range/2, single/1, interleave/2, fold_until/3, try_fold/3, first/1, at/2, length/1, each/2, yield/2]).
-export_type([action/1, iterator/1, step/2, chunk/2, sized_chunk/1]).

-type action(DSY) :: stop | {continue, DSY, fun(() -> action(DSY))}.

-opaque iterator(DSZ) :: {iterator, fun(() -> action(DSZ))}.

-type step(DTA, DTB) :: {next, DTA, DTB} | done.

-type chunk(DTC, DTD) :: {another_by,
        list(DTC),
        DTD,
        DTC,
        fun(() -> action(DTC))} |
    {last_by, list(DTC)}.

-type sized_chunk(DTE) :: {another, list(DTE), fun(() -> action(DTE))} |
    {last, list(DTE)} |
    no_more.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 37).
-spec stop() -> action(any()).
stop() ->
    stop.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 42).
-spec do_unfold(DTH, fun((DTH) -> step(DTI, DTH))) -> fun(() -> action(DTI)).
do_unfold(Initial, F) ->
    fun() -> case F(Initial) of
            {next, X, Acc} ->
                {continue, X, do_unfold(Acc, F)};

            done ->
                stop
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 75).
-spec unfold(DTM, fun((DTM) -> step(DTN, DTM))) -> iterator(DTN).
unfold(Initial, F) ->
    _pipe = Initial,
    _pipe@1 = do_unfold(_pipe, F),
    {iterator, _pipe@1}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 94).
-spec repeatedly(fun(() -> DTR)) -> iterator(DTR).
repeatedly(F) ->
    unfold(nil, fun(_) -> {next, F(), nil} end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 109).
-spec repeat(DTT) -> iterator(DTT).
repeat(X) ->
    repeatedly(fun() -> X end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 123).
-spec from_list(list(DTV)) -> iterator(DTV).
from_list(List) ->
    Yield = fun(Acc) -> case Acc of
            [] ->
                done;

            [Head | Tail] ->
                {next, Head, Tail}
        end end,
    unfold(List, Yield).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 134).
-spec do_transform(
    fun(() -> action(DTY)),
    DUA,
    fun((DUA, DTY) -> step(DUB, DUA))
) -> fun(() -> action(DUB)).
do_transform(Continuation, State, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                case F(State, El) of
                    done ->
                        stop;

                    {next, Yield, Next_state} ->
                        {continue, Yield, do_transform(Next, Next_state, F)}
                end
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 169).
-spec transform(iterator(DUF), DUH, fun((DUH, DUF) -> step(DUI, DUH))) -> iterator(DUI).
transform(Iterator, Initial, F) ->
    _pipe = do_transform(erlang:element(2, Iterator), Initial, F),
    {iterator, _pipe}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 178).
-spec do_fold(fun(() -> action(DUM)), fun((DUO, DUM) -> DUO), DUO) -> DUO.
do_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        {continue, Elem, Next} ->
            do_fold(Next, F, F(Accumulator, Elem));

        stop ->
            Accumulator
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 206).
-spec fold(iterator(DUP), DUR, fun((DUR, DUP) -> DUR)) -> DUR.
fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold(_pipe, F, Initial).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 220).
-spec run(iterator(any())) -> nil.
run(Iterator) ->
    fold(Iterator, nil, fun(_, _) -> nil end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 238).
-spec to_list(iterator(DUU)) -> list(DUU).
to_list(Iterator) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, [], fun(Acc, E) -> [E | Acc] end),
    lists:reverse(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 266).
-spec step(iterator(DUX)) -> step(DUX, iterator(DUX)).
step(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            done;

        {continue, E, A} ->
            {next, E, {iterator, A}}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 273).
-spec do_take(fun(() -> action(DVC)), integer()) -> fun(() -> action(DVC)).
do_take(Continuation, Desired) ->
    fun() -> case Desired > 0 of
            false ->
                stop;

            true ->
                case Continuation() of
                    stop ->
                        stop;

                    {continue, E, Next} ->
                        {continue, E, do_take(Next, Desired - 1)}
                end
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 306).
-spec take(iterator(DVF), integer()) -> iterator(DVF).
take(Iterator, Desired) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take(_pipe, Desired),
    {iterator, _pipe@1}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 312).
-spec do_drop(fun(() -> action(DVI)), integer()) -> action(DVI).
do_drop(Continuation, Desired) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Desired > 0 of
                true ->
                    do_drop(Next, Desired - 1);

                false ->
                    {continue, E, Next}
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 348).
-spec drop(iterator(DVL), integer()) -> iterator(DVL).
drop(Iterator, Desired) ->
    _pipe = fun() -> do_drop(erlang:element(2, Iterator), Desired) end,
    {iterator, _pipe}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 353).
-spec do_map(fun(() -> action(DVO)), fun((DVO) -> DVQ)) -> fun(() -> action(DVQ)).
do_map(Continuation, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, F(E), do_map(Continuation@1, F)}
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 379).
-spec map(iterator(DVS), fun((DVS) -> DVU)) -> iterator(DVU).
map(Iterator, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_map(_pipe, F),
    {iterator, _pipe@1}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 385).
-spec do_map2(
    fun(() -> action(DVW)),
    fun(() -> action(DVY)),
    fun((DVW, DVY) -> DWA)
) -> fun(() -> action(DWA)).
do_map2(Continuation1, Continuation2, Fun) ->
    fun() -> case Continuation1() of
            stop ->
                stop;

            {continue, A, Next_a} ->
                case Continuation2() of
                    stop ->
                        stop;

                    {continue, B, Next_b} ->
                        {continue, Fun(A, B), do_map2(Next_a, Next_b, Fun)}
                end
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 426).
-spec map2(iterator(DWC), iterator(DWE), fun((DWC, DWE) -> DWG)) -> iterator(DWG).
map2(Iterator1, Iterator2, Fun) ->
    _pipe = do_map2(
        erlang:element(2, Iterator1),
        erlang:element(2, Iterator2),
        Fun
    ),
    {iterator, _pipe}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 435).
-spec do_append(fun(() -> action(DWI)), fun(() -> action(DWI))) -> action(DWI).
do_append(First, Second) ->
    case First() of
        {continue, E, First@1} ->
            {continue, E, fun() -> do_append(First@1, Second) end};

        stop ->
            Second()
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 456).
-spec append(iterator(DWM), iterator(DWM)) -> iterator(DWM).
append(First, Second) ->
    _pipe = fun() ->
        do_append(erlang:element(2, First), erlang:element(2, Second))
    end,
    {iterator, _pipe}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 461).
-spec do_flatten(fun(() -> action(iterator(DWQ)))) -> action(DWQ).
do_flatten(Flattened) ->
    case Flattened() of
        stop ->
            stop;

        {continue, It, Next_iterator} ->
            do_append(
                erlang:element(2, It),
                fun() -> do_flatten(Next_iterator) end
            )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 484).
-spec flatten(iterator(iterator(DWU))) -> iterator(DWU).
flatten(Iterator) ->
    _pipe = fun() -> do_flatten(erlang:element(2, Iterator)) end,
    {iterator, _pipe}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 504).
-spec concat(list(iterator(DWY))) -> iterator(DWY).
concat(Iterators) ->
    flatten(from_list(Iterators)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 526).
-spec flat_map(iterator(DXC), fun((DXC) -> iterator(DXE))) -> iterator(DXE).
flat_map(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    flatten(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 535).
-spec do_filter(fun(() -> action(DXH)), fun((DXH) -> boolean())) -> action(DXH).
do_filter(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Iterator} ->
            case Predicate(E) of
                true ->
                    {continue, E, fun() -> do_filter(Iterator, Predicate) end};

                false ->
                    do_filter(Iterator, Predicate)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 568).
-spec filter(iterator(DXK), fun((DXK) -> boolean())) -> iterator(DXK).
filter(Iterator, Predicate) ->
    _pipe = fun() -> do_filter(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 576).
-spec do_filter_map(
    fun(() -> action(DXN)),
    fun((DXN) -> {ok, DXP} | {error, any()})
) -> action(DXP).
do_filter_map(Continuation, F) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case F(E) of
                {ok, E@1} ->
                    {continue, E@1, fun() -> do_filter_map(Next, F) end};

                {error, _} ->
                    do_filter_map(Next, F)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 612).
-spec filter_map(iterator(DXU), fun((DXU) -> {ok, DXW} | {error, any()})) -> iterator(DXW).
filter_map(Iterator, F) ->
    _pipe = fun() -> do_filter_map(erlang:element(2, Iterator), F) end,
    {iterator, _pipe}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 632).
-spec cycle(iterator(DYB)) -> iterator(DYB).
cycle(Iterator) ->
    _pipe = repeat(Iterator),
    flatten(_pipe).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 678).
-spec do_find(fun(() -> action(DYF)), fun((DYF) -> boolean())) -> {ok, DYF} |
    {error, nil}.
do_find(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                true ->
                    {ok, E};

                false ->
                    do_find(Next, F)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 712).
-spec find(iterator(DYJ), fun((DYJ) -> boolean())) -> {ok, DYJ} | {error, nil}.
find(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    do_find(_pipe, Is_desired).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 720).
-spec do_find_map(
    fun(() -> action(DYN)),
    fun((DYN) -> {ok, DYP} | {error, any()})
) -> {ok, DYP} | {error, nil}.
do_find_map(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                {ok, E@1} ->
                    {ok, E@1};

                {error, _} ->
                    do_find_map(Next, F)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 757).
-spec find_map(iterator(DYV), fun((DYV) -> {ok, DYX} | {error, any()})) -> {ok,
        DYX} |
    {error, nil}.
find_map(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    do_find_map(_pipe, Is_desired).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 765).
-spec do_index(fun(() -> action(DZD)), integer()) -> fun(() -> action({DZD,
    integer()})).
do_index(Continuation, Next) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, {E, Next}, do_index(Continuation@1, Next + 1)}
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 787).
-spec index(iterator(DZG)) -> iterator({DZG, integer()}).
index(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_index(_pipe, 0),
    {iterator, _pipe@1}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 802).
-spec iterate(DZJ, fun((DZJ) -> DZJ)) -> iterator(DZJ).
iterate(Initial, F) ->
    unfold(Initial, fun(Element) -> {next, Element, F(Element)} end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 809).
-spec do_take_while(fun(() -> action(DZL)), fun((DZL) -> boolean())) -> fun(() -> action(DZL)).
do_take_while(Continuation, Predicate) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Next} ->
                case Predicate(E) of
                    false ->
                        stop;

                    true ->
                        {continue, E, do_take_while(Next, Predicate)}
                end
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 836).
-spec take_while(iterator(DZO), fun((DZO) -> boolean())) -> iterator(DZO).
take_while(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take_while(_pipe, Predicate),
    {iterator, _pipe@1}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 845).
-spec do_drop_while(fun(() -> action(DZR)), fun((DZR) -> boolean())) -> action(DZR).
do_drop_while(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Predicate(E) of
                false ->
                    {continue, E, Next};

                true ->
                    do_drop_while(Next, Predicate)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 871).
-spec drop_while(iterator(DZU), fun((DZU) -> boolean())) -> iterator(DZU).
drop_while(Iterator, Predicate) ->
    _pipe = fun() -> do_drop_while(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 879).
-spec do_scan(fun(() -> action(DZX)), fun((DZZ, DZX) -> DZZ), DZZ) -> fun(() -> action(DZZ)).
do_scan(Continuation, F, Accumulator) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                Accumulated = F(Accumulator, El),
                {continue, Accumulated, do_scan(Next, F, Accumulated)}
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 909).
-spec scan(iterator(EAB), EAD, fun((EAD, EAB) -> EAD)) -> iterator(EAD).
scan(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_scan(_pipe, F, Initial),
    {iterator, _pipe@1}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 919).
-spec do_zip(fun(() -> action(EAF)), fun(() -> action(EAH))) -> fun(() -> action({EAF,
    EAH})).
do_zip(Left, Right) ->
    fun() -> case Left() of
            stop ->
                stop;

            {continue, El_left, Next_left} ->
                case Right() of
                    stop ->
                        stop;

                    {continue, El_right, Next_right} ->
                        {continue,
                            {El_left, El_right},
                            do_zip(Next_left, Next_right)}
                end
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 948).
-spec zip(iterator(EAK), iterator(EAM)) -> iterator({EAK, EAM}).
zip(Left, Right) ->
    _pipe = do_zip(erlang:element(2, Left), erlang:element(2, Right)),
    {iterator, _pipe}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 959).
-spec next_chunk(fun(() -> action(EAP)), fun((EAP) -> EAR), EAR, list(EAP)) -> chunk(EAP, EAR).
next_chunk(Continuation, F, Previous_key, Current_chunk) ->
    case Continuation() of
        stop ->
            {last_by, lists:reverse(Current_chunk)};

        {continue, E, Next} ->
            Key = F(E),
            case Key =:= Previous_key of
                true ->
                    next_chunk(Next, F, Key, [E | Current_chunk]);

                false ->
                    {another_by, lists:reverse(Current_chunk), Key, E, Next}
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 977).
-spec do_chunk(fun(() -> action(EAV)), fun((EAV) -> EAX), EAX, EAV) -> action(list(EAV)).
do_chunk(Continuation, F, Previous_key, Previous_element) ->
    case next_chunk(Continuation, F, Previous_key, [Previous_element]) of
        {last_by, Chunk} ->
            {continue, Chunk, fun stop/0};

        {another_by, Chunk@1, Key, El, Next} ->
            {continue, Chunk@1, fun() -> do_chunk(Next, F, Key, El) end}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1002).
-spec chunk(iterator(EBA), fun((EBA) -> any())) -> iterator(list(EBA)).
chunk(Iterator, F) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                do_chunk(Next, F, F(E), E)
        end end,
    {iterator, _pipe}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1022).
-spec next_sized_chunk(fun(() -> action(EBF)), integer(), list(EBF)) -> sized_chunk(EBF).
next_sized_chunk(Continuation, Left, Current_chunk) ->
    case Continuation() of
        stop ->
            case Current_chunk of
                [] ->
                    no_more;

                Remaining ->
                    {last, lists:reverse(Remaining)}
            end;

        {continue, E, Next} ->
            Chunk = [E | Current_chunk],
            case Left > 1 of
                false ->
                    {another, lists:reverse(Chunk), Next};

                true ->
                    next_sized_chunk(Next, Left - 1, Chunk)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1043).
-spec do_sized_chunk(fun(() -> action(EBJ)), integer()) -> fun(() -> action(list(EBJ))).
do_sized_chunk(Continuation, Count) ->
    fun() -> case next_sized_chunk(Continuation, Count, []) of
            no_more ->
                stop;

            {last, Chunk} ->
                {continue, Chunk, fun stop/0};

            {another, Chunk@1, Next_element} ->
                {continue, Chunk@1, do_sized_chunk(Next_element, Count)}
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1080).
-spec sized_chunk(iterator(EBN), integer()) -> iterator(list(EBN)).
sized_chunk(Iterator, Count) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_sized_chunk(_pipe, Count),
    {iterator, _pipe@1}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1089).
-spec do_intersperse(fun(() -> action(EBR)), EBR) -> action(EBR).
do_intersperse(Continuation, Separator) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            Next_interspersed = fun() -> do_intersperse(Next, Separator) end,
            {continue, Separator, fun() -> {continue, E, Next_interspersed} end}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1128).
-spec intersperse(iterator(EBU), EBU) -> iterator(EBU).
intersperse(Iterator, Elem) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                {continue, E, fun() -> do_intersperse(Next, Elem) end}
        end end,
    {iterator, _pipe}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1141).
-spec do_any(fun(() -> action(EBX)), fun((EBX) -> boolean())) -> boolean().
do_any(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            false;

        {continue, E, Next} ->
            case Predicate(E) of
                true ->
                    true;

                false ->
                    do_any(Next, Predicate)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1182).
-spec any(iterator(EBZ), fun((EBZ) -> boolean())) -> boolean().
any(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_any(_pipe, Predicate).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1190).
-spec do_all(fun(() -> action(ECB)), fun((ECB) -> boolean())) -> boolean().
do_all(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            true;

        {continue, E, Next} ->
            case Predicate(E) of
                true ->
                    do_all(Next, Predicate);

                false ->
                    false
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1231).
-spec all(iterator(ECD), fun((ECD) -> boolean())) -> boolean().
all(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_all(_pipe, Predicate).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1239).
-spec update_group_with(ECF) -> fun((gleam@option:option(list(ECF))) -> list(ECF)).
update_group_with(El) ->
    fun(Maybe_group) -> case Maybe_group of
            {some, Group} ->
                [El | Group];

            none ->
                [El]
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1248).
-spec group_updater(fun((ECJ) -> ECK)) -> fun((gleam@dict:dict(ECK, list(ECJ)), ECJ) -> gleam@dict:dict(ECK, list(ECJ))).
group_updater(F) ->
    fun(Groups, Elem) -> _pipe = Groups,
        gleam@dict:upsert(_pipe, F(Elem), update_group_with(Elem)) end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1270).
-spec group(iterator(ECR), fun((ECR) -> ECT)) -> gleam@dict:dict(ECT, list(ECR)).
group(Iterator, Key) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, gleam@dict:new(), group_updater(Key)),
    gleam@dict:map_values(_pipe@1, fun(_, Group) -> lists:reverse(Group) end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1300).
-spec reduce(iterator(ECX), fun((ECX, ECX) -> ECX)) -> {ok, ECX} | {error, nil}.
reduce(Iterator, F) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            _pipe = do_fold(Next, F, E),
            {ok, _pipe}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1330).
-spec last(iterator(EDB)) -> {ok, EDB} | {error, nil}.
last(Iterator) ->
    _pipe = Iterator,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1344).
-spec empty() -> iterator(any()).
empty() ->
    {iterator, fun stop/0}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1357).
-spec once(fun(() -> EDH)) -> iterator(EDH).
once(F) ->
    _pipe = fun() -> {continue, F(), fun stop/0} end,
    {iterator, _pipe}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 657).
-spec range(integer(), integer()) -> iterator(integer()).
range(Start, Stop) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            once(fun() -> Start end);

        gt ->
            unfold(Start, fun(Current) -> case Current < Stop of
                        false ->
                            {next, Current, Current - 1};

                        true ->
                            done
                    end end);

        lt ->
            unfold(Start, fun(Current@1) -> case Current@1 > Stop of
                        false ->
                            {next, Current@1, Current@1 + 1};

                        true ->
                            done
                    end end)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1371).
-spec single(EDJ) -> iterator(EDJ).
single(Elem) ->
    once(fun() -> Elem end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1375).
-spec do_interleave(fun(() -> action(EDL)), fun(() -> action(EDL))) -> action(EDL).
do_interleave(Current, Next) ->
    case Current() of
        stop ->
            Next();

        {continue, E, Next_other} ->
            {continue, E, fun() -> do_interleave(Next, Next_other) end}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1405).
-spec interleave(iterator(EDP), iterator(EDP)) -> iterator(EDP).
interleave(Left, Right) ->
    _pipe = fun() ->
        do_interleave(erlang:element(2, Left), erlang:element(2, Right))
    end,
    {iterator, _pipe}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1413).
-spec do_fold_until(
    fun(() -> action(EDT)),
    fun((EDV, EDT) -> gleam@list:continue_or_stop(EDV)),
    EDV
) -> EDV.
do_fold_until(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            Accumulator;

        {continue, Elem, Next} ->
            case F(Accumulator, Elem) of
                {continue, Accumulator@1} ->
                    do_fold_until(Next, F, Accumulator@1);

                {stop, Accumulator@2} ->
                    Accumulator@2
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1452).
-spec fold_until(
    iterator(EDX),
    EDZ,
    fun((EDZ, EDX) -> gleam@list:continue_or_stop(EDZ))
) -> EDZ.
fold_until(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold_until(_pipe, F, Initial).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1461).
-spec do_try_fold(
    fun(() -> action(EEB)),
    fun((EED, EEB) -> {ok, EED} | {error, EEE}),
    EED
) -> {ok, EED} | {error, EEE}.
do_try_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            {ok, Accumulator};

        {continue, Elem, Next} ->
            case F(Accumulator, Elem) of
                {ok, Result} ->
                    do_try_fold(Next, F, Result);

                {error, _} = Error ->
                    Error
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1496).
-spec try_fold(iterator(EEJ), EEL, fun((EEL, EEJ) -> {ok, EEL} | {error, EEM})) -> {ok,
        EEL} |
    {error, EEM}.
try_fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_try_fold(_pipe, F, Initial).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1519).
-spec first(iterator(EER)) -> {ok, EER} | {error, nil}.
first(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, _} ->
            {ok, E}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1549).
-spec at(iterator(EEV), integer()) -> {ok, EEV} | {error, nil}.
at(Iterator, Index) ->
    _pipe = Iterator,
    _pipe@1 = drop(_pipe, Index),
    first(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1555).
-spec do_length(fun(() -> action(any())), integer()) -> integer().
do_length(Continuation, Length) ->
    case Continuation() of
        stop ->
            Length;

        {continue, _, Next} ->
            do_length(Next, Length + 1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1579).
-spec length(iterator(any())) -> integer().
length(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    do_length(_pipe, 0).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1601).
-spec each(iterator(EFD), fun((EFD) -> any())) -> nil.
each(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    run(_pipe@1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/iterator.gleam", 1626).
-spec yield(EFG, fun(() -> iterator(EFG))) -> iterator(EFG).
yield(Element, Next) ->
    {iterator,
        fun() ->
            {continue, Element, fun() -> (erlang:element(2, Next()))() end}
        end}.
