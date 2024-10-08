-module(gleam@iterator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([unfold/2, repeatedly/1, repeat/1, from_list/1, transform/3, fold/3, run/1, to_list/1, step/1, take/2, drop/2, map/2, map2/3, append/2, flatten/1, concat/1, flat_map/2, filter/2, filter_map/2, cycle/1, find/2, find_map/2, index/1, iterate/2, take_while/2, drop_while/2, scan/3, zip/2, chunk/2, sized_chunk/2, intersperse/2, any/2, all/2, group/2, reduce/2, last/1, empty/0, once/1, range/2, single/1, interleave/2, fold_until/3, try_fold/3, first/1, at/2, length/1, each/2, yield/2]).
-export_type([action/1, iterator/1, step/2, chunk/2, sized_chunk/1]).

-type action(DSX) :: stop | {continue, DSX, fun(() -> action(DSX))}.

-opaque iterator(DSY) :: {iterator, fun(() -> action(DSY))}.

-type step(DSZ, DTA) :: {next, DSZ, DTA} | done.

-type chunk(DTB, DTC) :: {another_by,
        list(DTB),
        DTC,
        DTB,
        fun(() -> action(DTB))} |
    {last_by, list(DTB)}.

-type sized_chunk(DTD) :: {another, list(DTD), fun(() -> action(DTD))} |
    {last, list(DTD)} |
    no_more.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 37).
-spec stop() -> action(any()).
stop() ->
    stop.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 42).
-spec do_unfold(DTG, fun((DTG) -> step(DTH, DTG))) -> fun(() -> action(DTH)).
do_unfold(Initial, F) ->
    fun() -> case F(Initial) of
            {next, X, Acc} ->
                {continue, X, do_unfold(Acc, F)};

            done ->
                stop
        end end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 75).
-spec unfold(DTL, fun((DTL) -> step(DTM, DTL))) -> iterator(DTM).
unfold(Initial, F) ->
    _pipe = Initial,
    _pipe@1 = do_unfold(_pipe, F),
    {iterator, _pipe@1}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 94).
-spec repeatedly(fun(() -> DTQ)) -> iterator(DTQ).
repeatedly(F) ->
    unfold(nil, fun(_) -> {next, F(), nil} end).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 109).
-spec repeat(DTS) -> iterator(DTS).
repeat(X) ->
    repeatedly(fun() -> X end).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 123).
-spec from_list(list(DTU)) -> iterator(DTU).
from_list(List) ->
    Yield = fun(Acc) -> case Acc of
            [] ->
                done;

            [Head | Tail] ->
                {next, Head, Tail}
        end end,
    unfold(List, Yield).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 134).
-spec do_transform(
    fun(() -> action(DTX)),
    DTZ,
    fun((DTZ, DTX) -> step(DUA, DTZ))
) -> fun(() -> action(DUA)).
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 169).
-spec transform(iterator(DUE), DUG, fun((DUG, DUE) -> step(DUH, DUG))) -> iterator(DUH).
transform(Iterator, Initial, F) ->
    _pipe = do_transform(erlang:element(2, Iterator), Initial, F),
    {iterator, _pipe}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 178).
-spec do_fold(fun(() -> action(DUL)), fun((DUN, DUL) -> DUN), DUN) -> DUN.
do_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        {continue, Elem, Next} ->
            do_fold(Next, F, F(Accumulator, Elem));

        stop ->
            Accumulator
    end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 206).
-spec fold(iterator(DUO), DUQ, fun((DUQ, DUO) -> DUQ)) -> DUQ.
fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold(_pipe, F, Initial).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 220).
-spec run(iterator(any())) -> nil.
run(Iterator) ->
    fold(Iterator, nil, fun(_, _) -> nil end).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 238).
-spec to_list(iterator(DUT)) -> list(DUT).
to_list(Iterator) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, [], fun(Acc, E) -> [E | Acc] end),
    lists:reverse(_pipe@1).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 266).
-spec step(iterator(DUW)) -> step(DUW, iterator(DUW)).
step(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            done;

        {continue, E, A} ->
            {next, E, {iterator, A}}
    end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 273).
-spec do_take(fun(() -> action(DVB)), integer()) -> fun(() -> action(DVB)).
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 306).
-spec take(iterator(DVE), integer()) -> iterator(DVE).
take(Iterator, Desired) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take(_pipe, Desired),
    {iterator, _pipe@1}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 312).
-spec do_drop(fun(() -> action(DVH)), integer()) -> action(DVH).
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 348).
-spec drop(iterator(DVK), integer()) -> iterator(DVK).
drop(Iterator, Desired) ->
    _pipe = fun() -> do_drop(erlang:element(2, Iterator), Desired) end,
    {iterator, _pipe}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 353).
-spec do_map(fun(() -> action(DVN)), fun((DVN) -> DVP)) -> fun(() -> action(DVP)).
do_map(Continuation, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, F(E), do_map(Continuation@1, F)}
        end end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 379).
-spec map(iterator(DVR), fun((DVR) -> DVT)) -> iterator(DVT).
map(Iterator, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_map(_pipe, F),
    {iterator, _pipe@1}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 385).
-spec do_map2(
    fun(() -> action(DVV)),
    fun(() -> action(DVX)),
    fun((DVV, DVX) -> DVZ)
) -> fun(() -> action(DVZ)).
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 426).
-spec map2(iterator(DWB), iterator(DWD), fun((DWB, DWD) -> DWF)) -> iterator(DWF).
map2(Iterator1, Iterator2, Fun) ->
    _pipe = do_map2(
        erlang:element(2, Iterator1),
        erlang:element(2, Iterator2),
        Fun
    ),
    {iterator, _pipe}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 435).
-spec do_append(fun(() -> action(DWH)), fun(() -> action(DWH))) -> action(DWH).
do_append(First, Second) ->
    case First() of
        {continue, E, First@1} ->
            {continue, E, fun() -> do_append(First@1, Second) end};

        stop ->
            Second()
    end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 456).
-spec append(iterator(DWL), iterator(DWL)) -> iterator(DWL).
append(First, Second) ->
    _pipe = fun() ->
        do_append(erlang:element(2, First), erlang:element(2, Second))
    end,
    {iterator, _pipe}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 461).
-spec do_flatten(fun(() -> action(iterator(DWP)))) -> action(DWP).
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 484).
-spec flatten(iterator(iterator(DWT))) -> iterator(DWT).
flatten(Iterator) ->
    _pipe = fun() -> do_flatten(erlang:element(2, Iterator)) end,
    {iterator, _pipe}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 504).
-spec concat(list(iterator(DWX))) -> iterator(DWX).
concat(Iterators) ->
    flatten(from_list(Iterators)).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 526).
-spec flat_map(iterator(DXB), fun((DXB) -> iterator(DXD))) -> iterator(DXD).
flat_map(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    flatten(_pipe@1).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 535).
-spec do_filter(fun(() -> action(DXG)), fun((DXG) -> boolean())) -> action(DXG).
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 568).
-spec filter(iterator(DXJ), fun((DXJ) -> boolean())) -> iterator(DXJ).
filter(Iterator, Predicate) ->
    _pipe = fun() -> do_filter(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 576).
-spec do_filter_map(
    fun(() -> action(DXM)),
    fun((DXM) -> {ok, DXO} | {error, any()})
) -> action(DXO).
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 612).
-spec filter_map(iterator(DXT), fun((DXT) -> {ok, DXV} | {error, any()})) -> iterator(DXV).
filter_map(Iterator, F) ->
    _pipe = fun() -> do_filter_map(erlang:element(2, Iterator), F) end,
    {iterator, _pipe}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 632).
-spec cycle(iterator(DYA)) -> iterator(DYA).
cycle(Iterator) ->
    _pipe = repeat(Iterator),
    flatten(_pipe).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 678).
-spec do_find(fun(() -> action(DYE)), fun((DYE) -> boolean())) -> {ok, DYE} |
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 712).
-spec find(iterator(DYI), fun((DYI) -> boolean())) -> {ok, DYI} | {error, nil}.
find(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    do_find(_pipe, Is_desired).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 720).
-spec do_find_map(
    fun(() -> action(DYM)),
    fun((DYM) -> {ok, DYO} | {error, any()})
) -> {ok, DYO} | {error, nil}.
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 757).
-spec find_map(iterator(DYU), fun((DYU) -> {ok, DYW} | {error, any()})) -> {ok,
        DYW} |
    {error, nil}.
find_map(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    do_find_map(_pipe, Is_desired).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 765).
-spec do_index(fun(() -> action(DZC)), integer()) -> fun(() -> action({DZC,
    integer()})).
do_index(Continuation, Next) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, {E, Next}, do_index(Continuation@1, Next + 1)}
        end end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 787).
-spec index(iterator(DZF)) -> iterator({DZF, integer()}).
index(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_index(_pipe, 0),
    {iterator, _pipe@1}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 802).
-spec iterate(DZI, fun((DZI) -> DZI)) -> iterator(DZI).
iterate(Initial, F) ->
    unfold(Initial, fun(Element) -> {next, Element, F(Element)} end).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 809).
-spec do_take_while(fun(() -> action(DZK)), fun((DZK) -> boolean())) -> fun(() -> action(DZK)).
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 836).
-spec take_while(iterator(DZN), fun((DZN) -> boolean())) -> iterator(DZN).
take_while(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take_while(_pipe, Predicate),
    {iterator, _pipe@1}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 845).
-spec do_drop_while(fun(() -> action(DZQ)), fun((DZQ) -> boolean())) -> action(DZQ).
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 871).
-spec drop_while(iterator(DZT), fun((DZT) -> boolean())) -> iterator(DZT).
drop_while(Iterator, Predicate) ->
    _pipe = fun() -> do_drop_while(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 879).
-spec do_scan(fun(() -> action(DZW)), fun((DZY, DZW) -> DZY), DZY) -> fun(() -> action(DZY)).
do_scan(Continuation, F, Accumulator) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                Accumulated = F(Accumulator, El),
                {continue, Accumulated, do_scan(Next, F, Accumulated)}
        end end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 909).
-spec scan(iterator(EAA), EAC, fun((EAC, EAA) -> EAC)) -> iterator(EAC).
scan(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_scan(_pipe, F, Initial),
    {iterator, _pipe@1}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 919).
-spec do_zip(fun(() -> action(EAE)), fun(() -> action(EAG))) -> fun(() -> action({EAE,
    EAG})).
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 948).
-spec zip(iterator(EAJ), iterator(EAL)) -> iterator({EAJ, EAL}).
zip(Left, Right) ->
    _pipe = do_zip(erlang:element(2, Left), erlang:element(2, Right)),
    {iterator, _pipe}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 959).
-spec next_chunk(fun(() -> action(EAO)), fun((EAO) -> EAQ), EAQ, list(EAO)) -> chunk(EAO, EAQ).
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 977).
-spec do_chunk(fun(() -> action(EAU)), fun((EAU) -> EAW), EAW, EAU) -> action(list(EAU)).
do_chunk(Continuation, F, Previous_key, Previous_element) ->
    case next_chunk(Continuation, F, Previous_key, [Previous_element]) of
        {last_by, Chunk} ->
            {continue, Chunk, fun stop/0};

        {another_by, Chunk@1, Key, El, Next} ->
            {continue, Chunk@1, fun() -> do_chunk(Next, F, Key, El) end}
    end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1002).
-spec chunk(iterator(EAZ), fun((EAZ) -> any())) -> iterator(list(EAZ)).
chunk(Iterator, F) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                do_chunk(Next, F, F(E), E)
        end end,
    {iterator, _pipe}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1022).
-spec next_sized_chunk(fun(() -> action(EBE)), integer(), list(EBE)) -> sized_chunk(EBE).
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1043).
-spec do_sized_chunk(fun(() -> action(EBI)), integer()) -> fun(() -> action(list(EBI))).
do_sized_chunk(Continuation, Count) ->
    fun() -> case next_sized_chunk(Continuation, Count, []) of
            no_more ->
                stop;

            {last, Chunk} ->
                {continue, Chunk, fun stop/0};

            {another, Chunk@1, Next_element} ->
                {continue, Chunk@1, do_sized_chunk(Next_element, Count)}
        end end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1080).
-spec sized_chunk(iterator(EBM), integer()) -> iterator(list(EBM)).
sized_chunk(Iterator, Count) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_sized_chunk(_pipe, Count),
    {iterator, _pipe@1}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1089).
-spec do_intersperse(fun(() -> action(EBQ)), EBQ) -> action(EBQ).
do_intersperse(Continuation, Separator) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            Next_interspersed = fun() -> do_intersperse(Next, Separator) end,
            {continue, Separator, fun() -> {continue, E, Next_interspersed} end}
    end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1128).
-spec intersperse(iterator(EBT), EBT) -> iterator(EBT).
intersperse(Iterator, Elem) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                {continue, E, fun() -> do_intersperse(Next, Elem) end}
        end end,
    {iterator, _pipe}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1141).
-spec do_any(fun(() -> action(EBW)), fun((EBW) -> boolean())) -> boolean().
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1182).
-spec any(iterator(EBY), fun((EBY) -> boolean())) -> boolean().
any(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_any(_pipe, Predicate).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1190).
-spec do_all(fun(() -> action(ECA)), fun((ECA) -> boolean())) -> boolean().
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1231).
-spec all(iterator(ECC), fun((ECC) -> boolean())) -> boolean().
all(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_all(_pipe, Predicate).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1239).
-spec update_group_with(ECE) -> fun((gleam@option:option(list(ECE))) -> list(ECE)).
update_group_with(El) ->
    fun(Maybe_group) -> case Maybe_group of
            {some, Group} ->
                [El | Group];

            none ->
                [El]
        end end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1248).
-spec group_updater(fun((ECI) -> ECJ)) -> fun((gleam@dict:dict(ECJ, list(ECI)), ECI) -> gleam@dict:dict(ECJ, list(ECI))).
group_updater(F) ->
    fun(Groups, Elem) -> _pipe = Groups,
        gleam@dict:upsert(_pipe, F(Elem), update_group_with(Elem)) end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1270).
-spec group(iterator(ECQ), fun((ECQ) -> ECS)) -> gleam@dict:dict(ECS, list(ECQ)).
group(Iterator, Key) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, gleam@dict:new(), group_updater(Key)),
    gleam@dict:map_values(_pipe@1, fun(_, Group) -> lists:reverse(Group) end).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1300).
-spec reduce(iterator(ECW), fun((ECW, ECW) -> ECW)) -> {ok, ECW} | {error, nil}.
reduce(Iterator, F) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            _pipe = do_fold(Next, F, E),
            {ok, _pipe}
    end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1330).
-spec last(iterator(EDA)) -> {ok, EDA} | {error, nil}.
last(Iterator) ->
    _pipe = Iterator,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1344).
-spec empty() -> iterator(any()).
empty() ->
    {iterator, fun stop/0}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1357).
-spec once(fun(() -> EDG)) -> iterator(EDG).
once(F) ->
    _pipe = fun() -> {continue, F(), fun stop/0} end,
    {iterator, _pipe}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 657).
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1371).
-spec single(EDI) -> iterator(EDI).
single(Elem) ->
    once(fun() -> Elem end).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1375).
-spec do_interleave(fun(() -> action(EDK)), fun(() -> action(EDK))) -> action(EDK).
do_interleave(Current, Next) ->
    case Current() of
        stop ->
            Next();

        {continue, E, Next_other} ->
            {continue, E, fun() -> do_interleave(Next, Next_other) end}
    end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1405).
-spec interleave(iterator(EDO), iterator(EDO)) -> iterator(EDO).
interleave(Left, Right) ->
    _pipe = fun() ->
        do_interleave(erlang:element(2, Left), erlang:element(2, Right))
    end,
    {iterator, _pipe}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1413).
-spec do_fold_until(
    fun(() -> action(EDS)),
    fun((EDU, EDS) -> gleam@list:continue_or_stop(EDU)),
    EDU
) -> EDU.
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1452).
-spec fold_until(
    iterator(EDW),
    EDY,
    fun((EDY, EDW) -> gleam@list:continue_or_stop(EDY))
) -> EDY.
fold_until(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold_until(_pipe, F, Initial).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1461).
-spec do_try_fold(
    fun(() -> action(EEA)),
    fun((EEC, EEA) -> {ok, EEC} | {error, EED}),
    EEC
) -> {ok, EEC} | {error, EED}.
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

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1496).
-spec try_fold(iterator(EEI), EEK, fun((EEK, EEI) -> {ok, EEK} | {error, EEL})) -> {ok,
        EEK} |
    {error, EEL}.
try_fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_try_fold(_pipe, F, Initial).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1519).
-spec first(iterator(EEQ)) -> {ok, EEQ} | {error, nil}.
first(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, _} ->
            {ok, E}
    end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1549).
-spec at(iterator(EEU), integer()) -> {ok, EEU} | {error, nil}.
at(Iterator, Index) ->
    _pipe = Iterator,
    _pipe@1 = drop(_pipe, Index),
    first(_pipe@1).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1555).
-spec do_length(fun(() -> action(any())), integer()) -> integer().
do_length(Continuation, Length) ->
    case Continuation() of
        stop ->
            Length;

        {continue, _, Next} ->
            do_length(Next, Length + 1)
    end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1579).
-spec length(iterator(any())) -> integer().
length(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    do_length(_pipe, 0).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1601).
-spec each(iterator(EFC), fun((EFC) -> any())) -> nil.
each(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    run(_pipe@1).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1626).
-spec yield(EFF, fun(() -> iterator(EFF))) -> iterator(EFF).
yield(Element, Next) ->
    {iterator,
        fun() ->
            {continue, Element, fun() -> (erlang:element(2, Next()))() end}
        end}.
