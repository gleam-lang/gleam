-module(gleam@iterator).
-compile([no_auto_import, nowarn_unused_vars]).

-export([step/1, empty/0, once/1, single/1, first/1, unfold/2, repeatedly/1, repeat/1, from_list/1, range/2, iterate/2, transform/3, fold/3, run/1, to_list/1, reduce/2, last/1, take/2, drop/2, at/2, map/2, group/2, each/2, append/2, flatten/1, flat_map/2, cycle/1, filter/2, find/2, index/1, take_while/2, drop_while/2, scan/3, zip/2, chunk/2, sized_chunk/2, intersperse/2, any/2, all/2, interleave/2, fold_until/3, try_fold/3, length/1]).
-export_type([action/1, iterator/1, step/2, chunk/2, sized_chunk/1]).

-type action(BMM) :: stop | {continue, BMM, fun(() -> action(BMM))}.

-opaque iterator(BMN) :: {iterator, fun(() -> action(BMN))}.

-type step(BMO, BMP) :: {next, BMO, BMP} | done.

-type chunk(BMQ, BMR) :: {another_by,
        list(BMQ),
        BMR,
        BMQ,
        fun(() -> action(BMQ))} |
    {last_by, list(BMQ)}.

-type sized_chunk(BMS) :: {another, list(BMS), fun(() -> action(BMS))} |
    {last, list(BMS)} |
    no_more.

-spec stop() -> action(any()).
stop() ->
    stop.

-spec step(iterator(BOL)) -> step(BOL, iterator(BOL)).
step(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            done;

        {continue, E, A} ->
            {next, E, {iterator, A}}
    end.

-spec update_group_with(BTZ) -> fun((gleam@option:option(list(BTZ))) -> list(BTZ)).
update_group_with(El) ->
    fun(Maybe_group) -> case Maybe_group of
            {some, Group} ->
                [El | Group];

            none ->
                [El]
        end end.

-spec empty() -> iterator(any()).
empty() ->
    {iterator, fun stop/0}.

-spec once(fun(() -> BVB)) -> iterator(BVB).
once(F) ->
    _pipe = fun() -> {continue, F(), fun stop/0} end,
    {iterator, _pipe}.

-spec single(BVD) -> iterator(BVD).
single(Elem) ->
    once(fun() -> Elem end).

-spec first(iterator(BWL)) -> {ok, BWL} | {error, nil}.
first(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, _} ->
            {ok, E}
    end.

-spec do_unfold(BMV, fun((BMV) -> step(BMW, BMV))) -> fun(() -> action(BMW)).
do_unfold(Initial, F) ->
    fun() -> case F(Initial) of
            {next, X, Acc} ->
                {continue, X, do_unfold(Acc, F)};

            done ->
                stop
        end end.

-spec unfold(BNA, fun((BNA) -> step(BNB, BNA))) -> iterator(BNB).
unfold(Initial, F) ->
    _pipe = Initial,
    _pipe@1 = do_unfold(_pipe, F),
    {iterator, _pipe@1}.

-spec repeatedly(fun(() -> BNF)) -> iterator(BNF).
repeatedly(F) ->
    unfold(nil, fun(_) -> {next, F(), nil} end).

-spec repeat(BNH) -> iterator(BNH).
repeat(X) ->
    repeatedly(fun() -> X end).

-spec from_list(list(BNJ)) -> iterator(BNJ).
from_list(List) ->
    Yield = fun(Acc) -> case Acc of
            [] ->
                done;

            [Head | Tail] ->
                {next, Head, Tail}
        end end,
    unfold(List, Yield).

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

-spec iterate(BRD, fun((BRD) -> BRD)) -> iterator(BRD).
iterate(Initial, F) ->
    unfold(Initial, fun(Element) -> {next, Element, F(Element)} end).

-spec do_transform(
    fun(() -> action(BNM)),
    BNO,
    fun((BNO, BNM) -> step(BNP, BNO))
) -> fun(() -> action(BNP)).
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

-spec transform(iterator(BNT), BNV, fun((BNV, BNT) -> step(BNW, BNV))) -> iterator(BNW).
transform(Iterator, Initial, F) ->
    _pipe = do_transform(erlang:element(2, Iterator), Initial, F),
    {iterator, _pipe}.

-spec do_fold(fun(() -> action(BOA)), fun((BOC, BOA) -> BOC), BOC) -> BOC.
do_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        {continue, Elem, Next} ->
            do_fold(Next, F, F(Accumulator, Elem));

        stop ->
            Accumulator
    end.

-spec fold(iterator(BOD), BOF, fun((BOF, BOD) -> BOF)) -> BOF.
fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold(_pipe, F, Initial).

-spec run(iterator(any())) -> nil.
run(Iterator) ->
    fold(Iterator, nil, fun(_, _) -> nil end).

-spec to_list(iterator(BOI)) -> list(BOI).
to_list(Iterator) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, [], fun(Acc, E) -> [E | Acc] end),
    gleam@list:reverse(_pipe@1).

-spec reduce(iterator(BUR), fun((BUR, BUR) -> BUR)) -> {ok, BUR} | {error, nil}.
reduce(Iterator, F) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            _pipe = do_fold(Next, F, E),
            {ok, _pipe}
    end.

-spec last(iterator(BUV)) -> {ok, BUV} | {error, nil}.
last(Iterator) ->
    _pipe = Iterator,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-spec do_take(fun(() -> action(BOQ)), integer()) -> fun(() -> action(BOQ)).
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

-spec take(iterator(BOT), integer()) -> iterator(BOT).
take(Iterator, Desired) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take(_pipe, Desired),
    {iterator, _pipe@1}.

-spec do_drop(fun(() -> action(BOW)), integer()) -> action(BOW).
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

-spec drop(iterator(BOZ), integer()) -> iterator(BOZ).
drop(Iterator, Desired) ->
    _pipe = fun() -> do_drop(erlang:element(2, Iterator), Desired) end,
    {iterator, _pipe}.

-spec at(iterator(BWP), integer()) -> {ok, BWP} | {error, nil}.
at(Iterator, Index) ->
    _pipe = Iterator,
    _pipe@1 = drop(_pipe, Index),
    first(_pipe@1).

-spec do_map(fun(() -> action(BPC)), fun((BPC) -> BPE)) -> fun(() -> action(BPE)).
do_map(Continuation, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, F(E), do_map(Continuation@1, F)}
        end end.

-spec map(iterator(BPG), fun((BPG) -> BPI)) -> iterator(BPI).
map(Iterator, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_map(_pipe, F),
    {iterator, _pipe@1}.

-spec group_updater(fun((BUD) -> BUE)) -> fun((gleam@map:map_(BUE, list(BUD)), BUD) -> gleam@map:map_(BUE, list(BUD))).
group_updater(F) ->
    fun(Groups, Elem) -> _pipe = Groups,
        gleam@map:update(_pipe, F(Elem), update_group_with(Elem)) end.

-spec group(iterator(BUL), fun((BUL) -> BUN)) -> gleam@map:map_(BUN, list(BUL)).
group(Iterator, Key) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, gleam@map:new(), group_updater(Key)),
    gleam@map:map_values(
        _pipe@1,
        fun(_, Group) -> gleam@list:reverse(Group) end
    ).

-spec each(iterator(BWX), fun((BWX) -> any())) -> nil.
each(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    run(_pipe@1).

-spec do_append(fun(() -> action(BPK)), fun(() -> action(BPK))) -> action(BPK).
do_append(First, Second) ->
    case First() of
        {continue, E, First@1} ->
            {continue, E, fun() -> do_append(First@1, Second) end};

        stop ->
            Second()
    end.

-spec append(iterator(BPO), iterator(BPO)) -> iterator(BPO).
append(First, Second) ->
    _pipe = fun() ->
        do_append(erlang:element(2, First), erlang:element(2, Second))
    end,
    {iterator, _pipe}.

-spec do_flatten(fun(() -> action(iterator(BPS)))) -> action(BPS).
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

-spec flatten(iterator(iterator(BPW))) -> iterator(BPW).
flatten(Iterator) ->
    _pipe = fun() -> do_flatten(erlang:element(2, Iterator)) end,
    {iterator, _pipe}.

-spec flat_map(iterator(BQA), fun((BQA) -> iterator(BQC))) -> iterator(BQC).
flat_map(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    flatten(_pipe@1).

-spec cycle(iterator(BQL)) -> iterator(BQL).
cycle(Iterator) ->
    _pipe = repeat(Iterator),
    flatten(_pipe).

-spec do_filter(fun(() -> action(BQF)), fun((BQF) -> boolean())) -> action(BQF).
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

-spec filter(iterator(BQI), fun((BQI) -> boolean())) -> iterator(BQI).
filter(Iterator, Predicate) ->
    _pipe = fun() -> do_filter(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-spec do_find(fun(() -> action(BQP)), fun((BQP) -> boolean())) -> {ok, BQP} |
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

-spec find(iterator(BQT), fun((BQT) -> boolean())) -> {ok, BQT} | {error, nil}.
find(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    do_find(_pipe, Is_desired).

-spec do_index(fun(() -> action(BQX)), integer()) -> fun(() -> action({integer(),
    BQX})).
do_index(Continuation, Next) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, {Next, E}, do_index(Continuation@1, Next + 1)}
        end end.

-spec index(iterator(BRA)) -> iterator({integer(), BRA}).
index(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_index(_pipe, 0),
    {iterator, _pipe@1}.

-spec do_take_while(fun(() -> action(BRF)), fun((BRF) -> boolean())) -> fun(() -> action(BRF)).
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

-spec take_while(iterator(BRI), fun((BRI) -> boolean())) -> iterator(BRI).
take_while(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take_while(_pipe, Predicate),
    {iterator, _pipe@1}.

-spec do_drop_while(fun(() -> action(BRL)), fun((BRL) -> boolean())) -> action(BRL).
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

-spec drop_while(iterator(BRO), fun((BRO) -> boolean())) -> iterator(BRO).
drop_while(Iterator, Predicate) ->
    _pipe = fun() -> do_drop_while(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-spec do_scan(fun(() -> action(BRR)), fun((BRT, BRR) -> BRT), BRT) -> fun(() -> action(BRT)).
do_scan(Continuation, F, Accumulator) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                Accumulated = F(Accumulator, El),
                {continue, Accumulated, do_scan(Next, F, Accumulated)}
        end end.

-spec scan(iterator(BRV), BRX, fun((BRX, BRV) -> BRX)) -> iterator(BRX).
scan(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_scan(_pipe, F, Initial),
    {iterator, _pipe@1}.

-spec do_zip(fun(() -> action(BRZ)), fun(() -> action(BSB))) -> fun(() -> action({BRZ,
    BSB})).
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

-spec zip(iterator(BSE), iterator(BSG)) -> iterator({BSE, BSG}).
zip(Left, Right) ->
    _pipe = do_zip(erlang:element(2, Left), erlang:element(2, Right)),
    {iterator, _pipe}.

-spec next_chunk(fun(() -> action(BSJ)), fun((BSJ) -> BSL), BSL, list(BSJ)) -> chunk(BSJ, BSL).
next_chunk(Continuation, F, Previous_key, Current_chunk) ->
    case Continuation() of
        stop ->
            {last_by, gleam@list:reverse(Current_chunk)};

        {continue, E, Next} ->
            Key = F(E),
            case Key =:= Previous_key of
                true ->
                    next_chunk(Next, F, Key, [E | Current_chunk]);

                false ->
                    {another_by,
                        gleam@list:reverse(Current_chunk),
                        Key,
                        E,
                        Next}
            end
    end.

-spec do_chunk(fun(() -> action(BSP)), fun((BSP) -> BSR), BSR, BSP) -> action(list(BSP)).
do_chunk(Continuation, F, Previous_key, Previous_element) ->
    case next_chunk(Continuation, F, Previous_key, [Previous_element]) of
        {last_by, Chunk} ->
            {continue, Chunk, fun stop/0};

        {another_by, Chunk@1, Key, El, Next} ->
            {continue, Chunk@1, fun() -> do_chunk(Next, F, Key, El) end}
    end.

-spec chunk(iterator(BSU), fun((BSU) -> any())) -> iterator(list(BSU)).
chunk(Iterator, F) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                do_chunk(Next, F, F(E), E)
        end end,
    {iterator, _pipe}.

-spec next_sized_chunk(fun(() -> action(BSZ)), integer(), list(BSZ)) -> sized_chunk(BSZ).
next_sized_chunk(Continuation, Left, Current_chunk) ->
    case Continuation() of
        stop ->
            case Current_chunk of
                [] ->
                    no_more;

                Remaining ->
                    {last, gleam@list:reverse(Remaining)}
            end;

        {continue, E, Next} ->
            Chunk = [E | Current_chunk],
            case Left > 1 of
                false ->
                    {another, gleam@list:reverse(Chunk), Next};

                true ->
                    next_sized_chunk(Next, Left - 1, Chunk)
            end
    end.

-spec do_sized_chunk(fun(() -> action(BTD)), integer()) -> fun(() -> action(list(BTD))).
do_sized_chunk(Continuation, Count) ->
    fun() -> case next_sized_chunk(Continuation, Count, []) of
            no_more ->
                stop;

            {last, Chunk} ->
                {continue, Chunk, fun stop/0};

            {another, Chunk@1, Next_element} ->
                {continue, Chunk@1, do_sized_chunk(Next_element, Count)}
        end end.

-spec sized_chunk(iterator(BTH), integer()) -> iterator(list(BTH)).
sized_chunk(Iterator, Count) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_sized_chunk(_pipe, Count),
    {iterator, _pipe@1}.

-spec do_intersperse(fun(() -> action(BTL)), BTL) -> action(BTL).
do_intersperse(Continuation, Separator) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            Next_interspersed = fun() -> do_intersperse(Next, Separator) end,
            {continue, Separator, fun() -> {continue, E, Next_interspersed} end}
    end.

-spec intersperse(iterator(BTO), BTO) -> iterator(BTO).
intersperse(Iterator, Elem) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                {continue, E, fun() -> do_intersperse(Next, Elem) end}
        end end,
    {iterator, _pipe}.

-spec do_any(fun(() -> action(BTR)), fun((BTR) -> boolean())) -> boolean().
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

-spec any(iterator(BTT), fun((BTT) -> boolean())) -> boolean().
any(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_any(_pipe, Predicate).

-spec do_all(fun(() -> action(BTV)), fun((BTV) -> boolean())) -> boolean().
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

-spec all(iterator(BTX), fun((BTX) -> boolean())) -> boolean().
all(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_all(_pipe, Predicate).

-spec do_interleave(fun(() -> action(BVF)), fun(() -> action(BVF))) -> action(BVF).
do_interleave(Current, Next) ->
    case Current() of
        stop ->
            Next();

        {continue, E, Next_other} ->
            {continue, E, fun() -> do_interleave(Next, Next_other) end}
    end.

-spec interleave(iterator(BVJ), iterator(BVJ)) -> iterator(BVJ).
interleave(Left, Right) ->
    _pipe = fun() ->
        do_interleave(erlang:element(2, Left), erlang:element(2, Right))
    end,
    {iterator, _pipe}.

-spec do_fold_until(
    fun(() -> action(BVN)),
    fun((BVP, BVN) -> gleam@list:continue_or_stop(BVP)),
    BVP
) -> BVP.
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

-spec fold_until(
    iterator(BVR),
    BVT,
    fun((BVT, BVR) -> gleam@list:continue_or_stop(BVT))
) -> BVT.
fold_until(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold_until(_pipe, F, Initial).

-spec do_try_fold(
    fun(() -> action(BVV)),
    fun((BVX, BVV) -> {ok, BVX} | {error, BVY}),
    BVX
) -> {ok, BVX} | {error, BVY}.
do_try_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            {ok, Accumulator};

        {continue, Elem, Next} ->
            gleam@result:'try'(
                F(Accumulator, Elem),
                fun(Accumulator@1) -> do_try_fold(Next, F, Accumulator@1) end
            )
    end.

-spec try_fold(iterator(BWD), BWF, fun((BWF, BWD) -> {ok, BWF} | {error, BWG})) -> {ok,
        BWF} |
    {error, BWG}.
try_fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_try_fold(_pipe, F, Initial).

-spec do_length(fun(() -> action(any())), integer()) -> integer().
do_length(Continuation, Length) ->
    case Continuation() of
        stop ->
            Length;

        {continue, _, Next} ->
            do_length(Next, Length + 1)
    end.

-spec length(iterator(any())) -> integer().
length(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    do_length(_pipe, 0).
