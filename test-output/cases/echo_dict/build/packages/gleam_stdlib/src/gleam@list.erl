-module(gleam@list).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map2/3, index_map/2, try_map/2, drop/2, take/2, new/0, wrap/1, append/2, prepend/2, concat/1, flatten/1, flat_map/2, fold/3, count/2, group/2, map_fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, key_filter/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, try_each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, transpose/1, interleave/1, shuffle/1]).
-export_type([continue_or_stop/1, sorting/0]).

-type continue_or_stop(AAO) :: {continue, AAO} | {stop, AAO}.

-type sorting() :: ascending | descending.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 61).
-spec count_length(list(any()), integer()) -> integer().
count_length(List, Count) ->
    case List of
        [_ | List@1] ->
            count_length(List@1, Count + 1);

        _ ->
            Count
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 57).
-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 130).
-spec do_reverse(list(AAY), list(AAY)) -> list(AAY).
do_reverse(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            do_reverse(Rest, [Item | Accumulator])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 126).
-spec reverse(list(AAV)) -> list(AAV).
reverse(List) ->
    lists:reverse(List).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 158).
-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 194).
-spec contains(list(ABE), ABE) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [First | _] when First =:= Elem ->
            true;

        [_ | Rest] ->
            contains(Rest, Elem)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 221).
-spec first(list(ABG)) -> {ok, ABG} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _] ->
            {ok, X}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 250).
-spec rest(list(ABK)) -> {ok, list(ABK)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Rest] ->
            {ok, Rest}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 257).
-spec update_group(fun((ABP) -> ABQ)) -> fun((gleam@dict:dict(ABQ, list(ABP)), ABP) -> gleam@dict:dict(ABQ, list(ABP))).
update_group(F) ->
    fun(Groups, Elem) -> case gleam@dict:get(Groups, F(Elem)) of
            {ok, Existing} ->
                gleam@dict:insert(Groups, F(Elem), [Elem | Existing]);

            {error, _} ->
                gleam@dict:insert(Groups, F(Elem), [Elem])
        end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 302).
-spec do_filter(list(ACD), fun((ACD) -> boolean()), list(ACD)) -> list(ACD).
do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            New_acc = case Fun(First) of
                true ->
                    [First | Acc];

                false ->
                    Acc
            end,
            do_filter(Rest, Fun, New_acc)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 330).
-spec filter(list(ACH), fun((ACH) -> boolean())) -> list(ACH).
filter(List, Predicate) ->
    do_filter(List, Predicate, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 334).
-spec do_filter_map(
    list(ACK),
    fun((ACK) -> {ok, ACM} | {error, any()}),
    list(ACM)
) -> list(ACM).
do_filter_map(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            New_acc = case Fun(First) of
                {ok, First@1} ->
                    [First@1 | Acc];

                {error, _} ->
                    Acc
            end,
            do_filter_map(Rest, Fun, New_acc)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 366).
-spec filter_map(list(ACS), fun((ACS) -> {ok, ACU} | {error, any()})) -> list(ACU).
filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 370).
-spec do_map(list(ACZ), fun((ACZ) -> ADB), list(ADB)) -> list(ADB).
do_map(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            do_map(Rest, Fun, [Fun(First) | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 387).
-spec map(list(ADE), fun((ADE) -> ADG)) -> list(ADG).
map(List, Fun) ->
    do_map(List, Fun, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 411).
-spec do_map2(list(ADO), list(ADQ), fun((ADO, ADQ) -> ADS), list(ADS)) -> list(ADS).
do_map2(List1, List2, Fun, Acc) ->
    case {List1, List2} of
        {[], _} ->
            lists:reverse(Acc);

        {_, []} ->
            lists:reverse(Acc);

        {[A | As_], [B | Bs]} ->
            do_map2(As_, Bs, Fun, [Fun(A, B) | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 407).
-spec map2(list(ADI), list(ADK), fun((ADI, ADK) -> ADM)) -> list(ADM).
map2(List1, List2, Fun) ->
    do_map2(List1, List2, Fun, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 449).
-spec do_index_map(
    list(AEA),
    fun((AEA, integer()) -> AEC),
    integer(),
    list(AEC)
) -> list(AEC).
do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            Acc@1 = [Fun(First, Index) | Acc],
            do_index_map(Rest, Fun, Index + 1, Acc@1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 477).
-spec index_map(list(AEF), fun((AEF, integer()) -> AEH)) -> list(AEH).
index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 481).
-spec do_try_map(list(AEJ), fun((AEJ) -> {ok, AEL} | {error, AEM}), list(AEL)) -> {ok,
        list(AEL)} |
    {error, AEM}.
do_try_map(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, lists:reverse(Acc)};

        [First | Rest] ->
            case Fun(First) of
                {ok, First@1} ->
                    do_try_map(Rest, Fun, [First@1 | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 528).
-spec try_map(list(AET), fun((AET) -> {ok, AEV} | {error, AEW})) -> {ok,
        list(AEV)} |
    {error, AEW}.
try_map(List, Fun) ->
    do_try_map(List, Fun, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 555).
-spec drop(list(AFC), integer()) -> list(AFC).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Rest] ->
                    drop(Rest, N - 1)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 566).
-spec do_take(list(AFF), integer(), list(AFF)) -> list(AFF).
do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            lists:reverse(Acc);

        false ->
            case List of
                [] ->
                    lists:reverse(Acc);

                [First | Rest] ->
                    do_take(Rest, N - 1, [First | Acc])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 597).
-spec take(list(AFJ), integer()) -> list(AFJ).
take(List, N) ->
    do_take(List, N, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 610).
-spec new() -> list(any()).
new() ->
    [].

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 630).
-spec wrap(AFO) -> list(AFO).
wrap(Item) ->
    [Item].

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 651).
-spec do_append(list(AFU), list(AFU)) -> list(AFU).
do_append(First, Second) ->
    case First of
        [] ->
            Second;

        [Item | Rest] ->
            do_append(Rest, [Item | Second])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 647).
-spec append(list(AFQ), list(AFQ)) -> list(AFQ).
append(First, Second) ->
    lists:append(First, Second).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 671).
-spec prepend(list(AFY), AFY) -> list(AFY).
prepend(List, Item) ->
    [Item | List].

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 676).
-spec reverse_and_prepend(list(AGB), list(AGB)) -> list(AGB).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [First | Rest] ->
            reverse_and_prepend(Rest, [First | Suffix])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 683).
-spec do_concat(list(list(AGF)), list(AGF)) -> list(AGF).
do_concat(Lists, Acc) ->
    case Lists of
        [] ->
            lists:reverse(Acc);

        [List | Further_lists] ->
            do_concat(Further_lists, reverse_and_prepend(List, Acc))
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 703).
-spec concat(list(list(AGK))) -> list(AGK).
concat(Lists) ->
    do_concat(Lists, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 719).
-spec flatten(list(list(AGO))) -> list(AGO).
flatten(Lists) ->
    do_concat(Lists, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 732).
-spec flat_map(list(AGS), fun((AGS) -> list(AGU))) -> list(AGU).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    flatten(_pipe).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 745).
-spec fold(list(AGX), AGZ, fun((AGZ, AGX) -> AGZ)) -> AGZ.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 90).
-spec count(list(AAT), fun((AAT) -> boolean())) -> integer().
count(List, Predicate) ->
    fold(List, 0, fun(Acc, Value) -> case Predicate(Value) of
                true ->
                    Acc + 1;

                false ->
                    Acc
            end end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 298).
-spec group(list(ABX), fun((ABX) -> ABZ)) -> gleam@dict:dict(ABZ, list(ABX)).
group(List, Key) ->
    fold(List, gleam@dict:new(), update_group(Key)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 436).
-spec map_fold(list(ADV), ADX, fun((ADX, ADV) -> {ADX, ADY})) -> {ADX,
    list(ADY)}.
map_fold(List, Initial, Fun) ->
    _pipe = fold(
        List,
        {Initial, []},
        fun(Acc, Item) ->
            {Current_acc, Items} = Acc,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun lists:reverse/1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 767).
-spec fold_right(list(AHA), AHC, fun((AHC, AHA) -> AHC)) -> AHC.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 778).
-spec do_index_fold(
    list(AHD),
    AHF,
    fun((AHF, AHD, integer()) -> AHF),
    integer()
) -> AHF.
do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 800).
-spec index_fold(list(AHG), AHI, fun((AHI, AHG, integer()) -> AHI)) -> AHI.
index_fold(List, Initial, Fun) ->
    do_index_fold(List, Initial, Fun, 0).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 827).
-spec try_fold(list(AHJ), AHL, fun((AHL, AHJ) -> {ok, AHL} | {error, AHM})) -> {ok,
        AHL} |
    {error, AHM}.
try_fold(List, Initial, Fun) ->
    case List of
        [] ->
            {ok, Initial};

        [First | Rest] ->
            case Fun(Initial, First) of
                {ok, Result} ->
                    try_fold(Rest, Result, Fun);

                {error, _} = Error ->
                    Error
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 866).
-spec fold_until(list(AHR), AHT, fun((AHT, AHR) -> continue_or_stop(AHT))) -> AHT.
fold_until(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [First | Rest] ->
            case Fun(Initial, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 903).
-spec find(list(AHV), fun((AHV) -> boolean())) -> {ok, AHV} | {error, nil}.
find(List, Is_desired) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _ ->
                    find(Rest, Is_desired)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 939).
-spec find_map(list(AHZ), fun((AHZ) -> {ok, AIB} | {error, any()})) -> {ok, AIB} |
    {error, nil}.
find_map(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _ ->
                    find_map(Rest, Fun)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 974).
-spec all(list(AIH), fun((AIH) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    all(Rest, Predicate);

                false ->
                    false
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1011).
-spec any(list(AIJ), fun((AIJ) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    true;

                false ->
                    any(Rest, Predicate)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1022).
-spec do_zip(list(AIL), list(AIN), list({AIL, AIN})) -> list({AIL, AIN}).
do_zip(One, Other, Acc) ->
    case {One, Other} of
        {[First_one | Rest_one], [First_other | Rest_other]} ->
            do_zip(Rest_one, Rest_other, [{First_one, First_other} | Acc]);

        {_, _} ->
            lists:reverse(Acc)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1057).
-spec zip(list(AIR), list(AIT)) -> list({AIR, AIT}).
zip(List, Other) ->
    do_zip(List, Other, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1087).
-spec strict_zip(list(AIW), list(AIY)) -> {ok, list({AIW, AIY})} | {error, nil}.
strict_zip(List, Other) ->
    case erlang:length(List) =:= erlang:length(Other) of
        true ->
            {ok, zip(List, Other)};

        false ->
            {error, nil}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1097).
-spec do_unzip(list({AJD, AJE}), list(AJD), list(AJE)) -> {list(AJD), list(AJE)}.
do_unzip(Input, One, Other) ->
    case Input of
        [] ->
            {lists:reverse(One), lists:reverse(Other)};

        [{First_one, First_other} | Rest] ->
            do_unzip(Rest, [First_one | One], [First_other | Other])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1123).
-spec unzip(list({AJK, AJL})) -> {list(AJK), list(AJL)}.
unzip(Input) ->
    do_unzip(Input, [], []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1127).
-spec do_intersperse(list(AJP), AJP, list(AJP)) -> list(AJP).
do_intersperse(List, Separator, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Rest] ->
            do_intersperse(Rest, Separator, [X, Separator | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1150).
-spec intersperse(list(AJT), AJT) -> list(AJT).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_] ->
            List;

        [X | Rest] ->
            do_intersperse(Rest, Elem, [X])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1168).
-spec unique(list(AJW)) -> list(AJW).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1249).
-spec sequences(
    list(AKC),
    fun((AKC, AKC) -> gleam@order:order()),
    list(AKC),
    sorting(),
    AKC,
    list(list(AKC))
) -> list(list(AKC)).
sequences(List, Compare, Growing, Direction, Prev, Acc) ->
    Growing@1 = [Prev | Growing],
    case List of
        [] ->
            case Direction of
                ascending ->
                    [do_reverse(Growing@1, []) | Acc];

                descending ->
                    [Growing@1 | Acc]
            end;

        [New | Rest] ->
            case {Compare(Prev, New), Direction} of
                {gt, descending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {lt, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {eq, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {gt, ascending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {lt, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {eq, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1397).
-spec merge_ascendings(
    list(AKZ),
    list(AKZ),
    fun((AKZ, AKZ) -> gleam@order:order()),
    list(AKZ)
) -> list(AKZ).
merge_ascendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            do_reverse(List, Acc);

        {List, []} ->
            do_reverse(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_ascendings(Rest1, List2, Compare, [First1 | Acc]);

                gt ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc]);

                eq ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1350).
-spec merge_ascending_pairs(
    list(list(AKN)),
    fun((AKN, AKN) -> gleam@order:order()),
    list(list(AKN))
) -> list(list(AKN)).
merge_ascending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            do_reverse(Acc, []);

        [Sequence] ->
            do_reverse([do_reverse(Sequence, []) | Acc], []);

        [Ascending1, Ascending2 | Rest] ->
            Descending = merge_ascendings(Ascending1, Ascending2, Compare, []),
            merge_ascending_pairs(Rest, Compare, [Descending | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1424).
-spec merge_descendings(
    list(ALE),
    list(ALE),
    fun((ALE, ALE) -> gleam@order:order()),
    list(ALE)
) -> list(ALE).
merge_descendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            do_reverse(List, Acc);

        {List, []} ->
            do_reverse(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_descendings(List1, Rest2, Compare, [First2 | Acc]);

                gt ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc]);

                eq ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1372).
-spec merge_descending_pairs(
    list(list(AKT)),
    fun((AKT, AKT) -> gleam@order:order()),
    list(list(AKT))
) -> list(list(AKT)).
merge_descending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            do_reverse(Acc, []);

        [Sequence] ->
            do_reverse([do_reverse(Sequence, []) | Acc], []);

        [Descending1, Descending2 | Rest] ->
            Ascending = merge_descendings(Descending1, Descending2, Compare, []),
            merge_descending_pairs(Rest, Compare, [Ascending | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1316).
-spec merge_all(
    list(list(AKJ)),
    sorting(),
    fun((AKJ, AKJ) -> gleam@order:order())
) -> list(AKJ).
merge_all(Sequences, Direction, Compare) ->
    case {Sequences, Direction} of
        {[], _} ->
            [];

        {[Sequence], ascending} ->
            Sequence;

        {[Sequence@1], descending} ->
            do_reverse(Sequence@1, []);

        {_, ascending} ->
            Sequences@1 = merge_ascending_pairs(Sequences, Compare, []),
            merge_all(Sequences@1, descending, Compare);

        {_, descending} ->
            Sequences@2 = merge_descending_pairs(Sequences, Compare, []),
            merge_all(Sequences@2, ascending, Compare)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1187).
-spec sort(list(AJZ), fun((AJZ, AJZ) -> gleam@order:order())) -> list(AJZ).
sort(List, Compare) ->
    case List of
        [] ->
            [];

        [X] ->
            [X];

        [X@1, Y | Rest] ->
            Direction = case Compare(X@1, Y) of
                lt ->
                    ascending;

                eq ->
                    ascending;

                gt ->
                    descending
            end,
            Sequences = sequences(Rest, Compare, [X@1], Direction, Y, []),
            merge_all(Sequences, ascending, Compare)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1464).
-spec tail_recursive_range(integer(), integer(), list(integer())) -> list(integer()).
tail_recursive_range(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            [Stop | Acc];

        gt ->
            tail_recursive_range(Start, Stop + 1, [Stop | Acc]);

        lt ->
            tail_recursive_range(Start, Stop - 1, [Stop | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1460).
-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    tail_recursive_range(Start, Stop, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1472).
-spec do_repeat(ALM, integer(), list(ALM)) -> list(ALM).
do_repeat(Item, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(Item, Times - 1, [Item | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1493).
-spec repeat(ALP, integer()) -> list(ALP).
repeat(A, Times) ->
    do_repeat(A, Times, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1497).
-spec do_split(list(ALR), integer(), list(ALR)) -> {list(ALR), list(ALR)}.
do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {lists:reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {lists:reverse(Taken), []};

                [First | Rest] ->
                    do_split(Rest, N - 1, [First | Taken])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1530).
-spec split(list(ALW), integer()) -> {list(ALW), list(ALW)}.
split(List, Index) ->
    do_split(List, Index, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1534).
-spec do_split_while(list(AMA), fun((AMA) -> boolean()), list(AMA)) -> {list(AMA),
    list(AMA)}.
do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {lists:reverse(Acc), []};

        [First | Rest] ->
            case F(First) of
                false ->
                    {lists:reverse(Acc), List};

                _ ->
                    do_split_while(Rest, F, [First | Acc])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1567).
-spec split_while(list(AMF), fun((AMF) -> boolean())) -> {list(AMF), list(AMF)}.
split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1599).
-spec key_find(list({AMJ, AMK}), AMJ) -> {ok, AMK} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1630).
-spec key_filter(list({AMO, AMP}), AMO) -> list(AMP).
key_filter(Keyword_list, Desired_key) ->
    filter_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1643).
-spec do_pop(list(BEX), fun((BEX) -> boolean()), list(BEX)) -> {ok,
        {BEX, list(BEX)}} |
    {error, nil}.
do_pop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, lists:append(lists:reverse(Checked), Rest)}};

                false ->
                    do_pop(Rest, Predicate, [X | Checked])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1675).
-spec pop(list(AMW), fun((AMW) -> boolean())) -> {ok, {AMW, list(AMW)}} |
    {error, nil}.
pop(List, Is_desired) ->
    do_pop(List, Is_desired, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1682).
-spec do_pop_map(list(ANB), fun((ANB) -> {ok, AND} | {error, any()}), list(ANB)) -> {ok,
        {AND, list(ANB)}} |
    {error, nil}.
do_pop_map(List, Mapper, Checked) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, lists:append(lists:reverse(Checked), Rest)}};

                {error, _} ->
                    do_pop_map(Rest, Mapper, [X | Checked])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1719).
-spec pop_map(list(ANL), fun((ANL) -> {ok, ANN} | {error, any()})) -> {ok,
        {ANN, list(ANL)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1749).
-spec key_pop(list({ANU, ANV}), ANU) -> {ok, {ANV, list({ANU, ANV})}} |
    {error, nil}.
key_pop(List, Key) ->
    pop_map(
        List,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _ ->
                    {error, nil}
            end
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1776).
-spec key_set(list({AOA, AOB}), AOA, AOB) -> list({AOA, AOB}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1798).
-spec each(list(AOE), fun((AOE) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [First | Rest] ->
            F(First),
            each(Rest, F)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1824).
-spec try_each(list(AOH), fun((AOH) -> {ok, any()} | {error, AOK})) -> {ok, nil} |
    {error, AOK}.
try_each(List, Fun) ->
    case List of
        [] ->
            {ok, nil};

        [First | Rest] ->
            case Fun(First) of
                {ok, _} ->
                    try_each(Rest, Fun);

                {error, E} ->
                    {error, E}
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1838).
-spec do_partition(list(BHC), fun((BHC) -> boolean()), list(BHC), list(BHC)) -> {list(BHC),
    list(BHC)}.
do_partition(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {lists:reverse(Trues), lists:reverse(Falses)};

        [First | Rest] ->
            case Categorise(First) of
                true ->
                    do_partition(Rest, Categorise, [First | Trues], Falses);

                false ->
                    do_partition(Rest, Categorise, Trues, [First | Falses])
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1861).
-spec partition(list(AOU), fun((AOU) -> boolean())) -> {list(AOU), list(AOU)}.
partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1877).
-spec permutations(list(AOY)) -> list(list(AOY)).
permutations(List) ->
    case List of
        [] ->
            [[]];

        _ ->
            _pipe@3 = index_map(
                List,
                fun(I, I_idx) ->
                    _pipe = index_fold(
                        List,
                        [],
                        fun(Acc, J, J_idx) -> case I_idx =:= J_idx of
                                true ->
                                    Acc;

                                false ->
                                    [J | Acc]
                            end end
                    ),
                    _pipe@1 = lists:reverse(_pipe),
                    _pipe@2 = permutations(_pipe@1),
                    map(_pipe@2, fun(Permutation) -> [I | Permutation] end)
                end
            ),
            flatten(_pipe@3)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1896).
-spec do_window(list(list(APC)), list(APC), integer()) -> list(list(APC)).
do_window(Acc, List, N) ->
    Window = take(List, N),
    case erlang:length(Window) =:= N of
        true ->
            do_window([Window | Acc], drop(List, 1), N);

        false ->
            Acc
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1919).
-spec window(list(API), integer()) -> list(list(API)).
window(List, N) ->
    case N =< 0 of
        true ->
            [];

        false ->
            _pipe = do_window([], List, N),
            lists:reverse(_pipe)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1940).
-spec window_by_2(list(APM)) -> list({APM, APM}).
window_by_2(List) ->
    zip(List, drop(List, 1)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1953).
-spec drop_while(list(APP), fun((APP) -> boolean())) -> list(APP).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    drop_while(Rest, Predicate);

                false ->
                    [First | Rest]
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1967).
-spec do_take_while(list(APS), fun((APS) -> boolean()), list(APS)) -> list(APS).
do_take_while(List, Predicate, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    do_take_while(Rest, Predicate, [First | Acc]);

                false ->
                    lists:reverse(Acc)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1991).
-spec take_while(list(APW), fun((APW) -> boolean())) -> list(APW).
take_while(List, Predicate) ->
    do_take_while(List, Predicate, []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 1998).
-spec do_chunk(list(APZ), fun((APZ) -> AQB), AQB, list(APZ), list(list(APZ))) -> list(list(APZ)).
do_chunk(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [First | Rest] ->
            Key = F(First),
            case Key =:= Previous_key of
                false ->
                    New_acc = [lists:reverse(Current_chunk) | Acc],
                    do_chunk(Rest, F, Key, [First], New_acc);

                _ ->
                    do_chunk(Rest, F, Key, [First | Current_chunk], Acc)
            end;

        _ ->
            lists:reverse([lists:reverse(Current_chunk) | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2030).
-spec chunk(list(AQH), fun((AQH) -> any())) -> list(list(AQH)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            do_chunk(Rest, F, F(First), [First], [])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2037).
-spec do_sized_chunk(
    list(AQM),
    integer(),
    integer(),
    list(AQM),
    list(list(AQM))
) -> list(list(AQM)).
do_sized_chunk(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    lists:reverse(Acc);

                Remaining ->
                    lists:reverse([lists:reverse(Remaining) | Acc])
            end;

        [First | Rest] ->
            Chunk = [First | Current_chunk],
            case Left > 1 of
                false ->
                    do_sized_chunk(
                        Rest,
                        Count,
                        Count,
                        [],
                        [lists:reverse(Chunk) | Acc]
                    );

                true ->
                    do_sized_chunk(Rest, Count, Left - 1, Chunk, Acc)
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2079).
-spec sized_chunk(list(AQT), integer()) -> list(list(AQT)).
sized_chunk(List, Count) ->
    do_sized_chunk(List, Count, Count, [], []).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2103).
-spec reduce(list(AQX), fun((AQX, AQX) -> AQX)) -> {ok, AQX} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            {ok, fold(Rest, First, Fun)}
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2110).
-spec do_scan(list(ARB), ARD, list(ARD), fun((ARD, ARB) -> ARD)) -> list(ARD).
do_scan(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            lists:reverse(Accumulated);

        [First | Rest] ->
            Next = Fun(Accumulator, First),
            do_scan(Rest, Next, [Next | Accumulated], Fun)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2134).
-spec scan(list(ARG), ARI, fun((ARI, ARG) -> ARI)) -> list(ARI).
scan(List, Initial, Fun) ->
    do_scan(List, Initial, [], Fun).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2162).
-spec last(list(ARK)) -> {ok, ARK} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2181).
-spec combinations(list(ARO), integer()) -> list(list(ARO)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _ ->
            case Items of
                [] ->
                    [];

                [First | Rest] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Rest, N - 1),
                            fun(Com) -> [First | Com] end
                        ),
                        lists:reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Rest, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2199).
-spec do_combination_pairs(list(ARS)) -> list(list({ARS, ARS})).
do_combination_pairs(Items) ->
    case Items of
        [] ->
            [];

        [First | Rest] ->
            First_combinations = map(Rest, fun(Other) -> {First, Other} end),
            [First_combinations | do_combination_pairs(Rest)]
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2218).
-spec combination_pairs(list(ARW)) -> list({ARW, ARW}).
combination_pairs(Items) ->
    _pipe = do_combination_pairs(Items),
    flatten(_pipe).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2250).
-spec transpose(list(list(ASD))) -> list(list(ASD)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Rest] ->
            transpose(Rest);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                flatten(_pipe@1)
            end,
            Rest@1 = transpose(
                map(Rows, fun(_capture) -> drop(_capture, 1) end)
            ),
            [Firsts | Rest@1]
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2232).
-spec interleave(list(list(ARZ))) -> list(ARZ).
interleave(List) ->
    _pipe = transpose(List),
    flatten(_pipe).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2273).
-spec do_shuffle_pair_unwrap(list({float(), ASI}), list(ASI)) -> list(ASI).
do_shuffle_pair_unwrap(List, Acc) ->
    case List of
        [] ->
            Acc;

        [Elem_pair | Enumerable] ->
            do_shuffle_pair_unwrap(
                Enumerable,
                [erlang:element(2, Elem_pair) | Acc]
            )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2281).
-spec do_shuffle_by_pair_indexes(list({float(), ASM})) -> list({float(), ASM}).
do_shuffle_by_pair_indexes(List_of_pairs) ->
    sort(
        List_of_pairs,
        fun(A_pair, B_pair) ->
            gleam@float:compare(
                erlang:element(1, A_pair),
                erlang:element(1, B_pair)
            )
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/list.gleam", 2300).
-spec shuffle(list(ASP)) -> list(ASP).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(_pipe, [], fun(Acc, A) -> [{rand:uniform(), A} | Acc] end),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    do_shuffle_pair_unwrap(_pipe@2, []).
