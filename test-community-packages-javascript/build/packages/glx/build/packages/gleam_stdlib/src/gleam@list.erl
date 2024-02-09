-module(gleam@list).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map2/3, index_map/2, try_map/2, drop/2, take/2, new/0, append/2, prepend/2, concat/1, flatten/1, flat_map/2, fold/3, group/2, map_fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, at/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, key_filter/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, try_each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, transpose/1, interleave/1, shuffle/1]).
-export_type([length_mismatch/0, continue_or_stop/1]).

-type length_mismatch() :: length_mismatch.

-type continue_or_stop(XA) :: {continue, XA} | {stop, XA}.

-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-spec reverse(list(XF)) -> list(XF).
reverse(Xs) ->
    lists:reverse(Xs).

-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-spec contains(list(XN), XN) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [First | _] when First =:= Elem ->
            true;

        [_ | Rest] ->
            contains(Rest, Elem)
    end.

-spec first(list(XP)) -> {ok, XP} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _] ->
            {ok, X}
    end.

-spec rest(list(XT)) -> {ok, list(XT)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Xs] ->
            {ok, Xs}
    end.

-spec update_group(fun((XY) -> XZ)) -> fun((gleam@dict:dict(XZ, list(XY)), XY) -> gleam@dict:dict(XZ, list(XY))).
update_group(F) ->
    fun(Groups, Elem) -> case gleam@dict:get(Groups, F(Elem)) of
            {ok, Existing} ->
                gleam@dict:insert(Groups, F(Elem), [Elem | Existing]);

            {error, _} ->
                gleam@dict:insert(Groups, F(Elem), [Elem])
        end end.

-spec do_filter(list(YM), fun((YM) -> boolean()), list(YM)) -> list(YM).
do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                true ->
                    [X | Acc];

                false ->
                    Acc
            end,
            do_filter(Xs, Fun, New_acc)
    end.

-spec filter(list(YQ), fun((YQ) -> boolean())) -> list(YQ).
filter(List, Predicate) ->
    do_filter(List, Predicate, []).

-spec do_filter_map(list(YT), fun((YT) -> {ok, YV} | {error, any()}), list(YV)) -> list(YV).
do_filter_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                {ok, X@1} ->
                    [X@1 | Acc];

                {error, _} ->
                    Acc
            end,
            do_filter_map(Xs, Fun, New_acc)
    end.

-spec filter_map(list(AAB), fun((AAB) -> {ok, AAD} | {error, any()})) -> list(AAD).
filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

-spec do_map(list(AAI), fun((AAI) -> AAK), list(AAK)) -> list(AAK).
do_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

-spec map(list(AAN), fun((AAN) -> AAP)) -> list(AAP).
map(List, Fun) ->
    do_map(List, Fun, []).

-spec do_map2(list(AAX), list(AAZ), fun((AAX, AAZ) -> ABB), list(ABB)) -> list(ABB).
do_map2(List1, List2, Fun, Acc) ->
    case {List1, List2} of
        {[], _} ->
            reverse(Acc);

        {_, []} ->
            reverse(Acc);

        {[A | As_], [B | Bs]} ->
            do_map2(As_, Bs, Fun, [Fun(A, B) | Acc])
    end.

-spec map2(list(AAR), list(AAT), fun((AAR, AAT) -> AAV)) -> list(AAV).
map2(List1, List2, Fun) ->
    do_map2(List1, List2, Fun, []).

-spec do_index_map(
    list(ABJ),
    fun((integer(), ABJ) -> ABL),
    integer(),
    list(ABL)
) -> list(ABL).
do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            Acc@1 = [Fun(Index, X) | Acc],
            do_index_map(Xs, Fun, Index + 1, Acc@1)
    end.

-spec index_map(list(ABO), fun((integer(), ABO) -> ABQ)) -> list(ABQ).
index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

-spec do_try_map(list(ABS), fun((ABS) -> {ok, ABU} | {error, ABV}), list(ABU)) -> {ok,
        list(ABU)} |
    {error, ABV}.
do_try_map(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, reverse(Acc)};

        [X | Xs] ->
            case Fun(X) of
                {ok, Y} ->
                    do_try_map(Xs, Fun, [Y | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec try_map(list(ACC), fun((ACC) -> {ok, ACE} | {error, ACF})) -> {ok,
        list(ACE)} |
    {error, ACF}.
try_map(List, Fun) ->
    do_try_map(List, Fun, []).

-spec drop(list(ACL), integer()) -> list(ACL).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Xs] ->
                    drop(Xs, N - 1)
            end
    end.

-spec do_take(list(ACO), integer(), list(ACO)) -> list(ACO).
do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            reverse(Acc);

        false ->
            case List of
                [] ->
                    reverse(Acc);

                [X | Xs] ->
                    do_take(Xs, N - 1, [X | Acc])
            end
    end.

-spec take(list(ACS), integer()) -> list(ACS).
take(List, N) ->
    do_take(List, N, []).

-spec new() -> list(any()).
new() ->
    [].

-spec append(list(ACX), list(ACX)) -> list(ACX).
append(First, Second) ->
    lists:append(First, Second).

-spec prepend(list(ADF), ADF) -> list(ADF).
prepend(List, Item) ->
    [Item | List].

-spec reverse_and_prepend(list(ADI), list(ADI)) -> list(ADI).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [First | Rest] ->
            reverse_and_prepend(Rest, [First | Suffix])
    end.

-spec do_concat(list(list(ADM)), list(ADM)) -> list(ADM).
do_concat(Lists, Acc) ->
    case Lists of
        [] ->
            reverse(Acc);

        [List | Further_lists] ->
            do_concat(Further_lists, reverse_and_prepend(List, Acc))
    end.

-spec concat(list(list(ADR))) -> list(ADR).
concat(Lists) ->
    do_concat(Lists, []).

-spec flatten(list(list(ADV))) -> list(ADV).
flatten(Lists) ->
    do_concat(Lists, []).

-spec flat_map(list(ADZ), fun((ADZ) -> list(AEB))) -> list(AEB).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    concat(_pipe).

-spec fold(list(AEE), AEG, fun((AEG, AEE) -> AEG)) -> AEG.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-spec group(list(YG), fun((YG) -> YI)) -> gleam@dict:dict(YI, list(YG)).
group(List, Key) ->
    fold(List, gleam@dict:new(), update_group(Key)).

-spec map_fold(list(ABE), ABG, fun((ABG, ABE) -> {ABG, ABH})) -> {ABG,
    list(ABH)}.
map_fold(List, Acc, Fun) ->
    _pipe = fold(
        List,
        {Acc, []},
        fun(Acc@1, Item) ->
            {Current_acc, Items} = Acc@1,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun reverse/1).

-spec fold_right(list(AEH), AEJ, fun((AEJ, AEH) -> AEJ)) -> AEJ.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-spec do_index_fold(
    list(AEK),
    AEM,
    fun((AEM, AEK, integer()) -> AEM),
    integer()
) -> AEM.
do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-spec index_fold(list(AEN), AEP, fun((AEP, AEN, integer()) -> AEP)) -> AEP.
index_fold(Over, Initial, Fun) ->
    do_index_fold(Over, Initial, Fun, 0).

-spec try_fold(list(AEQ), AES, fun((AES, AEQ) -> {ok, AES} | {error, AET})) -> {ok,
        AES} |
    {error, AET}.
try_fold(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            {ok, Accumulator};

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {ok, Result} ->
                    try_fold(Rest, Result, Fun);

                {error, _} = Error ->
                    Error
            end
    end.

-spec fold_until(list(AEY), AFA, fun((AFA, AEY) -> continue_or_stop(AFA))) -> AFA.
fold_until(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            Accumulator;

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-spec find(list(AFC), fun((AFC) -> boolean())) -> {ok, AFC} | {error, nil}.
find(Haystack, Is_desired) ->
    case Haystack of
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

-spec find_map(list(AFG), fun((AFG) -> {ok, AFI} | {error, any()})) -> {ok, AFI} |
    {error, nil}.
find_map(Haystack, Fun) ->
    case Haystack of
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

-spec all(list(AFO), fun((AFO) -> boolean())) -> boolean().
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

-spec any(list(AFQ), fun((AFQ) -> boolean())) -> boolean().
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

-spec do_zip(list(AFS), list(AFU), list({AFS, AFU})) -> list({AFS, AFU}).
do_zip(Xs, Ys, Acc) ->
    case {Xs, Ys} of
        {[X | Xs@1], [Y | Ys@1]} ->
            do_zip(Xs@1, Ys@1, [{X, Y} | Acc]);

        {_, _} ->
            reverse(Acc)
    end.

-spec zip(list(AFY), list(AGA)) -> list({AFY, AGA}).
zip(List, Other) ->
    do_zip(List, Other, []).

-spec strict_zip(list(AGD), list(AGF)) -> {ok, list({AGD, AGF})} |
    {error, length_mismatch()}.
strict_zip(List, Other) ->
    case length(List) =:= length(Other) of
        true ->
            {ok, zip(List, Other)};

        false ->
            {error, length_mismatch}
    end.

-spec do_unzip(list({AVX, AVY}), list(AVX), list(AVY)) -> {list(AVX), list(AVY)}.
do_unzip(Input, Xs, Ys) ->
    case Input of
        [] ->
            {reverse(Xs), reverse(Ys)};

        [{X, Y} | Rest] ->
            do_unzip(Rest, [X | Xs], [Y | Ys])
    end.

-spec unzip(list({AGO, AGP})) -> {list(AGO), list(AGP)}.
unzip(Input) ->
    do_unzip(Input, [], []).

-spec do_intersperse(list(AGT), AGT, list(AGT)) -> list(AGT).
do_intersperse(List, Separator, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Rest] ->
            do_intersperse(Rest, Separator, [X, Separator | Acc])
    end.

-spec intersperse(list(AGX), AGX) -> list(AGX).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_] ->
            List;

        [X | Rest] ->
            do_intersperse(Rest, Elem, [X])
    end.

-spec at(list(AHA), integer()) -> {ok, AHA} | {error, nil}.
at(List, Index) ->
    case Index >= 0 of
        true ->
            _pipe = List,
            _pipe@1 = drop(_pipe, Index),
            first(_pipe@1);

        false ->
            {error, nil}
    end.

-spec unique(list(AHE)) -> list(AHE).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-spec merge_up(
    integer(),
    integer(),
    list(AHH),
    list(AHH),
    list(AHH),
    fun((AHH, AHH) -> gleam@order:order())
) -> list(AHH).
merge_up(Na, Nb, A, B, Acc, Compare) ->
    case {Na, Nb, A, B} of
        {0, 0, _, _} ->
            Acc;

        {_, 0, [Ax | Ar], _} ->
            merge_up(Na - 1, Nb, Ar, B, [Ax | Acc], Compare);

        {0, _, _, [Bx | Br]} ->
            merge_up(Na, Nb - 1, A, Br, [Bx | Acc], Compare);

        {_, _, [Ax@1 | Ar@1], [Bx@1 | Br@1]} ->
            case Compare(Ax@1, Bx@1) of
                gt ->
                    merge_up(Na, Nb - 1, A, Br@1, [Bx@1 | Acc], Compare);

                _ ->
                    merge_up(Na - 1, Nb, Ar@1, B, [Ax@1 | Acc], Compare)
            end;

        {_, _, _, _} ->
            Acc
    end.

-spec merge_down(
    integer(),
    integer(),
    list(AHM),
    list(AHM),
    list(AHM),
    fun((AHM, AHM) -> gleam@order:order())
) -> list(AHM).
merge_down(Na, Nb, A, B, Acc, Compare) ->
    case {Na, Nb, A, B} of
        {0, 0, _, _} ->
            Acc;

        {_, 0, [Ax | Ar], _} ->
            merge_down(Na - 1, Nb, Ar, B, [Ax | Acc], Compare);

        {0, _, _, [Bx | Br]} ->
            merge_down(Na, Nb - 1, A, Br, [Bx | Acc], Compare);

        {_, _, [Ax@1 | Ar@1], [Bx@1 | Br@1]} ->
            case Compare(Bx@1, Ax@1) of
                lt ->
                    merge_down(Na - 1, Nb, Ar@1, B, [Ax@1 | Acc], Compare);

                _ ->
                    merge_down(Na, Nb - 1, A, Br@1, [Bx@1 | Acc], Compare)
            end;

        {_, _, _, _} ->
            Acc
    end.

-spec merge_sort(
    list(AHR),
    integer(),
    fun((AHR, AHR) -> gleam@order:order()),
    boolean()
) -> list(AHR).
merge_sort(L, Ln, Compare, Down) ->
    N = Ln div 2,
    A = L,
    B = drop(L, N),
    case Ln < 3 of
        true ->
            case Down of
                true ->
                    merge_down(N, Ln - N, A, B, [], Compare);

                false ->
                    merge_up(N, Ln - N, A, B, [], Compare)
            end;

        false ->
            case Down of
                true ->
                    merge_down(
                        N,
                        Ln - N,
                        merge_sort(A, N, Compare, false),
                        merge_sort(B, Ln - N, Compare, false),
                        [],
                        Compare
                    );

                false ->
                    merge_up(
                        N,
                        Ln - N,
                        merge_sort(A, N, Compare, true),
                        merge_sort(B, Ln - N, Compare, true),
                        [],
                        Compare
                    )
            end
    end.

-spec sort(list(AHU), fun((AHU, AHU) -> gleam@order:order())) -> list(AHU).
sort(List, Compare) ->
    merge_sort(List, length(List), Compare, true).

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

-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    tail_recursive_range(Start, Stop, []).

-spec do_repeat(AIA, integer(), list(AIA)) -> list(AIA).
do_repeat(A, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(A, Times - 1, [A | Acc])
    end.

-spec repeat(AID, integer()) -> list(AID).
repeat(A, Times) ->
    do_repeat(A, Times, []).

-spec do_split(list(AIF), integer(), list(AIF)) -> {list(AIF), list(AIF)}.
do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {reverse(Taken), []};

                [X | Xs] ->
                    do_split(Xs, N - 1, [X | Taken])
            end
    end.

-spec split(list(AIK), integer()) -> {list(AIK), list(AIK)}.
split(List, Index) ->
    do_split(List, Index, []).

-spec do_split_while(list(AIO), fun((AIO) -> boolean()), list(AIO)) -> {list(AIO),
    list(AIO)}.
do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {reverse(Acc), []};

        [X | Xs] ->
            case F(X) of
                false ->
                    {reverse(Acc), List};

                _ ->
                    do_split_while(Xs, F, [X | Acc])
            end
    end.

-spec split_while(list(AIT), fun((AIT) -> boolean())) -> {list(AIT), list(AIT)}.
split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

-spec key_find(list({AIX, AIY}), AIX) -> {ok, AIY} | {error, nil}.
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

-spec key_filter(list({AJC, AJD}), AJC) -> list(AJD).
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

-spec do_pop(list(AZQ), fun((AZQ) -> boolean()), list(AZQ)) -> {ok,
        {AZQ, list(AZQ)}} |
    {error, nil}.
do_pop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, append(reverse(Checked), Rest)}};

                false ->
                    do_pop(Rest, Predicate, [X | Checked])
            end
    end.

-spec pop(list(AJK), fun((AJK) -> boolean())) -> {ok, {AJK, list(AJK)}} |
    {error, nil}.
pop(Haystack, Is_desired) ->
    do_pop(Haystack, Is_desired, []).

-spec do_pop_map(list(BAE), fun((BAE) -> {ok, BAR} | {error, any()}), list(BAE)) -> {ok,
        {BAR, list(BAE)}} |
    {error, nil}.
do_pop_map(Haystack, Mapper, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, append(reverse(Checked), Rest)}};

                {error, _} ->
                    do_pop_map(Rest, Mapper, [X | Checked])
            end
    end.

-spec pop_map(list(AJT), fun((AJT) -> {ok, AJV} | {error, any()})) -> {ok,
        {AJV, list(AJT)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

-spec key_pop(list({AKC, AKD}), AKC) -> {ok, {AKD, list({AKC, AKD})}} |
    {error, nil}.
key_pop(Haystack, Key) ->
    pop_map(
        Haystack,
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

-spec key_set(list({AKI, AKJ}), AKI, AKJ) -> list({AKI, AKJ}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-spec each(list(AKM), fun((AKM) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [X | Xs] ->
            F(X),
            each(Xs, F)
    end.

-spec try_each(list(AKP), fun((AKP) -> {ok, any()} | {error, AKS})) -> {ok, nil} |
    {error, AKS}.
try_each(List, Fun) ->
    case List of
        [] ->
            {ok, nil};

        [X | Xs] ->
            case Fun(X) of
                {ok, _} ->
                    try_each(Xs, Fun);

                {error, E} ->
                    {error, E}
            end
    end.

-spec do_partition(list(BBY), fun((BBY) -> boolean()), list(BBY), list(BBY)) -> {list(BBY),
    list(BBY)}.
do_partition(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {reverse(Trues), reverse(Falses)};

        [X | Xs] ->
            case Categorise(X) of
                true ->
                    do_partition(Xs, Categorise, [X | Trues], Falses);

                false ->
                    do_partition(Xs, Categorise, Trues, [X | Falses])
            end
    end.

-spec partition(list(ALC), fun((ALC) -> boolean())) -> {list(ALC), list(ALC)}.
partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

-spec permutations(list(ALG)) -> list(list(ALG)).
permutations(L) ->
    case L of
        [] ->
            [[]];

        _ ->
            _pipe = L,
            _pipe@5 = index_map(_pipe, fun(I_idx, I) -> _pipe@1 = L,
                    _pipe@2 = index_fold(
                        _pipe@1,
                        [],
                        fun(Acc, J, J_idx) -> case I_idx =:= J_idx of
                                true ->
                                    Acc;

                                false ->
                                    [J | Acc]
                            end end
                    ),
                    _pipe@3 = reverse(_pipe@2),
                    _pipe@4 = permutations(_pipe@3),
                    map(_pipe@4, fun(Permutation) -> [I | Permutation] end) end),
            concat(_pipe@5)
    end.

-spec do_window(list(list(ALK)), list(ALK), integer()) -> list(list(ALK)).
do_window(Acc, L, N) ->
    Window = take(L, N),
    case length(Window) =:= N of
        true ->
            do_window([Window | Acc], drop(L, 1), N);

        false ->
            Acc
    end.

-spec window(list(ALQ), integer()) -> list(list(ALQ)).
window(L, N) ->
    _pipe = do_window([], L, N),
    reverse(_pipe).

-spec window_by_2(list(ALU)) -> list({ALU, ALU}).
window_by_2(L) ->
    zip(L, drop(L, 1)).

-spec drop_while(list(ALX), fun((ALX) -> boolean())) -> list(ALX).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [X | Xs] ->
            case Predicate(X) of
                true ->
                    drop_while(Xs, Predicate);

                false ->
                    [X | Xs]
            end
    end.

-spec do_take_while(list(AMA), fun((AMA) -> boolean()), list(AMA)) -> list(AMA).
do_take_while(List, Predicate, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    do_take_while(Rest, Predicate, [First | Acc]);

                false ->
                    reverse(Acc)
            end
    end.

-spec take_while(list(AME), fun((AME) -> boolean())) -> list(AME).
take_while(List, Predicate) ->
    do_take_while(List, Predicate, []).

-spec do_chunk(list(AMH), fun((AMH) -> AMJ), AMJ, list(AMH), list(list(AMH))) -> list(list(AMH)).
do_chunk(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [First | Rest] ->
            Key = F(First),
            case Key =:= Previous_key of
                false ->
                    New_acc = [reverse(Current_chunk) | Acc],
                    do_chunk(Rest, F, Key, [First], New_acc);

                _ ->
                    do_chunk(Rest, F, Key, [First | Current_chunk], Acc)
            end;

        _ ->
            reverse([reverse(Current_chunk) | Acc])
    end.

-spec chunk(list(AMP), fun((AMP) -> any())) -> list(list(AMP)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            do_chunk(Rest, F, F(First), [First], [])
    end.

-spec do_sized_chunk(
    list(AMU),
    integer(),
    integer(),
    list(AMU),
    list(list(AMU))
) -> list(list(AMU)).
do_sized_chunk(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    reverse(Acc);

                Remaining ->
                    reverse([reverse(Remaining) | Acc])
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
                        [reverse(Chunk) | Acc]
                    );

                true ->
                    do_sized_chunk(Rest, Count, Left - 1, Chunk, Acc)
            end
    end.

-spec sized_chunk(list(ANB), integer()) -> list(list(ANB)).
sized_chunk(List, Count) ->
    do_sized_chunk(List, Count, Count, [], []).

-spec reduce(list(ANF), fun((ANF, ANF) -> ANF)) -> {ok, ANF} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            {ok, fold(Rest, First, Fun)}
    end.

-spec do_scan(list(ANJ), ANL, list(ANL), fun((ANL, ANJ) -> ANL)) -> list(ANL).
do_scan(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            reverse(Accumulated);

        [X | Xs] ->
            Next = Fun(Accumulator, X),
            do_scan(Xs, Next, [Next | Accumulated], Fun)
    end.

-spec scan(list(ANO), ANQ, fun((ANQ, ANO) -> ANQ)) -> list(ANQ).
scan(List, Initial, Fun) ->
    do_scan(List, Initial, [], Fun).

-spec last(list(ANS)) -> {ok, ANS} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-spec combinations(list(ANW), integer()) -> list(list(ANW)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _ ->
            case Items of
                [] ->
                    [];

                [X | Xs] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Xs, N - 1),
                            fun(Com) -> [X | Com] end
                        ),
                        reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Xs, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-spec do_combination_pairs(list(AOA)) -> list(list({AOA, AOA})).
do_combination_pairs(Items) ->
    case Items of
        [] ->
            [];

        [X | Xs] ->
            First_combinations = map(Xs, fun(Other) -> {X, Other} end),
            [First_combinations | do_combination_pairs(Xs)]
    end.

-spec combination_pairs(list(AOE)) -> list({AOE, AOE}).
combination_pairs(Items) ->
    _pipe = do_combination_pairs(Items),
    concat(_pipe).

-spec transpose(list(list(AOL))) -> list(list(AOL)).
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

        [[] | Xss] ->
            transpose(Xss);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                concat(_pipe@1)
            end,
            Rest = transpose(map(Rows, fun(_capture) -> drop(_capture, 1) end)),
            [Firsts | Rest]
    end.

-spec interleave(list(list(AOH))) -> list(AOH).
interleave(List) ->
    _pipe = transpose(List),
    concat(_pipe).

-spec do_shuffle_pair_unwrap(list({float(), AOQ}), list(AOQ)) -> list(AOQ).
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

-spec do_shuffle_by_pair_indexes(list({float(), AOU})) -> list({float(), AOU}).
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

-spec shuffle(list(AOX)) -> list(AOX).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(
        _pipe,
        [],
        fun(Acc, A) -> [{gleam@float:random(+0.0, 1.0), A} | Acc] end
    ),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    do_shuffle_pair_unwrap(_pipe@2, []).
