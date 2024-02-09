-module(non_empty_list).
-compile([no_auto_import, nowarn_unused_vars]).

-export([first/1, last/1, new/2, append_list/2, from_list/1, intersperse/2, map/2, prepend/2, reduce/2, rest/1, single/1, to_list/1, append/2, drop/2, reverse/1, map_fold/3, scan/3, shuffle/1, sort/2, take/2, unique/1, unzip/1, zip/2, strict_zip/2, flatten/1, flat_map/2, index_map/2]).
-export_type([non_empty_list/1, list_was_empty/0]).

-type non_empty_list(GJS) :: {non_empty_list, GJS, list(GJS)}.

-type list_was_empty() :: list_was_empty.

-spec first(non_empty_list(GKE)) -> GKE.
first(List) ->
    erlang:element(2, List).

-spec last(non_empty_list(GLP)) -> GLP.
last(List) ->
    _pipe = gleam@list:last(erlang:element(3, List)),
    gleam@result:unwrap(_pipe, erlang:element(2, List)).

-spec new(GMA, list(GMA)) -> non_empty_list(GMA).
new(First, Rest) ->
    {non_empty_list, First, Rest}.

-spec append_list(non_empty_list(GJX), list(GJX)) -> non_empty_list(GJX).
append_list(First, Second) ->
    new(
        erlang:element(2, First),
        gleam@list:append(erlang:element(3, First), Second)
    ).

-spec from_list(list(GKY)) -> {ok, non_empty_list(GKY)} |
    {error, list_was_empty()}.
from_list(List) ->
    case List of
        [] ->
            {error, list_was_empty};

        [First | Rest] ->
            {ok, new(First, Rest)}
    end.

-spec intersperse(non_empty_list(GLM), GLM) -> non_empty_list(GLM).
intersperse(List, Elem) ->
    new(
        erlang:element(2, List),
        [Elem | gleam@list:intersperse(erlang:element(3, List), Elem)]
    ).

-spec map(non_empty_list(GLR), fun((GLR) -> GLT)) -> non_empty_list(GLT).
map(List, Fun) ->
    new(
        Fun(erlang:element(2, List)),
        gleam@list:map(erlang:element(3, List), Fun)
    ).

-spec prepend(non_empty_list(GMD), GMD) -> non_empty_list(GMD).
prepend(List, Item) ->
    new(Item, [erlang:element(2, List) | erlang:element(3, List)]).

-spec reduce(non_empty_list(GMG), fun((GMG, GMG) -> GMG)) -> GMG.
reduce(List, Fun) ->
    gleam@list:fold(erlang:element(3, List), erlang:element(2, List), Fun).

-spec rest(non_empty_list(GMI)) -> list(GMI).
rest(List) ->
    erlang:element(3, List).

-spec single(GMV) -> non_empty_list(GMV).
single(First) ->
    new(First, []).

-spec to_list(non_empty_list(GNK)) -> list(GNK).
to_list(Non_empty) ->
    [erlang:element(2, Non_empty) | erlang:element(3, Non_empty)].

-spec append(non_empty_list(GJT), non_empty_list(GJT)) -> non_empty_list(GJT).
append(First, Second) ->
    new(
        erlang:element(2, First),
        gleam@list:append(erlang:element(3, First), to_list(Second))
    ).

-spec drop(non_empty_list(GKB), integer()) -> list(GKB).
drop(List, N) ->
    _pipe = List,
    _pipe@1 = to_list(_pipe),
    gleam@list:drop(_pipe@1, N).

-spec reverse(non_empty_list(GML)) -> non_empty_list(GML).
reverse(List) ->
    _assert_subject = begin
        _pipe = List,
        _pipe@1 = to_list(_pipe),
        _pipe@2 = gleam@list:reverse(_pipe@1),
        from_list(_pipe@2)
    end,
    {ok, Reversed} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"non_empty_list"/utf8>>,
                        function => <<"reverse"/utf8>>,
                        line => 378})
    end,
    Reversed.

-spec map_fold(non_empty_list(GLV), GLX, fun((GLX, GLV) -> {GLX, GLY})) -> {GLX,
    non_empty_list(GLY)}.
map_fold(List, Acc, Fun) ->
    {Acc@1, First_elem} = Fun(Acc, erlang:element(2, List)),
    _pipe = gleam@list:fold(
        erlang:element(3, List),
        {Acc@1, single(First_elem)},
        fun(Acc_non_empty, Item) ->
            {Acc@2, Non_empty} = Acc_non_empty,
            {Acc@3, New_item} = Fun(Acc@2, Item),
            {Acc@3, prepend(Non_empty, New_item)}
        end
    ),
    gleam@pair:map_second(_pipe, fun reverse/1).

-spec scan(non_empty_list(GMO), GMQ, fun((GMQ, GMO) -> GMQ)) -> non_empty_list(GMQ).
scan(List, Initial, Fun) ->
    _assert_subject = begin
        _pipe = List,
        _pipe@1 = to_list(_pipe),
        _pipe@2 = gleam@list:scan(_pipe@1, Initial, Fun),
        from_list(_pipe@2)
    end,
    {ok, Scanned} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"non_empty_list"/utf8>>,
                        function => <<"scan"/utf8>>,
                        line => 400})
    end,
    Scanned.

-spec shuffle(non_empty_list(GMS)) -> non_empty_list(GMS).
shuffle(List) ->
    _assert_subject = begin
        _pipe = List,
        _pipe@1 = to_list(_pipe),
        _pipe@2 = gleam@list:shuffle(_pipe@1),
        from_list(_pipe@2)
    end,
    {ok, Shuffled} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"non_empty_list"/utf8>>,
                        function => <<"shuffle"/utf8>>,
                        line => 423})
    end,
    Shuffled.

-spec sort(non_empty_list(GMX), fun((GMX, GMX) -> gleam@order:order())) -> non_empty_list(GMX).
sort(List, Compare) ->
    _assert_subject = begin
        _pipe = List,
        _pipe@1 = to_list(_pipe),
        _pipe@2 = gleam@list:sort(_pipe@1, Compare),
        from_list(_pipe@2)
    end,
    {ok, Sorted} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"non_empty_list"/utf8>>,
                        function => <<"sort"/utf8>>,
                        line => 459})
    end,
    Sorted.

-spec take(non_empty_list(GNH), integer()) -> list(GNH).
take(List, N) ->
    _pipe = List,
    _pipe@1 = to_list(_pipe),
    gleam@list:take(_pipe@1, N).

-spec unique(non_empty_list(GNN)) -> non_empty_list(GNN).
unique(List) ->
    _assert_subject = begin
        _pipe = List,
        _pipe@1 = to_list(_pipe),
        _pipe@2 = gleam@list:unique(_pipe@1),
        from_list(_pipe@2)
    end,
    {ok, Unique} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"non_empty_list"/utf8>>,
                        function => <<"unique"/utf8>>,
                        line => 557})
    end,
    Unique.

-spec unzip(non_empty_list({GNQ, GNR})) -> {non_empty_list(GNQ),
    non_empty_list(GNR)}.
unzip(List) ->
    _pipe = gleam@list:unzip(erlang:element(3, List)),
    _pipe@1 = gleam@pair:map_first(
        _pipe,
        fun(_capture) ->
            new(erlang:element(1, erlang:element(2, List)), _capture)
        end
    ),
    gleam@pair:map_second(
        _pipe@1,
        fun(_capture@1) ->
            new(erlang:element(2, erlang:element(2, List)), _capture@1)
        end
    ).

-spec zip(non_empty_list(GNV), non_empty_list(GNX)) -> non_empty_list({GNV, GNX}).
zip(List, Other) ->
    new(
        {erlang:element(2, List), erlang:element(2, Other)},
        gleam@list:zip(erlang:element(3, List), erlang:element(3, Other))
    ).

-spec strict_zip(non_empty_list(GNA), non_empty_list(GNC)) -> {ok,
        non_empty_list({GNA, GNC})} |
    {error, gleam@list:length_mismatch()}.
strict_zip(List, Other) ->
    case gleam@list:length(to_list(List)) =:= gleam@list:length(to_list(Other)) of
        true ->
            {ok, zip(List, Other)};

        false ->
            {error, length_mismatch}
    end.

-spec do_flatten(list(non_empty_list(GKP)), non_empty_list(GKP)) -> non_empty_list(GKP).
do_flatten(Lists, Accumulator) ->
    case Lists of
        [] ->
            reverse(Accumulator);

        [List | Further_lists] ->
            do_flatten(Further_lists, reverse_and_prepend(List, Accumulator))
    end.

-spec flatten(non_empty_list(non_empty_list(GKL))) -> non_empty_list(GKL).
flatten(Lists) ->
    do_flatten(erlang:element(3, Lists), reverse(erlang:element(2, Lists))).

-spec flat_map(non_empty_list(GKG), fun((GKG) -> non_empty_list(GKI))) -> non_empty_list(GKI).
flat_map(List, Fun) ->
    _pipe = List,
    _pipe@1 = map(_pipe, Fun),
    flatten(_pipe@1).

-spec reverse_and_prepend(non_empty_list(GKU), non_empty_list(GKU)) -> non_empty_list(GKU).
reverse_and_prepend(Prefix, Suffix) ->
    case erlang:element(3, Prefix) of
        [] ->
            new(erlang:element(2, Prefix), to_list(Suffix));

        [First | Rest] ->
            reverse_and_prepend(
                new(First, Rest),
                new(erlang:element(2, Prefix), to_list(Suffix))
            )
    end.

-spec do_index_map(
    list(GLH),
    list(GLJ),
    integer(),
    fun((integer(), GLH) -> GLJ)
) -> list(GLJ).
do_index_map(List, Accumulator, Index, Fun) ->
    case List of
        [] ->
            gleam@list:reverse(Accumulator);

        [First | Rest] ->
            do_index_map(
                Rest,
                [Fun(Index, First) | Accumulator],
                Index + 1,
                Fun
            )
    end.

-spec index_map(non_empty_list(GLD), fun((integer(), GLD) -> GLF)) -> non_empty_list(GLF).
index_map(List, Fun) ->
    new(
        Fun(0, erlang:element(2, List)),
        do_index_map(erlang:element(3, List), [], 1, Fun)
    ).
