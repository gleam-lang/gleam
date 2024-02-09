-module(trie).
-compile([no_auto_import, nowarn_unused_vars]).

-export([map/2, new/0, delete/2, fold/3, paths/1, size/1, is_empty/1, to_list/1, values/1, insert/3, from_list/1, singleton/2, get/2, has_path/2, subtrie/2, update/3]).
-export_type([trie/2]).

-opaque trie(EWX, EWY) :: {trie,
        gleam@option:option(EWY),
        gleam@map:map_(EWX, trie(EWX, EWY))}.

-spec do_delete(trie(EXG, EXH), list(EXG)) -> gleam@option:option(trie(EXG, EXH)).
do_delete(Trie, Path) ->
    case {Path, Trie} of
        {[], {trie, _, Children_map}} ->
            case gleam@map:size(Children_map) of
                0 ->
                    none;

                _ ->
                    {some, {trie, none, Children_map}}
            end;

        {[First | Rest], {trie, Entry, Children_map@1}} ->
            New_children = case gleam@map:get(Children_map@1, First) of
                {error, _} ->
                    Children_map@1;

                {ok, Child} ->
                    case do_delete(Child, Rest) of
                        none ->
                            gleam@map:delete(Children_map@1, First);

                        {some, Trie@1} ->
                            gleam@map:insert(Children_map@1, First, Trie@1)
                    end
            end,
            case {Entry, gleam@map:size(New_children)} of
                {none, 0} ->
                    none;

                {_, _} ->
                    {some, {trie, Entry, New_children}}
            end
    end.

-spec map(trie(EYX, EYY), fun((EYY) -> EZB)) -> trie(EYX, EZB).
map(Trie, Fun) ->
    {trie,
        gleam@option:map(erlang:element(2, Trie), Fun),
        gleam@map:map_values(
            erlang:element(3, Trie),
            fun(_, T) -> map(T, Fun) end
        )}.

-spec new() -> trie(any(), any()).
new() ->
    {trie, none, gleam@map:new()}.

-spec delete(trie(EWZ, EXA), list(EWZ)) -> trie(EWZ, EXA).
delete(Trie, Path) ->
    _pipe = do_delete(Trie, Path),
    gleam@option:unwrap(_pipe, new()).

-spec fold(trie(EXO, EXP), EXS, fun((EXS, list(EXO), EXP) -> EXS)) -> EXS.
fold(Trie, Initial, Fun) ->
    gleam@map:fold(
        erlang:element(3, Trie),
        begin
            _pipe = erlang:element(2, Trie),
            _pipe@1 = gleam@option:map(
                _pipe,
                fun(_capture) -> Fun(Initial, [], _capture) end
            ),
            gleam@option:unwrap(_pipe@1, Initial)
        end,
        fun(Acc, First, Trie@1) ->
            fold(
                Trie@1,
                Acc,
                fun(Acc@1, Rest, Value) -> Fun(Acc@1, [First | Rest], Value) end
            )
        end
    ).

-spec paths(trie(EZI, any())) -> list(list(EZI)).
paths(Trie) ->
    fold(Trie, [], fun(Rest, Path, _) -> [Path | Rest] end).

-spec size(trie(any(), any())) -> integer().
size(Trie) ->
    fold(Trie, 0, fun(Acc, _, _) -> Acc + 1 end).

-spec is_empty(trie(any(), any())) -> boolean().
is_empty(Trie) ->
    size(Trie) =:= 0.

-spec to_list(trie(FAG, FAH)) -> list({list(FAG), FAH}).
to_list(Trie) ->
    fold(Trie, [], fun(Rest, Path, Value) -> [{Path, Value} | Rest] end).

-spec values(trie(any(), FBG)) -> list(FBG).
values(Trie) ->
    fold(Trie, [], fun(Values, _, Value) -> [Value | Values] end).

-spec insert(trie(EYM, EYN), list(EYM), EYN) -> trie(EYM, EYN).
insert(Trie, Path, Value) ->
    case {Path, Trie} of
        {[], {trie, _, Children_map}} ->
            {trie, {some, Value}, Children_map};

        {[First | Rest], {trie, Entry, Children_map@1}} ->
            _pipe = gleam@map:get(Children_map@1, First),
            _pipe@1 = gleam@result:unwrap(_pipe, new()),
            _pipe@2 = insert(_pipe@1, Rest, Value),
            _pipe@3 = gleam@map:insert(Children_map@1, First, _pipe@2),
            {trie, Entry, _pipe@3}
    end.

-spec from_list(list({list(EXU), EXW})) -> trie(EXU, EXW).
from_list(List) ->
    gleam@list:fold(
        List,
        new(),
        fun(Trie, Pair) ->
            insert(Trie, erlang:element(1, Pair), erlang:element(2, Pair))
        end
    ).

-spec singleton(list(EZO), EZQ) -> trie(EZO, EZQ).
singleton(Path, Value) ->
    insert(new(), Path, Value).

-spec get(trie(EYA, EYB), list(EYA)) -> {ok, EYB} | {error, nil}.
get(From, Path) ->
    case {Path, From} of
        {[], {trie, none, _}} ->
            {error, nil};

        {[], {trie, {some, Value}, _}} ->
            {ok, Value};

        {[First | Rest], {trie, _, Children_map}} ->
            _pipe = Children_map,
            _pipe@1 = gleam@map:get(_pipe, First),
            gleam@result:then(_pipe@1, fun(_capture) -> get(_capture, Rest) end)
    end.

-spec has_path(trie(EYH, any()), list(EYH)) -> boolean().
has_path(Trie, Path) ->
    case get(Trie, Path) of
        {ok, _} ->
            true;

        {error, _} ->
            false
    end.

-spec subtrie(trie(EZX, EZY), list(EZX)) -> {ok, trie(EZX, EZY)} | {error, nil}.
subtrie(Trie, Prefix) ->
    case {Prefix, Trie} of
        {[], _} ->
            {ok, Trie};

        {[First | Rest], {trie, _, Children_map}} ->
            _pipe = Children_map,
            _pipe@1 = gleam@map:get(_pipe, First),
            _pipe@2 = gleam@result:'try'(
                _pipe@1,
                fun(_capture) -> subtrie(_capture, Rest) end
            ),
            gleam@result:map(_pipe@2, fun(Subtrie) -> _pipe@3 = gleam@map:new(),
                    _pipe@4 = gleam@map:insert(_pipe@3, First, Subtrie),
                    {trie, none, _pipe@4} end)
    end.

-spec do_update(
    trie(FAV, FAW),
    list(FAV),
    fun((gleam@option:option(FAW)) -> gleam@option:option(FAW))
) -> gleam@option:option(trie(FAV, FAW)).
do_update(Trie, Path, Fun) ->
    case {Path, Trie} of
        {[], {trie, Entry, Children_map}} ->
            case {Fun(Entry), gleam@map:size(Children_map)} of
                {none, 0} ->
                    none;

                {_ = New_entry, _} ->
                    {some, {trie, New_entry, Children_map}}
            end;

        {[First | Rest], {trie, Entry@1, Children_map@1}} ->
            New_children = case gleam@map:get(Children_map@1, First) of
                {ok, Child} ->
                    case do_update(Child, Rest, Fun) of
                        none ->
                            gleam@map:delete(Children_map@1, First);

                        {some, New_child} ->
                            gleam@map:insert(Children_map@1, First, New_child)
                    end;

                {error, _} ->
                    case Fun(none) of
                        none ->
                            Children_map@1;

                        {some, Value} ->
                            gleam@map:insert(
                                Children_map@1,
                                First,
                                singleton(Rest, Value)
                            )
                    end
            end,
            case {Entry@1, gleam@map:size(New_children)} of
                {none, 0} ->
                    none;

                {_, _} ->
                    {some, {trie, Entry@1, New_children}}
            end
    end.

-spec update(
    trie(FAM, FAN),
    list(FAM),
    fun((gleam@option:option(FAN)) -> gleam@option:option(FAN))
) -> trie(FAM, FAN).
update(Trie, Path, Fun) ->
    _pipe = do_update(Trie, Path, Fun),
    gleam@option:unwrap(_pipe, new()).
