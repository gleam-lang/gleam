-module(gleamy_structures@priority_queue).
-compile([no_auto_import, nowarn_unused_vars]).

-export([is_empty/1, new/1, from_list/2, pop/1, peek/1, push/2, count/1, reorder/2, to_list/1]).

-spec is_empty(gleamy_structures@heap@pairing_heap:heap(any())) -> boolean().
is_empty(Queue) ->
    case gleamy_structures@heap@pairing_heap:find_min(Queue) of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-spec new(fun((EZX, EZX) -> gleam@order:order())) -> gleamy_structures@heap@pairing_heap:heap(EZX).
new(Compare) ->
    gleamy_structures@heap@pairing_heap:new(Compare).

-spec from_list(list(EZQ), fun((EZQ, EZQ) -> gleam@order:order())) -> gleamy_structures@heap@pairing_heap:heap(EZQ).
from_list(List, Compare) ->
    gleam@list:fold(
        List,
        new(Compare),
        fun gleamy_structures@heap@pairing_heap:insert/2
    ).

-spec pop(gleamy_structures@heap@pairing_heap:heap(EZZ)) -> {ok,
        {EZZ, gleamy_structures@heap@pairing_heap:heap(EZZ)}} |
    {error, nil}.
pop(Queue) ->
    gleamy_structures@heap@pairing_heap:delete_min(Queue).

-spec peek(gleamy_structures@heap@pairing_heap:heap(FAE)) -> {ok, FAE} |
    {error, nil}.
peek(Queue) ->
    gleamy_structures@heap@pairing_heap:find_min(Queue).

-spec push(gleamy_structures@heap@pairing_heap:heap(FAI), FAI) -> gleamy_structures@heap@pairing_heap:heap(FAI).
push(Queue, Item) ->
    gleamy_structures@heap@pairing_heap:insert(Queue, Item).

-spec count(gleamy_structures@heap@pairing_heap:heap(any())) -> integer().
count(Queue) ->
    case gleamy_structures@heap@pairing_heap:delete_min(Queue) of
        {ok, {_, Q}} ->
            count(Q) + 1;

        {error, _} ->
            0
    end.

-spec reorder(
    gleamy_structures@heap@pairing_heap:heap(FAL),
    fun((FAL, FAL) -> gleam@order:order())
) -> gleamy_structures@heap@pairing_heap:heap(FAL).
reorder(Queue, Compare) ->
    case gleamy_structures@heap@pairing_heap:delete_min(Queue) of
        {ok, {X, Q}} ->
            gleamy_structures@heap@pairing_heap:insert(reorder(Q, Compare), X);

        {error, _} ->
            gleamy_structures@heap@pairing_heap:new(Compare)
    end.

-spec to_list(gleamy_structures@heap@pairing_heap:heap(FAO)) -> list(FAO).
to_list(Queue) ->
    case gleamy_structures@heap@pairing_heap:delete_min(Queue) of
        {ok, {X, Q}} ->
            [X | to_list(Q)];

        {error, _} ->
            []
    end.
