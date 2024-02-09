-module(gleam@queue).
-compile([no_auto_import, nowarn_unused_vars]).

-export([new/0, from_list/1, to_list/1, is_empty/1, length/1, push_back/2, push_front/2, reverse/1, pop_back/1, pop_front/1, is_logically_equal/3, is_equal/2]).
-export_type([queue/1]).

-opaque queue(ECR) :: {queue, list(ECR), list(ECR)}.

-spec new() -> queue(any()).
new() ->
    {queue, [], []}.

-spec from_list(list(ECU)) -> queue(ECU).
from_list(List) ->
    {queue, [], List}.

-spec to_list(queue(ECX)) -> list(ECX).
to_list(Queue) ->
    _pipe = erlang:element(3, Queue),
    gleam@list:append(_pipe, gleam@list:reverse(erlang:element(2, Queue))).

-spec is_empty(queue(any())) -> boolean().
is_empty(Queue) ->
    (erlang:element(2, Queue) =:= []) andalso (erlang:element(3, Queue) =:= []).

-spec length(queue(any())) -> integer().
length(Queue) ->
    gleam@list:length(erlang:element(2, Queue)) + gleam@list:length(
        erlang:element(3, Queue)
    ).

-spec push_back(queue(EDE), EDE) -> queue(EDE).
push_back(Queue, Item) ->
    {queue, [Item | erlang:element(2, Queue)], erlang:element(3, Queue)}.

-spec push_front(queue(EDH), EDH) -> queue(EDH).
push_front(Queue, Item) ->
    {queue, erlang:element(2, Queue), [Item | erlang:element(3, Queue)]}.

-spec reverse(queue(EDU)) -> queue(EDU).
reverse(Queue) ->
    {queue, erlang:element(3, Queue), erlang:element(2, Queue)}.

-spec pop_back(queue(EDK)) -> {ok, {EDK, queue(EDK)}} | {error, nil}.
pop_back(Queue) ->
    case Queue of
        {queue, [], []} ->
            {error, nil};

        {queue, [], Out} ->
            pop_back({queue, gleam@list:reverse(Out), []});

        {queue, [First | Rest], Out@1} ->
            Queue@1 = {queue, Rest, Out@1},
            {ok, {First, Queue@1}}
    end.

-spec pop_front(queue(EDP)) -> {ok, {EDP, queue(EDP)}} | {error, nil}.
pop_front(Queue) ->
    case Queue of
        {queue, [], []} ->
            {error, nil};

        {queue, In, []} ->
            pop_front({queue, [], gleam@list:reverse(In)});

        {queue, In@1, [First | Rest]} ->
            Queue@1 = {queue, In@1, Rest},
            {ok, {First, Queue@1}}
    end.

-spec check_equal(
    list(EDX),
    list(EDX),
    list(EDX),
    list(EDX),
    fun((EDX, EDX) -> boolean())
) -> boolean().
check_equal(Xs, X_tail, Ys, Y_tail, Eq) ->
    case {Xs, X_tail, Ys, Y_tail} of
        {[], [], [], []} ->
            true;

        {[X | Xs@1], _, [Y | Ys@1], _} ->
            case Eq(X, Y) of
                false ->
                    false;

                true ->
                    check_equal(Xs@1, X_tail, Ys@1, Y_tail, Eq)
            end;

        {[], [_ | _], _, _} ->
            check_equal(gleam@list:reverse(X_tail), [], Ys, Y_tail, Eq);

        {_, _, [], [_ | _]} ->
            check_equal(Xs, X_tail, gleam@list:reverse(Y_tail), [], Eq);

        {_, _, _, _} ->
            false
    end.

-spec is_logically_equal(queue(EEC), queue(EEC), fun((EEC, EEC) -> boolean())) -> boolean().
is_logically_equal(A, B, Element_is_equal) ->
    check_equal(
        erlang:element(3, A),
        erlang:element(2, A),
        erlang:element(3, B),
        erlang:element(2, B),
        Element_is_equal
    ).

-spec is_equal(queue(EEF), queue(EEF)) -> boolean().
is_equal(A, B) ->
    check_equal(
        erlang:element(3, A),
        erlang:element(2, A),
        erlang:element(3, B),
        erlang:element(2, B),
        fun(A@1, B@1) -> A@1 =:= B@1 end
    ).
