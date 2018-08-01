-module(gleam).

-export([identity/1, thread_map/3]).

%% Return the argument
%%
identity(X) -> X.

%% Both map over a list and reduce over it with some accumulator at
%% the same time. Used for modeling mapping over a list with some
%% additional mutable state.
%%
thread_map(Fun, List, State) ->
  Reducer =
    fun(Item, {AccList, AccState}) ->
      {NewItem, NewState} = Fun(Item, AccState),
      {[NewItem|AccList], NewState}
    end,
  {MappedList, NewState} = lists:foldl(Reducer, {[], State}, List),
  {lists:reverse(MappedList), NewState}.
