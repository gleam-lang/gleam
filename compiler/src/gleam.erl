-module(gleam).

-export([thread_map/3]).

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
