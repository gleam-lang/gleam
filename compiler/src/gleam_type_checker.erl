-module(gleam_type_checker).

%%%% 31.19

%% -export([check/1]).

%-ifdef(TEST).
%-include_lib("eunit/include/eunit.hrl").
%-endif.

%-include("gleam_records.hrl").

%-record(env, {uid = 0}).
%-record(var, {name = undefined, instance = undefined}).
%-record(op, {name = undefined, args = []}).

%% Type constructors

%int() ->
%  #op{name = int}.

%float() ->
%  #op{name = float}.

%% tuple(Elements) when is_list(Elements) ->
%%   #op{name = bool, args = Elements}.

%% function(From, To) when is_list(From) ->
%%   #op{name = function, args = [From, To]}.

%% A fresh variable is one with a unique name to distinguish it.
%%
%fresh_var(#env{uid = UID} = Env) ->
%  Var = #var{name = UID},
%  NewEnv = Env#env{uid = UID + 1},
%  {Var, NewEnv}.

%% Unification

%% Resolve a var to a concrete type, if one is known.
%%
%prune(#var{instance = Instance}) when Instance /= undefined ->
%  prune(Instance);
%prune(Type) ->
%  Type.

%% Determine if two types are the same
%%
%unify(Type1, Type2) ->
%  T1 = prune(Type1),
%  T2 = prune(Type2),

%  case {T1, T2} of
%    {#var{}, _} ->
%      case same_type(T1, T2) of
%        % The type variable T1 is the same type as T2, so
%        % set T2 as the instance of T1 to show this.
%        true ->
%          {ok, T1, T2};

%        false ->
%          case occurs_in_type(T1, T2) of
%            true -> {error, {recursive_unification, T1, T2}};
%            false -> {ok, T1#var{instance = T2}, T2}
%          end
%      end;

%    {_, #var{}} ->
%      case unify(T2, T1) of
%        {ok, _, _} = Tuple -> flip_args(Tuple);
%        {error, Tuple} -> flip_args(Tuple)
%      end;

%    {#op{name = N, args = A1}, #op{name = N, args = A2}} ->
%      case unify_op_args(A1, A2, [], []) of
%        {ok, NewA1, NewA2} ->
%          {ok, T1#op{args = NewA1}, T2#op{args = NewA2}};

%        type_mismatch ->
%          {error, {type_mismatch, T1, T2}}
%      end;

%    {#op{}, #op{}} ->
%      {error, {type_mismatch, T1, T2}}
%  end.

%flip_args({X, Y, Z}) ->
%  {X, Z, Y}.

%-ifdef(TEST).
%simple_unify_test() ->
%  Float = float(),
%  Int = int(),
%  ?assertEqual({ok, Int, Int}, unify(Int, Int)),
%  ?assertEqual({ok, Float, Float}, unify(Float, Float)),
%  ?assertEqual({error, {type_mismatch, Float, Int}}, unify(Float, Int)),
%  ?assertEqual({error, {type_mismatch, Int, Float}}, unify(Int, Float)).

%var_unify_test() ->
%  {Var, Env1} = fresh_var(#env{}),
%  Float = float(),
%  ?assertEqual({ok, Var#var{instance = Float}, Float}, unify(Var, Float)),
%  ?assertEqual({ok, Float, Var#var{instance = Float}}, unify(Float, Var)),
%  Int = int(),
%  ?assertEqual({ok, Var#var{instance = Int}, Int}, unify(Var, Int)),
%  ?assertEqual({ok, Int, Var#var{instance = Int}}, unify(Int, Var)),
%  {Var2, _Env2} = fresh_var(Env1),
%  ?assertEqual({ok, Var, Var}, unify(Var, Var)),
%  ?assertEqual({ok, Var2, Var2}, unify(Var2, Var2)),
%  ?assertEqual({ok, Var#var{instance = Var2}, Var2}, unify(Var, Var2)),
%  ?assertEqual({ok, Var2#var{instance = Var}, Var}, unify(Var2, Var)).
%-endif.

%unify_op_args([], [], Acc1, Acc2) ->
%  {ok, lists:reverse(Acc1), lists:reverse(Acc2)};
%unify_op_args([X | Xs], [Y | Ys], Acc1, Acc2) ->
%  case unify(X, Y) of
%    {ok, NewX, NewY} ->
%      unify_op_args(Xs, Ys, [NewX | Acc1], [NewY | Acc2]);

%    Error ->
%      Error
%  end;
%unify_op_args(_, _, _, _) ->
%  type_mismatch.

%occurs_in_type(V, Type) ->
%  T = prune(Type),
%  case same_type(V, T) of
%    true -> true;
%    false ->
%      case T of
%        #op{args = Args} -> lists:any(fun(ArgT) -> occurs_in_type(V, ArgT) end, Args);
%        _ -> false
%      end
%  end.

%same_type(#op{name = N, args = A1}, #op{name = N, args = A2}) ->
%  try lists:zip(A1, A2) of
%    Zipped -> lists:all(fun({X, Y}) -> same_type(X, Y) end, Zipped)
%  catch
%    % Argument lists were of different lengths.
%    {error, function_clause} -> false
%  end;
%same_type(#var{name = N}, #var{name = N}) ->
%  true;
%same_type(_, _) ->
%  false.
