-module(gleam_type).

-export([infer/1, fetch/1, type_to_string/1]).

-include("gleam_records.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type var_name() :: string().

-record(env,
        {level = 0 :: level(),
         vars = #{} :: #{var_name() => type()},
         type_refs = #{} :: #{reference() => type_var()}}).

-type env() :: #env{}.

-type error()
  :: {var_not_found, #ast_var{}}
  | {cannot_unify, {type(), type_var() | error, type(), type_var() | error}}
  | {incorrect_number_of_arguments, type()}
  | {not_a_function, type()}
  | recursive_types.

-spec new_var(env()) -> {type(), env()}.
new_var(Env) ->
  Ref = erlang:make_ref(),
  Type = #type_var{type = Ref},
  TypeVar = #type_var_unbound{id = make_ref(), level = env_level(Env)},
  NewEnv = env_put_type_ref(Ref, TypeVar, Env),
  {Type, NewEnv}.

-spec new_generic_var(env()) -> {type(), env()}.
new_generic_var(Env) ->
  Ref = erlang:make_ref(),
  Type = #type_var{type = Ref},
  TypeVar = #type_var_generic{id = make_ref()},
  NewEnv = env_put_type_ref(Ref, TypeVar, Env),
  {Type, NewEnv}.

-spec infer(ast_expression()) -> {ok, ast_expression()} | {error, error()}.
infer(Ast) ->
  try
    {InferredAst, Env} = infer(Ast, new_env()),
    ResolvedAst = resolve_type_vars(InferredAst, Env),
    {ok, ResolvedAst}
  catch
    throw:{gleam_type_error, Error} -> {error, Error}
  end.

infer_call(FunAst, Args, Env0) ->
  {AnnotatedFunAst, Env1} = infer(FunAst, Env0),
  FunType = fetch(AnnotatedFunAst),
  Arity = length(Args),
  {ArgTypes, ReturnType, Env2} = match_fun_type(Arity, FunType, Env1),
  CheckArg =
    fun({ArgType, ArgExpr}, CheckEnv0) ->
      {AnnotatedArgExpr, CheckEnv1} = infer(ArgExpr, CheckEnv0),
      ArgExprType = fetch(AnnotatedArgExpr),
      unify(ArgType, ArgExprType, CheckEnv1)
    end,
  Env3 = lists:foldl(CheckArg, Env2, lists:zip(ArgTypes, Args)),
  {ReturnType, Env3}.

-spec infer(ast_expression(), env()) -> {ast_expression(), env()}.
infer(Ast = #ast_operator{name = Name, args = Args}, Env0) ->
  {ReturnType, Env1} = infer_call(#ast_var{name = Name}, Args, Env0),
  AnnotatedAst = Ast#ast_operator{type = {ok, ReturnType}},
  {AnnotatedAst, Env1};

infer(Ast = #ast_local_call{name = Name, args = Args}, Env0) ->
  {ReturnType, Env1} = infer_call(#ast_var{name = Name}, Args, Env0),
  AnnotatedAst = Ast#ast_local_call{type = {ok, ReturnType}},
  {AnnotatedAst, Env1};

infer(Ast = #ast_fn{args = Args, body = Body}, Env) ->
  {ArgTypes, ArgsEnv} = gleam:thread_map(fun(_, E) -> new_var(E) end, Args, Env),
  Insert =
    fun({Name, Type}, E) ->
      env_extend(Name, Type, E)
    end,
  FnEnv = lists:foldl(Insert, ArgsEnv, lists:zip(Args, ArgTypes)),
  {ReturnAst, ReturnEnv} = infer(Body, FnEnv),
  ReturnType = fetch(ReturnAst),
  Type = #type_fn{args = ArgTypes, return = ReturnType},
  AnnotatedAst = Ast#ast_fn{type = {ok, Type}},
  FinalEnv = Env#env{type_refs = ReturnEnv#env.type_refs},
  {AnnotatedAst, FinalEnv};

infer(Ast = #ast_var{name = Name}, Env) ->
  case env_lookup(Name, Env) of
    {ok, Type} ->
      {InstantiatedType, NewEnv} = instantiate(Type, Env),
      AnnotatedAst = Ast#ast_var{type = {ok, InstantiatedType}},
      {AnnotatedAst, NewEnv};

    error ->
      fail({var_not_found, Ast})
  end;

infer(#ast_assignment{name = Name, value = Value, then = Then}, Env) ->
  {InferredValue, Env2} = infer(Value, increment_env_level(Env)),
  Env3 = decrement_env_level(Env2),
  {GeneralizedType, Env4} = generalize(fetch(InferredValue), Env3),
  ExtendedEnv = env_extend(Name, GeneralizedType, Env4),
  infer(Then, ExtendedEnv);

infer(Ast = #ast_tuple{elems = Elems}, Env) ->
  {AnnotatedElems, NewEnv} = gleam:thread_map(fun infer/2, Elems, Env),
  AnnotatedAst = Ast#ast_tuple{elems = AnnotatedElems},
  {AnnotatedAst, NewEnv};

infer(Ast = #ast_cons{head = Head, tail = Tail}, Env0) ->
  {AnnotatedTail, Env1} = infer(Tail, Env0),
  {AnnotatedHead, Env2} = infer(Head, Env1),
  TailType = fetch(AnnotatedTail),
  HeadType = fetch(AnnotatedHead),
  Env3 = unify(TailType, #type_app{type = "List", args = [HeadType]}, Env2),
  AnnotatedAst = Ast#ast_cons{head = AnnotatedHead, tail = AnnotatedTail},
  {AnnotatedAst, Env3};

infer(Ast = #ast_nil{}, Env) ->
  {Var, NewEnv} = new_var(Env),
  Type = #type_app{type = "List", args = [Var]},
  AnnotatedAst = Ast#ast_nil{type = {ok, Type}},
  {AnnotatedAst, NewEnv};

infer(Ast = #ast_int{}, Env) ->
  {Ast, Env};

infer(Ast = #ast_float{}, Env) ->
  {Ast, Env};

infer(Ast = #ast_string{}, Env) ->
  {Ast, Env};

infer(Ast = #ast_atom{}, Env) ->
  {Ast, Env}.


-spec fetch(ast_expression()) -> type().
fetch(#ast_operator{type = {ok, Type}}) ->
  Type;

fetch(#ast_local_call{type = {ok, Type}}) ->
  Type;

fetch(#ast_fn{type = {ok, Type}}) ->
  Type;

fetch(#ast_var{type = {ok, Type}}) ->
  Type;

fetch(#ast_nil{type = {ok, Type}}) ->
  Type;

fetch(#ast_tuple{elems = Elems}) ->
  ElemsTypes = lists:map(fun fetch/1, Elems),
  #type_app{type = "Tuple", args = ElemsTypes};

fetch(#ast_cons{head = Head}) ->
  #type_app{type = "List", args = [fetch(Head)]};

fetch(#ast_int{}) ->
  #type_const{type = "Int"};

fetch(#ast_atom{}) ->
  #type_const{type = "Atom"};

fetch(#ast_float{}) ->
  #type_const{type = "Float"};

fetch(#ast_string{}) ->
  #type_const{type = "String"};

fetch(Other) ->
  error({unable_to_fetch_type, Other}).


-spec resolve_type_vars(ast_expression(), env()) -> ast_expression().
resolve_type_vars(Ast = #ast_operator{type = {ok, Type}}, Env) ->
  NewType = do_resolve_type_vars(Type, Env),
  Ast#ast_operator{type = {ok, NewType}};

resolve_type_vars(Ast = #ast_local_call{type = {ok, Type}}, Env) ->
  NewType = do_resolve_type_vars(Type, Env),
  Ast#ast_local_call{type = {ok, NewType}};

resolve_type_vars(Ast = #ast_fn{type = {ok, Type}}, Env) ->
  NewType = do_resolve_type_vars(Type, Env),
  Ast#ast_fn{type = {ok, NewType}};

resolve_type_vars(Ast = #ast_var{type = {ok, Type}}, Env) ->
  NewType = do_resolve_type_vars(Type, Env),
  Ast#ast_var{type = {ok, NewType}};

resolve_type_vars(Ast = #ast_tuple{elems = Elems}, Env) ->
  NewElems = lists:map(fun(X) -> resolve_type_vars(X, Env) end, Elems),
  Ast#ast_tuple{elems = NewElems};

resolve_type_vars(Ast = #ast_cons{head = Head, tail = Tail}, Env) ->
  NewHead = resolve_type_vars(Head, Env),
  NewTail = resolve_type_vars(Tail, Env),
  Ast#ast_cons{head = NewHead, tail = NewTail};

resolve_type_vars(Ast = #ast_nil{type = {ok, Type}}, Env) ->
  NewType = do_resolve_type_vars(Type, Env),
  Ast#ast_nil{type = {ok, NewType}};

resolve_type_vars(Ast = #ast_int{}, _) -> Ast;
resolve_type_vars(Ast = #ast_atom{}, _) -> Ast;
resolve_type_vars(Ast = #ast_float{}, _) -> Ast;
resolve_type_vars(Ast = #ast_string{}, _) -> Ast;
resolve_type_vars(Ast, _) -> error({unable_to_resolve_type_vars_for, Ast}).


-spec do_resolve_type_vars(type(), env()) -> type().
do_resolve_type_vars(Type = #type_const{}, _) ->
  Type;

do_resolve_type_vars(Type = #type_app{args = Args}, Env) ->
  NewArgs = lists:map(fun(X) -> do_resolve_type_vars(X, Env) end, Args),
  Type#type_app{args = NewArgs};

do_resolve_type_vars(Type = #type_fn{args = Args, return = Return}, Env) ->
  NewArgs = lists:map(fun(X) -> do_resolve_type_vars(X, Env) end, Args),
  NewReturn = do_resolve_type_vars(Return, Env),
  Type#type_fn{args = NewArgs, return = NewReturn};

do_resolve_type_vars(#type_var{type = Ref}, Env) ->
  case env_lookup_type_ref(Ref, Env) of
    #type_var_unbound{id = Id} ->
      #type_var{type = Id};

    #type_var_link{type = Type} ->
      do_resolve_type_vars(Type, Env)
  end.


-spec new_env() -> env().
new_env() ->
  Int = #type_const{type = "Int"},
  Bool = #type_const{type = "Bool"},
  Float = #type_const{type = "Float"},
  BinOp = fun(A, B, C) -> #type_fn{args = [A, B], return = C} end,
  EndoOp = fun(T) -> BinOp(T, T, T) end,

  E0 = #env{},

  {V1, E1} = new_generic_var(E0),
  Eq = BinOp(V1, V1, Bool),

  {V2, E2} = new_generic_var(E1),
  NEq = BinOp(V2, V2, Bool),

  {PipeIn, E3} = new_generic_var(E2),
  {PipeOut, E4} = new_generic_var(E3),
  PipeFn = #type_fn{args = [PipeIn], return = PipeOut},
  Pipe = BinOp(PipeIn, PipeFn, PipeOut),

  LastE = E4,

  Core = [
    {"+", EndoOp(Int)},
    {"-", EndoOp(Int)},
    {"/", EndoOp(Int)},
    {"*", EndoOp(Int)},
    {"+.", EndoOp(Float)},
    {"-.", EndoOp(Float)},
    {"/.", EndoOp(Float)},
    {"*.", EndoOp(Float)},
    {"==", Eq},
    {"!=", NEq},
    {"|>", Pipe}
  ],
  Insert = fun({Name, Type}, Env) -> env_extend(Name, Type, Env) end,
  lists:foldl(Insert, LastE, Core).

-spec fail(tuple()) -> no_return().
fail(Error) ->
  throw({gleam_type_error, Error}).

-spec increment_env_level(env()) -> env().
increment_env_level(Env = #env{level = Level}) ->
  Env#env{level = Level + 1}.

-spec decrement_env_level(env()) -> env().
decrement_env_level(Env = #env{level = Level}) ->
  Env#env{level = Level - 1}.

-spec env_level(env()) -> level().
env_level(#env{level = Level}) ->
  Level.

-spec put_env_level(level(), env()) -> env().
put_env_level(Level, Env) ->
  Env#env{level = Level}.

-ifdef(TEST).
increment_env_level_test() ->
  Env = new_env(),
  Env2 = Env#env{level = 42},
  Env3 = increment_env_level(Env2),
  ?assertEqual(43, Env3#env.level).
-endif.

-spec env_extend(var_name(), type(), env()) -> env().
env_extend(Name, GeneralizedType, Env = #env{vars = Vars}) ->
  NewVars = maps:put(Name, GeneralizedType, Vars),
  Env#env{vars = NewVars}.

-spec env_lookup(var_name(), env()) -> error | {ok, type()}.
env_lookup(Name, #env{vars = Vars}) ->
  maps:find(Name, Vars).

-spec env_lookup_type_ref(type_var_reference(), env()) -> type_var().
env_lookup_type_ref(Name, #env{type_refs = Refs}) ->
  {ok, TVar} = maps:find(Name, Refs),
  TVar.

-spec env_put_type_ref(type_var_reference(), type_var(), env()) -> env().
env_put_type_ref(Ref, TypeVar, Env = #env{type_refs = Refs}) ->
  NewRefs = maps:put(Ref, TypeVar, Refs),
  Env#env{type_refs = NewRefs}.

-spec generalize(env(), type()) -> {type(), env()}.
generalize(Type = #type_const{}, Env) ->
  {Type, Env};

generalize(Type = #type_app{args = Args}, Env0) ->
  {GeneralizedArgs, Env1} = gleam:thread_map(fun generalize/2, Args, Env0),
  GeneralizedType = Type#type_app{args = GeneralizedArgs},
  {GeneralizedType, Env1};

generalize(Type = #type_fn{args = Args, return = Return}, Env0) ->
  {GeneralizedArgs, Env1} = gleam:thread_map(fun generalize/2, Args, Env0),
  {GeneralizedReturn, Env2} = generalize(Return, Env1),
  GeneralizedType = Type#type_fn{args = GeneralizedArgs, return = GeneralizedReturn},
  {GeneralizedType, Env2};

generalize(Type = #type_var{type = Ref}, Env0) ->
  Level = env_level(Env0),
  case env_lookup_type_ref(Ref, Env0) of
    #type_var_unbound{id = Id, level = OtherLevel} when OtherLevel > Level ->
      GenericType = #type_var_generic{id = Id},
      Env1 = env_put_type_ref(Ref, GenericType, Env0),
      {Type, Env1};

    #type_var_link{type = LinkedType} ->
      generalize(LinkedType, Env0);

    #type_var_unbound{} ->
      {Type, Env0};

    #type_var_generic{} ->
      {Type, Env0}
  end.

-spec instantiate(type(), env()) -> {type(), env()}.
instantiate(Type, Env) ->
  {NewType, {NewEnv, _IdVarMap}} = do_instantiate(Type, {Env, #{}}),
  {NewType, NewEnv}.

-spec do_instantiate(type(), {env(), map()}) -> {type(), {env(), map()}}.
do_instantiate(Type = #type_const{}, State) ->
  {Type, State};

do_instantiate(Type = #type_app{args = Args}, State0) ->
  {NewArgs, State1} = gleam:thread_map(fun do_instantiate/2, Args, State0),
  NewType = Type#type_app{args = NewArgs},
  {NewType, State1};

do_instantiate(#type_fn{args = Args, return = Return}, State0) ->
  {NewArgs, State1} = gleam:thread_map(fun do_instantiate/2, Args, State0),
  {NewReturn, State2} = do_instantiate(Return, State1),
  NewType = #type_fn{args = NewArgs, return = NewReturn},
  {NewType, State2};

do_instantiate(Type = #type_var{type = Ref}, State = {Env, IdVarMap}) ->
  case env_lookup_type_ref(Ref, Env) of
    #type_var_link{type = LinkedType} ->
      do_instantiate(LinkedType, State);

    #type_var_generic{id = Id} ->
      case maps:find(Id, IdVarMap) of
        {ok, FoundType} ->
          {FoundType, State};

        error ->
          {Var, Env1} = new_var(Env),
          IdVarMap1 = maps:put(Id, Var, IdVarMap),
          {Var, {Env1, IdVarMap1}}
      end;

    #type_var_unbound{} ->
      {Type, State}
  end.


-spec occurs_check_adjust_levels(id(), type(), env()) -> env().
occurs_check_adjust_levels(Id, #type_var{type = Ref}, Env) ->
  Level = env_level(Env),
  case env_lookup_type_ref(Ref, Env) of
    #type_var_link{type = Type} ->
      occurs_check_adjust_levels(Id, Type, Env);

    #type_var_unbound{id = OtherId} when Id =:= OtherId ->
      fail(recursive_types);

    V = #type_var_unbound{level = OtherLevel} when OtherLevel > Level ->
      Var = V#type_var_unbound{level = Level},
      env_put_type_ref(Ref, Var, Env);

    #type_var_unbound{} ->
      Env

    % This should never hapen.
    % #type_var_generic{} ->
  end;

occurs_check_adjust_levels(Id, #type_app{args = Args}, Env0) ->
  Check = fun(Arg, E) ->
    occurs_check_adjust_levels(Id, Arg, E)
  end,
  lists:foldl(Check, Env0, Args);

occurs_check_adjust_levels(Id, #type_fn{args = Args, return = Return}, Env0) ->
  Check = fun(Arg, E) ->
    occurs_check_adjust_levels(Id, Arg, E)
  end,
  Env1 = lists:foldl(Check, Env0, Args),
  occurs_check_adjust_levels(Id, Return, Env1);

occurs_check_adjust_levels(_Id, #type_const{}, Env) ->
  Env.


-spec unify(type(), type(), env()) -> env().
unify(Same, Same, Env) ->
  Env;
unify(Type1, Type2, Env) ->
  case {Type1, tvar_value(Type1, Env), Type2, tvar_value(Type2, Env)} of
    {#type_app{type = Same, args = Args1}, _,
     #type_app{type = Same, args = Args2}, _} when length(Args1) =:= length(Args2) ->
      Check = fun({X, Y}, E) -> unify(X, Y, E) end,
      lists:foldl(Check, Env, lists:zip(Args1, Args2));

    {#type_fn{args = Args1, return = Return1}, _,
     #type_fn{args = Args2, return = Return2}, _} ->
      Check = fun({X, Y}, E) -> unify(X, Y, E) end,
      NewEnv = lists:foldl(Check, Env, lists:zip(Args1, Args2)),
      unify(Return1, Return2, NewEnv);

    {#type_var{}, {ok, #type_var_link{type = LinkedType}},
     Type, _} ->
      unify(LinkedType, Type, Env);

    {Type, _,
     #type_var{}, {ok, #type_var_link{type = LinkedType}}} ->
      unify(Type, LinkedType, Env);

    {_, {ok, #type_var_unbound{id = Same}},
     _, {ok, #type_var_unbound{id = Same}}} ->
      error(should_only_be_one_instance_of_a_particular_type_variable);

    {#type_var{type = Ref}, {ok, #type_var_unbound{id = Id}},
     Type, _} ->
      Env1 = occurs_check_adjust_levels(Id, Type, Env),
      env_put_type_ref(Ref, #type_var_link{type = Type}, Env1);

    {Type, _,
     #type_var{type = Ref}, {ok, #type_var_unbound{id = Id}}} ->
      Env1 = occurs_check_adjust_levels(Id, Type, Env),
      env_put_type_ref(Ref, #type_var_link{type = Type}, Env1);

    Other ->
      fail({cannot_unify, Other})
  end.


-spec tvar_value(type(), env()) -> {ok, type_var()} | error.
tvar_value(#type_var{type = Ref}, Env) ->
  {ok, env_lookup_type_ref(Ref, Env)};
tvar_value(_, _) ->
  error.


-spec match_fun_type(non_neg_integer(), type(), env()) -> {list(type()), type(), env()}.
match_fun_type(Arity, #type_var{type = Ref}, Env) ->
  case env_lookup_type_ref(Ref, Env) of
    #type_var_unbound{level = Level} ->
      PrevLevel = env_level(Env),
      AdjustedEnv = put_env_level(Level, Env),
      Expand = fun
        (_, E, 0) ->
          {[], E};

        (F, E, N) ->
          {Tail, TailEnv} = F(F, E, N - 1),
          {Var, E1} = new_var(TailEnv),
          {[Var | Tail], E1}
      end,
      {ArgsTypes, Env1} = Expand(Expand, AdjustedEnv, Arity),
      {ReturnType, Env2} = new_var(Env1),
      FnType = #type_fn{return = ReturnType, args = ArgsTypes},
      Link = #type_var_link{type = FnType},
      Env3 = env_put_type_ref(Ref, Link, Env2),
      Env4 = put_env_level(PrevLevel, Env3),
      {ArgsTypes, ReturnType, Env4};
    % false -> fail({incorrect_number_of_arguments, Type})

    #type_var_link{type = LinkedType} ->
      match_fun_type(Arity, LinkedType, Env)
  end;

match_fun_type(Arity, Type = #type_fn{args = Args, return = Return}, Env) ->
  case Arity =:= length(Args) of
    true -> {Args, Return, Env};
    false -> fail({incorrect_number_of_arguments, Type})
  end;

match_fun_type(_Arity, Type, _Env) ->
  fail({not_a_function, Type}).

% TODO: Don't use process dictionary
type_to_string(Type) ->
  put(gleam_id_name_map, #{}),
  put(gleam_type_to_string_count, 0),
  NextName =
    fun() ->
      I = get(gleam_type_to_string_count),
      put(gleam_type_to_string_count, I + 1),
      [97 + I rem 26]
    end,
  ToString =
    fun
      (_, #type_const{type = Name}) ->
        Name;

      (F, #type_app{type = AppType, args = Args}) ->
        AppType
        ++ "("
        ++ lists:concat(lists:join(", ", lists:map(fun(X) -> F(F, X) end, Args)))
        ++ ")";

      (F, #type_fn{args = ParamTypeList, return = ReturnType}) ->
        "fn("
        ++ lists:concat(lists:join(", ", lists:map(fun(X) -> F(F, X) end,
                                                    ParamTypeList)))
        ++ ") { "
        ++ F(F, ReturnType)
        ++ " }";

      (_, #type_var{type = Id}) ->
        Names = get(gleam_id_name_map),
        case maps:find(Id, Names) of
          {ok, Name} ->
            Name;

          error ->
            Name = NextName(),
            put(gleam_id_name_map, maps:put(Id, Name, Names)),
            Name
        end
    end,
  String = ToString(ToString, Type),
  put(gleam_id_name_map, undefined),
  put(gleam_type_to_string_count, undefined),
  String.
