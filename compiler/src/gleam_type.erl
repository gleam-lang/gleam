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
  | {recursive_row_type, type(), type()}
  | {not_a_row, type(), env()}
  | {row_does_not_contain_label, type(), env()}
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

-spec infer(ast_expression(), env()) -> {ast_expression(), env()}.
infer(Ast, Env0) ->
  case Ast of
    #ast_module{statements = Statements} ->
      {NewStatements, {Row, Env1}} = gleam:thread_map(fun module_statement/2,
                                                      Statements,
                                                      {#type_row_empty{}, Env0}),
      ModuleType = #type_module{row = Row},
      AnnotatedAst = Ast#ast_module{type = {ok, ModuleType}, statements = NewStatements},
      {AnnotatedAst, Env1};

    #ast_seq{first = First, then = Then} ->
      {AnnotatedFirst, Env1} = infer(First, Env0),
      {AnnotatedThen, Env2} = infer(Then, Env1),
      AnnotatedAst = Ast#ast_seq{first = AnnotatedFirst, then = AnnotatedThen},
      {AnnotatedAst, Env2};

    #ast_operator{name = Name, args = Args} ->
      {ReturnType, Env1} = infer_call(#ast_var{name = Name}, Args, Env0),
      AnnotatedAst = Ast#ast_operator{type = {ok, ReturnType}},
      {AnnotatedAst, Env1};

    #ast_local_call{fn = Fn, args = Args} ->
      {ReturnType, Env1} = infer_call(Fn, Args, Env0),
      AnnotatedAst = Ast#ast_local_call{type = {ok, ReturnType}},
      {AnnotatedAst, Env1};

    #ast_fn{args = Args, body = Body} ->
      {ArgTypes, ArgsEnv} = gleam:thread_map(fun(_, E) -> new_var(E) end, Args, Env0),
      Insert =
        fun({Name, Type}, E) ->
          env_extend(Name, Type, E)
        end,
      FnEnv = lists:foldl(Insert, ArgsEnv, lists:zip(Args, ArgTypes)),
      {ReturnAst, ReturnEnv} = infer(Body, FnEnv),
      ReturnType = fetch(ReturnAst),
      Type = #type_fn{args = ArgTypes, return = ReturnType},
      AnnotatedAst = Ast#ast_fn{type = {ok, Type}},
      FinalEnv = Env0#env{type_refs = ReturnEnv#env.type_refs},
      {AnnotatedAst, FinalEnv};

    #ast_var{name = Name} ->
      case env_lookup(Name, Env0) of
        {ok, Type} ->
          {InstantiatedType, NewEnv} = instantiate(Type, Env0),
          AnnotatedAst = Ast#ast_var{type = {ok, InstantiatedType}},
          {AnnotatedAst, NewEnv};

        error ->
          fail({var_not_found, Ast})
      end;

    #ast_assignment{name = Name, value = Value, then = Then} ->
      {InferredValue, Env2} = infer(Value, increment_env_level(Env0)),
      Env3 = decrement_env_level(Env2),
      {GeneralizedType, Env4} = generalize(fetch(InferredValue), Env3),
      ExtendedEnv = env_extend(Name, GeneralizedType, Env4),
      infer(Then, ExtendedEnv);

    #ast_tuple{elems = Elems} ->
      {AnnotatedElems, NewEnv} = gleam:thread_map(fun infer/2, Elems, Env0),
      AnnotatedAst = Ast#ast_tuple{elems = AnnotatedElems},
      {AnnotatedAst, NewEnv};

    #ast_cons{head = Head, tail = Tail} ->
      {AnnotatedTail, Env1} = infer(Tail, Env0),
      {AnnotatedHead, Env2} = infer(Head, Env1),
      TailType = fetch(AnnotatedTail),
      HeadType = fetch(AnnotatedHead),
      Env3 = unify(TailType, #type_app{type = "List", args = [HeadType]}, Env2),
      AnnotatedAst = Ast#ast_cons{head = AnnotatedHead, tail = AnnotatedTail},
      {AnnotatedAst, Env3};

    #ast_nil{} ->
      {Var, NewEnv} = new_var(Env0),
      Type = #type_app{type = "List", args = [Var]},
      AnnotatedAst = Ast#ast_nil{type = {ok, Type}},
      {AnnotatedAst, NewEnv};

    #ast_record_extend{parent = Parent, label = Label, value = Value} ->
      {RestRowType, Env1} = new_var(Env0),
      {FieldType, Env2} = new_var(Env1),
      ParentType = #type_record{row = RestRowType},
      {AnnotatedValue, Env3} = infer(Value, Env2),
      {AnnotatedParent, Env4} = infer(Parent, Env3),
      TypeOfValue = fetch(AnnotatedValue),
      TypeOfParent = fetch(AnnotatedParent),
      Env5 = unify(FieldType, TypeOfValue, Env4),
      Env6 = unify(ParentType, TypeOfParent, Env5),
      Type = #type_record{row = #type_row_extend{label = Label,
                                                 type = FieldType,
                                                 parent = RestRowType}},
      AnnotatedAst = Ast#ast_record_extend{type = {ok, Type},
                                           parent = AnnotatedParent,
                                           value = AnnotatedValue},
      {AnnotatedAst, Env6};

    #ast_record_select{record = Record, label = Label} ->
      {RowParentType, Env1} = new_var(Env0),
      {FieldType, Env2} = new_var(Env1),
      RowType = #type_row_extend{label = Label, type = FieldType, parent = RowParentType},
      ParamType = #type_record{row = RowType},
      {AnnotatedRecord, Env3} = infer(Record, Env2),
      RecordType = fetch(AnnotatedRecord),
      Env4 = unify(ParamType, RecordType, Env3),
      AnnotatedAst = Ast#ast_record_select{type = {ok, FieldType}, record = AnnotatedRecord},
      {AnnotatedAst, Env4};

    #ast_raise{} ->
      {Var, Env1} = new_var(Env0),
      AnnotatedAst = #ast_raise{type = {ok, Var}},
      {AnnotatedAst, Env1};

    #ast_throw{} ->
      {Var, Env1} = new_var(Env0),
      AnnotatedAst = #ast_throw{type = {ok, Var}},
      {AnnotatedAst, Env1};

    #ast_record_empty{} ->
      {Ast, Env0};

    #ast_int{} ->
      {Ast, Env0};

    #ast_float{} ->
      {Ast, Env0};

    #ast_string{} ->
      {Ast, Env0};

    #ast_atom{} ->
      {Ast, Env0}
  end.


-spec module_statement(ast_expression(), {type(), env()}) -> {ast_expression(), {type(), env()}}.
module_statement(Statement, {Row, Env0}) ->
  case Statement of
    #ast_mod_fn{name = Name, args = Args, body = Body} ->
      Fn = #ast_fn{args = Args, body = Body},
      {AnnotatedFn, Env1} = infer(Fn, Env0),
      FnType = fetch(AnnotatedFn),
      NewRow = #type_row_extend{label = Name, type = FnType, parent = Row},
      NewState = {NewRow, Env1},
      {AnnotatedFn, NewState}
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


-spec fetch(ast_expression()) -> type().
fetch(Ast) ->
  case Ast of
    #ast_operator{type = {ok, Type}} ->
      Type;

    #ast_local_call{type = {ok, Type}} ->
      Type;

    #ast_fn{type = {ok, Type}} ->
      Type;

    #ast_var{type = {ok, Type}} ->
      Type;

    #ast_nil{type = {ok, Type}} ->
      Type;

    #ast_module{type = {ok, Type}} ->
      Type;

    #ast_record_extend{type = {ok, Type}} ->
      Type;

    #ast_record_select{type = {ok, Type}} ->
      Type;

    #ast_raise{type = {ok, Type}} ->
      Type;

    #ast_throw{type = {ok, Type}} ->
      Type;

    #ast_seq{then = Then} ->
      fetch(Then);

    #ast_record_empty{} ->
      #type_record{row = #type_row_empty{}};

    #ast_tuple{elems = Elems} ->
      ElemsTypes = lists:map(fun fetch/1, Elems),
      #type_app{type = "Tuple", args = ElemsTypes};

    #ast_cons{head = Head} ->
      #type_app{type = "List", args = [fetch(Head)]};

    #ast_int{} ->
      #type_const{type = "Int"};

    #ast_atom{} ->
      #type_const{type = "Atom"};

    #ast_float{} ->
      #type_const{type = "Float"};

    #ast_string{} ->
      #type_const{type = "String"}
  end.


-spec resolve_type_vars(ast_expression(), env()) -> ast_expression().
resolve_type_vars(Ast, Env) ->
  case Ast of
    #ast_module{type = {ok, Type}, statements = Statements} ->
      NewType = type_resolve_type_vars(Type, Env),
      NewStatements = lists:map(fun(X) -> resolve_type_vars(X, Env) end, Statements),
      Ast#ast_module{type = {ok, NewType}, statements = NewStatements};

    #ast_seq{first = First, then = Then} ->
      Ast#ast_seq{first = resolve_type_vars(First, Env),
                  then = resolve_type_vars(Then, Env)};

    #ast_operator{type = {ok, Type}} ->
      NewType = type_resolve_type_vars(Type, Env),
      Ast#ast_operator{type = {ok, NewType}};

    #ast_local_call{type = {ok, Type}} ->
      NewType = type_resolve_type_vars(Type, Env),
      Ast#ast_local_call{type = {ok, NewType}};

    % TODO: resolve type vars in args and body?
    #ast_fn{type = {ok, Type}} ->
      NewType = type_resolve_type_vars(Type, Env),
      Ast#ast_fn{type = {ok, NewType}};

    #ast_var{type = {ok, Type}} ->
      NewType = type_resolve_type_vars(Type, Env),
      Ast#ast_var{type = {ok, NewType}};

    #ast_tuple{elems = Elems} ->
      NewElems = lists:map(fun(X) -> resolve_type_vars(X, Env) end, Elems),
      Ast#ast_tuple{elems = NewElems};

    #ast_cons{head = Head, tail = Tail} ->
      NewHead = resolve_type_vars(Head, Env),
      NewTail = resolve_type_vars(Tail, Env),
      Ast#ast_cons{head = NewHead, tail = NewTail};

    #ast_nil{type = {ok, Type}} ->
      NewType = type_resolve_type_vars(Type, Env),
      Ast#ast_nil{type = {ok, NewType}};

    #ast_record_extend{type = {ok, Type}, parent = Parent, value = Value} ->
      NewType = type_resolve_type_vars(Type, Env),
      NewParent = resolve_type_vars(Parent, Env),
      NewValue = resolve_type_vars(Value, Env),
      Ast#ast_record_extend{type = {ok, NewType},
                            parent = NewParent,
                            value = NewValue};

    #ast_record_select{type = {ok, Type}, record = Record} ->
      NewType = type_resolve_type_vars(Type, Env),
      NewRecord = resolve_type_vars(Record, Env),
      Ast#ast_record_select{type = {ok, NewType}, record = NewRecord};

    #ast_throw{} ->
      Ast;

    #ast_raise{} ->
      Ast;

    #ast_record_empty{} ->
      Ast;

    #ast_int{} ->
      Ast;

    #ast_atom{} ->
      Ast;

    #ast_float{} ->
      Ast;

    #ast_string{} ->
      Ast
  end.


% TODO: We don't seem to be actually resolving type vars in the statements
% themselves yet. This seems like a problem, but I've not found any tests that
% fail yet.
% -spec statement_resolve_type_vars(ast_expression(), env()) -> ast_expression().
% statement_resolve_type_vars(Statement, Env) ->
%   case Statement of
%     % TODO: resolve type vars in args and body?
%     #ast_mod_fn{type = {ok, Type}} ->
%       NewType = type_resolve_type_vars(Type, Env),
%       Statement#ast_mod_fn{type = {ok, NewType}}
%   end.


-spec type_resolve_type_vars(type(), env()) -> type().
type_resolve_type_vars(Type, Env) ->
  case Type of
    #type_const{} ->
      Type;

    #type_row_empty{} ->
      Type;

    #type_record{row = Row} ->
      Type#type_record{row = type_resolve_type_vars(Row, Env)};

    #type_module{row = Row} ->
      % TODO: We don't seem to be actually resolving type vars in the
      % statements themselves yet. This seems like a problem, but I've not
      % found any tests that fail yet.
      Type#type_module{row = type_resolve_type_vars(Row, Env)};

    #type_row_extend{parent = Parent, type = Value} ->
      Type#type_row_extend{parent = type_resolve_type_vars(Parent, Env),
                           type = type_resolve_type_vars(Value, Env)};

    #type_app{args = Args} ->
      NewArgs = lists:map(fun(X) -> type_resolve_type_vars(X, Env) end, Args),
      Type#type_app{args = NewArgs};

    #type_fn{args = Args, return = Return} ->
      NewArgs = lists:map(fun(X) -> type_resolve_type_vars(X, Env) end, Args),
      NewReturn = type_resolve_type_vars(Return, Env),
      Type#type_fn{args = NewArgs, return = NewReturn};

    #type_var{type = Ref} ->
      case env_lookup_type_ref(Ref, Env) of
        #type_var_unbound{id = Id} ->
          #type_var{type = Id};

        #type_var_link{type = InnerType} ->
          type_resolve_type_vars(InnerType, Env)
      end
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
generalize(Type, Env0) ->
  case Type of
    #type_const{} ->
      {Type, Env0};

    #type_app{args = Args} ->
      {GeneralizedArgs, Env1} = gleam:thread_map(fun generalize/2, Args, Env0),
      GeneralizedType = Type#type_app{args = GeneralizedArgs},
      {GeneralizedType, Env1};

    #type_fn{args = Args, return = Return} ->
      {GeneralizedArgs, Env1} = gleam:thread_map(fun generalize/2, Args, Env0),
      {GeneralizedReturn, Env2} = generalize(Return, Env1),
      GeneralizedType = Type#type_fn{args = GeneralizedArgs, return = GeneralizedReturn},
      {GeneralizedType, Env2};

    #type_row_extend{parent = Parent, type = FieldType} ->
      {GeneralizedFieldType, Env1} = generalize(FieldType, Env0),
      {GeneralizedParent, Env2} = generalize(Parent, Env1),
      GeneralizedType = Type#type_row_extend{parent = GeneralizedParent,
                                             type = GeneralizedFieldType},
      {GeneralizedType, Env2};

    #type_row_empty{} ->
      {Type, Env0};

    #type_record{row = Row} ->
      {GeneralizedRow, Env1} = generalize(Row, Env0),
      GeneralizedType = Type#type_record{row = GeneralizedRow},
      {GeneralizedType, Env1};

    #type_var{type = Ref} ->
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
      end
  end.

-spec instantiate(type(), env()) -> {type(), env()}.
instantiate(Type, Env) ->
  {NewType, {NewEnv, _IdVarMap}} = do_instantiate(Type, {Env, #{}}),
  {NewType, NewEnv}.

-spec do_instantiate(type(), {env(), map()}) -> {type(), {env(), map()}}.
do_instantiate(Type, State0) ->
  case Type of
    #type_const{} ->
      {Type, State0};

    #type_record{row = Row} ->
      {NewRow, NewState} = do_instantiate(Row, State0),
      NewType = Type#type_record{row = NewRow},
      {NewType, NewState};

    #type_row_extend{parent = Parent, type = FieldType} ->
      {NewParent, State1} = do_instantiate(Parent, State0),
      {NewFieldType, State2} = do_instantiate(FieldType, State1),
      NewType = Type#type_row_extend{parent = NewParent, type = NewFieldType},
      {NewType, State2};

    #type_row_empty{} ->
      {Type, State0};

    #type_app{args = Args} ->
      {NewArgs, State1} = gleam:thread_map(fun do_instantiate/2, Args, State0),
      NewType = Type#type_app{args = NewArgs},
      {NewType, State1};

    #type_fn{args = Args, return = Return} ->
      {NewArgs, State1} = gleam:thread_map(fun do_instantiate/2, Args, State0),
      {NewReturn, State2} = do_instantiate(Return, State1),
      NewType = #type_fn{args = NewArgs, return = NewReturn},
      {NewType, State2};

    #type_var{type = Ref} ->
      {Env, IdVarMap} = State0,
      case env_lookup_type_ref(Ref, Env) of
        #type_var_link{type = LinkedType} ->
          do_instantiate(LinkedType, State0);

        #type_var_generic{id = Id} ->
          case maps:find(Id, IdVarMap) of
            {ok, FoundType} ->
              {FoundType, State0};

            error ->
              {Var, Env1} = new_var(Env),
              IdVarMap1 = maps:put(Id, Var, IdVarMap),
              {Var, {Env1, IdVarMap1}}
          end;

        #type_var_unbound{} ->
          {Type, State0}
      end
  end.


-spec occurs_check_adjust_levels(id(), type(), env()) -> env().
occurs_check_adjust_levels(Id, Type, Env) ->
  case Type of
    #type_var{type = Ref} ->
      Level = env_level(Env),
      case env_lookup_type_ref(Ref, Env) of
        #type_var_link{type = InnerType} ->
          occurs_check_adjust_levels(Id, InnerType, Env);

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

    #type_app{args = Args} ->
      Check = fun(Arg, E) ->
        occurs_check_adjust_levels(Id, Arg, E)
      end,
      lists:foldl(Check, Env, Args);

    #type_fn{args = Args, return = Return} ->
      Check = fun(Arg, E) ->
        occurs_check_adjust_levels(Id, Arg, E)
      end,
      Env1 = lists:foldl(Check, Env, Args),
      occurs_check_adjust_levels(Id, Return, Env1);

    #type_record{row = Row} ->
      occurs_check_adjust_levels(Id, Row, Env);

    #type_row_extend{parent = Parent, type = InnerType} ->
      Env1 = occurs_check_adjust_levels(Id, InnerType, Env),
      occurs_check_adjust_levels(Id, Parent, Env1);

    #type_row_empty{} ->
      Env;

    #type_const{} ->
      Env
  end.


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

    {#type_record{row = Row1}, _,
     #type_record{row = Row2}, _} ->
      unify(Row1, Row2, Env);

    % | TRowExtend(label1, field_ty1, rest_row1), (TRowExtend _ as row2) -> begin
    % 	let rest_row1_tvar_ref_option = match rest_row1 with
    % 		| TVar ({contents = Unbound _} as tvar_ref) -> Some tvar_ref
    % 		| _ -> None
    % 	in
    % 	let rest_row2 = rewrite_row row2 label1 field_ty1 in
    % 	begin match rest_row1_tvar_ref_option with
    % 		| Some {contents = Link _} -> error "recursive row types"
    % 		| _ -> ()
    % 	end ;
    % 	unify rest_row1 rest_row2
    % end
    {#type_row_extend{label = Label, type = FieldType, parent = Parent} = Row,
     _,
     OtherRow,
     _} ->
      VarType1 = tvar_value(Parent, Env),
      {OtherParent, Env1} = rewrite_row(OtherRow, Label, FieldType, Env),
      VarType2 = tvar_value(Parent, Env1),
      case {VarType1, VarType2} of
        {{ok, #type_var_unbound{}}, {ok, #type_var_link{}}} ->
          fail({recursive_row_type, Row, OtherRow});

        _ ->
          unify(Parent, OtherParent, Env1)
      end;

    Other ->
      fail({cannot_unify, Other})
  end.


-spec rewrite_row(type(), string(), type(), env()) -> {type(), env()}.
rewrite_row(OtherRow, Label, Field, Env0) ->
  case {OtherRow, tvar_value(OtherRow, Env0)} of
    {#type_row_empty{}, _} ->
      fail({row_does_not_contain_label, Label});

    {#type_row_extend{label = OtherLabel, type = OtherField, parent = OtherParent}, _}
      when Label =:= OtherLabel ->
      Env1 = unify(Field, OtherField, Env0),
      {OtherParent, Env1};

    {#type_row_extend{label = OtherLabel, type = OtherField, parent = OtherParent}, _} ->
      {NewOtherParent, Env1} = rewrite_row(OtherParent, Label, Field, Env0),
      NewOtherRow = #type_row_extend{label = OtherLabel,
                                     type = OtherField,
                                     parent = NewOtherParent},
      {NewOtherRow, Env1};

    {_, {ok, #type_var_link{type = LinkedType}}} ->
      rewrite_row(LinkedType, Label, Field, Env0);

    {_, {ok, #type_var_unbound{}}} ->
      error(not_implemented);

    _ ->
      fail({not_a_row, OtherRow, Env0})
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

match_fun_type(Arity, Type, Env) ->
  case Type of
    #type_fn{args = Args, return = Return} ->
      case Arity =:= length(Args) of
        true -> {Args, Return, Env};
        false -> fail({incorrect_number_of_arguments, Type})
      end;

    _ ->
      fail({not_a_function, Type})
  end.

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
      (_, #type_row_empty{}) ->
        "";

      (F, #type_record{row = Row}) ->
        FieldsString =
          fun
            (_, [{Label, ValueType}]) ->
              Label ++ " = " ++ F(F, ValueType);

            (FS, [{Label, ValueType} | Rest]) ->
              Label ++ " = " ++ F(F, ValueType) ++ ", " ++ FS(FS, Rest);

            (_, []) ->
              ""
          end,
        case collect_row_fields(Row) of
          {Parent, []} ->
            "{ " ++ F(F, Parent) ++ " }";

          {Parent, Fields} ->
            "{" ++ F(F, Parent) ++ " | " ++ FieldsString(FieldsString, Fields) ++ "}";

          Fields ->
            "{" ++ FieldsString(FieldsString, Fields) ++ "}"
        end;

      (F, #type_module{row = Row}) ->
        FieldsString =
          fun
            (FS, [{Label, ValueType} | Rest]) ->
              case F(F, ValueType) of
                [$f, $n | FnString] ->
                  " fn " ++ Label ++ FnString ++ FS(FS, Rest)
              end;

            (_, []) ->
              ""
          end,
        case collect_row_fields(Row) of
          {Parent, []} ->
            "module { " ++ F(F, Parent) ++ " }";

          {Parent, Fields} ->
            "module {"
            ++ F(F, Parent)
            ++ " | "
            ++ FieldsString(FieldsString, lists:reverse(Fields))
            ++ "}";

          Fields ->
            "module {"
            ++ FieldsString(FieldsString, lists:reverse(Fields))
            ++ "}"
        end;

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
        ++ ") => "
        ++ F(F, ReturnType);

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

collect_row_fields(Row) ->
  case collect_row_fields(Row, []) of
    {Parent, Fields} ->
      {Parent, lists:sort(Fields)};

    Fields ->
      lists:sort(Fields)
  end.

collect_row_fields(#type_row_empty{}, Fields) ->
  Fields;
collect_row_fields(#type_row_extend{parent = Parent, label = Label, type = Type}, Fields) ->
  NewFields = [{Label, Type} | Fields],
  collect_row_fields(Parent, NewFields);
collect_row_fields(Other, Fields) ->
  {Other, Fields}.
