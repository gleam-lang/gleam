-module(gleam_type).

-export([annotate/2, fetch/1, type_to_string/1, error_to_iolist/2]).

-include("gleam_records.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(var_data,
        {type :: type(),
         scope :: scope()}).

% TODO: Remove this. We don't need to store the arity any more. We can infer it
% from type_app
-record(type_data,
        {type :: type(),
         arity :: non_neg_integer()}).

-type var_name() :: string().
-type type_name() :: string().

-record(env,
        {uid = 0,
         level = 0 :: level(),
         vars = #{} :: #{var_name() => type()},
         types = #{} :: #{type_name() => type()},
         importables = #{} :: importables(),
         type_refs = #{} :: #{reference() => type_var()}}).

-type env() :: #env{}.
-type line_number() :: non_neg_integer().

-type error()
  :: {var_not_found, line_number(), string()}
  | {type_not_found, line_number(), string(), non_neg_integer()}
  | {module_not_found, line_number(), string()}
  | {cannot_unify, type(), type()}
  | {incorrect_number_of_arguments, line_number(), non_neg_integer(), non_neg_integer()}
  | {not_a_function, line_number(), non_neg_integer(), type()}
  | {recursive_row_type, type(), type()}
  | {not_a_row, type(), env()}
  | {row_does_not_contain_label, type(), env()}
  | {multiple_hole_fn, line_number(), non_neg_integer()}
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


-spec annotate(ast_expression(), #{atom() => type()})
      -> {ok, ast_expression()} | {error, error()}.
annotate(Ast, ImportableVars) ->
  try
    Env0 = register_statements(Ast, new_env(ImportableVars)),
    {InferredAst, Env1} = infer(Ast, Env0),
    ResolvedAst = resolve_type_vars(InferredAst, Env1),
    {ok, ResolvedAst}
  catch
    throw:{gleam_type_error, Error} -> {error, Error}
  end.


% Scan across the statements of a module, registering the top level defined
% functions in the env with a minimal type definition. This allows statements
% to make use of functions defined later in the file without throwing a
% variable-not-found error when inferring.
%
-spec register_statements(ast_expression(), env()) -> env().
register_statements(Ast, Env0) ->
  case Ast of
    #ast_module{statements = Statements} ->
      lists:foldl(fun register_statement/2, Env0, Statements);

    _ ->
      Env0
  end.

-spec register_statement(ast_expression(), env()) -> env().
register_statement(Statement, Env0) ->
  case Statement of
    #ast_mod_fn{name = Name, args = Args} ->
      Env1 = increment_env_level(Env0),
      {ArgTypes, Env2} = lists:mapfoldl(fun(_, E) -> new_var(E) end, Env1, Args),
      {ReturnType, Env3} = new_var(Env2),
      Type = #type_fn{args = ArgTypes, return = ReturnType},
      env_extend(Name, Type, module, Env3);

    _ ->
      Env0
  end.

-spec unify_clauses([#ast_clause{}], type(), env()) -> env().
unify_clauses(Clauses, SubjectType, Env0) ->
  [#ast_clause{pattern = FirstPattern} = First | Rest] = Clauses,
  Env1 = unify(SubjectType, pattern_fetch(FirstPattern), Env0),
  Unify =
    fun(#ast_clause{pattern = P2, value = V2} = Clause2,
        {#ast_clause{pattern = P1, value = V1}, E0}) ->
      E1 = unify(pattern_fetch(P1), pattern_fetch(P2), E0),
      E2 = unify(fetch(V1), fetch(V2), E1),
      {Clause2, E2}
    end,
  {_, Env2} = lists:foldl(Unify, {First, Env1}, Rest),
  Env2.


-spec pattern_fetch(ast_pattern()) -> type().
pattern_fetch(Pattern) ->
  case Pattern of
    #ast_var{type = {ok, Type}} ->
      Type;

    #ast_enum{type = {ok, Type}} ->
      Type;

    #ast_nil{type = {ok, Type}} ->
      Type;

    #ast_hole{type = {ok, Type}} ->
      Type;

    #ast_record_empty{} ->
      #type_record{row = #type_row_empty{}};

    #ast_tuple{elems = Elems} ->
      ElemsTypes = lists:map(fun pattern_fetch/1, Elems),
      #type_app{type = "Tuple", args = ElemsTypes};

    #ast_cons{head = Head} ->
      #type_app{type = "List", args = [pattern_fetch(Head)]};

    #ast_int{} ->
      #type_const{type = "Int"};

    #ast_atom{} ->
      #type_const{type = "Atom"};

    #ast_float{} ->
      #type_const{type = "Float"};

    #ast_string{} ->
      #type_const{type = "String"}
  end.


-spec infer_clause(#ast_clause{}, env()) -> {#ast_clause{}, env()}.
infer_clause(#ast_clause{pattern = Pattern, value = Value} = Clause, Env0) ->
  {AnnotatedPattern, Env1} = infer_pattern(Pattern, Env0),
  {AnnotatedValue, Env2} = infer(Value, Env1),
  NewClause = Clause#ast_clause{pattern = AnnotatedPattern, value = AnnotatedValue},
  {NewClause, Env2}.


infer_enum_pattern(FunAst, Args, Env0) ->
  {AnnotatedFunAst, Env1} = infer(FunAst, Env0),
  FunType = fetch(AnnotatedFunAst),
  Arity = length(Args),
  {ArgTypes, ReturnType, Env2} = match_fun_type(Arity, FunType, line_number(FunAst), Env1),
  CheckArg =
    fun({ArgType, ArgExpr}, CheckEnv0) ->
      {AnnotatedArgExpr, CheckEnv1} = infer_pattern(ArgExpr, CheckEnv0),
      ArgExprType = pattern_fetch(AnnotatedArgExpr),
      unify(ArgType, ArgExprType, CheckEnv1)
    end,
  Env3 = lists:foldl(CheckArg, Env2, lists:zip(ArgTypes, Args)),
  {ReturnType, Env3}.


-spec infer_pattern(ast_pattern(), env()) -> {ast_pattern(), env()}.
infer_pattern(Pattern, Env0) ->
  case Pattern of
    #ast_nil{} ->
      {Var, NewEnv} = new_var(Env0),
      Type = #type_app{type = "List", args = [Var]},
      AnnotatedPattern = Pattern#ast_nil{type = {ok, Type}},
      {AnnotatedPattern, NewEnv};

    #ast_cons{head = Head, tail = Tail} ->
      {AnnotatedTail, Env1} = infer_pattern(Tail, Env0),
      {AnnotatedHead, Env2} = infer_pattern(Head, Env1),
      TailType = pattern_fetch(AnnotatedTail),
      HeadType = pattern_fetch(AnnotatedHead),
      Env3 = unify(TailType, #type_app{type = "List", args = [HeadType]}, Env2),
      AnnotatedPattern = Pattern#ast_cons{head = AnnotatedHead, tail = AnnotatedTail},
      {AnnotatedPattern, Env3};

    #ast_enum{name = Name, elems = Args} ->
      Fn = #ast_var{name = Name, scope = module},
      {ReturnType, Env1} = infer_enum_pattern(Fn, Args, Env0),
      AnnotatedPattern = Pattern#ast_enum{type = {ok, ReturnType}},
      {AnnotatedPattern, Env1};

    #ast_tuple{elems = Elems} ->
      {AnnotatedElems, NewEnv} = lists:mapfoldl(fun infer_pattern/2, Env0, Elems),
      AnnotatedPattern = Pattern#ast_tuple{elems = AnnotatedElems},
      {AnnotatedPattern, NewEnv};

    #ast_var{name = Name, scope = Scope} ->
      {Var, Env1} = new_var(Env0),
      Env2 = env_extend(Name, Var, Scope, Env1),
      AnnotatedPattern = Pattern#ast_var{type = {ok, Var}, scope = Scope},
      {AnnotatedPattern, Env2};

    #ast_hole{} ->
      {Var, Env1} = new_var(Env0),
      AnnotatedPattern = Pattern#ast_hole{type = {ok, Var}},
      {AnnotatedPattern, Env1};

    #ast_int{} ->
      {Pattern, Env0};

    #ast_float{} ->
      {Pattern, Env0};

    #ast_string{} ->
      {Pattern, Env0};

    #ast_atom{} ->
      {Pattern, Env0}
  end.

-spec infer(ast_expression(), env()) -> {ast_expression(), env()}.
infer(Ast, Env0) ->
  case Ast of
    #ast_module{statements = Statements} ->
      {NewStatements, {Row, Env1}} = lists:mapfoldl(fun module_statement/2,
                                                    {#type_row_empty{}, Env0},
                                                    Statements),
      ModuleType = #type_module{row = Row},
      AnnotatedAst = Ast#ast_module{type = {ok, ModuleType}, statements = NewStatements},
      {AnnotatedAst, Env1};

    #ast_case{subject = Subject, clauses = Clauses} ->
      {AnnotatedSubject, Env1} = infer(Subject, Env0),
      {AnnotatedClauses, Env2} = lists:mapfoldl(fun infer_clause/2, Env1, Clauses),
      SubjectType = fetch(AnnotatedSubject),
      Env3 = unify_clauses(AnnotatedClauses, SubjectType, Env2),
      Type = fetch_clause_type(hd(AnnotatedClauses)),
      AnnotatedAst = #ast_case{type = {ok, Type},
                               subject = AnnotatedSubject,
                               clauses = AnnotatedClauses},
      {AnnotatedAst, Env3};

    #ast_seq{first = First, then = Then} ->
      {AnnotatedFirst, Env1} = infer(First, Env0),
      {AnnotatedThen, Env2} = infer(Then, Env1),
      AnnotatedAst = Ast#ast_seq{first = AnnotatedFirst, then = AnnotatedThen},
      {AnnotatedAst, Env2};

    #ast_operator{name = Name, args = Args} ->
      {ReturnType, _, AnnotatedArgs, Env1} = infer_call(#ast_var{name = Name}, Args, Env0),
      AnnotatedAst = Ast#ast_operator{type = {ok, ReturnType}, args = AnnotatedArgs},
      {AnnotatedAst, Env1};

    #ast_call{meta = Meta, fn = Fn, args = Args} ->
      NumHoles = length(lists:filter(fun(#ast_hole{}) -> true; (_) -> false end, Args)),
      case NumHoles of
        0 ->
          {ReturnType, AnnotatedFn, AnnotatedArgs, Env1} = infer_call(Fn, Args, Env0),
          AnnotatedAst = Ast#ast_call{type = {ok, ReturnType},
                                            fn = AnnotatedFn,
                                            args = AnnotatedArgs},
          {AnnotatedAst, Env1};

        1 ->
          {UID, Env1} = uid(Env0),
          VarName = "$$gleam_hole_var" ++ integer_to_list(UID),
          Var = #ast_var{name = VarName, scope = local},
          NewArgs = lists:map(fun(#ast_hole{}) -> Var; (X) -> X end, Args),
          Call = #ast_call{meta = Meta, fn = Fn, args = NewArgs},
          NewFn = #ast_fn{meta = Meta, args = [VarName], body = Call},
          infer(NewFn, Env1);

        _ ->
          fail({multiple_hole_fn, line_number(Ast), NumHoles})
      end;

    #ast_enum{name = Name, elems = Args} ->
      Fn = #ast_var{name = Name, scope = module},
      {ReturnType, _, AnnotatedArgs, Env1} = infer_call(Fn, Args, Env0),
      AnnotatedAst = Ast#ast_enum{type = {ok, ReturnType}, elems = AnnotatedArgs},
      {AnnotatedAst, Env1};

    #ast_fn{args = Args, body = Body} ->
      {ArgTypes, ArgsEnv} = lists:mapfoldl(fun(_, E) -> new_var(E) end, Env0, Args),
      Insert =
        fun({Name, Type}, E) ->
          env_extend(Name, Type, local, E)
        end,
      FnEnv = lists:foldl(Insert, ArgsEnv, lists:zip(Args, ArgTypes)),
      {AnnotatedBody, ReturnEnv} = infer(Body, FnEnv),
      ReturnType = fetch(AnnotatedBody),
      Type = #type_fn{args = ArgTypes, return = ReturnType},
      AnnotatedAst = Ast#ast_fn{type = {ok, Type}, body = AnnotatedBody},
      FinalEnv = Env0#env{type_refs = ReturnEnv#env.type_refs},
      {AnnotatedAst, FinalEnv};

    #ast_var{meta = Meta, name = Name} ->
      case env_lookup(Name, Env0) of
        {ok, #var_data{type = Type, scope = Scope}} ->
          {InstantiatedType, NewEnv} = instantiate(Type, Env0),
          AnnotatedAst = Ast#ast_var{type = {ok, InstantiatedType}, scope = Scope},
          {AnnotatedAst, NewEnv};

        error ->
          fail({var_not_found, Meta#meta.line, Name})
      end;

    #ast_assignment{pattern = Pattern, value = Value, then = Then} ->
      {_ValueType, AnnotatedValue, Env1} = infer_assignment(Pattern, Value, Env0),
      {AnnotatedThen, Env2} = infer(Then, Env1),
      AnnotatedAst = Ast#ast_assignment{value = AnnotatedValue, then = AnnotatedThen},
      {AnnotatedAst, Env2};

    #ast_tuple{elems = Elems} ->
      {AnnotatedElems, NewEnv} = lists:mapfoldl(fun infer/2, Env0, Elems),
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
      RowType = #type_row_extend{label = Label,
                                 type = FieldType,
                                 parent = RowParentType},
      ParamType = #type_record{row = RowType},
      {AnnotatedRecord, Env3} = infer(Record, Env2),
      RecordType = fetch(AnnotatedRecord),
      Env4 = unify(ParamType, RecordType, Env3),
      AnnotatedAst = Ast#ast_record_select{type = {ok, FieldType},
                                           record = AnnotatedRecord},
      {AnnotatedAst, Env4};

    #ast_module_select{module = Module, label = Label} ->
      {RowParentType, Env1} = new_var(Env0),
      {FieldType, Env2} = new_var(Env1),
      RowType = #type_row_extend{label = Label,
                                 type = FieldType,
                                 parent = RowParentType},
      ParamType = #type_module{row = RowType},
      {AnnotatedModule, Env3} = infer(Module, Env2),
      ModuleType = fetch(AnnotatedModule),
      Env4 = unify(ParamType, ModuleType, Env3),
      AnnotatedAst = Ast#ast_module_select{type = {ok, FieldType},
                                           module = AnnotatedModule},
      {AnnotatedAst, Env4};

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


-spec ast_type_to_type(ast_type(), boolean(), env()) -> {type(), env()}.
ast_type_to_type(AstType, Create, Env0) ->
  case AstType of
    #ast_type_constructor{name = Name, args = []} ->
      Type = case env_lookup_type(Name, Env0) of
        {ok, #type_data{type = T, arity = 0}} -> T;
        _ -> fail({type_not_found, line_number(AstType), Name, 0})
      end,
      {Type, Env0};

    #ast_type_constructor{name = Name, args = Args} ->
      Arity = length(Args),
      Type = case env_lookup_type(Name, Env0) of
        {ok, #type_data{type = AppType = #type_app{args = AppArgs}}}
        when length(AppArgs) =:= Arity ->
          AppType;

        _ ->
          fail({type_not_found, line_number(AstType), Name, Arity})
      end,
      {ArgsTypes, Env1} = lists:mapfoldl(fun(T, E) -> ast_type_to_type(T, Create, E) end,
                                         Env0,
                                         Args),
      T = Type#type_app{args = ArgsTypes},
      {T, Env1};

    #ast_type_var{name = Name} ->
      case {Create, env_lookup_type(Name, Env0)} of
        {_, {ok, #type_data{type = Var}}} ->
          {Var, Env0};

        {false, error} ->
          fail({type_not_found, line_number(AstType), Name, 0});

        {true, error} ->
          {Var, Env1} = new_generic_var(Env0),
          Env2 = env_register_type(Name, Var, Env1),
          {Var, Env2}
      end
  end.


-spec find_private_type(type(), env()) -> {ok, type()} | {error, not_found}.
find_private_type(Type, Env) ->
  case Type of
    #type_var{type = Ref} ->
      case env_lookup_type_ref(Ref, Env) of
        #type_var_generic{} ->
          {error, not_found};

        #type_var_unbound{} ->
          {error, not_found};

        #type_var_link{type = InnerType} ->
          find_private_type(InnerType, Env)
      end;

    #type_app{public = false} ->
      {ok, Type};

    #type_app{public = true, args = Args} ->
      find_private_type_in_list(Args, Env);

    #type_const{public = false} ->
      {ok, Type};

    #type_const{public = true} ->
      {error, not_found};

    #type_module{row = Row} ->
      find_private_type(Row, Env);

    #type_record{row = Row} ->
      find_private_type(Row, Env);

    #type_row_empty{} ->
      {error, not_found};

    #type_row_extend{parent = Parent, type = FieldType} ->
      case find_private_type(FieldType, Env) of
        {ok, T} -> {ok, T};
        _ -> find_private_type(Parent, Env)
      end;

    #type_fn{args = Args, return = Return} ->
      case find_private_type(Return, Env) of
        {ok, T} -> {ok, T};
        _ -> find_private_type_in_list(Args, Env)
      end
  end.


-spec find_private_type_in_list([type()], env()) -> {ok, type()} | {error, not_found}.
find_private_type_in_list(Types, Env) ->
  case Types of
    [] ->
      {error, not_found};

    [Type | Rest] ->
      case find_private_type(Type, Env) of
        {ok, X} -> {ok, X};
        _ -> find_private_type_in_list(Rest, Env)
      end
  end.


-spec infer_assignment(ast_expression(), ast_expression(), env())
      -> {type(), ast_expression(), env()}.
infer_assignment(Pattern, Value, Env0) ->
  {InferredValue, Env2} = infer(Value, increment_env_level(Env0)),
  Env3 = decrement_env_level(Env2),
  {GeneralizedType, Env4} = generalize(fetch(InferredValue), Env3),
  {AnnotatedPattern, Env5} = infer_pattern(Pattern, Env4),
  Env6 = unify(fetch(AnnotatedPattern), GeneralizedType, Env5),
  {GeneralizedType, InferredValue, Env6}.


-spec module_statement(mod_statement(), {type(), env()})
      -> {mod_statement(), {type(), env()}}.
module_statement(Statement, {Row, Env0}) ->
  case Statement of
    #ast_mod_enum{public = Public, name = Name, args = Args, constructors = Constructors} ->
      % Store the original types so the type vars in the enum definition
      % (i.e. the a in Maybe(a)) do not leak outside the definition.
      OriginalTypes = Env0#env.types,

      % Register each type var in the env for later use in constructors.
      NewVar =
        fun(TypeVarName, E0) ->
          {Var, E1} = new_generic_var(E0),
          E2 = env_register_type(TypeVarName, Var, E1),
          {Var, E2}
        end,
      {ArgsTypes, Env1} = lists:mapfoldl(NewVar, Env0, Args),
      Type =
        case Args of
          [] -> #type_const{public = Public, type = Name};
          _ -> #type_app{public = Public, type = Name, args = ArgsTypes}
        end,

      % Create a function type for each constructor of the enum.
      % Makes use of the type vars inserted in to the env previously.
      RegisterConstructor =
        fun(#ast_enum_def{name = CName, args = CArgs} = Def, InnerEnv0) ->
          {CArgsTypes, InnerEnv1} = lists:mapfoldl(fun(A, E) -> ast_type_to_type(A, false, E) end,
                                                   InnerEnv0,
                                                   CArgs),
          T = #type_fn{args = CArgsTypes, return = Type},

          case Public andalso find_private_type(T, InnerEnv1) of
            {ok, PrivateType} -> fail({type_not_public, line_number(Def), PrivateType});
            {error, _} -> ok;
            false -> ok
          end,

          env_extend(CName, T, module, InnerEnv1)
        end,
      Env2 = lists:foldl(RegisterConstructor, Env1, Constructors),

      % Reset the env types to prevent the type vars leaking out.
      Env3 = Env2#env{types = OriginalTypes},
      % Register the new enum type
      Env4 = env_register_type(Name, Type, Env3),
      {Statement, {Row, Env4}};

    #ast_mod_fn{public = Public, name = Name, args = Args, body = Body} ->
      Fn = #ast_fn{args = Args, body = Body},
      {AnnotatedFn, Env1} = infer(Fn, increment_env_level(Env0)),
      Env2 = decrement_env_level(Env1),
      {FnType, Env3} = generalize(fetch(AnnotatedFn), Env2),

      case Public andalso find_private_type(FnType, Env3) of
        {ok, T} -> fail({type_not_public, line_number(Statement), T});
        _ -> ok
      end,

      % Unify with previously registered type
      {ok, #var_data{type = Type}} = env_lookup(Name, Env3),
      Env4 = unify(Type, FnType, Env3),


      #ast_fn{args = NewArgs, body = NewBody} = AnnotatedFn,
      NewRow = case Public of
        true -> #type_row_extend{label = Name, type = FnType, parent = Row};
        false -> Row
      end,
      NewState = {NewRow, Env4},
      AnnotatedStatement = Statement#ast_mod_fn{type = {ok, FnType},
                                                args = NewArgs,
                                                body = NewBody},
      {AnnotatedStatement, NewState};

    #ast_mod_external_fn{public = Public, name = Name, args = Args, return = Return} ->
      {ArgsTypes, Env1} = lists:mapfoldl(fun(A, E) -> ast_type_to_type(A, true, E) end,
                                         Env0, Args),
      {ReturnType, Env2} = ast_type_to_type(Return, true, Env1),
      Type = #type_fn{args = ArgsTypes, return = ReturnType},
      NewRow = case Public of
        true -> #type_row_extend{label = Name, type = Type, parent = Row};
        false -> Row
      end,
      Env3 = env_extend(Name, Type, module, Env2),
      NewState = {NewRow, Env3},
      AnnotatedStatement = Statement#ast_mod_external_fn{type = {ok, Type}},
      {AnnotatedStatement, NewState};

    #ast_mod_test{body = Body} ->
      Fn = #ast_fn{args = [], body = Body},
      {AnnotatedFn, Env1} = infer(Fn, Env0),
      AnnotatedAst = Statement#ast_mod_test{body = AnnotatedFn#ast_fn.body},
      NewState = {Row, Env1},
      {AnnotatedAst, NewState};

    #ast_mod_import{module = ModuleName} ->
      case maps:find(ModuleName, Env0#env.importables) of
        {ok, #compiled_module{type = Type}} ->
          ModuleValue = #ast_atom{value = "gleam_" ++ ModuleName},
          Env1 = env_extend(ModuleName, Type, {constant, ModuleValue}, Env0),
          {Statement, {Row, Env1}};

        error ->
          fail({module_not_found, line_number(Statement), ModuleName})
      end;

    #ast_mod_external_type{public = Public, name = Name} ->
      Env1 = env_register_type(Name, #type_const{public = Public, type = Name}, Env0),
      {Statement, {Row, Env1}}
  end.


% TODO: We need to return the args types from this so we can the args after
% calling this.
-spec infer_call(ast_expression(), [ast_expression()], env())
      -> {type(), ast_expression(), [ast_expression()], env()}.
infer_call(FunAst, Args, Env0) ->
  {AnnotatedFunAst, Env1} = infer(FunAst, Env0),
  FunType = fetch(AnnotatedFunAst),
  Arity = length(Args),
  {ArgTypes, ReturnType, Env2} = match_fun_type(Arity, FunType, line_number(FunAst), Env1),
  CheckArg =
    fun({ArgType, ArgExpr}, CheckEnv0) ->
      {AnnotatedArgExpr, CheckEnv1} = infer(ArgExpr, CheckEnv0),
      ArgExprType = fetch(AnnotatedArgExpr),
      CheckEnv2 = unify(ArgType, ArgExprType, CheckEnv1),
      {AnnotatedArgExpr, CheckEnv2}
    end,
  {AnnotatedArgs, Env3} = lists:mapfoldl(CheckArg, Env2, lists:zip(ArgTypes, Args)),
  {ReturnType, AnnotatedFunAst, AnnotatedArgs, Env3}.


-spec fetch_clause_type(#ast_clause{}) -> type().
fetch_clause_type(#ast_clause{value = Value}) ->
  fetch(Value).


-spec fetch(ast_expression()) -> type().
fetch(Ast) ->
  case Ast of
    #ast_operator{type = {ok, Type}} ->
      Type;

    #ast_enum{type = {ok, Type}} ->
      Type;

    #ast_case{type = {ok, Type}} ->
      Type;

    #ast_call{type = {ok, Type}} ->
      Type;

    #ast_fn{type = {ok, Type}} ->
      Type;

    #ast_var{type = {ok, Type}} ->
      Type;

    #ast_nil{type = {ok, Type}} ->
      Type;

    #ast_module{type = {ok, Type}} ->
      Type;

    #ast_module_select{type = {ok, Type}} ->
      Type;

    #ast_record_extend{type = {ok, Type}} ->
      Type;

    #ast_record_select{type = {ok, Type}} ->
      Type;

    #ast_seq{then = Then} ->
      fetch(Then);

    #ast_assignment{then = Then} ->
      fetch(Then);

    #ast_record_empty{} ->
      #type_record{row = #type_row_empty{}};

    #ast_tuple{elems = Elems} ->
      ElemsTypes = lists:map(fun fetch/1, Elems),
      #type_app{public = true, type = "Tuple", args = ElemsTypes};

    #ast_cons{head = Head} ->
      #type_app{public = true, type = "List", args = [fetch(Head)]};

    #ast_int{} ->
      #type_const{public = true, type = "Int"};

    #ast_atom{} ->
      #type_const{public = true, type = "Atom"};

    #ast_float{} ->
      #type_const{public = true, type = "Float"};

    #ast_string{} ->
      #type_const{public = true, type = "String"}
  end.


-spec resolve_type_vars(ast_expression(), env()) -> ast_expression().
resolve_type_vars(Ast, Env) ->
  case Ast of
    #ast_module{type = {ok, Type}, statements = Statements} ->
      NewType = type_resolve_type_vars(Type, Env),
      NewStatements = lists:map(fun(X) -> statement_resolve_type_vars(X, Env) end,
                                Statements),
      Ast#ast_module{type = {ok, NewType}, statements = NewStatements};

    #ast_case{type = {ok, Type}, clauses = Clauses} ->
      Resolve =
        fun(#ast_clause{value = Value} = Clause) ->
          Clause#ast_clause{value = resolve_type_vars(Value, Env)}
        end,
      NewClauses = lists:map(Resolve, Clauses),
      NewType = type_resolve_type_vars(Type, Env),
      Ast#ast_case{type = {ok, NewType}, clauses = NewClauses};

    #ast_seq{first = First, then = Then} ->
      Ast#ast_seq{first = resolve_type_vars(First, Env),
                  then = resolve_type_vars(Then, Env)};

    #ast_assignment{value = Value, then = Then} ->
      Ast#ast_assignment{value = resolve_type_vars(Value, Env),
                         then = resolve_type_vars(Then, Env)};

    #ast_operator{type = {ok, Type}} ->
      NewType = type_resolve_type_vars(Type, Env),
      Ast#ast_operator{type = {ok, NewType}};

    #ast_call{type = {ok, Type}} ->
      NewType = type_resolve_type_vars(Type, Env),
      Ast#ast_call{type = {ok, NewType}};

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


-spec statement_resolve_type_vars(ast_expression(), env()) -> ast_expression().
statement_resolve_type_vars(Statement, Env) ->
  case Statement of
    #ast_mod_import{} ->
      Statement;

    #ast_mod_enum{} ->
      Statement;

    #ast_mod_external_type{} ->
      Statement;

    % TODO: resolve type vars in args and body?
    #ast_mod_fn{type = {ok, Type}} ->
      NewType = type_resolve_type_vars(Type, Env),
      Statement#ast_mod_fn{type = {ok, NewType}};

    % TODO: resolve type vars in args and body?
    #ast_mod_external_fn{type = {ok, Type}} ->
      NewType = type_resolve_type_vars(Type, Env),
      Statement#ast_mod_external_fn{type = {ok, NewType}};

    % TODO: resolve type vars in body?
    #ast_mod_test{} ->
      % NewType = type_resolve_type_vars(Type, Env),
      % Statement#ast_mod_fn{type = {ok, NewType}}
      Statement
  end.


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
        #type_var_generic{id = Id} ->
          #type_var{type = Id};

        #type_var_unbound{id = Id} ->
          #type_var{type = Id};

        #type_var_link{type = InnerType} ->
          type_resolve_type_vars(InnerType, Env)
      end
  end.


-spec new_env(#{string() => type()}) -> env().
new_env(ImportableVars) ->
  E0 = #env{importables = ImportableVars},
  Int = #type_const{public = true, type = "Int"},
  Atom = #type_const{public = true, type = "Atom"},
  Bool = #type_const{public = true, type = "Bool"},
  Float = #type_const{public = true, type = "Float"},
  String = #type_const{public = true, type = "String"},
  BinOp = fun(A, B, C) -> #type_fn{args = [A, B], return = C} end,
  EndoOp = fun(T) -> BinOp(T, T, T) end,
  ZeroFn = fun(A) -> #type_fn{args = [], return = A} end,

  {V1, E1} = new_generic_var(E0),
  Eq = BinOp(V1, V1, Bool),

  {V2, E2} = new_generic_var(E1),
  NEq = BinOp(V2, V2, Bool),

  {PipeIn, E3} = new_generic_var(E2),
  {PipeOut, E4} = new_generic_var(E3),
  PipeFn = #type_fn{args = [PipeIn], return = PipeOut},
  Pipe = BinOp(PipeIn, PipeFn, PipeOut),

  {V3, E5} = new_generic_var(E4),
  LT = BinOp(V3, V3, Bool),

  {V4, E6} = new_generic_var(E5),
  LET = BinOp(V4, V4, Bool),

  {V5, E7} = new_generic_var(E6),
  GT = BinOp(V5, V5, Bool),

  {V6, E8} = new_generic_var(E7),
  GET = BinOp(V6, V6, Bool),

  E9 = env_register_type("String", String, E8),
  E10 = env_register_type("Int", Int, E9),
  E11 = env_register_type("Bool", Bool, E10),
  E12 = env_register_type("Atom", Atom, E11),

  {V7, E13} = new_generic_var(E12),
  List = #type_app{public = true, type = "List", args = [V7]},
  E14 = env_register_type("List", List, E13),

  {V8, E15} = new_generic_var(E14),
  {V9, E16} = new_generic_var(E15),
  Tuple = #type_app{public = true, type = "Tuple", args = [V8, V9]},
  E17 = env_register_type("Tuple", Tuple, E16),

  LastE = E17,

  Core = [
    {"+", EndoOp(Int)},
    {"-", EndoOp(Int)},
    {"/", EndoOp(Int)},
    {"*", EndoOp(Int)},
    {"+.", EndoOp(Float)},
    {"-.", EndoOp(Float)},
    {"/.", EndoOp(Float)},
    {"*.", EndoOp(Float)},
    {"<", LT},
    {"<=", LET},
    {">", GT},
    {">=", GET},
    {"==", Eq},
    {"!=", NEq},
    {"|>", Pipe},
    {"True", ZeroFn(Bool)},
    {"False", ZeroFn(Bool)}
  ],
  Insert = fun({Name, Type}, Env) -> env_extend(Name, Type, module, Env) end,
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
  Env = new_env(#{}),
  Env2 = Env#env{level = 42},
  Env3 = increment_env_level(Env2),
  ?assertEqual(43, Env3#env.level).
-endif.


-spec env_extend(var_name(), type(), scope(), env()) -> env().
env_extend(Name, Type, Scope, Env = #env{vars = Vars}) ->
  VarData = #var_data{type = Type, scope = Scope},
  NewVars = maps:put(Name, VarData, Vars),
  Env#env{vars = NewVars}.

% TODO: Raise if we attempt to overwrite an existing type.
-spec env_register_type(type_name(), type(), env()) -> env().
env_register_type(Name, Type, Env = #env{types = Types}) ->
  Arity =
    case Type of
      #type_app{args = Args} -> length(Args);
      _ -> 0
    end,
  NewTypes = maps:put(Name, #type_data{type = Type, arity = Arity}, Types),
  Env#env{types = NewTypes}.

-spec env_lookup(var_name(), env()) -> error | {ok, #var_data{}}.
env_lookup(Name, #env{vars = Vars}) ->
  maps:find(Name, Vars).

-spec env_lookup_type(type_name(), env()) -> error | {ok, type()}.
env_lookup_type(Name, #env{types = Types}) ->
  maps:find(Name, Types).

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
      {GeneralizedArgs, Env1} = lists:mapfoldl(fun generalize/2, Env0, Args),
      GeneralizedType = Type#type_app{args = GeneralizedArgs},
      {GeneralizedType, Env1};

    #type_fn{args = Args, return = Return} ->
      {GeneralizedArgs, Env1} = lists:mapfoldl(fun generalize/2, Env0, Args),
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

    #type_module{row = Row} ->
      {GeneralizedRow, Env1} = generalize(Row, Env0),
      GeneralizedType = Type#type_module{row = GeneralizedRow},
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

    #type_module{row = Row} ->
      {NewRow, NewState} = do_instantiate(Row, State0),
      NewType = Type#type_module{row = NewRow},
      {NewType, NewState};

    #type_row_extend{parent = Parent, type = FieldType} ->
      {NewParent, State1} = do_instantiate(Parent, State0),
      {NewFieldType, State2} = do_instantiate(FieldType, State1),
      NewType = Type#type_row_extend{parent = NewParent, type = NewFieldType},
      {NewType, State2};

    #type_row_empty{} ->
      {Type, State0};

    #type_app{args = Args} ->
      {NewArgs, State1} = lists:mapfoldl(fun do_instantiate/2, State0, Args),
      NewType = Type#type_app{args = NewArgs},
      {NewType, State1};

    #type_fn{args = Args, return = Return} ->
      {NewArgs, State1} = lists:mapfoldl(fun do_instantiate/2, State0, Args),
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
          Env;

        % This is permitted due to patterns in assignments.
        % It was not allowed in the original implementation. Is this correct?
        #type_var_generic{} ->
          Env
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

    #type_module{row = Row} ->
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

    {#type_module{row = Row1}, _,
     #type_module{row = Row2}, _} ->
      unify(Row1, Row2, Env);

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

    _Other ->
      T1 = type_resolve_type_vars(Type1, Env),
      T2 = type_resolve_type_vars(Type2, Env),
      fail({cannot_unify, T2, T1})
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
      {Parent, Env1} = new_var(Env0),
      NewRow = #type_row_extend{label = Label, type = Field, parent = Parent},
      Env2 = env_put_type_ref(OtherRow, #type_var_link{type = NewRow}, Env1),
      {NewRow, Env2};

    _ ->
      fail({not_a_row, OtherRow, Env0})
  end.


-spec tvar_value(type(), env()) -> {ok, type_var()} | error.
tvar_value(#type_var{type = Ref}, Env) ->
  {ok, env_lookup_type_ref(Ref, Env)};
tvar_value(_, _) ->
  error.


-spec match_fun_type(non_neg_integer(), type(), line_number(), env())
      -> {list(type()), type(), env()}.
match_fun_type(Arity, #type_var{type = Ref}, LineNumber, Env) ->
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

    #type_var_link{type = LinkedType} ->
      match_fun_type(Arity, LinkedType, LineNumber, Env)
  end;

match_fun_type(Arity, Type, LineNumber, Env) ->
  case Type of
    #type_fn{args = Args, return = Return} ->
      Expected = length(Args),
      case Arity =:= Expected of
        true -> {Args, Return, Env};
        false -> fail({incorrect_number_of_arguments, LineNumber, Expected, Arity})
      end;

    _ ->
      fail({not_a_function, LineNumber, Arity, Type})
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
                  " fn " ++ Label ++ FnString ++ FS(FS, Rest);

                Other ->
                  " " ++ Label ++ " = " ++ Other
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
            ++ " |"
            ++ FieldsString(FieldsString, Fields)
            ++ "}";

          Fields ->
            "module {"
            ++ FieldsString(FieldsString, Fields)
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
        ++ lists:concat(lists:join(", ", lists:map(fun(X) -> F(F, X) end, ParamTypeList)))
        ++ ") -> "
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

collect_row_fields(Row, Fields) ->
  case Row of
    #type_row_empty{} ->
    Fields;

    #type_row_extend{parent = Parent, label = Label, type = Type} ->
      NewFields = [{Label, Type} | Fields],
      collect_row_fields(Parent, NewFields);

    Other ->
      {Other, Fields}
  end.

-spec uid(env()) -> {integer(), env()}.
uid(#env{uid = UID} = Env) ->
  {UID, Env#env{uid = UID + 1}}.


-spec error_to_iolist(error(), string()) -> iodata().
error_to_iolist(Error, Src) ->
  case Error of
    {var_not_found, LineNumber, Name} ->
      io_lib:format(
        "error: No variable with name `~s` found in this scope.\n"
        "\n"
        "~s\n"
        "\n",
        [Name, show_code(LineNumber, Src)]
      );

    {module_not_found, LineNumber, Name} ->
      io_lib:format(
        "error: No module with name `~s` found.\n"
        "\n"
        "~s\n"
        "\n",
        [Name, show_code(LineNumber, Src)]
      );

    {type_not_found, LineNumber, Name, _Arity} ->
      io_lib:format(
        "error: No type with name `~s` found in this scope.\n"
        "\n"
        "~s\n"
        "\n",
        [Name, show_code(LineNumber, Src)]
      );

    {not_a_function, LineNumber, NumArgs, Type} ->
      io_lib:format(
        "error: A non-function value is being called with ~B arguments.\n"
        "\n"
        "~s\n"
        "\n"
        "The value is of type `~s`\n"
        "\n",
        [NumArgs, show_code(LineNumber, Src), type_to_string(Type)]
      );

    {multiple_hole_fn, LineNumber, NumHoles} ->
      io_lib:format(
        "error: A function is being captured with ~B `_` placeholders, but \n"
        "the function capture syntax only permits one `_` placeholder.\n"
        "\n"
        "~s\n"
        "\n"
        "Rewrite the capture as an anonymous function `fn(a, b) { ... }`\n"
        "\n",
        [NumHoles, show_code(LineNumber, Src)]
      );

    {cannot_unify, T1, T2} ->
      io_lib:format(
        "error: Type mismatch. The inferred type is\n"
        "\n"
        "    ~s\n"
        "\n"
        "But somewhere else wants\n"
        "\n"
        "    ~s\n"
        "\n"
        "Types are inferred top to bottom, left to right, so the problem \n"
        "may be earlier in the file.\n"
        "\n",
        [type_to_string(T1), type_to_string(T2)]
      );

    {incorrect_number_of_arguments, LineNumber, Expected, Given} ->
      io_lib:format(
        "error: A function expected ~B arguments, but it is being called\n"
        "with ~B instead.\n"
        "\n"
        "~s\n"
        "\n",
        [Expected, Given, show_code(LineNumber, Src)]
      )
  end.


show_code(LineNumber, Src) ->
  N = integer_to_list(LineNumber),
  P = lists:map(fun(_) -> $\s end, N),
  PaddedSrc =
    case LineNumber of
      1 -> ["\n", Src];
      _ -> Src
    end,
  case take(3, drop(LineNumber - 2, string:split(PaddedSrc, "\n", all))) of
    [L1, L2, L3] ->
      [" ", P, " | ", L1, "\n",
       " ", N, " | ", L2, "\n",
       " ", P, " | ", L3];

    [L1, L2] ->
      [" ", P, " | ", L1, "\n",
       " ", N, " | ", L2, "\n",
       " ", P, " |"];

    [L1] ->
      [" ", P, " |\n",
       " ", N, " | ", L1, "\n",
       " ", P, " |"]
  end.


drop(N, [_ | Xs] = List) ->
  case N =< 0 of
    true -> List;
    false -> drop(N - 1, Xs)
  end.


take(N, List) ->
  case {N, List} of
    {_, _}  when N =< 0 -> [];
    {_, []} -> [];
    {_, [X | Xs]} -> [X | take(N - 1, Xs)]
  end.


-spec line_number(ast_expression()) -> line_number().
line_number(Ast) ->
  Meta = element(2, Ast),
  Meta#meta.line.
