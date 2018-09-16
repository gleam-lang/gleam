-module(gleam_codegen).
-include("gleam_records.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([module/2]).

-define(is_uppercase_char(C), C >= $A andalso C =< $Z).

-define(is_erlang_module_operator(N),
        N =:= "+" orelse  N =:= "-" orelse  N =:= "*" orelse  N =:= "/" orelse
        N =:= "+." orelse N =:= "-." orelse N =:= "*." orelse N =:= "/." orelse
        N =:= "<=" orelse N =:= "<"  orelse N =:= ">"  orelse N =:= ">=" orelse
        N =:= "/" orelse  N =:= "==" orelse N =:= "!=").

% Holds state used in code generation.
-record(env, {uid = 0}).

module(#ast_module{name = Name, functions = Funs, exports = Exports, tests = Tests}, Options) ->
  PrefixedName = prefix_module(Name),
  {TestExports, TestFuns} = case proplists:get_value(gen_tests, Options, false) of
                              false -> {[], []};
                              true -> tests(Tests, PrefixedName)
                            end,
  C_name = cerl:c_atom(PrefixedName),
  C_exports = [cerl:c_fname(module_info, 0),
               cerl:c_fname(module_info, 1) |
               lists:map(fun export/1, Exports) ++ TestExports],
  C_definitions = [module_info(PrefixedName, []),
                   module_info(PrefixedName, [cerl:c_var(item)]) |
                   lists:map(fun named_function/1, Funs) ++ TestFuns],
  Attributes = [],
  Core = cerl:c_module(C_name, C_exports, Attributes, C_definitions),
  {ok, Core}.

tests(Tests, PrefixedName) ->
  C_tests = lists:map(fun test/1, Tests),
  lists:unzip([module_test(PrefixedName) | C_tests]).

module_test(PrefixedName) ->
  Body = cerl:c_call(cerl:c_atom(eunit), cerl:c_atom(test), [cerl:c_atom(PrefixedName)]),
  Name = cerl:c_fname(test, 0),
  Fun = cerl:c_fun([], Body),
  {export({"test", 0}), {Name, Fun}}.

test(#ast_test{name = Name, body = Body}) ->
  TestName = Name ++ "_test",
  C_fun = named_function(#ast_function{name = TestName, args = [], body = Body}),
  C_export = export({TestName, 0}),
  {C_export, C_fun}.

export({Name, Arity}) when is_list(Name), is_integer(Arity) ->
  cerl:c_fname(list_to_atom(Name), Arity).

module_info(ModuleName, Params) when is_list(ModuleName) ->
  Body = cerl:c_call(cerl:c_atom(erlang),
                     cerl:c_atom(get_module_info),
                     [cerl:c_atom(list_to_atom(ModuleName)) | Params]),
  C_fun = cerl:c_fun(Params, Body),
  C_fname = cerl:c_fname(module_info, length(Params)),
  {C_fname, C_fun}.

named_function(#ast_function{name = Name, args = Args, body = Body}) ->
  Env = #env{},
  Arity = length(Args),
  C_fname = cerl:c_fname(list_to_atom(Name), Arity),
  {C_fun, _NewEnv} = function(Args, Body, Env),
  {C_fname, C_fun}.

function(Args, Body, Env) ->
  C_args = lists:map(fun var/1, Args),
  {C_body, NewEnv} = expression(Body, Env),
  C_fun = cerl:c_fun(C_args, C_body),
  {C_fun, NewEnv}.

var(Atom) ->
  cerl:c_var(list_to_atom(Atom)).

map_clauses(Clauses, Env) ->
  gleam:thread_map(fun clause/2, Clauses, Env).

map_expressions(Expressions, Env) ->
  gleam:thread_map(fun expression/2, Expressions, Env).

expression(#ast_string{value = Value}, Env) when is_binary(Value) ->
  Chars = binary_to_list(Value),
  ByteSequence = lists:map(fun binary_string_byte/1, Chars),
  {cerl:c_binary(ByteSequence), Env};

expression(#ast_tuple{elems = Elems}, Env) ->
  {C_elems, NewEnv} = map_expressions(Elems, Env),
  {cerl:c_tuple(C_elems), NewEnv};

expression(#ast_atom{value = Value}, Env) when is_list(Value) ->
  {cerl:c_atom(Value), Env};

expression(#ast_int{value = Value}, Env) when is_integer(Value) ->
  {cerl:c_int(Value), Env};

expression(#ast_float{value = Value}, Env) when is_float(Value) ->
  {cerl:c_float(Value), Env};

expression(#ast_var{name = Name}, Env) when is_list(Name) ->
  {cerl:c_var(list_to_atom(Name)), Env};

expression(#ast_cons{head = Head, tail = Tail}, Env) ->
  {C_head, Env1} = expression(Head, Env),
  {C_tail, Env2} = expression(Tail, Env1),
  {cerl:c_cons(C_head, C_tail), Env2};

expression(#ast_local_call{name = Name, args = Args}, Env)
when ?is_erlang_module_operator(Name) ->
  ErlangName = erlang_operator_name(Name),
  expression(#ast_call{module = "erlang", name = ErlangName, args = Args}, Env);

expression(#ast_local_call{meta = Meta, name = Name, args = Args}, Env) ->
  NumHoles = length(lists:filter(fun(#ast_hole{}) -> true; (_) -> false end, Args)),
  case NumHoles of
    0 ->
      C_fname = cerl:c_fname(list_to_atom(Name), length(Args)),
      {C_args, NewEnv} = map_expressions(Args, Env),
      {cerl:c_apply(C_fname, C_args), NewEnv};
    1 ->
      % It's a fn(_) capture, convert it into a closure
      hole_closure(Meta, Name, Args, Env);
    _ ->
      throw({error, multiple_hole_closure})
  end;

expression(#ast_call{module = Mod, name = Name, args = Args}, Env) when is_list(Name) ->
  C_module = cerl:c_atom(prefix_module(Mod)),
  C_name = cerl:c_atom(Name),
  {C_args, NewEnv} = map_expressions(Args, Env),
  {cerl:c_call(C_module, C_name, C_args), NewEnv};

expression(#ast_assignment{name = Name, value = Value, then = Then}, Env) when is_list(Name) ->
  C_var = cerl:c_var(list_to_atom(Name)),
  {C_value, Env1} = expression(Value, Env),
  {C_then, Env2} = expression(Then, Env1),
  {cerl:c_let([C_var], C_value, C_then), Env2};

expression(#ast_adt{name = Name, elems = []}, Env) when is_list(Name) ->
  AtomName = list_to_atom(adt_name_value(Name)),
  {cerl:c_atom(AtomName), Env};

expression(#ast_adt{name = Name, meta = Meta, elems = Elems}, Env) when is_list(Name) ->
  AtomValue = adt_name_value(Name),
  Atom = #ast_atom{meta = Meta, value = AtomValue},
  expression(#ast_tuple{elems = [Atom | Elems]}, Env);

expression(#ast_record{fields = Fields}, Env) ->
  {C_pairs, NewEnv} = gleam:thread_map(fun record_field/2,Fields, Env ),
  Core = cerl:c_map(C_pairs),
  {Core, NewEnv};

expression(#ast_record_access{meta = Meta, record = Record, key = Key}, Env) ->
  Atom = #ast_atom{meta = Meta, value = Key},
  Call = #ast_call{meta = Meta,
                   module = "maps",
                   name = "get",
                   args = [Atom, Record]},
  expression(Call, Env);

% TODO: We can check the lhs here to see if it is a fn(_)
% capture. If it is we can avoid the creation of the intermediary
% closure by directly rewriting the arguments.
% TODO: Avoid creating an extra var if the closure is already a var.
expression(#ast_closure_call{closure = Closure, args = Args}, Env0) ->
  {C_closure, Env1} = expression(Closure, Env0),
  {C_args, Env2} = map_expressions(Args, Env1),
  {UID, Env3} = uid(Env2),
  Name = list_to_atom("$$gleam_closure_var" ++ integer_to_list(UID)),
  C_var = cerl:c_var(Name),
  C_apply = cerl:c_apply(C_var, C_args),
  C_let = cerl:c_let([C_var], C_closure, C_apply),
  {C_let, Env3};

expression(#ast_case{subject = Subject, clauses = Clauses}, Env) ->
  {C_subject, Env1} = expression(Subject, Env),
  {C_clauses, Env2} = map_clauses(Clauses, Env1),
  {cerl:c_case(C_subject, C_clauses), Env2};

expression(#ast_raise{meta = Meta, value = Value}, Env) ->
  Call = #ast_call{meta = Meta,
                   module = "erlang",
                   name = "error",
                   args = [Value]},
  expression(Call, Env);

expression(#ast_throw{meta = Meta, value = Value}, Env) ->
  Call = #ast_call{meta = Meta,
                   module = "erlang",
                   name = "throw",
                   args = [Value]},
  expression(Call, Env);

expression(#ast_pipe{meta = Meta, rhs = Rhs, lhs = Lhs}, Env) ->
  Call = #ast_closure_call{meta = Meta, closure = Rhs, args = [Lhs]},
  expression(Call, Env);

expression(#ast_closure{args = Args, body = Body}, Env) ->
  function(Args, Body, Env);

expression(#ast_nil{}, Env) ->
  {cerl:c_nil(), Env};

% We generate a unique variable name for each hole to prevent
% the BEAM thinking two holes are the same.
expression(#ast_hole{}, Env) ->
  {UID, NewEnv} = uid(Env),
  Name = list_to_atom([$_ | integer_to_list(UID)]),
  {cerl:c_var(Name), NewEnv};

expression(#ast_seq{first = First, then = Then}, Env) ->
  {C_first, Env1} = expression(First, Env),
  {C_then, Env2} = expression(Then, Env1),
  C_seq = cerl:c_seq(C_first, C_then),
  {C_seq, Env2};

expression(Expressions, Env) when is_list(Expressions) ->
  {C_exprs, Env1} = map_expressions(Expressions, Env),
  [Head | Tail] = lists:reverse(C_exprs),
  C_seq = lists:foldl(fun cerl:c_seq/2, Head, Tail),
  {C_seq, Env1}.

hole_closure(Meta, Name, Args, Env) when is_list(Name) ->
  {UID, NewEnv} = uid(Env),
  VarName = "$$gleam_hole_var" ++ integer_to_list(UID),
  Var = #ast_var{name = VarName},
  NewArgs = lists:map(fun(#ast_hole{}) -> Var; (X) -> X end, Args),
  Call = #ast_local_call{meta = Meta, name = Name, args = NewArgs},
  Closure = #ast_closure{meta = Meta, args = [VarName], body = Call},
  expression(Closure, NewEnv).

record_field(#ast_record_field{key = Key, value = Val}, Env0) when is_list(Key) ->
  C_key = cerl:c_atom(Key),
  {C_val, Env1} = expression(Val, Env0),
  Core = cerl:c_map_pair(C_key, C_val),
  {Core, Env1}.

clause(#ast_clause{pattern = Pattern, value = Value}, Env) ->
  {C_pattern, Env1} = expression(Pattern, Env),
  {C_value, Env2} = expression(Value, Env1),
  C_clause = cerl:c_clause([C_pattern], C_value),
  {C_clause, Env2}.

adt_name_value(Chars) when is_list(Chars) ->
  adt_name_value(Chars, []).

adt_name_value([C | Chars], []) when ?is_uppercase_char(C) ->
  adt_name_value(Chars, [C + 32]);
adt_name_value([C | Chars], Acc) when ?is_uppercase_char(C) ->
  adt_name_value(Chars, [C + 32, $_ | Acc]);
adt_name_value([C | Chars], Acc) ->
  adt_name_value(Chars, [C | Acc]);
adt_name_value([], Acc) ->
  lists:reverse(Acc).

% Also update ?is_erlang_module_operator/1
erlang_operator_name("/") -> "div";
erlang_operator_name("+.") -> "+";
erlang_operator_name("-.") -> "-";
erlang_operator_name("*.") -> "*";
erlang_operator_name("/.") -> "/";
erlang_operator_name("<=") -> "=<";
erlang_operator_name("==") -> "=:=";
erlang_operator_name("!=") -> "=/=";
erlang_operator_name(Name) when is_list(Name) -> Name.

c_list(Elems) ->
  Rev = lists:reverse(Elems),
  lists:foldl(fun cerl:c_cons/2, cerl:c_nil(), Rev).

binary_string_byte(Char) ->
  cerl:c_bitstr(cerl:c_int(Char),
                cerl:c_int(8),
                cerl:c_int(1),
                cerl:c_atom(integer),
                c_list([cerl:c_atom(unsigned), cerl:c_atom(big)])).

prefix_module(Name = [C | _]) when ?is_uppercase_char(C) ->
  "Gleam." ++ Name;
prefix_module(Name) when is_list(Name) ->
  Name.

uid(#env{uid = UID} = Env) ->
  {UID, Env#env{uid = UID + 1}}.
