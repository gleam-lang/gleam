-module(gleam_codegen).
-include("gleam_records.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([module/1]).

module(#gleam_ast_module{name = Name, functions = Funs, exports = Exports}) ->
  PrefixedName = list_to_atom("Gleam." ++ atom_to_list(Name)),
  C_name = cerl:c_atom(PrefixedName),
  % TODO: use export statements
  C_exports =
    [ cerl:c_fname(module_info, 0)
    , cerl:c_fname(module_info, 1)
    | lists:map(fun export/1, Exports)
    ],
  C_definitions =
    [ module_info(PrefixedName, [])
    , module_info(PrefixedName, [cerl:c_var(item)])
    | lists:map(fun function/1, Funs)
    ],
  Attributes = [],
  Core = cerl:c_module(C_name, C_exports, Attributes, C_definitions),
  {ok, Core}.


export({Name, Arity}) ->
  cerl:c_fname(Name, Arity).

module_info(ModuleName, Params) ->
  Body = cerl:c_call(cerl:c_atom(erlang),
                     cerl:c_atom(get_module_info),
                     [cerl:c_atom(ModuleName) | Params]),
  C_fun = cerl:c_fun(Params, Body),
  C_fname = cerl:c_fname(module_info, length(Params)),
  {C_fname, C_fun}.

function(#gleam_ast_function{name = Name, args = Args, body = Body}) ->
  Arity = length(Args),
  C_fname = cerl:c_fname(Name, Arity),
  C_args = lists:map(fun cerl:c_var/1, Args),
  C_body = body(Body),
  {C_fname, cerl:c_fun(C_args, C_body)}.


body(Expressions) ->
  % TODO: Support multiple expressions
  [Expression] = Expressions,
  expression(Expression).


expression(#gleam_ast_var{name = Name}) ->
  cerl:c_var(Name).
