-module(gleam_codegen).
-include("gleam_records.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([module/1]).

module(#gleam_ast_module{functions = Funs} = Mod) ->
  C_name = mod_name(Mod),
  % TODO: use export statements
  C_exports = lists:map(fun export/1, Funs),
  C_definitions = lists:map(fun function/1, Funs),
  Attributes = [],
  Core = cerl:c_module(C_name, C_exports, Attributes, C_definitions),
  {ok, Core}.


mod_name(#gleam_ast_module{name = Name}) ->
  Atom = list_to_atom("Gleam." ++ atom_to_list(Name)),
  cerl:c_atom(Atom).


export(#gleam_ast_function{name = Name, args = Args}) ->
  Arity = length(Args),
  cerl:c_fname(Name, Arity).


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


-ifdef(TEST).
mod_name_test() ->
  Mod = #gleam_ast_module{name ='SomeModule'},
  ?assertEqual(cerl:c_atom('Gleam.SomeModule'), mod_name(Mod)).
-endif.
