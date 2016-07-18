-module(gleam_codegen_test).
-include_lib("eunit/include/eunit.hrl").

compile(Code) ->
  {ok, Tokens, _} = gleam_tokenizer:string(Code),
  {ok, AST} = gleam_parser:parse(Tokens),
  {ok, Mod} = gleam_module:from_ast(AST),
  {ok, Forms} = gleam_codegen:module(Mod),
  {ok, _, Bin} = compile:forms(Forms, [report, verbose, from_core]),
  Bin.

empty_module_test() ->
  Name = codegen_empty_mod,
  Path = "codegen_empty_mod.beam",
  Code = "module codegen_empty_mod",
  Binary = compile(Code),
  {module, Name} = code:load_binary(Name, Path, Binary),
  ?assertEqual({file, Path}, code:is_loaded(Name)),
  ?assert(code:delete(Name)).
