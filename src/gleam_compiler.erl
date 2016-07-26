-module(gleam_compiler).
-export([source_to_binary/1]).

source_to_binary(Source) ->
  {ok, Tokens, _} = gleam_tokenizer:string(Source),
  {ok, AST} = gleam_parser:parse(Tokens),
  {ok, Mod} = gleam_module:from_ast(AST),
  {ok, Forms} = gleam_codegen:module(Mod),
  {ok, _, Bin} = compile:forms(Forms, [report, verbose, from_core]),
  Bin.
