-module(gleam_compiler).
-export([source_to_binary/1]).
-include("gleam_records.hrl").

source_to_binary(Source) ->
  {ok, Tokens, _} = gleam_tokenizer:string(Source),
  {ok, #ast_module{} = AST} = gleam_parser:parse(Tokens),
  {ok, Forms} = gleam_codegen:module(AST),
  {ok, _, Bin} = compile:forms(Forms, [report, verbose, from_core]),
  Bin.
