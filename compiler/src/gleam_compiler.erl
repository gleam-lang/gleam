-module(gleam_compiler).
-include("gleam_records.hrl").

-export([source_to_binary/1, source_to_binary/2, compile_file/1]).

source_to_binary(Source) ->
  source_to_binary(Source, []).

source_to_binary(Source, Options) ->
  {ok, Tokens, _} = gleam_tokenizer:string(Source),
  {ok, #ast_module{} = AST} = gleam_parser:parse(Tokens),
  {ok, Forms} = gleam_codegen:module(AST, Options),
  {ok, _, Bin} = compile:forms(Forms, [report, verbose, from_core]),
  Bin.

compile_file(Path) ->
  {ok, Source} = file:read_file(Path),
  ListSource = binary_to_list(Source),
  BeamFileName = "Gleam." ++ filename:basename(filename:rootname(Path)) ++ ".beam",
  BeamPath = filename:join(filename:dirname(Path), BeamFileName),
  Beam = source_to_binary(ListSource),
  ok = file:write_file(BeamPath, Beam).
