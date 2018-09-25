-module(gleam_compiler).
-include("gleam_records.hrl").

-export([source_to_binary/2, source_to_binary/3, compile_file/2]).

source_to_binary(Source, ModName) ->
  source_to_binary(Source, ModName, []).

source_to_binary(Source, ModName, Options) ->
  {ok, Tokens, _} = gleam_tokenizer:string(Source),
  {ok, Ast} = gleam_parser:parse(Tokens),
  % _ = gleam_type:infer(Ast), % TODO
  {ok, Forms} = gleam_codegen:module(Ast, ModName, Options),
  {ok, _, Bin} = compile:forms(Forms, [report, verbose, from_core]),
  Bin.

compile_file(Path, ModName) ->
  {ok, Source} = file:read_file(Path),
  ListSource = binary_to_list(Source),
  BeamFileName = "Gleam." ++ filename:basename(filename:rootname(Path)) ++ ".beam",
  BeamPath = filename:join(filename:dirname(Path), BeamFileName),
  Beam = source_to_binary(ListSource, ModName),
  ok = file:write_file(BeamPath, Beam).
