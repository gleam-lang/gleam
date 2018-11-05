-module(gleam_compiler).
-include("gleam_records.hrl").

-export([source_to_binary/2, source_to_binary/3, compile_file/2]).

source_to_binary(Source, ModName) ->
  source_to_binary(Source, ModName, []).

source_to_binary(Source, ModName, Options) ->
  {ok, Tokens, _} = gleam_tokenizer:string(Source),
  {ok, Ast} = gleam_parser:parse(Tokens),
  case gleam_type:infer(Ast) of
    {ok, AnnotatedAst} ->
      {ok, Forms} = gleam_codegen:module(AnnotatedAst, ModName, Options),
      {ok, _, Bin} = compile:forms(Forms, [report, verbose, from_core]),
      {ok, Bin};

    {error, Error} ->
      {error, lists:flatten(gleam_type:error_to_iolist(Error))}
  end.

compile_file(Path, ModName) ->
  {ok, Source} = file:read_file(Path),
  ListSource = binary_to_list(Source),
  BeamFileName = "gleam_" ++ filename:basename(filename:rootname(Path)) ++ ".beam",
  BeamPath = filename:join(filename:dirname(Path), BeamFileName),
  case source_to_binary(ListSource, ModName) of
    {ok, Beam} ->
      ok = file:write_file(BeamPath, Beam);

    {error, Error} ->
      {error, Error}
  end.
