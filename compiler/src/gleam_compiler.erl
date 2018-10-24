-module(gleam_compiler).
-include("gleam_records.hrl").

-export([source_to_binary/2, source_to_binary/3, compile_file/2]).

source_to_binary(Source, ModName) ->
  source_to_binary(Source, ModName, []).

source_to_binary(Source, ModName, Options) ->
  {ok, Tokens, _} = gleam_tokenizer:string(Source),
  {ok, Ast} = gleam_parser:parse(Tokens),
  {ok, Forms} =
    case gleam_type:infer(Ast) of
      {ok, AnnotatedAst} ->
        gleam_codegen:module(AnnotatedAst, ModName, Options);

      % TODO: Remove this clause. We want to error out.
      % Temp measure while I get full inference working.
      {error, TypeInferError} ->
        ?print(TypeInferError),
        gleam_codegen:module(Ast, ModName, Options)
    end,
  {ok, _, Bin} = compile:forms(Forms, [report, verbose, from_core]),
  Bin.

compile_file(Path, ModName) ->
  {ok, Source} = file:read_file(Path),
  ListSource = binary_to_list(Source),
  BeamFileName = "gleam_" ++ filename:basename(filename:rootname(Path)) ++ ".beam",
  BeamPath = filename:join(filename:dirname(Path), BeamFileName),
  Beam = source_to_binary(ListSource, ModName),
  ok = file:write_file(BeamPath, Beam).
