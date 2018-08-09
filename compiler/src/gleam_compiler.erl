-module(gleam_compiler).
-include("gleam_records.hrl").

-export([source_to_binary/1, source_to_binary/2, compile_file/1]).

source_to_binary(Source) ->
  source_to_binary(Source, []).

source_to_binary(Source, Options) ->
  {ok, Tokens, _} = gleam_tokenizer:string(Source),
  {ok, #ast_module{} = AST} = gleam_parser:parse(Tokens),
  {ok, Core} = gleam_codegen:module(AST, Options),
  {ok, _, Beam0} = compile:forms(Core, [report, verbose, from_core, debug_info]),

  % Add debug chunks
  {ok, AbstractChunk} = get_abstract(Core),
  {ok, Beam1} = add_chunks(Beam0, [AbstractChunk]),
  Beam1.

compile_file(Path) ->
  {ok, Source} = file:read_file(Path),
  ListSource = binary_to_list(Source),
  BeamFileName = "Gleam." ++ filename:basename(filename:rootname(Path)) ++ ".beam",
  BeamPath = filename:join(filename:dirname(Path), BeamFileName),
  Beam = source_to_binary(ListSource),
  ok = file:write_file(BeamPath, Beam).

add_chunks(Beam, Chunks) ->
  {ok, _Name, ExistingChunks} = beam_lib:all_chunks(Beam),
  {ok, _BeamWithChunks} = beam_lib:build_module(Chunks ++ ExistingChunks).

get_abstract(Core) ->
  Opts = [],
  {ok, _Chunk} = gleam_abstract_code:make_chunk(Core, Opts).
