-module(gleam_compiler).
-include("gleam_records.hrl").

-export([source_to_binary/2, source_to_binary/3, compile_file/2, fetch_docs/1]).

-spec source_to_binary(string(), string()) -> {error, string()} | {ok, binary()}.
source_to_binary(Source, ModName) ->
  source_to_binary(Source, ModName, []).


source_to_binary(Source, ModName, Options) ->
  {ok, Tokens, _} = gleam_tokenizer:string(Source),
  {ok, Ast} = gleam_parser:parse(Tokens),
  case gleam_type:infer(Ast) of
    {ok, AnnotatedAst} ->
      Chunks = docs_chunk(AnnotatedAst),
      Opts = [report, verbose, from_core, {extra_chunks, Chunks}],
      {ok, Forms} = gleam_codegen:module(AnnotatedAst, ModName, Options),
      {ok, _, Bin} = compile:forms(Forms, Opts),
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


% http://erlang.org/eep/eeps/eep-0048.html
docs_chunk(ModuleAst) ->
  ModuleType = gleam_type:fetch(ModuleAst),
  ModuleDoc = none,
  ModuleDocMeta = #{gleam_module_type => ModuleType},
  DocsChunkData = term_to_binary(
    {docs_v1,
      erl_anno:new(1),
      gleam,
      <<"text/markdown">>,
      ModuleDoc,
      ModuleDocMeta,
      []}, % FunctionDocs ++ MacroDocs ++ CallbackDocs ++ TypeDocs
    [compressed]
  ),
  [{<<"Docs">>, DocsChunkData}].


-spec fetch_docs(atom()) -> term().
fetch_docs(Module) when is_atom(Module) ->
  case code:get_object_code(Module) of
    {_Module, Beam, _BeamPath} ->
      case beam_lib:chunks(Beam, ["Docs"]) of
        {ok, {_module, [{"Docs", Chunk}]}} ->
          binary_to_term(Chunk);

        {error, beam_lib, {missing_chunk, _, "Docs"}} ->
          {error, docs_chunk_not_found}
      end;

    error ->
      {error, module_not_found}
  end.

