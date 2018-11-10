-module(gleam_compiler).
-include("gleam_records.hrl").

-export([source_to_binary/2, source_to_binary/3, compile_file/2, fetch_docs/1,
         fetch_docs_from_binary/1, module_dependencies/1]).


-type docs_chunk_data() :: term().

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
      {error, lists:flatten(gleam_type:error_to_iolist(Error, Source))}
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


-spec fetch_docs(atom())
      -> {ok, docs_chunk_data()} | {error, module_not_found | docs_chunk_not_found}.
fetch_docs(Module) when is_atom(Module) ->
  case code:get_object_code(Module) of
    {_Module, Beam, _BeamPath} ->
      fetch_docs_from_binary(Beam);

    error ->
      {error, module_not_found}
  end.


-spec fetch_docs_from_binary(binary())
      -> {ok, docs_chunk_data()} | {error, docs_chunk_not_found}.
fetch_docs_from_binary(Beam) ->
  case beam_lib:chunks(Beam, ["Docs"]) of
    {ok, {_module, [{"Docs", Chunk}]}} ->
      {ok, binary_to_term(Chunk)};

    {error, beam_lib, {missing_chunk, _, "Docs"}} ->
      {error, docs_chunk_not_found}
  end.


-spec module_dependencies(ast_expression()) -> {ok, [atom()]}.
module_dependencies(#ast_module{statements = Statements}) ->
  F =
    fun(Statement, Deps) ->
      case Statement of
        #ast_mod_import{module = Module} ->
          [Module | Deps];

        _ ->
          Deps
      end
    end,
  {ok, lists:foldl(F, [], Statements)}.
