-module(gleam_compiler).
-include("gleam_records.hrl").

-export([compile/4, compile_all/3, fetch_docs_from_binary/1, module_dependencies/1]).

% TODO: Remove
-export([source_to_binary/2, source_to_binary/3, compile_file/2, fetch_docs/1]).

-type docs_chunk_data() :: term().

-spec source_to_binary(string(), string()) -> {error, string()} | {ok, binary()}.
source_to_binary(Source, ModName) ->
  source_to_binary(Source, ModName, []).


-spec compile(file:name_all(), Src::string(), importables(), list())
      -> {ok, compiled_module()} | {error, string()}.
compile(ModPath, Src, Importables, Options) ->
  ModName = filename:basename(ModPath, ".gleam"),
  pipeline(Src,
           [
            fun tokenize/1,
            fun parse/1,
            infer_types(Importables, Src),
            generate_core_erlang(ModName, Options),
            generate_beam_binary(ModPath)
           ]).


-spec compile_all([{file:name_all(), string()}], importables(), list())
      -> {ok, importables()} | {error, {module_name(), string()}}.
compile_all(Inputs, Importables, Options) ->
  Compile =
    fun({ModPath, Src}, Imps) ->
      ModName = filename:basename(ModPath, ".gleam"),
      case compile(ModPath, Src, Imps, Options) of
        {ok, CompiledModule} ->
          {ok, maps:put(ModName, CompiledModule, Imps)};

        {error, Error} ->
          {error, {ModName, Error, Imps}}
      end
    end,
  fold_while(Compile, Importables, Inputs).


tokenize(Src) ->
  case gleam_tokenizer:string(Src) of
    {ok, Tokens, _} ->
      {ok, Tokens}
  end.


parse(Tokens) ->
  case gleam_parser:parse(Tokens) of
    {ok, Ast} ->
      {ok, Ast}
  end.


infer_types(Importables, Src) ->
  fun(Ast) ->
    case gleam_type:annotate(Ast, Importables) of
      {ok, AnnotatedAst} ->
        {ok, AnnotatedAst};

    {error, Error} ->
      {error, lists:flatten(gleam_type:error_to_iolist(Error, Src))}
    end
  end.


generate_core_erlang(ModuleName, Options) ->
  fun(Ast) ->
    case gleam_codegen:module(Ast, ModuleName, Options) of
      {ok, Core} ->
        {ok, {Ast, Core}}
    end
  end.


generate_beam_binary(ModPath) ->
  fun({Ast, Core}) ->
    Chunks = docs_chunk(Ast),
    Opts = [report, verbose, from_core, {extra_chunks, Chunks}],
    {ok, _, Binary} = compile:forms(Core, Opts),
    Compiled = #compiled_module{binary = Binary,
                                type = gleam_type:fetch(Ast),
                                source_path = ModPath},
    {ok, Compiled}
  end.


pipeline(InitialValue, Funs) ->
  lists:foldl(fun(Fn, Acc) -> flat_map(Fn, Acc) end, {ok, InitialValue}, Funs).


flat_map(Fun, X) ->
  case X of
    {ok, Y} -> Fun(Y);
    _ -> X
  end.


fold_while(Fun, Acc, List) ->
  case List of
    [] ->
      {ok, Acc};

    [X | Rest] ->
      case Fun(X, Acc) of
        {ok, NextAcc} -> fold_while(Fun, NextAcc, Rest);
        {error, Error} -> {error, Error}
      end
  end.


source_to_binary(Source, ModName, Options) ->
  case compile(ModName, Source, #{}, Options) of
    {ok, Compiled} -> {ok, Compiled#compiled_module.binary};
    Error -> Error
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
