-module(gleam_compiler_test).
-include_lib("eunit/include/eunit.hrl").
-include("gleam_records.hrl").

module_docs_chunk_test() ->
  Source =
    "pub fn one() { 1 }\n"
    "pub fn double(x) { x + x }\n"
  ,
  {ok, Beam} = gleam_compiler:source_to_binary(Source, "module_docs_chunk_test", []),
  {ok, Docs} = gleam_compiler:fetch_docs_from_binary(Beam),
  FnOneType = #type_fn{args = [], return = #type_const{type = "Int"}},
  FnDoubleType = #type_fn{args = [#type_const{type = "Int"}], return = #type_const{type = "Int"}},
  RowType = #type_row_extend{
               label = "double",
               type = FnDoubleType,
               parent = #type_row_extend{label = "one",
                                         type = FnOneType,
                                         parent = #type_row_empty{}}},
  Expected = {
    docs_v1,
    1,
    gleam,
    <<"text/markdown">>,
    none,
    #{gleam_module_type => #type_module{row = RowType}},
    []
  },
  ?assertEqual(Expected, Docs).

module_dependencies_test() ->
  Src =
    "import foo\n"
    "fn id(x) { y }\n"
    "import bar\n"
    "import baz\n"
  ,
  {ok, Tokens, _} = gleam_tokenizer:string(Src),
  {ok, Ast} = gleam_parser:parse(Tokens),
  ?assertEqual({ok, ["baz", "bar", "foo"]},
               gleam_compiler:module_dependencies(Ast)).

