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


compile_all_test() ->
  Mods = [
    {
      "src/top.gleam",
      "pub fn go() { 1 }"
    },
    {
      "src/middle_a.gleam",
      "import top\n"
      "pub fn go() { top:go() }"
    },
    {
      "src/middle_b.gleam",
      "import top\n"
      "pub fn go() { top:go() }"
    },
    {
      "src/bottom.gleam",
      "import top\n"
      "import middle_a\n"
      "import middle_b\n"
      "pub fn go() { top:go() + middle_a:go() + middle_b:go() }"
    }
  ],
  {ok, #{"top" := #compiled_module{} = TopMod,
         "middle_a" := #compiled_module{},
         "middle_b" := #compiled_module{},
         "bottom" := #compiled_module{}}} = gleam_compiler:compile_all(Mods, #{}, []),
  ?assertEqual("src/top.gleam", TopMod#compiled_module.source_path).


compile_all_fail_test() ->
  Mods = [
    {
      "src/top.gleam",
      "pub fn go() { 1 }"
    },
    {
      "src/bottom.gleam",
      "import top\n"
      "pub fn go() { top:go() + 'two' }"
    }
  ],
  {error, {"bottom",
           "error: Type mismatch" ++ _,
           #{"top" := #compiled_module{}}}} = gleam_compiler:compile_all(Mods, #{}, []).
