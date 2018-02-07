-module(gleam_codegen_test).
-include_lib("eunit/include/eunit.hrl").
-define(refute(Value), ?assert(not Value)).

with_module(Name, Source, Fun) ->
  FileName = "Gleam." ++ atom_to_list(Name) ++ ".beam",
  BeamBinary = gleam_compiler:source_to_binary(Source),
  {module, Name} = code:load_binary(Name, FileName, BeamBinary),
  ?assertEqual({file, FileName}, code:is_loaded(Name)),
  try Fun() of
    X -> X
  after
    code:purge(Name),
    ?assert(code:delete(Name))
  end.

module_identity_test() ->
  Source =
    "module CodegenModuleIdentity\n"
    "export identity/1\n"
    "let identity(x) = x\n"
    "let hidden(x) = x\n"
  ,
  with_module('Gleam.CodegenModuleIdentity', Source, fun() ->
    ?assert(erlang:function_exported('Gleam.CodegenModuleIdentity', identity, 1)),
    ?refute(erlang:function_exported('Gleam.CodegenModuleIdentity', hidden, 1)),
    ?assertEqual(ok, 'Gleam.CodegenModuleIdentity':identity(ok)),
    ?assertEqual(42, 'Gleam.CodegenModuleIdentity':identity(42)),
    ?assert(erlang:function_exported('Gleam.CodegenModuleIdentity', module_info, 0)),
    ?assert(erlang:function_exported('Gleam.CodegenModuleIdentity', module_info, 1)),
    Info = 'Gleam.CodegenModuleIdentity':module_info(),
    ?assertMatch([{module, 'Gleam.CodegenModuleIdentity'} | _], Info)
  end).

maths_test() ->
  Source =
    "module CodegenMaths\n"
    "export intAdd/2, intSub/2, intMult/2, intDiv/2, floatAdd/2, floatSub/2,\n"
    "       floatMult/2, floatDiv/2\n"
    "let intAdd(x, y) = x + y\n"
    "let intSub(x, y) = x - y\n"
    "let intMult(x, y) = x * y\n"
    "let intDiv(x, y) = x / y\n"
    "let floatAdd(x, y) = x +. y\n"
    "let floatSub(x, y) = x -. y\n"
    "let floatMult(x, y) = x *. y\n"
    "let floatDiv(x, y) = x /. y\n"
  ,
  with_module('Gleam.CodegenMaths', Source, fun() ->
    ?assertEqual(2, 'Gleam.CodegenMaths':intAdd(1, 1)),
    ?assertEqual(9, 'Gleam.CodegenMaths':intSub(10, 1)),
    ?assertEqual(8, 'Gleam.CodegenMaths':intMult(4, 2)),
    ?assertEqual(4, 'Gleam.CodegenMaths':intDiv(9, 2)),
    ?assertEqual(5.1, 'Gleam.CodegenMaths':floatAdd(4.0, 1.1)),
    ?assertEqual(8.4, 'Gleam.CodegenMaths':floatSub(10.0, 1.6)),
    ?assertEqual(7.5, 'Gleam.CodegenMaths':floatMult(3.0, 2.5)),
    ?assertEqual(5.0, 'Gleam.CodegenMaths':floatDiv(10.0, 2.0))
  end).

comparison_test() ->
  Source =
    "module CodegenComparison\n"
    "export lt?/2, lte?/2, gt?/2, gte?/2"
    "let lt?(x, y) = x < y\n"
    "let lte?(x, y) = x <= y\n"
    "let gt?(x, y) = x > y\n"
    "let gte?(x, y) = x >= y\n"
  ,
  with_module('Gleam.CodegenComparison', Source, fun() ->
    ?assert('Gleam.CodegenComparison':'lt?'(1, 2)),
    ?refute('Gleam.CodegenComparison':'lt?'(1, 1)),
    ?refute('Gleam.CodegenComparison':'lt?'(2, 1)),
    ?assert('Gleam.CodegenComparison':'lte?'(1, 2)),
    ?assert('Gleam.CodegenComparison':'lte?'(1, 1)),
    ?refute('Gleam.CodegenComparison':'lte?'(2, 1)),
    ?refute('Gleam.CodegenComparison':'gt?'(1, 2)),
    ?assert('Gleam.CodegenComparison':'gt?'(1, 1)),
    ?assert('Gleam.CodegenComparison':'gt?'(2, 1)),
    ?refute('Gleam.CodegenComparison':'gte?'(1, 2)),
    ?refute('Gleam.CodegenComparison':'gte?'(1, 1)),
    ?assert('Gleam.CodegenComparison':'gte?'(2, 1))
  end).
