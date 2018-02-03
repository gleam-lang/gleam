-module(gleam_codegen_test).
-include_lib("eunit/include/eunit.hrl").

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
  "let hidden(x) = x\n",
  with_module('Gleam.CodegenModuleIdentity', Source, fun() ->
    ?assert(erlang:function_exported('Gleam.CodegenModuleIdentity', identity, 1)),
    ?assert(not erlang:function_exported('Gleam.CodegenModuleIdentity', hidden, 1)),
    ?assertEqual(ok, 'Gleam.CodegenModuleIdentity':identity(ok)),
    ?assertEqual(42, 'Gleam.CodegenModuleIdentity':identity(42)),
    ?assert(erlang:function_exported('Gleam.CodegenModuleIdentity', module_info, 0)),
    ?assert(erlang:function_exported('Gleam.CodegenModuleIdentity', module_info, 1)),
    Info = 'Gleam.CodegenModuleIdentity':module_info(),
    ?assertMatch([{module, 'Gleam.CodegenModuleIdentity'} | _], Info)
  end).
