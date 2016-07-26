-module(gleam_codegen_test).
-include_lib("eunit/include/eunit.hrl").

unload(Name) ->
  code:purge(Name),
  ?assert(code:delete(Name)).

load(Name, Path, Binary) ->
  {module, Name} = code:load_binary(Name, Path, Binary),
  ?assertEqual({file, Path}, code:is_loaded(Name)).

empty_module_test() ->
  Source = "module codegen_empty_mod",
  Binary = gleam_compiler:source_to_binary(Source),
  load(codegen_empty_mod, "codegen_empty_mod.beam", Binary),
  unload(codegen_empty_mod).

% pub_fun_module_test() ->
%   Source = "module codegen_pub_fun_mod\n"
%            "public id { def (x) { x } }",
%   Binary = gleam_compiler:source_to_binary(Source),
%   load(codegen_pub_fun_mod, "codegen_pub_fun_mod.beam", Binary),
%   ?assert(erlang:function_exported(codegen_pub_fun_mod, id, 1)),
%   ?assertEqual(ok, codegen_pub_fun_mod:id(ok)),
%   ?assertEqual(123, codegen_pub_fun_mod:id(123)),
%   unload(codegen_pub_fun_mod).
