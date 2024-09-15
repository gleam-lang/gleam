-module(submodule_ffi).

-export([main/0, main2/0]).

-include("../headers/submodule_ffi_header.hrl").

main() ->
  String = header_function(),
  <<"Hello, from the nested Erlang module!\n"/utf8, String/binary>>.

main2() ->
  String = header_function(),
  <<"Hello again, from the nested Erlang module!\n"/utf8, String/binary>>.
