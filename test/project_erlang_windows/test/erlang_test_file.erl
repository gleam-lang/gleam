-module(erlang_test_file).

-export([main/0]).

-include("erlang_test_header.hrl").

main() ->
  String = header_function(),
  <<"Hello, from the Erlang test module!\n"/utf8, String/binary>>.
