-module(erlang_file).

-export([main/0]).

-include("erlang_header.hrl").

main() ->
  String = header_function(),
  <<"Hello, from the Erlang module!\n"/utf8, String/binary>>.
