-module(hello_world).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([]).

x() ->
    1.

-ifdef(TEST).
x_test() ->
    2.
-endif.
