-module(main).
-compile(no_auto_import).

-export([main/1]).

-spec main(fun((binary()) -> binary())) -> integer().
main(Print) ->
    Print(<<"Hello, world!\n"/utf8>>),
    0.
