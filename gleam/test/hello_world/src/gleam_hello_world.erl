-module(gleam_hello_world).

-export([hello/0, times/2]).

hello() ->
    <<"Hello, world!">>.

times(I, F) ->
    case I of
        1 ->
            F();

        I1 ->
            F(),
            Times(I1 - 1, F)
    end.
