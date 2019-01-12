-module(gleam_hello_world).

-export([rev/1, greet/1, list/0, x/0, run/0]).

rev(A) ->
    lists:reverse(A).

greet(Name) ->
    case Name of
        <<"your mate Dave">> ->
            [<<"Oi! Dave! What are you doing here? Go on. Clear off. Haven't you got better things to do?">> | []];

        Name1 ->
            [<<"Hello, ">> | [begin
                1,
                Name1
            end | [<<"!">> | []]]]
    end.

list() ->
    case 1 of
        2 ->
            <<"ok">>;

        Name ->
            123123123,
            <<"one two three. one two three. one two three. one two three. one two three.">>
    end.

x() ->
    {'ok',
     begin
         1,
         2
     end}.

run() ->
    X = 1,
    X1 = 2 + 3,
    X1.
