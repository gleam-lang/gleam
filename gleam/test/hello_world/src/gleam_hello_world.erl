-module(hello_world).

-export([rev/1]).
rev(A) ->
    lists:reverse(A).

-export([greet/1]).
greet(Name) ->
    case Name of
        <<"your mate Dave">> ->
            [<<"Oi! Dave! What are you doing here? Go on. Clear off. Haven't you got better things to do?">> | []];

        Name1 ->
            [<<"Hello, ">> | [Name1 | [<<"!">> | []]]]
    end.

-export([list/0]).
list() ->
    case 1 of
        2 ->
            <<"ok">>;

        Name ->
            123123123,
            <<"one two three. one two three. one two three. one two three. one two three.">>
    end.

-export([x/0]).
x() ->
    {'ok', 2}.

-export([run/0]).
run() ->
    X = 1,
    X1 = 2 + 3,
    X1.
