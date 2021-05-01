-module(main).
-compile(no_auto_import).

-export([main/1, int_tests/0]).

-spec main(fun((binary()) -> binary())) -> integer().
main(Print) ->
    Print(<<"\nHere we go!\n\n"/utf8>>),
    Stats = test:run([test:suite(<<"int"/utf8>>, int_tests())], Print),
    case erlang:element(3, Stats) of
        0 ->
            0;

        _ ->
            1
    end.

-spec int_tests() -> list(test:test()).
int_tests() ->
    [test:suite(
         <<"addition"/utf8>>,
         [test:test_equal(<<"0 + 0"/utf8>>, 0, 0 + 0),
          test:test_equal(<<"1 + 1"/utf8>>, 2, 1 + 1),
          test:test_equal(<<"5 + 1"/utf8>>, 6, 5 + 1),
          test:test_equal(<<"1 + 3"/utf8>>, 4, 1 + 3),
          test:test_equal(<<"1 + -3"/utf8>>, -2, 1 + -3)]
     ),
     test:suite(
         <<"subtraction"/utf8>>,
         [test:test_equal(<<"0 - 0"/utf8>>, 0, 0 - 0),
          test:test_equal(<<"1 - 1"/utf8>>, 0, 1 - 1),
          test:test_equal(<<"5 - 1"/utf8>>, 4, 5 - 1),
          test:test_equal(<<"1 - 3"/utf8>>, -2, 1 - 3),
          test:test_equal(<<"1 - -3"/utf8>>, 4, 1 - -3)]
     )].
