-module(gleeunit_ffi).

-export([find_files/2, should_equal/2, should_not_equal/2, should_be_ok/1,
         should_be_error/1]).

-include_lib("eunit/include/eunit.hrl").

find_files(Pattern, In) ->
  Results = filelib:wildcard(binary_to_list(Pattern), binary_to_list(In)),
  lists:map(fun list_to_binary/1, Results).


should_equal(Actual, Expected) -> 
    ?assertEqual(Expected, Actual),
    nil.
should_not_equal(Actual, Expected) -> 
    ?assertNotEqual(Expected, Actual),
    nil.
should_be_ok(A) -> 
    ?assertMatch({ok, _}, A),
    element(2, A).
should_be_error(A) -> 
    ?assertMatch({error, _}, A),
    element(2, A).
