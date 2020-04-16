-module(gleam_should).
-include_lib("eunit/include/eunit.hrl").

-export([should_equal/2, should_not_equal/2]).

should_equal(Actual, Expected) -> ?assertEqual(Expected, Actual).
should_not_equal(Actual, Expected) -> ?assertNotEqual(Expected, Actual).

