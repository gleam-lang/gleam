-module(other).
-compile(no_auto_import).

-export([create_user/1]).

create_user(UserId) ->
    {UserId, <<"">>, 22}.
