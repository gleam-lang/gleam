-module(any).
-compile(no_auto_import).

-export([from/1, unsafeCoerce/1]).

from(A) ->
    gleam__stdlib:identity(A).

unsafeCoerce(A) ->
    gleam__stdlib:identity(A).
