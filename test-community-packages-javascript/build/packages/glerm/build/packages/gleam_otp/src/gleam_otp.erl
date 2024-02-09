-module(gleam_otp).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([main/0]).

-spec spawn_task(integer()) -> gleam@otp@task:task(nil).
spawn_task(I) ->
    gleam@otp@task:async(fun() -> case (I rem 500) =:= 0 of
                true ->
                    gleam@io:println(
                        <<"Hello from "/utf8, (gleam@int:to_string(I))/binary>>
                    );

                false ->
                    nil
            end end).

-spec main() -> integer().
main() ->
    gleam@io:debug(
        gleam_otp_test_external:get_message_queue_length(erlang:self())
    ),
    _pipe = gleam@list:range(0, 1000000),
    _pipe@1 = gleam@list:map(_pipe, fun spawn_task/1),
    gleam@list:each(_pipe@1, fun gleam@otp@task:await_forever/1),
    gleam@io:debug(
        gleam_otp_test_external:get_message_queue_length(erlang:self())
    ).
