-module(gleam@otp@intensity_tracker).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([new/2, trim_window/3, add_event/1]).
-export_type([intensity_tracker/0, too_intense/0]).

-opaque intensity_tracker() :: {intensity_tracker,
        integer(),
        integer(),
        list(integer())}.

-type too_intense() :: too_intense.

-spec new(integer(), integer()) -> intensity_tracker().
new(Limit, Period) ->
    {intensity_tracker, Limit, Period, []}.

-spec now_seconds() -> integer().
now_seconds() ->
    erlang:monotonic_time(1).

-spec trim_window(list(integer()), integer(), integer()) -> list(integer()).
trim_window(Events, Now, Period) ->
    case Events of
        [] ->
            [];

        [Event | Events@1] ->
            case Now >= (Event + Period) of
                true ->
                    [Event | trim_window(Events@1, Now, Period)];

                false ->
                    []
            end
    end.

-spec add_event(intensity_tracker()) -> {ok, intensity_tracker()} |
    {error, too_intense()}.
add_event(Tracker) ->
    Now = now_seconds(),
    Events = trim_window(
        [Now | erlang:element(4, Tracker)],
        Now,
        erlang:element(3, Tracker)
    ),
    case gleam@list:length(Events) >= erlang:element(2, Tracker) of
        true ->
            {error, too_intense};

        false ->
            {ok, erlang:setelement(4, Tracker, Events)}
    end.
