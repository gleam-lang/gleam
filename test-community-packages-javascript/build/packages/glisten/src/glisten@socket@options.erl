-module(glisten@socket@options).
-compile([no_auto_import, nowarn_unused_vars]).

-export([to_map/1, merge_with_defaults/1]).
-export_type([socket_mode/0, active_state/0, tcp_option/0]).

-type socket_mode() :: binary.

-type active_state() :: once | passive | {count, integer()} | active.

-type tcp_option() :: {backlog, integer()} |
    {nodelay, boolean()} |
    {linger, {boolean(), integer()}} |
    {send_timeout, integer()} |
    {send_timeout_close, boolean()} |
    {reuseaddr, boolean()} |
    {active_mode, active_state()} |
    {mode, socket_mode()} |
    {certfile, binary()} |
    {keyfile, binary()} |
    {alpn_preferred_protocols, list(binary())}.

-spec to_map(list(tcp_option())) -> gleam@map:map_(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic()).
to_map(Options) ->
    Opt_decoder = gleam@dynamic:tuple2(
        fun gleam@dynamic:dynamic/1,
        fun gleam@dynamic:dynamic/1
    ),
    _pipe = Options,
    _pipe@1 = gleam@list:map(_pipe, fun(Opt) -> case Opt of
                {active_mode, passive} ->
                    gleam@dynamic:from(
                        {erlang:binary_to_atom(<<"active"/utf8>>), false}
                    );

                {active_mode, active} ->
                    gleam@dynamic:from(
                        {erlang:binary_to_atom(<<"active"/utf8>>), true}
                    );

                {active_mode, {count, N}} ->
                    gleam@dynamic:from(
                        {erlang:binary_to_atom(<<"active"/utf8>>), N}
                    );

                {active_mode, once} ->
                    gleam@dynamic:from(
                        {erlang:binary_to_atom(<<"active"/utf8>>),
                            erlang:binary_to_atom(<<"once"/utf8>>)}
                    );

                Other ->
                    gleam@dynamic:from(Other)
            end end),
    _pipe@2 = gleam@list:filter_map(_pipe@1, Opt_decoder),
    _pipe@3 = gleam@list:map(
        _pipe@2,
        fun(_capture) ->
            gleam@pair:map_first(_capture, fun gleam@dynamic:unsafe_coerce/1)
        end
    ),
    gleam@map:from_list(_pipe@3).

-spec merge_with_defaults(list(tcp_option())) -> list(tcp_option()).
merge_with_defaults(Options) ->
    Overrides = to_map(Options),
    _pipe = [{backlog, 1024},
        {nodelay, true},
        {linger, {true, 30}},
        {send_timeout, 30000},
        {send_timeout_close, true},
        {reuseaddr, true},
        {mode, binary},
        {active_mode, passive}],
    _pipe@1 = to_map(_pipe),
    _pipe@2 = gleam@map:merge(_pipe@1, Overrides),
    _pipe@3 = gleam@map:to_list(_pipe@2),
    _pipe@4 = gleam@list:map(_pipe@3, fun gleam@dynamic:from/1),
    gleam@list:map(_pipe@4, fun gleam@dynamic:unsafe_coerce/1).
