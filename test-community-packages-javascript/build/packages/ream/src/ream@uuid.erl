-module(ream@uuid).
-compile([no_auto_import, nowarn_unused_vars]).

-export([to_int/1, from_int/1, from_string/1, parts/1, to_string/1, new/0]).

-spec to_int(bitstring()) -> integer().
to_int(Uuid) ->
    <<Uuid_int:128>> = Uuid,
    Uuid_int.

-spec from_int(integer()) -> bitstring().
from_int(Uuid) ->
    <<Uuid:128>>.

-spec from_string(binary()) -> bitstring().
from_string(Uuid) ->
    _assert_subject = begin
        _pipe = Uuid,
        _pipe@1 = gleam@string:replace(_pipe, <<"-"/utf8>>, <<""/utf8>>),
        gleam@int:base_parse(_pipe@1, 16)
    end,
    {ok, Uuid_int} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"ream/uuid"/utf8>>,
                        function => <<"from_string"/utf8>>,
                        line => 28})
    end,
    from_int(Uuid_int).

-spec to_hex(integer(), integer()) -> binary().
to_hex(N, Size) ->
    _pipe = gleam@int:to_base16(N),
    _pipe@1 = gleam@string:lowercase(_pipe),
    gleam@string:pad_left(_pipe@1, Size, <<"0"/utf8>>).

-spec parts(bitstring()) -> list(binary()).
parts(Uuid) ->
    <<P1:32, P2:16, P3:16, P4:16, P5:48>> = Uuid,
    [to_hex(P1, 8), to_hex(P2, 4), to_hex(P3, 4), to_hex(P4, 4), to_hex(P5, 12)].

-spec to_string(bitstring()) -> binary().
to_string(Uuid) ->
    _pipe = parts(Uuid),
    gleam@string:join(_pipe, <<"-"/utf8>>).

-spec new() -> bitstring().
new() ->
    <<U0:48, _:4, U1:12, _:2, U2:62>> = crypto:strong_rand_bytes(16),
    <<U0:48, 4:4, U1:12, 2:2, U2:62>>.
