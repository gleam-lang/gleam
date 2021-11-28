-module(gleam_stdlib).
-include_lib("eunit/include/eunit.hrl").

-export([should_equal/2, should_not_equal/2, should_be_ok/1, should_be_error/1,
         map_get/2, iodata_append/2, identity/1, decode_int/1, decode_bool/1,
         decode_float/1, decode_thunk/1, decode_list/1, decode_option/2,
         decode_field/2, parse_int/1, parse_float/1, less_than/2,
         string_pop_grapheme/1, string_starts_with/2, wrap_list/1,
         string_ends_with/2, string_pad/4, decode_map/1, uri_parse/1,
         bit_string_int_to_u32/1, bit_string_int_from_u32/1, decode_result/1,
         bit_string_slice/3, decode_bit_string/1, compile_regex/2, regex_scan/2,
         percent_encode/1, percent_decode/1, regex_check/2, regex_split/2,
         base_decode64/1, parse_query/1, bit_string_concat/1, size_of_tuple/1,
         decode_tuple/1, tuple_get/2, classify_dynamic/1]).

%% Taken from OTP's uri_string module
-define(DEC2HEX(X),
        if ((X) >= 0) andalso ((X) =< 9) -> (X) + $0;
           ((X) >= 10) andalso ((X) =< 15) -> (X) + $A - 10
        end).

%% Taken from OTP's uri_string module
-define(HEX2DEC(X),
        if ((X) >= $0) andalso ((X) =< $9) -> (X) - $0;
           ((X) >= $A) andalso ((X) =< $F) -> (X) - $A + 10;
           ((X) >= $a) andalso ((X) =< $f) -> (X) - $a + 10
        end).

should_equal(Actual, Expected) -> 
    ?assertEqual(Expected, Actual),
    nil.
should_not_equal(Actual, Expected) -> 
    ?assertNotEqual(Expected, Actual),
    nil.
should_be_ok(A) -> 
    ?assertMatch({ok, _}, A),
    nil.
should_be_error(A) -> 
    ?assertMatch({error, _}, A),
    nil.

map_get(Map, Key) ->
    case maps:find(Key, Map) of
        error -> {error, nil};
        OkFound -> OkFound
    end.

iodata_append(Iodata, String) -> [Iodata, String].

identity(X) -> X.

decode_error_msg(Expected, Data) ->
    {error, {decode_error, Expected, classify_dynamic(Data)}}.

classify_dynamic(X) when is_atom(X) -> <<"Atom">>;
classify_dynamic(X) when is_binary(X) -> <<"String">>;
classify_dynamic(X) when is_bitstring(X) -> <<"BitString">>;
classify_dynamic(X) when is_integer(X) -> <<"Int">>;
classify_dynamic(X) when is_float(X) -> <<"Float">>;
classify_dynamic(X) when is_list(X) -> <<"List">>;
classify_dynamic(X) when is_boolean(X) -> <<"Bool">>;
classify_dynamic(X) when is_map(X) -> <<"Map">>;
classify_dynamic(X) when is_tuple(X) -> 
    iolist_to_binary(["Tuple of ", integer_to_list(tuple_size(X)), " elements"]);
classify_dynamic(X) when 
    is_function(X, 0) orelse is_function(X, 1) orelse is_function(X, 2) orelse
    is_function(X, 3) orelse is_function(X, 4) orelse is_function(X, 5) orelse
    is_function(X, 6) orelse is_function(X, 7) orelse is_function(X, 8) orelse
    is_function(X, 9) orelse is_function(X, 10) orelse is_function(X, 11) orelse
    is_function(X, 12) -> <<"Function">>;
classify_dynamic(_) -> <<"Some other type">>.

decode_map(Data) when is_map(Data) -> {ok, Data};
decode_map(Data) -> decode_error_msg(<<"Map">>, Data).

decode_bit_string(Data) when is_bitstring(Data) -> {ok, Data};
decode_bit_string(Data) -> decode_error_msg(<<"BitString">>, Data).

decode_int(Data) when is_integer(Data) -> {ok, Data};
decode_int(Data) -> decode_error_msg(<<"Int">>, Data).

decode_float(Data) when is_float(Data) -> {ok, Data};
decode_float(Data) -> decode_error_msg(<<"Float">>, Data).

decode_bool(Data) when is_boolean(Data) -> {ok, Data};
decode_bool(Data) -> decode_error_msg(<<"Bool">>, Data).

decode_thunk(Data) when is_function(Data, 0) -> {ok, Data};
decode_thunk(Data) -> decode_error_msg("zero arity function", Data).

decode_list(Data) when is_list(Data) -> {ok, Data};
decode_list(Data) -> decode_error_msg(<<"List">>, Data).

decode_field(Data, Key) ->
    case Data of
        #{Key := Value} -> {ok, Value};
        _ -> decode_error_msg(io_lib:format("Value with field `~p`", [Key]), Data)
    end.

size_of_tuple(Data) -> tuple_size(Data).

tuple_get(_tup, Index) when Index < 0 -> {error, nil};
tuple_get(Data, Index) when Index >= tuple_size(Data) -> {error, nil};
tuple_get(Data, Index) -> {ok, element(Index + 1, Data)}.

decode_tuple(Data) when is_tuple(Data) -> {ok, Data};
decode_tuple(Data) -> decode_error_msg(<<"Tuple">>, Data).

decode_option(Term, F) ->
    Decode = fun(Inner) ->
        case F(Inner) of
            {ok, Decoded} -> {ok, {some, Decoded}};
            Error -> Error
        end
    end,
    case Term of
        undefined -> {ok, none};
        error -> {ok, none};
        null -> {ok, none};
        none -> {ok, none};
        nil -> {ok, none};
        {some, Inner} -> Decode(Inner);
        _ -> Decode(Term)
    end.

decode_result(Term) ->
    case Term of
        {ok, Inner} -> {ok, {ok, Inner}};
        ok -> {ok, {ok, nil}};
        {error, Inner} -> {ok, {error, Inner}};
        error -> {ok, {error, nil}};
        _ -> decode_error_msg(<<"Result">>, Term)
    end.

parse_int(String) ->
    case catch binary_to_integer(String) of
        Int when is_integer(Int) -> {ok, Int};
        _ -> {error, nil}
    end.

parse_float(String) ->
    case catch binary_to_float(String) of
        Float when is_float(Float) -> {ok, Float};
        _ -> {error, nil}
    end.

less_than(Lhs, Rhs) ->
    Lhs < Rhs.

string_starts_with(_, <<>>) -> true;
string_starts_with(String, Prefix) when byte_size(Prefix) > byte_size(String) -> false;
string_starts_with(String, Prefix) ->
    PrefixSize = byte_size(Prefix),
    Prefix == binary_part(String, 0, PrefixSize).

string_ends_with(_, <<>>) -> true;
string_ends_with(String, Suffix) when byte_size(Suffix) > byte_size(String) -> false;
string_ends_with(String, Suffix) ->
    SuffixSize = byte_size(Suffix),
    Suffix == binary_part(String, byte_size(String) - SuffixSize, SuffixSize).

string_pad(String, Length, Dir, PadString) ->
    Chars = string:pad(String, Length, Dir, binary_to_list(PadString)),
    case unicode:characters_to_binary(Chars) of
        Bin when is_binary(Bin) -> Bin;
        Error -> erlang:error({gleam_error, {string_invalid_utf8, Error}})
    end.

string_pop_grapheme(String) ->
    case string:next_grapheme(String) of
        [ Next | Rest ] ->
            {ok, {unicode:characters_to_binary([Next]), unicode:characters_to_binary(Rest)}};
        _ -> {error, nil}
    end.

bit_string_concat(BitStrings) ->
    iolist_to_binary(BitStrings).

bit_string_slice(Bin, Pos, Len) ->
    try {ok, binary:part(Bin, Pos, Len)}
    catch error:badarg -> {error, nil}
    end.

bit_string_int_to_u32(I) when 0 =< I, I < 4294967296 ->
    {ok, <<I:32>>};
bit_string_int_to_u32(_) ->
    {error, nil}.

bit_string_int_from_u32(<<I:32>>) ->
    {ok, I};
bit_string_int_from_u32(_) ->
    {error, nil}.

compile_regex(String, Options) ->
    {options, Caseless, Multiline} = Options,
    OptionsList = [
        unicode,
        Caseless andalso caseless,
        Multiline andalso multiline
    ],
    FilteredOptions = [Option || Option <- OptionsList, Option /= false],
    case re:compile(String, FilteredOptions) of
        {ok, MP} -> {ok, MP};
        {error, {Str, Pos}} ->
            {error, {compile_error, unicode:characters_to_binary(Str), Pos}}
    end.

regex_check(Regex, String) ->
    re:run(String, Regex) /= nomatch.

regex_split(Regex, String) ->
    re:split(String, Regex).

regex_submatches(String, {S, L}) ->
    SubMatch = string:slice(String, S, L),
    case string:is_empty(SubMatch) of
        true -> none;
        false -> {some, SubMatch}
    end.

regex_matches(String, [{S, L} | Submatches]) ->
    Submatches1 = lists:map(fun(X) -> regex_submatches(String, X) end, Submatches),
    {match, binary:part(String, S, L), Submatches1}.

regex_scan(Regex, String) ->
    case re:run(String, Regex, [global]) of
        {match, Captured} -> lists:map(fun(X) -> regex_matches(String, X) end, Captured);
        nomatch -> []
    end.

base_decode64(S) ->
    try {ok, base64:decode(S)}
    catch error:badarith -> {error, nil}
    end.

wrap_list(X) when is_list(X) -> X;
wrap_list(X) -> [X].

parse_query(Query) ->
    case uri_string:dissect_query(Query) of
        {error, _, _} -> {error, nil};
        Pairs -> 
            Pairs1 = lists:map(fun
                ({K, true}) -> {K, <<"">>};
                (Pair) -> Pair
            end, Pairs),
            {ok, Pairs1}
    end.

percent_encode(B) -> percent_encode(B, <<>>).
percent_encode(<<>>, Acc) ->
    Acc;
percent_encode(<<H,T/binary>>, Acc) ->
    case percent_ok(H) of
        true ->
            percent_encode(T, <<Acc/binary,H>>);
        false ->
            <<A:4,B:4>> = <<H>>,
            percent_encode(T, <<Acc/binary,$%,(?DEC2HEX(A)),(?DEC2HEX(B))>>)
    end.

percent_decode(Cs) -> percent_decode(Cs, <<>>).
percent_decode(<<$%, C0, C1, Cs/binary>>, Acc) ->
    case is_hex_digit(C0) andalso is_hex_digit(C1) of
        true ->
            B = ?HEX2DEC(C0)*16+?HEX2DEC(C1),
            percent_decode(Cs, <<Acc/binary, B>>);
        false ->
            {error, nil}
    end;
percent_decode(<<C,Cs/binary>>, Acc) ->
    percent_decode(Cs, <<Acc/binary, C>>);
percent_decode(<<>>, Acc) ->
    check_utf8(Acc).

percent_ok($!) -> true;
percent_ok($$) -> true;
percent_ok($') -> true;
percent_ok($() -> true;
percent_ok($)) -> true;
percent_ok($*) -> true;
percent_ok($+) -> true;
percent_ok($-) -> true;
percent_ok($.) -> true;
percent_ok($_) -> true;
percent_ok($~) -> true;
percent_ok(C) when $0 =< C, C =< $9 -> true;
percent_ok(C) when $A =< C, C =< $Z -> true;
percent_ok(C) when $a =< C, C =< $z -> true;
percent_ok(_) -> false.

is_hex_digit(C) ->
  ($0 =< C andalso C =< $9) orelse ($a =< C andalso C =< $f) orelse ($A =< C andalso C =< $F).

check_utf8(Cs) ->
    case unicode:characters_to_list(Cs) of
        {incomplete, _, _} -> {error, nil};
        {error, _, _} -> {error, nil};
        _ -> {ok, Cs}
    end.

uri_parse(String) ->
    case uri_string:parse(String) of
        {error, _, _} -> {error, nil};
        Uri ->
        % #{
        %     host := Host, path := Path, port := Port, query := Query, 
        %     scheme := Scheme, userinfo := Userinfo
        % } ->
    % scheme: Option(String),
    % userinfo: Option(String),
    % host: Option(String),
    % port: Option(Int),
    % path: String,
    % query: Option(String),
    % fragment: Option(String),
            {ok, {uri,
                maps_get_optional(Uri, scheme),
                maps_get_optional(Uri, userinfo),
                maps_get_optional(Uri, host),
                maps_get_optional(Uri, port),
                maps_get_or(Uri, path, <<>>), 
                maps_get_optional(Uri, query),
                maps_get_optional(Uri, fragment)
            }}
    end.

maps_get_optional(Map, Key) ->
    try {some, maps:get(Key, Map)}
    catch _:_ -> none
    end.

maps_get_or(Map, Key, Default) ->
    try maps:get(Key, Map)
    catch _:_ -> Default
    end.
