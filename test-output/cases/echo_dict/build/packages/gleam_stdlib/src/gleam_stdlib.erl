-module(gleam_stdlib).

-export([
    map_get/2, iodata_append/2, identity/1, decode_int/1, decode_bool/1,
    decode_float/1, decode_list/1, decode_option/2, decode_field/2, parse_int/1,
    parse_float/1, less_than/2, string_pop_grapheme/1, string_starts_with/2,
    wrap_list/1, string_ends_with/2, string_pad/4, decode_map/1, uri_parse/1,
    bit_array_int_to_u32/1, bit_array_int_from_u32/1, decode_result/1,
    bit_array_slice/3, decode_bit_array/1, compile_regex/2, regex_scan/2,
    percent_encode/1, percent_decode/1, regex_check/2, regex_split/2,
    base_decode64/1, parse_query/1, bit_array_concat/1,
    bit_array_base64_encode/2, size_of_tuple/1,
    decode_tuple/1, decode_tuple2/1, decode_tuple3/1, decode_tuple4/1,
    decode_tuple5/1, decode_tuple6/1, tuple_get/2, classify_dynamic/1, print/1,
    println/1, print_error/1, println_error/1, inspect/1, float_to_string/1,
    int_from_base_string/2, utf_codepoint_list_to_string/1, contains_string/2,
    crop_string/2, base16_decode/1, string_replace/3, regex_replace/3, slice/3, bit_array_to_int_and_size/1
]).

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

-define(is_lowercase_char(X), (X > 96 andalso X < 123)).
-define(is_underscore_char(X), (X == 95)).
-define(is_digit_char(X), (X > 47 andalso X < 58)).

uppercase(X) -> X - 32.

map_get(Map, Key) ->
    case maps:find(Key, Map) of
        error -> {error, nil};
        OkFound -> OkFound
    end.

iodata_append(Iodata, String) -> [Iodata, String].

identity(X) -> X.

decode_error_msg(Expected, Data) when is_binary(Expected) ->
    decode_error(Expected, classify_dynamic(Data)).
decode_error(Expected, Got) when is_binary(Expected) andalso is_binary(Got) ->
    {error, [{decode_error, Expected, Got, []}]}.

classify_dynamic(nil) -> <<"Nil">>;
classify_dynamic(X) when is_boolean(X) -> <<"Bool">>;
classify_dynamic(X) when is_atom(X) -> <<"Atom">>;
classify_dynamic(X) when is_binary(X) -> <<"String">>;
classify_dynamic(X) when is_bitstring(X) -> <<"BitArray">>;
classify_dynamic(X) when is_integer(X) -> <<"Int">>;
classify_dynamic(X) when is_float(X) -> <<"Float">>;
classify_dynamic(X) when is_list(X) -> <<"List">>;
classify_dynamic(X) when is_map(X) -> <<"Dict">>;
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
decode_map(Data) -> decode_error_msg(<<"Dict">>, Data).

decode_bit_array(Data) when is_bitstring(Data) -> {ok, Data};
decode_bit_array(Data) -> decode_error_msg(<<"BitArray">>, Data).

decode_int(Data) when is_integer(Data) -> {ok, Data};
decode_int(Data) -> decode_error_msg(<<"Int">>, Data).

decode_float(Data) when is_float(Data) -> {ok, Data};
decode_float(Data) -> decode_error_msg(<<"Float">>, Data).

decode_bool(Data) when is_boolean(Data) -> {ok, Data};
decode_bool(Data) -> decode_error_msg(<<"Bool">>, Data).

decode_list(Data) when is_list(Data) -> {ok, Data};
decode_list(Data) -> decode_error_msg(<<"List">>, Data).

decode_field(Data, Key) when is_map(Data) ->
    case Data of
        #{Key := Value} -> {ok, {some, Value}};
        _ ->
            {ok, none}
    end;
decode_field(Data, _) ->
    decode_error_msg(<<"Dict">>, Data).

size_of_tuple(Data) -> tuple_size(Data).

tuple_get(_tup, Index) when Index < 0 -> {error, nil};
tuple_get(Data, Index) when Index >= tuple_size(Data) -> {error, nil};
tuple_get(Data, Index) -> {ok, element(Index + 1, Data)}.

decode_tuple(Data) when is_tuple(Data) -> {ok, Data};
decode_tuple(Data) -> decode_error_msg(<<"Tuple">>, Data).

decode_tuple2({_,_} = A) -> {ok, A};
decode_tuple2([A,B]) -> {ok, {A,B}};
decode_tuple2(Data) -> decode_error_msg(<<"Tuple of 2 elements">>, Data).

decode_tuple3({_,_,_} = A) -> {ok, A};
decode_tuple3([A,B,C]) -> {ok, {A,B,C}};
decode_tuple3(Data) -> decode_error_msg(<<"Tuple of 3 elements">>, Data).

decode_tuple4({_,_,_,_} = A) -> {ok, A};
decode_tuple4([A,B,C,D]) -> {ok, {A,B,C,D}};
decode_tuple4(Data) -> decode_error_msg(<<"Tuple of 4 elements">>, Data).

decode_tuple5({_,_,_,_,_} = A) -> {ok, A};
decode_tuple5([A,B,C,D,E]) -> {ok, {A,B,C,D,E}};
decode_tuple5(Data) -> decode_error_msg(<<"Tuple of 5 elements">>, Data).

decode_tuple6({_,_,_,_,_,_} = A) -> {ok, A};
decode_tuple6([A,B,C,D,E,F]) -> {ok, {A,B,C,D,E,F}};
decode_tuple6(Data) -> decode_error_msg(<<"Tuple of 6 elements">>, Data).

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

int_from_base_string(String, Base) ->
    case catch binary_to_integer(String, Base) of
        Int when is_integer(Int) -> {ok, Int};
        _ -> {error, nil}
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
        [ Next | Rest ] when is_binary(Rest) ->
            {ok, {unicode:characters_to_binary([Next]), Rest}};

        [ Next | Rest ]  ->
            {ok, {unicode:characters_to_binary([Next]), unicode:characters_to_binary(Rest)}};

        _ -> {error, nil}
    end.

bit_array_concat(BitArrays) ->
    list_to_bitstring(BitArrays).

-if(?OTP_RELEASE >= 26).
bit_array_base64_encode(Bin, Padding) ->
    base64:encode(Bin, #{padding => Padding}).
-else.
bit_array_base64_encode(_Bin, _Padding) ->
    erlang:error(<<"Erlang OTP/26 or higher is required to use base64:encode">>).
-endif.

bit_array_slice(Bin, Pos, Len) ->
    try {ok, binary:part(Bin, Pos, Len)}
    catch error:badarg -> {error, nil}
    end.

bit_array_int_to_u32(I) when 0 =< I, I < 4294967296 ->
    {ok, <<I:32>>};
bit_array_int_to_u32(_) ->
    {error, nil}.

bit_array_int_from_u32(<<I:32>>) ->
    {ok, I};
bit_array_int_from_u32(_) ->
    {error, nil}.

compile_regex(String, Options) ->
    {options, Caseless, Multiline} = Options,
    OptionsList = [
        unicode,
        ucp,
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

regex_submatches(_, {-1, 0}) -> none;
regex_submatches(String, {Start, Length}) ->
    BinarySlice = binary:part(String, {Start, Length}),
    case string:is_empty(binary_to_list(BinarySlice)) of
        true -> none;
        false -> {some, BinarySlice}
    end.

regex_matches(String, [{Start, Length} | Submatches]) ->
    Submatches1 = lists:map(fun(X) -> regex_submatches(String, X) end, Submatches),
    {match, binary:part(String, Start, Length), Submatches1}.

regex_scan(Regex, String) ->
    case re:run(String, Regex, [global]) of
        {match, Captured} -> lists:map(fun(X) -> regex_matches(String, X) end, Captured);
        nomatch -> []
    end.

regex_replace(Regex, Subject, Replacement) ->
    re:replace(Subject, Regex, Replacement, [global, {return, binary}]).

base_decode64(S) ->
    try {ok, base64:decode(S)}
    catch error:_ -> {error, nil}
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

print(String) ->
    io:put_chars(String),
    nil.

println(String) ->
    io:put_chars([String, $\n]),
    nil.

print_error(String) ->
    io:put_chars(standard_error, String),
    nil.

println_error(String) ->
    io:put_chars(standard_error, [String, $\n]),
    nil.

inspect(true) ->
    "True";
inspect(false) ->
    "False";
inspect(nil) ->
    "Nil";
inspect(Data) when is_map(Data) ->
    Fields = [
        [<<"#(">>, inspect(Key), <<", ">>, inspect(Value), <<")">>]
        || {Key, Value} <- maps:to_list(Data)
    ],
    ["dict.from_list([", lists:join(", ", Fields), "])"];
inspect(Atom) when is_atom(Atom) ->
    Binary = erlang:atom_to_binary(Atom),
    case inspect_maybe_gleam_atom(Binary, none, <<>>) of
        {ok, Inspected} -> Inspected;
        {error, _} -> ["atom.create_from_string(\"", Binary, "\")"]
	end;
inspect(Any) when is_integer(Any) ->
    erlang:integer_to_list(Any);
inspect(Any) when is_float(Any) ->
    io_lib_format:fwrite_g(Any);
inspect(Binary) when is_binary(Binary) ->
    case inspect_maybe_utf8_string(Binary, <<>>) of
        {ok, InspectedUtf8String} -> InspectedUtf8String;
        {error, not_a_utf8_string} ->
            Segments = [erlang:integer_to_list(X) || <<X>> <= Binary],
            ["<<", lists:join(", ", Segments), ">>"]
    end;
inspect(Bits) when is_bitstring(Bits) ->
    inspect_bit_array(Bits);
inspect(List) when is_list(List) ->
    case inspect_list(List) of
        {proper, Elements} -> ["[", Elements, "]"];
        {improper, Elements} -> ["//erl([", Elements, "])"]
    end;
inspect(Any) when is_tuple(Any) % Record constructors
  andalso is_atom(element(1, Any))
  andalso element(1, Any) =/= false
  andalso element(1, Any) =/= true
  andalso element(1, Any) =/= nil
->
    [Atom | ArgsList] = erlang:tuple_to_list(Any),
    Args = lists:join(<<", ">>,
        lists:map(fun inspect/1, ArgsList)
    ),
    [inspect(Atom), "(", Args, ")"];
inspect(Tuple) when is_tuple(Tuple) ->
    Elements = lists:map(fun inspect/1, erlang:tuple_to_list(Tuple)),
    ["#(", lists:join(", ", Elements), ")"];
inspect(Any) when is_function(Any) ->
    {arity, Arity} = erlang:fun_info(Any, arity),
    ArgsAsciiCodes = lists:seq($a, $a + Arity - 1),
    Args = lists:join(<<", ">>,
        lists:map(fun(Arg) -> <<Arg>> end, ArgsAsciiCodes)
    ),
    ["//fn(", Args, ") { ... }"];
inspect(Any) ->
    ["//erl(", io_lib:format("~p", [Any]), ")"].


inspect_maybe_gleam_atom(<<>>, none, _) ->
    {error, nil};
inspect_maybe_gleam_atom(<<First, _Rest/binary>>, none, _) when ?is_digit_char(First) ->
    {error, nil};
inspect_maybe_gleam_atom(<<"_", _Rest/binary>>, none, _) ->
    {error, nil};
inspect_maybe_gleam_atom(<<"_">>, _PrevChar, _Acc) ->
    {error, nil};
inspect_maybe_gleam_atom(<<"_",  _Rest/binary>>, $_, _Acc) ->
    {error, nil};
inspect_maybe_gleam_atom(<<First, _Rest/binary>>, _PrevChar, _Acc)
    when not (?is_lowercase_char(First) orelse ?is_underscore_char(First) orelse ?is_digit_char(First)) ->
    {error, nil};
inspect_maybe_gleam_atom(<<First, Rest/binary>>, none, Acc) ->
    inspect_maybe_gleam_atom(Rest, First, <<Acc/binary, (uppercase(First))>>);
inspect_maybe_gleam_atom(<<"_", Rest/binary>>, _PrevChar, Acc) ->
    inspect_maybe_gleam_atom(Rest, $_, Acc);
inspect_maybe_gleam_atom(<<First, Rest/binary>>, $_, Acc) ->
    inspect_maybe_gleam_atom(Rest, First, <<Acc/binary, (uppercase(First))>>);
inspect_maybe_gleam_atom(<<First, Rest/binary>>, _PrevChar, Acc) ->
    inspect_maybe_gleam_atom(Rest, First, <<Acc/binary, First>>);
inspect_maybe_gleam_atom(<<>>, _PrevChar, Acc) ->
    {ok, Acc};
inspect_maybe_gleam_atom(A, B, C) ->
    erlang:display({A, B, C}),
    throw({gleam_error, A, B, C}).

inspect_list([]) ->
    {proper, []};
inspect_list([First]) ->
    {proper, [inspect(First)]};
inspect_list([First | Rest]) when is_list(Rest) ->
    {Kind, Inspected} = inspect_list(Rest),
    {Kind, [inspect(First), <<", ">> | Inspected]};
inspect_list([First | ImproperTail]) ->
    {improper, [inspect(First), <<" | ">>, inspect(ImproperTail)]}.

inspect_bit_array(Bits) ->
    Text = inspect_bit_array(Bits, <<"<<">>),
    <<Text/binary, ">>">>.

inspect_bit_array(<<>>, Acc) ->
    Acc;
inspect_bit_array(<<X, Rest/bitstring>>, Acc) ->
    inspect_bit_array(Rest, append_segment(Acc, erlang:integer_to_binary(X)));
inspect_bit_array(Rest, Acc) ->
    Size = bit_size(Rest),
    <<X:Size>> = Rest,
    X1 = erlang:integer_to_binary(X),
    Size1 = erlang:integer_to_binary(Size),
    Segment = <<X1/binary, ":size(", Size1/binary, ")">>,
    inspect_bit_array(<<>>, append_segment(Acc, Segment)).

bit_array_to_int_and_size(A) ->
    Size = bit_size(A),
    <<A1:Size>> = A,
    {A1, Size}.

append_segment(<<"<<">>, Segment) ->
    <<"<<", Segment/binary>>;
append_segment(Acc, Segment) ->
    <<Acc/binary, ", ", Segment/binary>>.


inspect_maybe_utf8_string(Binary, Acc) ->
    case Binary of
        <<>> -> {ok, <<$", Acc/binary, $">>};
        <<First/utf8, Rest/binary>> ->
            Escaped = case First of
                $" -> <<$\\, $">>;
                $\\ -> <<$\\, $\\>>;
                $\r -> <<$\\, $r>>;
                $\n -> <<$\\, $n>>;
                $\t -> <<$\\, $t>>;
                $\f -> <<$\\, $f>>;
                X when X > 126, X < 160 -> convert_to_u(X);
                X when X < 32 -> convert_to_u(X);
                Other -> <<Other/utf8>>
            end,
            inspect_maybe_utf8_string(Rest, <<Acc/binary, Escaped/binary>>);
        _ -> {error, not_a_utf8_string}
    end.

convert_to_u(Code) ->
    list_to_binary(io_lib:format("\\u{~4.16.0B}", [Code])).

float_to_string(Float) when is_float(Float) ->
    erlang:iolist_to_binary(io_lib_format:fwrite_g(Float)).

utf_codepoint_list_to_string(List) ->
    case unicode:characters_to_binary(List) of
        {error, _} -> erlang:error({gleam_error, {string_invalid_utf8, List}});
        Binary -> Binary
    end.

crop_string(String, Prefix) ->
    case string:find(String, Prefix) of
        nomatch -> String;
        New -> New
    end.

contains_string(String, Substring) ->
    is_bitstring(string:find(String, Substring)).

base16_decode(String) ->
    try
        {ok, binary:decode_hex(String)}
    catch
        _:_ -> {error, nil}
    end.

string_replace(String, Pattern, Replacement) ->
    string:replace(String, Pattern, Replacement, all).

slice(String, Index, Length) ->
    case string:slice(String, Index, Length) of
        X when is_binary(X) -> X;
        X when is_list(X) -> unicode:characters_to_binary(X)
    end.
