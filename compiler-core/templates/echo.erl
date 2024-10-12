echo(Value) ->
    Inspect =


inspect(true) -> "True";
inspect(false) -> "False";
inspect(nil) -> "Nil";
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
