// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use erlang_generation::{BitArraySegmentSpecifier, ErlangBuilder, ErlangModuleName};
use num_bigint::BigInt;
use src_span::SrcSpan;

/// This generates all the definitions for the `echo` function and all the
/// helpers it might need.
///
/// Make sure to call this when at the module's top level!
///
pub fn echo_with_helpers<Output>(builder: &mut impl ErlangBuilder<Output>) {
    echo(builder);
    echo_inspect(builder);
    inspect_bit_array(builder);
    inspect_bit_array_pieces(builder);
    inspect_binary(builder);
    inspect_atom(builder);
    inspect_list(builder);
    inspect_map(builder);
    inspect_record(builder);
    inspect_tuple(builder);
    inspect_function(builder);
    inspect_maybe_utf8_string(builder);
    inspect_escape_grapheme(builder);
    inspect_convert_to_u(builder);
    inspect_list_loop(builder);
    inspect_maybe_gleam_atom(builder);
    inspect_uppercase(builder);
}

/// This generates the definitions needed by echo.
/// Make sure to only include this if echo is needed in a module, otherwise
/// you'd end up generating more code than actually needed.
fn echo<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let echo = builder.start_function(
        location,
        "echo",
        4,
        [
            (location, "Value"),
            (location, "Message"),
            (location, "Filepath"),
            (location, "Line"),
        ],
    );

    // StringLine = erlang:integer_to_list(Line),
    builder.match_operator(location);
    builder.variable_pattern(location, "StringLine");
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "integer_to_list");
    builder.variable(location, "Line");
    builder.end_call(call);

    // StringValue = echo@inspect(Value),
    builder.match_operator(location);
    builder.variable_pattern(location, "StringValue");
    let call = builder.start_call(location);
    builder.atom_expression(location, "echo@inspect");
    let call = builder.end_called_expression(call);
    builder.variable(location, "Value");
    builder.end_call(call);

    // StringMessage =
    //   case Message of
    //       nil -> "";
    //       M -> [" ", M]
    //   end,
    builder.match_operator(location);
    builder.variable_pattern(location, "StringMessage");
    let case = builder.start_case(location);
    builder.variable(location, "Message");
    let case = builder.end_case_subject(case);
    {
        let clause = builder.start_case_clause(location);
        builder.atom_pattern(location, "nil");
        let clause = builder.end_clause_pattern(clause);
        let clause = builder.end_clause_guards(clause);
        builder.string(location, "");
        builder.end_clause_body(clause);

        let clause = builder.start_case_clause(location);
        builder.variable_pattern(location, "M");
        let clause = builder.end_clause_pattern(clause);
        let clause = builder.end_clause_guards(clause);
        builder.cons_list(location);
        builder.string(location, " ");
        builder.cons_list(location);
        builder.variable(location, "M");
        builder.empty_list(location);
        builder.end_clause_body(clause);
    }
    builder.end_case(case);

    builder.match_operator(location);
    builder.variable_pattern(location, "Grey");
    builder.string(location, "\\u{1B}[90m");

    builder.match_operator(location);
    builder.variable_pattern(location, "ResetColour");
    builder.string(location, "\\u{1B}[39m");

    // Finally, we piece everything together and print it.
    // io:put_chars(
    //   standard_error,
    //   [
    //     Grey, Filepath, $:, StringLine, ResetColour, StringMessage, $\n,
    //     StringValue, $\n
    //   ]
    // )
    let call = builder.start_remote_call(location, ErlangModuleName::io(), "put_chars");
    {
        builder.atom_expression(location, "standard_error");

        builder.cons_list(location);
        builder.variable(location, "Grey");
        builder.cons_list(location);
        builder.variable(location, "Filepath");
        builder.cons_list(location);
        builder.string(location, ":");
        builder.cons_list(location);
        builder.variable(location, "StringLine");
        builder.cons_list(location);
        builder.variable(location, "ResetColour");
        builder.cons_list(location);
        builder.variable(location, "StringMessage");
        builder.cons_list(location);
        builder.string(location, r#"\n"#);
        builder.cons_list(location);
        builder.variable(location, "StringValue");
        builder.cons_list(location);
        builder.string(location, r#"\n"#);
        builder.empty_list(location);
    }
    builder.end_call(call);

    // Echo ends by returning the value it's been given to print.
    builder.variable(location, "Value");
    builder.end_function(echo);
}

/// This generates the `echo@inspect` helper function that `echo` relies on.
fn echo_inspect<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let echo_inspect = builder.start_function(location, "echo@inspect", 1, [(location, "Value")]);
    let case = builder.start_case(location);
    builder.variable(location, "Value");
    let case = builder.end_case_subject(case);
    {
        // nil -> "Nil";
        let clause = builder.start_case_clause(location);
        builder.atom_pattern(location, "nil");
        let clause = builder.end_clause_pattern(clause);
        let clause = builder.end_clause_guards(clause);
        builder.string(location, "Nil");
        builder.end_clause_body(clause);

        // true -> "True";
        let clause = builder.start_case_clause(location);
        builder.atom_pattern(location, "true");
        let clause = builder.end_clause_pattern(clause);
        let clause = builder.end_clause_guards(clause);
        builder.string(location, "True");
        builder.end_clause_body(clause);

        // false -> "False";
        let clause = builder.start_case_clause(location);
        builder.atom_pattern(location, "false");
        let clause = builder.end_clause_pattern(clause);
        let clause = builder.end_clause_guards(clause);
        builder.string(location, "False");
        builder.end_clause_body(clause);

        // Int when erlang:is_integer(Int) -> erlang:integer_to_list(Int);
        let clause = clause_with_erlang_type_check(builder, "Int", "integer");
        let call =
            builder.start_remote_call(location, ErlangModuleName::erlang(), "integer_to_list");
        builder.variable(location, "Int");
        builder.end_call(call);
        builder.end_clause_body(clause);

        // Float when erlang:is_float(Float) -> io_lib_format:fwrite_g(Float);
        let clause = clause_with_erlang_type_check(builder, "Float", "float");
        let call =
            builder.start_remote_call(location, ErlangModuleName::io_lib_format(), "fwrite_g");
        builder.variable(location, "Float");
        builder.end_call(call);
        builder.end_clause_body(clause);

        // Binary when erlang:is_binary(Binary) -> inspect@binary(Binary);
        let clause = clause_with_erlang_type_check(builder, "Binary", "binary");
        let call = call_function(builder, location, "inspect@binary");
        builder.variable(location, "Binary");
        builder.end_call(call);
        builder.end_clause_body(clause);

        // Bits when erlang:is_bitstring(Bits) -> inspect@bit_array(Bits);
        let clause = clause_with_erlang_type_check(builder, "Bits", "bitstring");
        let call = call_function(builder, location, "inspect@bit_array");
        builder.variable(location, "Bits");
        builder.end_call(call);
        builder.end_clause_body(clause);

        // Atom when erlang:is_atom(Atom) -> inspect@atom(Atom);
        let clause = clause_with_erlang_type_check(builder, "Atom", "atom");
        let call = call_function(builder, location, "inspect@atom");
        builder.variable(location, "Atom");
        builder.end_call(call);
        builder.end_clause_body(clause);

        // List when erlang:is_list(List) -> inspect@list(List);
        let clause = clause_with_erlang_type_check(builder, "List", "list");
        let call = call_function(builder, location, "inspect@list");
        builder.variable(location, "List");
        builder.end_call(call);
        builder.end_clause_body(clause);

        // Map when erlang:is_map(Map) -> inspect@map(Map);
        let clause = clause_with_erlang_type_check(builder, "Map", "map");
        let call = call_function(builder, location, "inspect@map");
        builder.variable(location, "Map");
        builder.end_call(call);
        builder.end_clause_body(clause);

        // Record when ?could_be_record(Record) -> inspect@record(Record);
        let clause = builder.start_case_clause(location);
        builder.variable_pattern(location, "Record");
        let clause = builder.end_clause_pattern(clause);
        let guard = builder.start_clause_guard();
        {
            builder.binary_operator(location, "andalso");
            let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "is_tuple");
            builder.variable(location, "Record");
            builder.end_call(call);

            builder.binary_operator(location, "andalso");
            let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "is_atom");
            variable_tuple_element(builder, "Record", 1);
            builder.end_call(call);

            builder.binary_operator(location, "andalso");
            builder.binary_operator(location, "=/=");
            variable_tuple_element(builder, "Record", 1);
            builder.atom_expression(location, "false");

            builder.binary_operator(location, "andalso");
            builder.binary_operator(location, "=/=");
            variable_tuple_element(builder, "Record", 1);
            builder.atom_expression(location, "true");

            builder.binary_operator(location, "=/=");
            variable_tuple_element(builder, "Record", 1);
            builder.atom_expression(location, "nil");
        }
        builder.end_clause_guard(guard);
        let clause = builder.end_clause_guards(clause);
        let call = call_function(builder, location, "inspect@record");
        builder.variable(location, "Record");
        builder.end_call(call);
        builder.end_clause_body(clause);

        // Tuple when erlang:is_tuple(Tuple) -> inspect@tuple(Tuple);
        let clause = clause_with_erlang_type_check(builder, "Tuple", "tuple");
        let call = call_function(builder, location, "inspect@tuple");
        builder.variable(location, "Tuple");
        builder.end_call(call);
        builder.end_clause_body(clause);

        // Function when erlang:is_function(Function) -> inspect@function(Function);
        let clause = clause_with_erlang_type_check(builder, "Function", "function");
        let call = call_function(builder, location, "inspect@function");
        builder.variable(location, "Function");
        builder.end_call(call);
        builder.end_clause_body(clause);

        // Anything else is printed as a gleam comment wrapping the Erlang
        // representation of such term.
        //
        // Any -> ["//erl(", io_lib:format("~p", [Any]), ")"]
        let clause = builder.start_case_clause(location);
        builder.variable_pattern(location, "Any");
        let clause = builder.end_clause_pattern(clause);
        let clause = builder.end_clause_guards(clause);
        builder.cons_list(location);
        builder.string(location, "//erl(");
        builder.cons_list(location);
        let call = builder.start_remote_call(location, ErlangModuleName::io_lib(), "format");
        {
            builder.string(location, "~p");
            builder.cons_list(location);
            builder.variable(location, "Any");
            builder.empty_list(location);
        }
        builder.end_call(call);
        builder.cons_list(location);
        builder.string(location, ")");
        builder.empty_list(location);

        builder.end_clause_body(clause);
    }
    builder.end_case(case);
    builder.end_function(echo_inspect);
}

/// This generates the `inspect@bit_array` helper function that `echo` relies on.
fn inspect_bit_array<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function = builder.start_function(location, "inspect@bit_array", 1, [(location, "Bits")]);

    // Pieces = inspect@bit_array_pieces(Bits, []),
    builder.match_operator(location);
    builder.variable_pattern(location, "Pieces");
    let call = call_function(builder, location, "inspect@bit_array_pieces");
    builder.variable(location, "Bits");
    builder.empty_list(location);
    builder.end_call(call);

    // Inner = lists:join(", ", lists:reverse(Pieces))
    builder.match_operator(location);
    builder.variable_pattern(location, "Inner");
    let call = builder.start_remote_call(location, ErlangModuleName::lists(), "join");
    {
        builder.string(location, ", ");
        let call = builder.start_remote_call(location, ErlangModuleName::lists(), "reverse");
        builder.variable(location, "Pieces");
        builder.end_call(call);
    }
    builder.end_call(call);

    // ["<<", Inner, ">>"].
    builder.cons_list(location);
    builder.string(location, "<<");
    builder.cons_list(location);
    builder.variable(location, "Inner");
    builder.cons_list(location);
    builder.string(location, ">>");
    builder.empty_list(location);

    builder.end_function(function);
}

/// This generates the `inspect@bit_array_pieces` helper function that `echo` relies on.
fn inspect_bit_array_pieces<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function = builder.start_function(
        location,
        "inspect@bit_array_pieces",
        2,
        [(location, "Bits"), (location, "Acc")],
    );

    let case = builder.start_case(location);
    builder.variable(location, "Bits");
    let case = builder.end_case_subject(case);

    // <<>> -> Acc;
    let clause = builder.start_case_clause(location);
    let pattern = builder.start_bit_array_pattern(location);
    builder.end_bit_array_pattern(pattern);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    builder.variable(location, "Acc");
    builder.end_clause_body(clause);

    // <<Byte, Rest/bitstring>> -> inspect@bit_array_pieces(Rest, [erlang:integer_to_binary(Byte) | Acc])
    let clause = builder.start_case_clause(location);
    let pattern = builder.start_bit_array_pattern(location);
    builder.bit_array_segment(location);
    builder.variable_pattern(location, "Byte");
    builder.bit_array_segment_default_size();
    builder.bit_array_segment_specifiers([]);

    builder.bit_array_segment(location);
    builder.variable_pattern(location, "Rest");
    builder.bit_array_segment_default_size();
    builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Bitstring]);
    builder.end_bit_array_pattern(pattern);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let call = call_function(builder, location, "inspect@bit_array_pieces");
    {
        builder.variable(location, "Rest");
        builder.cons_list(location);
        let call =
            builder.start_remote_call(location, ErlangModuleName::erlang(), "integer_to_binary");
        builder.variable(location, "Byte");
        builder.end_call(call);
        builder.variable(location, "Acc");
    }
    builder.end_call(call);
    builder.end_clause_body(clause);

    // _ ->
    //   Size = erlang:bit_size(Bits),
    //   <<RemainingBits:Size>> = Bits,
    //   SizeString = [":size(", erlang:integer_to_binary(Size), ")"],
    //   Piece = [erlang:integer_to_binary(RemainingBits), SizeString],
    //   [Piece | Acc];
    let clause = builder.start_case_clause(location);
    builder.variable_pattern(location, "_");
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);

    builder.match_operator(location);
    builder.variable_pattern(location, "Size");
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "bit_size");
    builder.variable(location, "Bits");
    builder.end_call(call);

    builder.match_operator(location);
    let pattern = builder.start_bit_array_pattern(location);
    builder.bit_array_segment(location);
    builder.variable_pattern(location, "RemainingBits");
    builder.variable(location, "Size");
    builder.bit_array_segment_specifiers([]);
    builder.end_bit_array_pattern(pattern);
    builder.variable(location, "Bits");

    builder.match_operator(location);
    builder.variable_pattern(location, "SizeString");
    builder.cons_list(location);
    builder.string(location, ":size(");
    builder.cons_list(location);
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "integer_to_binary");
    builder.variable(location, "Size");
    builder.end_call(call);
    builder.cons_list(location);
    builder.string(location, ")");
    builder.empty_list(location);

    builder.match_operator(location);
    builder.variable_pattern(location, "Piece");
    builder.cons_list(location);
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "integer_to_binary");
    builder.variable(location, "RemainingBits");
    builder.end_call(call);
    builder.cons_list(location);
    builder.variable(location, "SizeString");
    builder.empty_list(location);

    builder.cons_list(location);
    builder.variable(location, "Piece");
    builder.variable(location, "Acc");

    builder.end_clause_body(clause);

    builder.end_case(case);
    builder.end_function(function);
}

/// This generates the `inspect@binary` helper function that `echo` relies on.
fn inspect_binary<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function = builder.start_function(location, "inspect@binary", 1, [(location, "Binary")]);

    // case inspect@maybe_utf8_string(Binary, false, <<>>) of ...
    let case = builder.start_case(location);
    let call = call_function(builder, location, "inspect@maybe_utf8_string");
    builder.variable(location, "Binary");
    builder.atom_expression(location, "false");
    let bit_array = builder.start_bit_array(location);
    builder.end_bit_array(bit_array);
    builder.end_call(call);
    let case = builder.end_case_subject(case);

    // We always display <<>> as the empty string, that's a totally arbitrary
    // decision, we could have also gone with <<>> instead.

    // _ when Binary =:= <<>> -> ~"\"\"";
    let clause = builder.start_case_clause(location);
    builder.discard_pattern(location);
    let clause = builder.end_clause_pattern(clause);
    let guard = builder.start_clause_guard();
    builder.binary_operator(location, "=:=");
    builder.variable(location, "Binary");
    let bit_array = builder.start_bit_array(location);
    builder.end_bit_array(bit_array);
    builder.end_clause_guard(guard);
    let clause = builder.end_clause_guards(clause);
    builder.string(location, r#"\"\""#);
    builder.end_clause_body(clause);

    // {ok, InspectedUtf8String} -> InspectedUtf8String;
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    builder.atom_pattern(location, "ok");
    builder.variable_pattern(location, "InspectedUtf8String");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    builder.variable(location, "InspectedUtf8String");
    builder.end_clause_body(clause);

    // {error, not_a_printable_string} ->
    //   Segments = [erlang:integer_to_list(X) || <<X>> <= Binary],
    //   ["<<", lists:join(", ", Segments), ">>"];
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    builder.atom_pattern(location, "error");
    builder.atom_pattern(location, "not_a_printable_string");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    builder.match_operator(location);
    builder.variable_pattern(location, "Segments");
    let call = builder.start_remote_call(location, ErlangModuleName::lists(), "map");
    {
        builder.function_reference(
            location,
            Some(ErlangModuleName::erlang()),
            "integer_to_list",
            1,
        );
        let call = builder.start_remote_call(location, ErlangModuleName::binary(), "bin_to_list");
        builder.variable(location, "Binary");
        builder.end_call(call);
    }
    builder.end_call(call);

    builder.cons_list(location);
    builder.string(location, "<<");
    builder.cons_list(location);
    let call = builder.start_remote_call(location, ErlangModuleName::lists(), "join");
    builder.string(location, ", ");
    builder.variable(location, "Segments");
    builder.end_call(call);
    builder.cons_list(location);
    builder.string(location, ">>");
    builder.empty_list(location);

    builder.end_clause_body(clause);

    builder.end_case(case);
    builder.end_function(function);
}

/// This generates the `inspect@atom` helper function that `echo` relies on.
fn inspect_atom<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function = builder.start_function(location, "inspect@atom", 1, [(location, "Atom")]);

    // Binary = erlang:atom_to_binary(Atom),
    builder.match_operator(location);
    builder.variable_pattern(location, "Binary");
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "atom_to_binary");
    builder.variable(location, "Atom");
    builder.end_call(call);

    // case inspect@maybe_gleam_atom(Binary, none, <<>>) of
    let case = builder.start_case(location);
    let call = call_function(builder, location, "inspect@maybe_gleam_atom");
    builder.variable(location, "Binary");
    builder.atom_expression(location, "none");
    let bit_array = builder.start_bit_array(location);
    builder.end_bit_array(bit_array);
    builder.end_call(call);
    let case = builder.end_case_subject(case);

    // {ok, Inspected} -> Inspected;
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    builder.atom_pattern(location, "ok");
    builder.variable_pattern(location, "Inspected");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    builder.variable(location, "Inspected");
    builder.end_clause_body(clause);

    // {error, _} -> ["atom.create(\"", Binary, "\")"]
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    builder.atom_pattern(location, "error");
    builder.variable_pattern(location, "_");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    builder.cons_list(location);
    builder.string(location, r#"atom.create(\""#);
    builder.cons_list(location);
    builder.variable(location, "Binary");
    builder.cons_list(location);
    builder.string(location, r#"\")"#);
    builder.empty_list(location);
    builder.end_clause_body(clause);

    builder.end_case(case);
    builder.end_function(function);
}

/// This generates the `inspect@list` helper function that `echo` relies on.
fn inspect_list<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function = builder.start_function(location, "inspect@list", 1, [(location, "List")]);

    // case inspect@list_loop(List, true) of ...
    let case = builder.start_case(location);
    let call = call_function(builder, location, "inspect@list_loop");
    builder.variable(location, "List");
    builder.atom_expression(location, "true");
    builder.end_call(call);
    let case = builder.end_case_subject(case);

    // {charlist, _} -> ["charlist.from_string(\"", erlang:list_to_binary(List), "\")"];
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    builder.atom_pattern(location, "charlist");
    builder.variable_pattern(location, "_");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    builder.cons_list(location);
    builder.string(location, r#"charlist.from_string(\""#);
    builder.cons_list(location);
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "list_to_binary");
    builder.variable(location, "List");
    builder.end_call(call);
    builder.cons_list(location);
    builder.string(location, r#"\")"#);
    builder.empty_list(location);
    builder.end_clause_body(clause);

    // {proper, Elements} -> ["[", Elements, "]"];
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    builder.atom_pattern(location, "proper");
    builder.variable_pattern(location, "Elements");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    builder.cons_list(location);
    builder.string(location, "[");
    builder.cons_list(location);
    builder.variable(location, "Elements");
    builder.cons_list(location);
    builder.string(location, "]");
    builder.empty_list(location);
    builder.end_clause_body(clause);

    // {improper, Elements} -> ["//erl([", Elements, "])"]
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    builder.atom_pattern(location, "improper");
    builder.variable_pattern(location, "Elements");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    builder.cons_list(location);
    builder.string(location, "//erl([");
    builder.cons_list(location);
    builder.variable(location, "Elements");
    builder.cons_list(location);
    builder.string(location, "])");
    builder.empty_list(location);
    builder.end_clause_body(clause);

    builder.end_case(case);
    builder.end_function(function);
}

/// This generates the `inspect@map` helper function that `echo` relies on.
fn inspect_map<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function = builder.start_function(location, "inspect@map", 1, [(location, "Map")]);

    // Fields = lists:map(..., maps:to_list(Map))
    builder.match_operator(location);
    builder.variable_pattern(location, "Fields");

    let call = builder.start_remote_call(location, ErlangModuleName::lists(), "map");
    {
        // fun(Entry) ->
        //   {Key, Value} = Entry,
        //   [<<"#(">>, echo@inspect(Key), <<", ">>, echo@inspect(Value), <<")">>]
        // end.
        let anonymous = builder.start_anonymous_function(location, [(location, "Entry")]);
        {
            builder.match_operator(location);
            let tuple = builder.start_tuple_pattern(location);
            builder.variable_pattern(location, "Key");
            builder.variable_pattern(location, "Value");
            builder.end_tuple_pattern(tuple);
            builder.variable(location, "Entry");

            builder.cons_list(location);
            builder.string(location, "#(");
            builder.cons_list(location);
            let call = call_function(builder, location, "echo@inspect");
            builder.variable(location, "Key");
            builder.end_call(call);
            builder.cons_list(location);
            builder.string(location, ", ");
            builder.cons_list(location);
            let call = call_function(builder, location, "echo@inspect");
            builder.variable(location, "Value");
            builder.end_call(call);
            builder.cons_list(location);
            builder.string(location, ")");
            builder.empty_list(location);
        }
        builder.end_function(anonymous);

        let call = builder.start_remote_call(location, ErlangModuleName::maps(), "to_list");
        builder.variable(location, "Map");
        builder.end_call(call);
    }
    builder.end_call(call);

    builder.cons_list(location);
    builder.string(location, "dict.from_list([");
    builder.cons_list(location);
    let call = builder.start_remote_call(location, ErlangModuleName::lists(), "join");
    builder.string(location, ", ");
    builder.variable(location, "Fields");
    builder.end_call(call);
    builder.cons_list(location);
    builder.string(location, "])");
    builder.empty_list(location);

    builder.end_function(function);
}

/// This generates the `inspect@record` helper function that `echo` relies on.
fn inspect_record<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function = builder.start_function(location, "inspect@record", 1, [(location, "Record")]);

    // Tuple = erlang:tuple_to_list(Record),
    builder.match_operator(location);
    builder.variable_pattern(location, "Tuple");
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "tuple_to_list");
    builder.variable(location, "Record");
    builder.end_call(call);
    // [Atom | ArgsList] = Tuple,
    builder.match_operator(location);
    builder.cons_list_pattern(location);
    builder.variable_pattern(location, "Atom");
    builder.variable_pattern(location, "ArgsList");
    builder.variable(location, "Tuple");

    // case inspect@maybe_gleam_atom(Atom, none, <<>>) of ...
    let case = builder.start_case(location);
    let call = call_function(builder, location, "inspect@maybe_gleam_atom");
    builder.variable(location, "Atom");
    builder.atom_expression(location, "none");
    let bit_array = builder.start_bit_array(location);
    builder.end_bit_array(bit_array);
    builder.end_call(call);
    let case = builder.end_case_subject(case);

    // {ok, Tag} ->
    //   Args = lists:join(", ", lists:map(fun echo@inspect/1, ArgsList)),
    //   [Tag, "(", Args, ")"];
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    builder.atom_pattern(location, "ok");
    builder.variable_pattern(location, "Tag");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    {
        builder.match_operator(location);
        builder.variable_pattern(location, "Args");
        let call = builder.start_remote_call(location, ErlangModuleName::lists(), "join");
        {
            builder.string(location, ", ");
            let call = builder.start_remote_call(location, ErlangModuleName::lists(), "map");
            builder.function_reference(location, None, "echo@inspect", 1);
            builder.variable(location, "ArgsList");
            builder.end_call(call);
        }
        builder.end_call(call);

        builder.cons_list(location);
        builder.variable(location, "Tag");
        builder.cons_list(location);
        builder.string(location, "(");
        builder.cons_list(location);
        builder.variable(location, "Args");
        builder.cons_list(location);
        builder.string(location, ")");
        builder.empty_list(location);
    }
    builder.end_clause_body(clause);

    // _ -> inspect@tuple(Tuple)
    let clause = builder.start_case_clause(location);
    builder.variable_pattern(location, "_");
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let call = call_function(builder, location, "inspect@tuple");
    builder.variable(location, "Tuple");
    builder.end_call(call);
    builder.end_clause_body(clause);

    builder.end_case(case);
    builder.end_function(function);
}

/// This generates the `inspect@tuple` helper function that `echo` relies on.
fn inspect_tuple<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function = builder.start_function(location, "inspect@tuple", 1, [(location, "Tuple")]);

    // case Tuple of ...
    let case = builder.start_case(location);
    builder.variable(location, "Tuple");
    let case = builder.end_case_subject(case);

    // _ when erlang_is_tuple(Tuple) ->
    //   inspect@tuple(erlang:tuple_to_list(Tuple));
    let clause = builder.start_case_clause(location);
    builder.variable_pattern(location, "_");
    let clause = builder.end_clause_pattern(clause);
    let guard = builder.start_clause_guard();
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "is_tuple");
    builder.variable(location, "Tuple");
    builder.end_call(call);
    builder.end_clause_guard(guard);
    let clause = builder.end_clause_guards(clause);
    let call = call_function(builder, location, "inspect@tuple");
    {
        let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "tuple_to_list");
        builder.variable(location, "Tuple");
        builder.end_call(call);
    }
    builder.end_call(call);
    builder.end_clause_body(clause);

    // _ ->
    //  Elements = lists:map(fun echo@inspect/1, Tuple),
    //  ["#(", lists:join(", ", Elements), ")"].
    let clause = builder.start_case_clause(location);
    builder.variable_pattern(location, "_");
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);

    builder.match_operator(location);
    builder.variable_pattern(location, "Elements");
    let call = builder.start_remote_call(location, ErlangModuleName::lists(), "map");
    builder.function_reference(location, None, "echo@inspect", 1);
    builder.variable(location, "Tuple");
    builder.end_call(call);

    builder.cons_list(location);
    builder.string(location, "#(");
    builder.cons_list(location);
    let call = builder.start_remote_call(location, ErlangModuleName::lists(), "join");
    builder.string(location, ", ");
    builder.variable(location, "Elements");
    builder.end_call(call);
    builder.cons_list(location);
    builder.string(location, ")");
    builder.empty_list(location);
    builder.end_clause_body(clause);

    builder.end_case(case);

    builder.end_function(function);
}

/// This generates the `inspect@function` helper function that `echo` relies on.
fn inspect_function<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function =
        builder.start_function(location, "inspect@function", 1, [(location, "Function")]);

    // {arity, Arity} = erlang:fun_info(Function, arity),
    builder.match_operator(location);
    let tuple = builder.start_tuple_pattern(location);
    builder.atom_pattern(location, "arity");
    builder.variable_pattern(location, "Arity");
    builder.end_tuple_pattern(tuple);
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "fun_info");
    builder.variable(location, "Function");
    builder.atom_expression(location, "arity");
    builder.end_call(call);

    // ArgsAsciiCodes = lists:seq($a, $a + Arity - 1),
    builder.match_operator(location);
    builder.variable_pattern(location, "ArgsAsciiCodes");
    let call = builder.start_remote_call(location, ErlangModuleName::lists(), "seq");
    builder.int_expression(location, 97.into());
    builder.binary_operator(location, "+");
    builder.int_expression(location, 97.into());
    builder.binary_operator(location, "-");
    builder.variable(location, "Arity");
    builder.int_expression(location, 1.into());
    builder.end_call(call);

    // Args = lists:join(", ", lists:map(fun(Arg) -> <<Arg>> end, ArgsAsciiCodes)),
    builder.match_operator(location);
    builder.variable_pattern(location, "Args");
    let call = builder.start_remote_call(location, ErlangModuleName::lists(), "join");
    {
        builder.string(location, ", ");
        let call = builder.start_remote_call(location, ErlangModuleName::lists(), "map");
        let function = builder.start_anonymous_function(location, [(location, "Arg")]);
        let bit_array = builder.start_bit_array(location);

        builder.bit_array_segment(location);
        builder.variable(location, "Arg");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([]);

        builder.end_bit_array(bit_array);
        builder.end_function(function);
        builder.variable(location, "ArgsAsciiCodes");
        builder.end_call(call);
    }
    builder.end_call(call);

    builder.cons_list(location);
    builder.string(location, "//fn(");
    builder.cons_list(location);
    builder.variable(location, "Args");
    builder.cons_list(location);
    builder.string(location, ") { ... }");
    builder.empty_list(location);

    builder.end_function(function);
}

/// This generates the `inspect@maybe_utf8_string` helper function that `echo` relies on.
fn inspect_maybe_utf8_string<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function = builder.start_function(
        location,
        "inspect@maybe_utf8_string",
        3,
        [
            (location, "Binary"),
            (location, "HasPrintableChars"),
            (location, "Acc"),
        ],
    );
    let case = builder.start_case(location);
    builder.variable(location, "Binary");
    let case = builder.end_case_subject(case);

    // <<>> if HasPrintableChars -> {ok, [~"\"", Acc, ~"\""]};
    let clause = builder.start_case_clause(location);
    let bit_array = builder.start_bit_array_pattern(location);
    builder.end_bit_array_pattern(bit_array);
    let clause = builder.end_clause_pattern(clause);
    let guard = builder.start_clause_guard();
    builder.variable(location, "HasPrintableChars");
    builder.end_clause_guard(guard);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "ok");
    builder.cons_list(location);
    builder.string(location, "\\\"");
    builder.cons_list(location);
    builder.variable(location, "Acc");
    builder.cons_list(location);
    builder.string(location, "\\\"");
    builder.empty_list(location);
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    // <<>> -> {error, not_a_printable_string};
    let clause = builder.start_case_clause(location);
    let bit_array = builder.start_bit_array_pattern(location);
    builder.end_bit_array_pattern(bit_array);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "error");
    builder.atom_expression(location, "not_a_printable_string");
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    // <<First/utf8, Rest/binary>> ->
    //   {Escaped, IsPrintableChar} = inspect@escape_grapheme(First),
    //   HasPrintableChars1 = HasPrintableChars orelse IsPrintableChar,
    //   inspect@maybe_utf8_string(Rest, HasPrintableChars1, <<Acc/binary, Escaped/binary>>);
    let clause = builder.start_case_clause(location);
    let bit_array = builder.start_bit_array_pattern(location);

    builder.bit_array_segment(location);
    builder.variable_pattern(location, "First");
    builder.bit_array_segment_default_size();
    builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Utf8]);

    builder.bit_array_segment(location);
    builder.variable_pattern(location, "Rest");
    builder.bit_array_segment_default_size();
    builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);

    builder.end_bit_array_pattern(bit_array);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);

    builder.match_operator(location);
    let tuple = builder.start_tuple_pattern(location);
    builder.variable_pattern(location, "Escaped");
    builder.variable_pattern(location, "IsPrintableChar");
    builder.end_tuple_pattern(tuple);
    let call = call_function(builder, location, "inspect@escape_grapheme");
    builder.variable(location, "First");
    builder.end_call(call);

    builder.match_operator(location);
    builder.variable_pattern(location, "HasPrintableChars1");
    builder.binary_operator(location, "orelse");
    builder.variable(location, "HasPrintableChars");
    builder.variable(location, "IsPrintableChar");

    let call = call_function(builder, location, "inspect@maybe_utf8_string");
    builder.variable(location, "Rest");
    builder.variable(location, "HasPrintableChars1");
    let bit_array = builder.start_bit_array(location);

    builder.bit_array_segment(location);
    builder.variable(location, "Acc");
    builder.bit_array_segment_default_size();
    builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);

    builder.bit_array_segment(location);
    builder.variable(location, "Escaped");
    builder.bit_array_segment_default_size();
    builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);

    builder.end_bit_array(bit_array);
    builder.end_call(call);

    builder.end_clause_body(clause);

    //  _ -> {error, not_a_printable_string}
    let clause = builder.start_case_clause(location);
    builder.variable_pattern(location, "_");
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "error");
    builder.atom_expression(location, "not_a_printable_string");
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);
    builder.end_case(case);
    builder.end_function(function);
}

/// This generates the `inspect@escape_grapheme` helper function that `echo` relies on.
fn inspect_escape_grapheme<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function =
        builder.start_function(location, "inspect@escape_grapheme", 1, [(location, "Char")]);
    // case Char of
    let case = builder.start_case(location);
    builder.variable(location, "Char");
    let case = builder.end_case_subject(case);

    escape_character_clause(builder, '"', "\\\"");
    escape_character_clause(builder, '\\', "\\\\");
    escape_character_clause(builder, '\r', "r");
    escape_character_clause(builder, '\n', "n");
    escape_character_clause(builder, '\t', "t");
    escape_character_clause(builder, 12 as char, "f");

    // X when X > 126, X < 160 -> {inspect@convert_to_u(X), false};
    let clause = builder.start_case_clause(location);
    builder.variable_pattern(location, "X");
    let clause = builder.end_clause_pattern(clause);
    let guard = builder.start_clause_guard();
    builder.binary_operator(location, "andalso");
    {
        builder.binary_operator(location, ">");
        builder.variable(location, "X");
        builder.int_expression(location, 126.into())
    }
    {
        builder.binary_operator(location, "<");
        builder.variable(location, "X");
        builder.int_expression(location, 160.into())
    }
    builder.end_clause_guard(guard);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    {
        let call = call_function(builder, location, "inspect@convert_to_u");
        builder.variable(location, "X");
        builder.end_call(call);

        builder.atom_expression(location, "false");
    }
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    // X when X < 32 -> {inspect@convert_to_u(X), false};
    let clause = builder.start_case_clause(location);
    builder.variable_pattern(location, "X");
    let clause = builder.end_clause_pattern(clause);
    let guard = builder.start_clause_guard();
    builder.binary_operator(location, "<");
    builder.variable(location, "X");
    builder.int_expression(location, 32.into());
    builder.end_clause_guard(guard);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    {
        let call = call_function(builder, location, "inspect@convert_to_u");
        builder.variable(location, "X");
        builder.end_call(call);

        builder.atom_expression(location, "false");
    }
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    // Other -> {<<Other/utf8>>, true}
    let clause = builder.start_case_clause(location);
    builder.variable_pattern(location, "Other");
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);

    let tuple = builder.start_tuple(location);
    let bit_array = builder.start_bit_array(location);
    builder.bit_array_segment(location);
    builder.variable(location, "Other");
    builder.bit_array_segment_default_size();
    builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Utf8]);
    builder.end_bit_array(bit_array);

    builder.atom_expression(location, "true");
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    builder.end_case(case);
    builder.end_function(function);
}

/// This generates the `inspect@convert_to_u` helper function that `echo` relies on.
fn inspect_convert_to_u<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function =
        builder.start_function(location, "inspect@convert_to_u", 1, [(location, "Code")]);
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "list_to_binary");
    {
        let call = builder.start_remote_call(location, ErlangModuleName::io_lib(), "format");
        builder.string(location, r#"\\u{~4.16.0B}"#);
        builder.cons_list(location);
        builder.variable(location, "Code");
        builder.empty_list(location);
        builder.end_call(call);
    }
    builder.end_call(call);
    builder.end_function(function);
}

/// This generates the `inspect@list_loop` helper function that `echo` relies on.
fn inspect_list_loop<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function = builder.start_function(
        location,
        "inspect@list_loop",
        2,
        [(location, "List"), (location, "Ascii")],
    );
    let case = builder.start_case(location);
    builder.variable(location, "List");
    let case = builder.end_case_subject(case);

    // [] -> {proper, []};
    let clause = builder.start_case_clause(location);
    builder.empty_list_pattern(location);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "proper");
    builder.empty_list(location);
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    // [First] when Ascii andalso ?is_ascii_character(First) ->
    //   {charlist, nil}
    let clause = builder.start_case_clause(location);
    builder.cons_list_pattern(location);
    builder.variable_pattern(location, "First");
    builder.empty_list_pattern(location);
    let clause = builder.end_clause_pattern(clause);
    let guard = builder.start_clause_guard();
    builder.binary_operator(location, "andalso");
    builder.variable(location, "Ascii");
    is_ascii_character(builder, "First");
    builder.end_clause_guard(guard);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "charlist");
    builder.atom_expression(location, "nil");
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    // [First] -> {proper, [echo@inspect(First)]};
    let clause = builder.start_case_clause(location);
    builder.cons_list_pattern(location);
    builder.variable_pattern(location, "First");
    builder.empty_list_pattern(location);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "proper");
    builder.cons_list(location);
    let call = call_function(builder, location, "echo@inspect");
    builder.variable(location, "First");
    builder.end_call(call);
    builder.empty_list(location);
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    // [First | Rest] when erlang:is_list(Rest) ->
    //   StillAscii = Ascii andalso ?is_ascii_character(First),
    //   {Kind, Inspected} = inspect@list_loop(Rest, StillAscii),
    //   {Kind, [echo@inspect(First), ", " | Inspected]};
    let clause = builder.start_case_clause(location);
    builder.cons_list_pattern(location);
    builder.variable_pattern(location, "First");
    builder.variable_pattern(location, "Rest");
    let clause = builder.end_clause_pattern(clause);
    let guard = builder.start_clause_guard();
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "is_list");
    builder.variable(location, "Rest");
    builder.end_call(call);
    builder.end_clause_guard(guard);
    let clause = builder.end_clause_guards(clause);

    builder.match_operator(location);
    builder.variable_pattern(location, "StillAscii");
    builder.binary_operator(location, "andalso");
    builder.variable(location, "Ascii");
    is_ascii_character(builder, "First");

    builder.match_operator(location);
    let tuple = builder.start_tuple_pattern(location);
    builder.variable_pattern(location, "Kind");
    builder.variable_pattern(location, "Inspected");
    builder.end_tuple_pattern(tuple);
    let call = call_function(builder, location, "inspect@list_loop");
    builder.variable(location, "Rest");
    builder.variable(location, "StillAscii");
    builder.end_call(call);

    let tuple = builder.start_tuple(location);
    builder.variable(location, "Kind");
    builder.cons_list(location);
    let call = call_function(builder, location, "echo@inspect");
    builder.variable(location, "First");
    builder.end_call(call);
    builder.cons_list(location);
    builder.string(location, ", ");
    builder.variable(location, "Inspected");
    builder.end_tuple(tuple);

    builder.end_clause_body(clause);

    // [First | ImproperRest] ->
    //   {improper, [echo@inspect(First), " | ", echo@inspect(ImproperRest)]}
    let clause = builder.start_case_clause(location);
    builder.cons_list_pattern(location);
    builder.variable_pattern(location, "First");
    builder.variable_pattern(location, "ImproperRest");
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "improper");

    builder.cons_list(location);
    let call = call_function(builder, location, "echo@inspect");
    builder.variable(location, "First");
    builder.end_call(call);
    builder.cons_list(location);
    builder.string(location, " | ");
    builder.cons_list(location);
    let call = call_function(builder, location, "echo@inspect");
    builder.variable(location, "ImproperRest");
    builder.end_call(call);
    builder.empty_list(location);

    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    builder.end_case(case);
    builder.end_function(function);
}

/// This generates the `inspect@maybe_gleam_atom` helper function that `echo` relies on.
fn inspect_maybe_gleam_atom<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function = builder.start_function(
        location,
        "inspect@maybe_gleam_atom",
        3,
        [
            (location, "Atom"),
            (location, "PrevChar"),
            (location, "Acc"),
        ],
    );

    //case {Atom, PrevChar} of ...
    let case = builder.start_case(location);
    let tuple = builder.start_tuple(location);
    builder.variable(location, "Atom");
    builder.variable(location, "PrevChar");
    builder.end_tuple(tuple);
    let case = builder.end_case_subject(case);

    // _ when erlang:is_atom(Atom) ->
    //   Binary = erlang:atom_to_binary(Atom),
    //   inspect@maybe_gleam_atom(Binary, PrevChar, Acc);
    let clause = builder.start_case_clause(location);
    builder.variable_pattern(location, "_");
    let clause = builder.end_clause_pattern(clause);
    let guard = builder.start_clause_guard();
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "is_atom");
    builder.variable(location, "Atom");
    builder.end_call(call);
    builder.end_clause_guard(guard);
    let clause = builder.end_clause_guards(clause);
    builder.match_operator(location);
    builder.variable_pattern(location, "Binary");
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "atom_to_binary");
    builder.variable(location, "Atom");
    builder.end_call(call);
    let call = call_function(builder, location, "inspect@maybe_gleam_atom");
    builder.variable(location, "Binary");
    builder.variable(location, "PrevChar");
    builder.variable(location, "Acc");
    builder.end_call(call);
    builder.end_clause_body(clause);

    // {<<>>, none} -> {error, nil};
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    let bit_array = builder.start_bit_array_pattern(location);
    builder.end_bit_array_pattern(bit_array);
    builder.atom_pattern(location, "none");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "error");
    builder.atom_expression(location, "nil");
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    // {<<First, _/binary>>, none} when is_digit_char(First) ->
    //   {error, nil};
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    let bit_array = builder.start_bit_array_pattern(location);
    {
        builder.bit_array_segment(location);
        builder.variable_pattern(location, "First");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([]);

        builder.bit_array_segment(location);
        builder.variable_pattern(location, "_");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);
    }
    builder.end_bit_array_pattern(bit_array);
    builder.atom_pattern(location, "none");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let guard = builder.start_clause_guard();
    is_digit_character(builder, "First");
    builder.end_clause_guard(guard);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "error");
    builder.atom_expression(location, "nil");
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    // {<<"_", _/binary>>, none} ->
    //   {error, nil};
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    let bit_array = builder.start_bit_array_pattern(location);
    {
        builder.bit_array_segment(location);
        builder.string_pattern(location, "_");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([]);

        builder.bit_array_segment(location);
        builder.variable_pattern(location, "_");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);
    }
    builder.end_bit_array_pattern(bit_array);
    builder.atom_pattern(location, "none");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "error");
    builder.atom_expression(location, "nil");
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    // {<<"_">>, _} -> {error, nil};
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    let bit_array = builder.start_bit_array_pattern(location);
    {
        builder.bit_array_segment(location);
        builder.string_pattern(location, "_");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([]);
    }
    builder.end_bit_array_pattern(bit_array);
    builder.variable_pattern(location, "_");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "error");
    builder.atom_expression(location, "nil");
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    // {<<"_", _/binary>>, $_} -> {error, nil};
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    let bit_array = builder.start_bit_array_pattern(location);
    {
        builder.bit_array_segment(location);
        builder.string_pattern(location, "_");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([]);

        builder.bit_array_segment(location);
        builder.variable_pattern(location, "_");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);
    }
    builder.end_bit_array_pattern(bit_array);
    builder.int_pattern(location, BigInt::from('_' as usize));
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "error");
    builder.atom_expression(location, "nil");
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    // {<<First, _/binary>>, _} when not ?is_atom_char(First) -> {error, nil};
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    let bit_array = builder.start_bit_array_pattern(location);
    {
        builder.bit_array_segment(location);
        builder.variable_pattern(location, "First");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([]);

        builder.bit_array_segment(location);
        builder.variable_pattern(location, "_");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);
    }
    builder.end_bit_array_pattern(bit_array);
    builder.variable_pattern(location, "_");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let guard = builder.start_clause_guard();
    builder.unary_operator(location, "not");
    is_atom_char(builder, "First");
    builder.end_clause_guard(guard);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "error");
    builder.atom_expression(location, "nil");
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    // {<<First, Rest/binary>>, none} ->
    //   inspect@maybe_gleam_atom(Rest, First, <<Acc/binary, (inspect@uppercase(First))>>);
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    let bit_array = builder.start_bit_array_pattern(location);
    {
        builder.bit_array_segment(location);
        builder.variable_pattern(location, "First");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([]);

        builder.bit_array_segment(location);
        builder.variable_pattern(location, "Rest");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);
    }
    builder.end_bit_array_pattern(bit_array);
    builder.atom_pattern(location, "none");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let call = call_function(builder, location, "inspect@maybe_gleam_atom");
    builder.variable(location, "Rest");
    builder.variable(location, "First");
    let bit_array = builder.start_bit_array(location);
    {
        builder.bit_array_segment(location);
        builder.variable(location, "Acc");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);

        builder.bit_array_segment(location);
        let call = call_function(builder, location, "inspect@uppercase");
        builder.variable(location, "First");
        builder.end_call(call);
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([]);
    }
    builder.end_bit_array(bit_array);
    builder.end_call(call);
    builder.end_clause_body(clause);

    // {<<"_", Rest/binary>>, _} ->
    //   inspect@maybe_gleam_atom(Rest, $_, Acc);
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    let bit_array = builder.start_bit_array_pattern(location);
    {
        builder.bit_array_segment(location);
        builder.string_pattern(location, "_");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([]);

        builder.bit_array_segment(location);
        builder.variable_pattern(location, "Rest");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);
    }
    builder.end_bit_array_pattern(bit_array);
    builder.variable_pattern(location, "_");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let call = call_function(builder, location, "inspect@maybe_gleam_atom");
    builder.variable(location, "Rest");
    builder.int_expression(location, BigInt::from('_' as usize));
    builder.variable(location, "Acc");
    builder.end_call(call);
    builder.end_clause_body(clause);

    // {<<First, Rest/binary>>, $_} ->
    //   inspect@maybe_gleam_atom(Rest, First, <<Acc/binary, (inspect@uppercase(First))>>);
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    let bit_array = builder.start_bit_array_pattern(location);
    {
        builder.bit_array_segment(location);
        builder.variable_pattern(location, "First");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([]);

        builder.bit_array_segment(location);
        builder.variable_pattern(location, "Rest");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);
    }
    builder.end_bit_array_pattern(bit_array);
    builder.int_pattern(location, BigInt::from('_' as usize));
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let call = call_function(builder, location, "inspect@maybe_gleam_atom");
    builder.variable(location, "Rest");
    builder.variable(location, "First");
    let bit_array = builder.start_bit_array(location);
    {
        builder.bit_array_segment(location);
        builder.variable(location, "Acc");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);

        builder.bit_array_segment(location);
        let call = call_function(builder, location, "inspect@uppercase");
        builder.variable(location, "First");
        builder.end_call(call);
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([]);
    }
    builder.end_bit_array(bit_array);
    builder.end_call(call);
    builder.end_clause_body(clause);

    // {<<First, Rest/binary>>, _} ->
    //   inspect@maybe_gleam_atom(Rest, First, <<Acc/binary, First>>);
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    let bit_array = builder.start_bit_array_pattern(location);
    {
        builder.bit_array_segment(location);
        builder.variable_pattern(location, "First");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([]);

        builder.bit_array_segment(location);
        builder.variable_pattern(location, "Rest");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);
    }
    builder.end_bit_array_pattern(bit_array);
    builder.variable_pattern(location, "_");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let call = call_function(builder, location, "inspect@maybe_gleam_atom");
    builder.variable(location, "Rest");
    builder.variable(location, "First");
    let bit_array = builder.start_bit_array(location);
    {
        builder.bit_array_segment(location);
        builder.variable(location, "Acc");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);

        builder.bit_array_segment(location);
        builder.variable(location, "First");
        builder.bit_array_segment_default_size();
        builder.bit_array_segment_specifiers([]);
    }
    builder.end_bit_array(bit_array);
    builder.end_call(call);
    builder.end_clause_body(clause);

    // {<<>>, _} ->
    //   {ok, Acc};
    let clause = builder.start_case_clause(location);
    let tuple = builder.start_tuple_pattern(location);
    let bit_array = builder.start_bit_array_pattern(location);
    builder.end_bit_array_pattern(bit_array);
    builder.variable_pattern(location, "_");
    builder.end_tuple_pattern(tuple);
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "ok");
    builder.variable(location, "Acc");
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);

    // _ ->
    //   erlang:throw({gleam_error, echo, Atom, PrevChar, Acc});
    let clause = builder.start_case_clause(location);
    builder.variable_pattern(location, "_");
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "throw");
    let tuple = builder.start_tuple(location);
    builder.atom_expression(location, "gleam_error");
    builder.atom_expression(location, "echo");
    builder.variable(location, "Atom");
    builder.variable(location, "PrevChar");
    builder.variable(location, "Acc");
    builder.end_tuple(tuple);
    builder.end_call(call);
    builder.end_clause_body(clause);

    builder.end_case(case);
    builder.end_function(function);
}

/// This generates the `inspect@uppercase` helper function that `echo` relies on.
fn inspect_uppercase<Output>(builder: &mut impl ErlangBuilder<Output>) {
    let location = SrcSpan::default();
    let function = builder.start_function(location, "inspect@uppercase", 1, [(location, "X")]);
    builder.binary_operator(location, "-");
    builder.variable(location, "X");
    builder.int_expression(location, 32.into());
    builder.end_function(function);
}

/// Generates the checks to see if the variable with the given name is a
/// character that can appear in a gleam atom name
fn is_atom_char<Output>(builder: &mut impl ErlangBuilder<Output>, argument: &'static str) {
    let location = SrcSpan::default();

    builder.binary_operator(location, "orelse");
    // Lowercase char
    {
        builder.binary_operator(location, "andalso");
        {
            builder.binary_operator(location, ">");
            builder.variable(location, argument);
            builder.int_expression(location, 96.into());
        }
        {
            builder.binary_operator(location, "<");
            builder.variable(location, argument);
            builder.int_expression(location, 123.into());
        }
    }
    builder.binary_operator(location, "orelse");
    // Underscore char
    {
        builder.binary_operator(location, "==");
        builder.variable(location, argument);
        builder.int_expression(location, 95.into());
    }
    // Or a digit char
    is_digit_character(builder, argument);
}

/// Generates the checks to see if the variable with the given name is a digit.
fn is_digit_character<Output>(builder: &mut impl ErlangBuilder<Output>, argument: &'static str) {
    let location = SrcSpan::default();
    builder.binary_operator(location, "andalso");
    {
        builder.binary_operator(location, ">");
        builder.variable(location, argument);
        builder.int_expression(location, 47.into());
    }
    {
        builder.binary_operator(location, "<");
        builder.variable(location, argument);
        builder.int_expression(location, 58.into());
    }
}

/// Generates the binary operator check to make sure a variable with the given
/// name is an ascii character
fn is_ascii_character<Output>(builder: &mut impl ErlangBuilder<Output>, variable: &'static str) {
    let location = SrcSpan::default();

    builder.binary_operator(location, "andalso");
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "is_integer");
    builder.variable(location, variable);
    builder.end_call(call);
    builder.binary_operator(location, "andalso");
    {
        builder.binary_operator(location, ">=");
        builder.variable(location, variable);
        builder.int_expression(location, 32.into());
    }
    {
        builder.binary_operator(location, "=<");
        builder.variable(location, variable);
        builder.int_expression(location, 126.into());
    }
}

// This produces a case clause in the form: <char_value> -> {~"\\<char>", true}
// Note how this returns a tuple with "true" as its second item, that's required
// by `inspect@escape_grapheme` which uses this!
fn escape_character_clause<Output>(
    builder: &mut impl ErlangBuilder<Output>,
    char: char,
    escaped: &'static str,
) {
    let location = SrcSpan::default();
    let clause = builder.start_case_clause(location);
    builder.int_pattern(location, BigInt::from(char as usize));
    let clause = builder.end_clause_pattern(clause);
    let clause = builder.end_clause_guards(clause);
    let tuple = builder.start_tuple(location);
    builder.string(location, &format!("\\\\{escaped}"));
    builder.atom_expression(location, "true");
    builder.end_tuple(tuple);
    builder.end_clause_body(clause);
}

/// This generates a clause with in the following form:
/// `Wibble when erlang:is_wibble(Wibble) -> ...`
///
/// > This is used for code generated by the compiler that has no corresponding
/// > Gleam AST node, so all locations will be the default empty location.
///
fn clause_with_erlang_type_check<Output, Builder: ErlangBuilder<Output>>(
    builder: &mut Builder,
    variable: &'static str,
    expected_erlang_type: &'static str,
) -> Builder::ClauseBody {
    let location = SrcSpan::default();
    let clause = builder.start_case_clause(location);
    builder.variable_pattern(location, variable);
    let clause = builder.end_clause_pattern(clause);
    let guard = builder.start_clause_guard();
    let call = builder.start_remote_call(
        location,
        ErlangModuleName::erlang(),
        &format!("is_{expected_erlang_type}"),
    );
    builder.variable(location, variable);
    builder.end_call(call);
    builder.end_clause_guard(guard);
    builder.end_clause_guards(clause)
}

/// This generates a call `erlang:element({position}, {variable_name})`.
///
/// > This is used for code generated by the compiler that has no corresponding
/// > Gleam AST node, so all locations will be the default empty location.
///
fn variable_tuple_element<Output>(
    builder: &mut impl ErlangBuilder<Output>,
    variable_name: &'static str,
    position: usize,
) {
    let location = SrcSpan::default();
    let call = builder.start_remote_call(location, ErlangModuleName::erlang(), "element");
    builder.int_expression(location, position.into());
    builder.variable(location, variable_name);
    builder.end_call(call);
}

/// This is useful when generating code for a regular function call where we're
/// calling a function with a known name (as opposed to possibly calling any
/// expression as a function).
///
fn call_function<Output, Builder: ErlangBuilder<Output>>(
    builder: &mut Builder,
    location: SrcSpan,
    function: &'static str,
) -> Builder::Call {
    let call = builder.start_call(location);
    builder.atom_expression(location, function);
    builder.end_called_expression(call)
}
