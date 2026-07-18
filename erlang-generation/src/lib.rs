// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use ecow::EcoString;
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::Zero;
use regex::Regex;
use std::sync::OnceLock;

/// This is to raise an `unreachable` pretty printed error when we try producing
/// some piece of code that is not allowed in the current position.
macro_rules! invalid_code_for_position {
    ($this:expr, $expected:literal) => {
        unreachable!("{}", $this.error_with_position($expected))
    };
}

/// This represent an erlang module name.
/// Gleam modules use `/` as a separator, but when turned into erlang `@` is
/// used as a separator instead.
///
/// If we were to allow strings directly, a very common mistake to make would be
/// to pass a Gleam module name to a function that's expecting an Erlang module
/// name!
///
/// With this those bugs cannot happen, and since this is a newtype wrapper its
/// not adding any overhead over using plain strings.
///
pub struct ErlangModuleName(EcoString);

impl ErlangModuleName {
    #[inline]
    /// Creates a new erlang module name from a Gleam module name.
    pub fn new(gleam_module_name: &str) -> Self {
        Self(gleam_module_name.replace("/", "@").into())
    }

    /// The Erlang/OTP `erlang` module.
    pub fn erlang() -> Self {
        Self("erlang".into())
    }

    /// The Erlang/OTP `unicode` module.
    pub fn unicode() -> Self {
        Self("unicode".into())
    }
}

/// All the possible specifiers that can be used in a bit array segment.
pub enum BitArraySegmentSpecifier {
    Utf8,
    Utf16,
    Utf32,
    Integer,
    Float,
    Binary,
    Bitstring,
    Signed,
    Unsigned,
    Little,
    Big,
    Native,
    Unit(u8),
}

/// This represents what can go in a moduledoc or doc comment. Either the atom
/// false, if the function/module is to be hidden from the public docs, or the
/// string content to include.
pub enum DocContent<'string> {
    False,
    String(&'string str),
}

/// A trait defining operations to describe the content of an Erlang module.
///
/// This might look strange in places, for example why is there a `start_tuple`
/// and `end_tuple` function, but lists are built using `cons_list` and not a
/// `start_list` and `end_list` function?
///
/// That's because this API has been made primarily to be able to generate
/// the Erlang Abstract Format data structure. All the methods almost map 1:1
/// to how Erlang constructs are represented in that format.
///
/// You might want to keep a reference to it open as you go over this module,
/// it's gonna be handy:
/// https://www.erlang.org/doc/apps/erts/absform.html
///
pub trait ErlangBuilder<Output> {
    /// Represents an open function that has yet to be closed.
    /// A function definition is started with `ErlangBuilder::start_function` and
    /// _must_ be closed using `ErlangBuilder::end_function`.
    type Function;

    /// Represents an open function call that has yet to be closed.
    type Call;

    /// Represents an open function call where the called expression still has
    /// to be generated.
    type CalledExpression;

    /// Represents an open case expression that has yet to be closed.
    type Case;

    /// Represents an open case subject that has yet to be closed.
    type CaseSubject;

    /// Represents an open case clause pattern that has yet to be generated.
    type ClausePattern;

    /// Represents a set of clause guards that has yet to be closed.
    type ClauseGuards;

    /// Represents a single clause guard that has yet to be closed.
    type Guard;

    /// Represents an open clause body that has yet to be closed.
    type ClauseBody;

    /// Represents an open tuple that has yet to be closed.
    type Tuple;

    /// Represents an open map that has yet to be closed.
    type Map;

    /// Represents an open bit array that has yet to be closed.
    type BitArray;

    /// Represents an open bit array pattern that has yet to be closed.
    type BitArrayPattern;

    /// Represents an open tuple type that has yet to be closed.
    type TupleType;

    /// Represents an open tuple pattern that has yet to be closed.
    type TuplePattern;

    /// Represents an open record attribute.
    type RecordAttribute;

    /// Represents an open function type annotation that has yet to be closed after
    /// generating the arguments types and the return type.
    type FunctionType;

    /// Represents an open named type that has yet to be closed after generating
    /// the types it takes as an argument (if any).
    type NamedType;

    /// Represents an open remote named type that has yet to be closed after
    /// generating the types it takes as an argument (if any).
    type RemoteNamedType;

    /// Represents an open alternative type that has yet to be closed after
    /// generating all of its alternatives.
    type UnionType;

    /// Represents an open function type annotation that has yet to be closed after
    /// generating the arguments types and the return type.
    type FunctionSpec;

    /// Represents an open block that has yet to be closed after generating the
    /// statements that go inside it.
    type Block;

    /// Represents an open list of arguments' types in a function type annotation
    /// that has yet to be closed.
    type FunctionTypeArguments;

    /// Represents an open type spec attribute that has yet to be closed after
    /// generating the type it stands for.
    type TypeSpec;

    /// Creates a new `ErlangBuilder` data structure to generate Erlang code.
    /// If a module name is provided this will also automatically take care of
    /// generating the appropriate `-module` annotation at the very beginning.
    ///
    /// It's optional because it might not always be needed. For example when
    /// producing `-record` annotations there's no need to have a module name.
    ///
    fn new(module_name: Option<ErlangModuleName>) -> Self;

    /// Consumes the given `ErlangBuilder` turning it into some other
    /// representation.
    /// For example that might be a binary representation, or a textual pretty
    /// printed one.
    ///
    fn into_output(self) -> Output;

    /// Adds to the module an export attribute for the given exported functions.
    ///
    /// For example:
    ///
    /// ```ignore
    /// builder.export_attribute(vec![("wibble", 1), ("wobble", 2)]);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -export([wibble/1, wobble/2]).
    /// ```
    ///
    fn export_attribute<Name: AsRef<str>>(
        &mut self,
        exported: impl IntoIterator<Item = (Name, usize)>,
    );

    /// Adds to the module an export_type attribute for the given exported types.
    ///
    /// For example:
    ///
    /// ```ignore
    /// builder.export_attribute(vec![("wibble", 1), ("wobble", 2)]);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -export_type([wibble/1, wobble/2]).
    /// ```
    ///
    fn export_type_attribute<Name: AsRef<str>>(
        &mut self,
        exported: impl IntoIterator<Item = (Name, usize)>,
    );

    /// Creates a `-doc` attribute with the given content.
    /// For example:
    ///
    /// ```ignore
    /// todo!()
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -doc(false).
    /// ```
    ///
    fn doc_attribute(&mut self, content: DocContent<'_>);

    /// Creates a `-moduledoc` attribute with the given content.
    /// For example:
    ///
    /// ```ignore
    /// todo!()
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -moduledoc(false).
    /// ```
    ///
    fn moduledoc_attribute(&mut self, content: DocContent<'_>);

    /// This generates the code for a `-compile([]).` attribute where all the
    /// strings produces by the given iterator are going to be passed as atom
    /// literals.
    ///
    /// For example:
    ///
    /// ```ignore
    /// builder.compile_attribute(vec!["no_warn", "inline"]);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -compile([no_warn, inline]).
    /// ```
    ///
    fn compile_attribute<'a>(&mut self, arguments: impl IntoIterator<Item = &'a str>);

    /// This generates a `-file` attribute.
    /// For example:
    ///
    /// ```ignore
    /// builder.file_attribute("wibble.gleam", 2.into());
    /// ```
    ///
    /// Correspods to:
    ///
    /// ```erl
    /// -file("wibble.gleam", 2)
    /// ```
    ///
    fn file_attribute(&mut self, file: &str, line: u32);

    /// Starts a `-record` attribute.
    /// After this you're supposed to generate a sequence of `record_field`, and
    /// once you're done you must end it with `end_record_attribute`.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let record = builder.start_record_attribute("wobble");
    /// builder.record_field();
    /// builder.atom("wibble");
    /// builder.literal_atom_type("ok");
    /// builder.close_record_attribute(record);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -record(wobble, { wibble :: ok }).
    /// ```
    ///
    fn start_record_attribute(&mut self, record_name: &str) -> Self::RecordAttribute;

    /// This closes the currently open record attribute.
    /// Code generated after this is not gonna be part of it.
    ///
    fn end_record_attribute(&mut self, record: Self::RecordAttribute);

    /// This creates a record field inside a record attribute.
    /// After this you're supposed to generate two things:
    /// - an atom representing the name of the field
    /// - a type representing the type of the field
    ///
    /// For an example on how to use this you can check the
    /// `start_record_attribute` docs.
    fn record_field(&mut self);

    /// This starts a function type spec.
    /// Everything that is generated after this call is interpreted as the
    /// annotated type of the function. So this must be followed by a single
    /// function type.
    ///
    /// After that is complete, this has to be closed using `end_function_spec`.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let spec = builder.start_function_spec("wibble", 1)
    /// let function_type = builder.start_function_type();
    /// builder.int_type();
    /// let function_type = builder.end_function_type_arguments(function_type);
    /// builder.variable_type("A");
    /// builder.end_function_type(function_type);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -spec wibble(integer(), A) -> A.
    /// ```
    ///
    fn start_function_spec(&mut self, name: &str, arity: usize) -> Self::FunctionSpec;

    /// This closes the currently open function spec.
    /// Code generated after this is not gonna be part of this function spec.
    ///
    fn end_function_spec(&mut self, function_spec: Self::FunctionSpec);

    /// This starts an Erlang type spec.
    /// After this call you're expected to generate a single type; that's going
    /// to be the definition of the type.
    /// After that is done you should call `end_type_spec`.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let spec = builder.start_type_spec(false, "wibble", ["A"]);
    ///
    /// let union = builder.start_union_type();
    /// builder.literal_atom_type("nil");
    ///
    /// let list = builder.start_named_type("list");
    /// builder.type_variable("A")
    /// builder.close_named_type();
    ///
    /// builder.end_union_type(union);
    /// builder.end_type_spec(spec);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -spec wibble(A) :: nil | list(A).
    /// ```
    ///
    fn start_type_spec<Name: AsRef<str>>(
        &mut self,
        opaque: bool,
        name: &str,
        type_parameters: impl IntoIterator<Item = Name>,
    ) -> Self::TypeSpec;

    fn end_type_spec(&mut self, type_spec: Self::TypeSpec);

    /// This starts a function type.
    /// Any code generated after this is gonna be an argument type of the open
    /// function type until `end_function_type_arguments` is called.
    /// After that you must generate a single type that's gonna be the return
    /// type, and then end the function.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let function_type = builder.start_function_type();
    /// builder.int_type();
    /// builder.variable_type("A");
    /// let function_type builder.end_function_type_arguments(function_type);
    /// builder.variable_type("A");
    /// builder.end_function_type(function_type);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// (integer(), A) -> A.
    /// ```
    ///
    fn start_function_type(&mut self) -> Self::FunctionTypeArguments;

    /// This closes the currently open function type arguments list.
    /// This means that the next type that is generated is going to be the
    /// return type of the open function type.
    ///
    /// After that you must call `end_function_type` to close the function
    /// type.
    ///
    fn end_function_type_arguments(
        &mut self,
        function_type: Self::FunctionTypeArguments,
    ) -> Self::FunctionType;

    /// This takes a function type and closes it.
    /// Code generated after this is not gonna be part of this function type.
    ///
    fn end_function_type(&mut self, function_type: Self::FunctionType);

    /// This starts a named type (either defined previously in this module, or
    /// a built-in Erlang type) with the given name.
    /// Any code generated after this is gonna be an argument of the open
    /// named type type until `end_named_type` is called.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let type_ = builder.start_named_type("wibble");
    /// builder.end_named_type(type_);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// wibble().
    /// ```
    ///
    fn start_named_type(&mut self, name: &str) -> Self::NamedType;

    /// This takes a named type and closes it.
    /// Code generated after this is not gonna be part of this named type.
    ///
    fn end_named_type(&mut self, named_type: Self::NamedType);

    /// This starts a remote named type with the given module and name.
    /// Any code generated after this is gonna be an argument of the open
    /// named type type until `end_named_type` is called.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let type_ = builder.start_remote_named_type("wibble", "wobble");
    /// builder.end_named_type(type_);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// wibble:wobble().
    /// ```
    ///
    fn start_remote_named_type(
        &mut self,
        module: ErlangModuleName,
        name: &str,
    ) -> Self::RemoteNamedType;

    /// This takes a remote named type and closes it.
    /// Code generated after this is not gonna be part of this named type.
    ///
    fn end_remote_named_type(&mut self, named_type: Self::RemoteNamedType);

    /// This starts a tuple type.
    /// Any code generated after this is gonna be one of the tuple items.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let tuple = builder.start_tuple_type();
    /// builder.literal_atom_type("nil");
    /// builder.literal_atom_type("ok");
    /// builder.end_tuple_type(tuple);
    /// ```
    ///
    /// Corresponds to the following Erlang type:
    ///
    /// ```erl
    /// {nil, ok}.
    /// ```
    ///
    fn start_tuple_type(&mut self) -> Self::TupleType;

    /// This takes a tuple type and closes it.
    /// Code generated after this is not gonna be part of this tuple type.
    ///
    fn end_tuple_type(&mut self, tuple: Self::TupleType);

    /// This starts a union type.
    /// Any code generated after this is gonna be a possible alternative of this
    /// type.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let ok_or_error = builder.start_union_type();
    /// builder.literal_atom_type("ok");
    /// builder.literal_atom_type("error");
    /// builder.end_union_type(ok_or_error);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// ok | error.
    /// ```
    ///
    fn start_union_type(&mut self) -> Self::UnionType;

    /// This takes a union type and closes it.
    /// Code generated after this is not gonna be part of this union type.
    ///
    fn end_union_type(&mut self, union_type: Self::UnionType);

    /// This generated the code for a type variable with the given name.
    ///
    /// For example, if we were to define the type of the identity function we
    /// could do it like this:
    ///
    /// ```ignore
    /// let function = builder.start_function_type();
    /// builder.type_variable("A");
    /// let function = builder.end_function_type_arguments();
    /// builder.type_variable("A");
    /// builder.end_function(function);
    /// ```
    ///
    /// And it corresponds to:
    ///
    /// ```erl
    /// (A) -> A.
    /// ```
    ///
    fn type_variable(&mut self, name: &str);

    /// This generated the code for a literal atom type (as opposed to the
    /// type `atom()`).
    ///
    /// For example, the annotation of a function returning the atom `nil`
    /// is be defined like this:
    ///
    /// ```ignore
    /// let function = builder.start_function_type();
    /// let function = builder.end_function_type_arguments();
    /// builder.literal_atom_type("nil");
    /// builder.end_function(function);
    /// ```
    ///
    /// And it corresponds to:
    ///
    /// ```erl
    /// % type of a function returning nil!
    /// () -> nil.
    /// ```
    ///
    fn literal_atom_type(&mut self, name: &str);

    /// This starts a module function definition.
    /// Any code generated after this is gonna be a statement of the open
    /// function until `end_function` is called.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let function = builder.start_function("first_name", 0, vec![]);
    /// builder.string("Giacomo");
    /// builder.end_function(function);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// first_name() -> ~"Giacomo".
    /// ```
    ///
    fn start_function<Name: AsRef<str>>(
        &mut self,
        name: &str,
        arity: usize,
        arguments_names: impl IntoIterator<Item = Name>,
    ) -> Self::Function;

    /// This starts an expression defining an anonymous function.
    /// Any code generated after this is gonna be a statement inside the
    /// anonymous function's body until `end_anonymous_function` is called.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let function = builder.start_anonymous_function([]);
    /// builder.string("Erlang rocks");
    /// builder.end_anonymous_function(function);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// fun() -> ~"Erlang rocks" end.
    /// ```
    ///
    fn start_anonymous_function<Name: AsRef<str>>(
        &mut self,
        arguments_names: impl IntoIterator<Item = Name>,
    ) -> Self::Function;

    /// This takes a function and closes it.
    /// Code generated after this is not gonna be part of this function.
    ///
    fn end_function(&mut self, function: Self::Function);

    /// This starts a block expression.
    /// Any code generated after this is gonna be a statement inside the open
    /// block.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let block = builder.start_block();
    /// builder.string("Giacomo");
    /// builder.int(1);
    /// builder.end_block(block);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// begin
    ///   ~"Giacomo",
    ///   1
    /// end.
    /// ```
    ///
    fn start_block(&mut self) -> Self::Block;

    /// This takes a block and closes it.
    /// Code generated after this is not gonna be part of this block.
    ///
    fn end_block(&mut self, block: Self::Block);

    /// This starts a remote call.
    /// Any code generated after this is gonna be an argument of the open
    /// function call `end_call` is called.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let call = builder.start_remote_call("io", "format");
    /// builder.string("Giacomo");
    /// builder.end_call(call);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// io:format(~"Giacomo").
    /// ```
    ///
    fn start_remote_call(&mut self, module: ErlangModuleName, function: &str) -> Self::Call;

    /// This starts a function call.
    /// The expression generated immediately after this is going to be the thing
    /// that is called, after generating it you must call the
    /// `end_called_expression` function and generate its arguments.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let call = builder.start_call();
    /// builder.atom("wibble")
    /// let call = builder.end_called_expression(call);
    /// builder.string("Hello");
    /// builder.string("Giacomo");
    /// builder.end_call(call);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// wibble(~"Hello", ~"Giacomo").
    /// ```
    ///
    fn start_call(&mut self) -> Self::CalledExpression;

    /// This must be called after generating the expression that is called in a
    /// function call. After that you can generate the arguments of the function
    /// call.
    fn end_called_expression(&mut self, called: Self::CalledExpression) -> Self::Call;

    /// This takes an open call and closes it.
    /// Code generated after this is not gonna be an argument to this call.
    ///
    fn end_call(&mut self, call: Self::Call);

    /// This starts a tuple.
    /// Any code generated after this is gonna be an item of the tuple.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let tuple = builder.start_tuple();
    /// builder.string("Hello");
    /// builder.int(1);
    /// builder.end_tuple(tuple);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// {~"Hello", 1}.
    /// ```
    ///
    fn start_tuple(&mut self) -> Self::Tuple;

    /// This takes an open tuple and closes it.
    /// Code generated after this is not gonna be an item of the tuple.
    ///
    fn end_tuple(&mut self, tuple: Self::Tuple);

    /// This starts an Erlang map.
    /// After this call you can add fields to the map using the `map_field`
    /// function.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let map = builder.start_map();
    ///
    /// builder.map_field();
    /// builder.atom("gleam_error")
    /// builder.atom("todo");
    ///
    /// builder.map_field();
    /// builder.atom("line");
    /// builder.int(6.into());
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// #{
    ///   gleam_error => todo,
    ///   line => 6
    /// }.
    /// ```
    ///
    fn start_map(&mut self) -> Self::Map;

    /// This takes an open map and closes it.
    /// Code generated after this is not gonna be a map field.
    ///
    fn end_map(&mut self, map: Self::Map);

    /// This is used to add new fields to an open map.
    /// After calling this you must generate exactly two values: the first one
    /// is going to be the key, while the second one is going to be the
    /// associated value.
    fn map_field(&mut self);

    /// This starts an Erlang bitstring (that's a Gleam's BitArray).
    /// Any code generated after this is gonna be a segment of the bitstring.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let bit_array = builder.start_bit_array();
    ///
    /// builder.bit_array_segment();
    /// builder.int(1);
    /// builder.atom("default");
    /// builder.atom("default");
    ///
    /// builder.bit_array_segment();
    /// builder.string("hello");
    /// builder.atom("deafult");
    /// builder.atom("default");
    ///
    /// builder.end_bit_array(bit_array);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// <<1, ~"hello">>.
    /// ```
    ///
    fn start_bit_array(&mut self) -> Self::BitArray;

    /// This takes an open bit array and closes it.
    /// Code generated after this is not gonna be a segment of the bit array.
    ///
    fn end_bit_array(&mut self, bit_array: Self::BitArray);

    /// This starts a new bit array segment. Make sure to call it after
    /// `start_bit_array`!
    /// Bit array segments are a bit tricky, after calling this you're supposed
    /// to generate three distinct bits in the following order:
    ///
    /// 1. The expression representing the bit array segment
    /// 2. The expression representing the segment size (or the atom `default`
    ///    if you want to use... you guessed it, the default)
    /// 3. A list of type specifiers (those are atoms like `utf8`, `binary`,
    ///    ...) or the atom `default` if you're ok with Erlang's default value,
    ///    those are generated using the `bit_array_segment_specifiers` function.
    ///
    /// After generating those three bits the segment is automatically over and
    /// you can go on to the next one!
    ///
    /// If this API seems a bit tricky and easy to get wrong, it is! But this is
    /// a low level API based on the shape of the Erlang Abstract Format itself,
    /// we don't make the rules.
    ///
    /// If you wanna check an example of how this is used you can have a read at
    /// the ones in `start_bit_array`.
    fn bit_array_segment(&mut self);

    /// This generates a specifiers list for the currently open bit array
    /// segment.
    /// You always have to call this function, even if the segment has no
    /// specifiers; in that case you can pass this an empty list and the
    /// Erlang's default will be applied.
    ///
    fn bit_array_segment_specifiers(
        &mut self,
        specifiers: impl IntoIterator<Item = BitArraySegmentSpecifier>,
    );

    /// This creates a list.
    /// The next two generated values are going to be respectively the first
    /// item and the tail of the list.
    ///
    /// For example:
    ///
    /// ```ignore
    /// builder.cons_list();
    /// builder.variable("Hello");
    /// builder.cons_list();
    /// builder.string("Giacomo");
    /// builder.empty_list();
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// [Hello | [ ~"Giacomo" | []]].
    /// ```
    ///
    /// Which, with some syntax sugar, is how we represent a list with two
    /// elements:
    ///
    /// ```erl
    /// [Hello, ~"Giacomo"]
    /// ```
    ///
    fn cons_list(&mut self);

    /// This creates an empty list.
    ///
    fn empty_list(&mut self);

    /// This starts a new case expression.
    /// After this function is called you're supposed to first generate a single
    /// expression; that's going to be the case subject being matched on.
    /// After that you must call `end_case_subject`.
    ///
    /// After that you're supposed to generate the case branches using the
    /// `start_case_clause` function.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let case = builder.start_case();
    /// builder.variable("wibble");
    /// let case = builder.end_case_subject();
    ///
    /// let clause = builder.start_case_clause();
    /// builder.discard_pattern();
    /// let clause = builder.end_clause_pattern();
    /// let clause = builder.end_clause_guards();
    /// builder.int(1.into());
    /// builder.end_clause_body();
    ///
    /// builder.end_case(case);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// case Wibble of
    ///   _ -> 1
    /// end.
    /// ```
    ///
    fn start_case(&mut self) -> Self::CaseSubject;

    /// This ends the open case subject, after this you can start generating the
    /// case clauses.
    fn end_case_subject(&mut self, case: Self::CaseSubject) -> Self::Case;

    /// This ends an open case expression.
    /// Any code generated after this is not going to be part of it.
    ///
    fn end_case(&mut self, case: Self::Case);

    /// This starts a new case clause inside a case expression.
    /// After this is called you must generate a single pattern and then call
    /// `end_clause_pattern`.
    ///
    /// For an example on how to generate a full case clause check the
    /// `start_case` documentation.
    fn start_case_clause(&mut self) -> Self::ClausePattern;

    /// This ends the case clause's pattern. After this you must generate the
    /// clause guards and then call `end_clause_guards`.
    /// If the clause you're generating has no guards you can immediately call
    /// that function without generating anything inbetween.
    fn end_clause_pattern(&mut self, clause_pattern: Self::ClausePattern) -> Self::ClauseGuards;

    /// You must call this before generating the guard of a case clause.
    /// For example:
    ///
    /// ```ignore
    /// let case = builder.start_case();
    /// builder.variable("wibble");
    /// let case = builder.end_case_subject();
    ///
    /// let clause = builder.start_case_clause();
    /// builder.discard_pattern();
    /// let clause = builder.end_clause_pattern();
    /// let guard = builder.start_clause_guard();
    /// builder.atom("true");
    /// builder.builder.end_clause_guard(guard);
    /// let clause = builder.end_clause_guards();
    /// builder.int(1.into());
    /// builder.end_clause_body();
    ///
    /// builder.end_case(case);
    /// ```
    ///
    /// A clause might have multiple guards but, the way Gleam is compiled, you
    /// should always call this function just once. A gleam guard is always
    /// compiled as a single erlang guard with a single expression.
    ///
    fn start_clause_guard(&mut self) -> Self::Guard;

    /// This ends an open clause guards. Anything that is generated after
    /// this is going to be a new guard among the guards of a case's guards.
    fn end_clause_guard(&mut self, clause_guard: Self::Guard);

    /// This ends the case clause's guards. Anything that is generated after
    /// this is going to be a statement inside the current case clause until
    /// `end_clause_body` is called.
    fn end_clause_guards(&mut self, clause_guards: Self::ClauseGuards) -> Self::ClauseBody;

    /// This takes an open clause body and ends it.
    /// After this you can start generating new case clauses, or end the
    /// currently open case expression if this was the last clause!
    fn end_clause_body(&mut self, clause_body: Self::ClauseBody);

    /// This creates a variable expression with the given name.
    /// For example:
    ///
    /// ```erl
    /// wibble(X) -> X.
    /// %            ^ This here!
    /// ```
    ///
    fn variable(&mut self, name: &str);

    /// This generated the code that is going to apply the given unary operator
    /// to the expression that is going to be generated next.
    /// For example:
    ///
    /// ```ignore
    /// builder.unary_operator("-");
    /// builder.variable("X");
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -X.
    /// ```
    ///
    fn unary_operator(&mut self, operator: &str);

    /// This generated the code that is going to apply the given binary operator
    /// to the two expressions generated after it.
    /// For example:
    ///
    /// ```ignore
    /// builder.binary_operator("+");
    /// builder.variable("X");
    /// builder.int(1);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// X + 1.
    /// ```
    ///
    fn binary_operator(&mut self, operator: &'static str);

    /// This generates the code for a function reference.
    /// For example:
    ///
    /// ```ignore
    /// builder.function_reference(None, "wibble", 1);
    /// builder.function_reference(Some("io"), "format", 2);
    /// ```
    ///
    /// Correspond to:
    ///
    /// ```erl
    /// fun wibble/1,
    /// fun io:format/2.
    /// ```
    ///
    fn function_reference(&mut self, module: Option<ErlangModuleName>, name: &str, arity: usize);

    /// This is used to create the code that corresponds to an assignment.
    /// A call to this function must always be followed by the generation of
    /// a pattern (the left-hand side of the assignment), and of an expression
    /// (the right-hand side of the assignment).
    ///
    /// For example:
    ///
    /// ```ignore
    /// builder.match_operator();
    /// builder.variable_pattern("X");
    /// builder.int(1);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// X = 1.
    /// ```
    ///
    fn match_operator(&mut self);

    /// This is used to create the code that corresponds to a match pattern.
    /// A call to this function must always be followed by the generation of
    /// a pattern (the left-hand side of the assignment), and of another pattern
    /// (the right-hand side of the assignment).
    ///
    /// For example:
    ///
    /// ```ignore
    /// builder.match_pattern();
    /// builder.int_pattern(1);
    /// builder.variable_pattern("X");
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// 1 = X.
    /// ```
    ///
    /// You could use this to compare equality of arbitrary complex patterns:
    /// `{1, A, [_, _]} = {X, Y, [A | _]}` however you'll most likely ever need
    /// this only when generating code for Gleam's "as" patterns where the right
    /// hand side is just a variable pattern.
    ///
    fn match_pattern(&mut self);

    /// This creates a variable pattern with the given name.
    /// For example:
    ///
    /// ```erl
    /// wibble() ->
    ///   X = 1.
    /// % ^ This here!
    /// ```
    ///
    fn variable_pattern(&mut self, name: &str);

    /// This creates a discard pattern.
    /// For example:
    ///
    /// ```erl
    /// wibble() ->
    ///   _ = 1.
    /// % ^ This here!
    /// ```
    ///
    fn discard_pattern(&mut self);

    /// This creates an integer pattern.
    /// For example:
    ///
    /// ```erl
    /// wibble() ->
    ///   1 = X.
    /// % ^ This here!
    /// ```
    ///
    fn int_pattern(&mut self, number: BigInt);

    /// This creates an integer pattern.
    /// For example:
    ///
    /// ```erl
    /// wibble() ->
    ///   1 = X.
    /// % ^ This here!
    /// ```
    ///
    fn float_pattern(&mut self, number: f64);

    /// This creates a string pattern.
    /// For example:
    ///
    /// ```erl
    /// wibble() ->
    ///   <<"Hello"/utf8>> = X.
    /// % ^^^^^^^^^^^^^^^^ This here!
    /// ```
    ///
    fn string_pattern(&mut self, content: &str);

    /// This creates an atom pattern.
    /// For example:
    ///
    /// ```erl
    /// wibble() ->
    ///   ok = X.
    /// % ^^ This here!
    /// ```
    ///
    fn atom_pattern(&mut self, name: &str);

    /// This starts a tuple pattern.
    /// Any code generated after this is gonna be an item of the tuple pattern.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let tuple = builder.start_tuple_pattern();
    /// builder.int_pattern(1);
    /// builder.discard_pattern();
    /// builder.end_tuple(tuple);
    /// ```
    ///
    /// Corresponds to the following pattern:
    ///
    /// ```erl
    /// {~"Hello", _}.
    /// ```
    ///
    fn start_tuple_pattern(&mut self) -> Self::TuplePattern;

    /// This takes an open tuple pattern and closes it.
    /// Any code generated after this is not gonna be part of that pattern.
    fn end_tuple_pattern(&mut self, tuple: Self::TuplePattern);

    /// This starts an Erlang bitstring (that's a Gleam's BitArray) pattern.
    /// Any code generated after this is gonna be a segment of the pattern.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let bit_array = builder.start_bit_array_pattern();
    ///
    /// builder.bit_array_pattern_segment();
    /// builder.int_pattern(1);
    /// builder.atom("default");
    /// builder.atom("default");
    ///
    /// builder.bit_array_pattern_segment();
    /// builder.discard_pattern();
    /// builder.atom("deafult");
    /// builder.atom("default");
    ///
    /// builder.end_bit_array_pattern(bit_array);
    /// ```
    ///
    /// Corresponds to the following pattern:
    ///
    /// ```erl
    /// <<1, _>>.
    /// ```
    ///
    fn start_bit_array_pattern(&mut self) -> Self::BitArrayPattern;

    /// This takes an open bit array pattern and closes it.
    /// Code generated after this is not gonna be a segment of the bit array.
    ///
    fn end_bit_array_pattern(&mut self, bit_array: Self::BitArrayPattern);

    /// This creates a list pattern.
    /// The next two generated values are going to be respectively the pattern
    /// for the first item and the pattern for the tail of the list.
    ///
    /// For example:
    ///
    /// ```ignore
    /// builder.cons_list_pattern();
    /// builder.discard_pattern();
    /// builder.cons_list_pattern();
    /// builder.string("Louis");
    /// builder.empty_list_pattern();
    /// ```
    ///
    /// Corresponds to the following pattern:
    ///
    /// ```erl
    /// [_ | [ ~"Louis" | []]].
    /// ```
    ///
    /// Which, with some syntax sugar, is how we represent a pattern matching
    /// on a list with two elements, where the second element is the string
    /// `~"Louis"`:
    ///
    /// ```erl
    /// [_, ~"Louis"]
    /// ```
    ///
    fn cons_list_pattern(&mut self);

    /// This creates a pattern matching on the empty list.
    ///
    fn empty_list_pattern(&mut self);

    /// This creates a string literal, where the string is represented as a
    /// bit array with the utf8 bytes making up the string.
    /// This is how Gleam string literals are represented in Erlang.
    ///
    /// For example:
    ///
    /// ```ignore
    /// builder.string("ksiąskę");
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// ~"ksiąskę".
    /// ```
    ///
    /// Which is the same as `<<"ksiąskę"/utf8>>`
    /// Or the same as writing the bytes directly:
    ///
    /// ```erl
    /// <<107, 115, 105, 196, 133, 115, 107, 196, 153>>
    /// ```
    ///
    fn string(&mut self, string: &str);

    /// This creates an integer literal from the given value.
    ///
    /// For example:
    ///
    /// ```ignore
    /// builder.int(BigInt::from(2))
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// 2.
    /// ```
    ///
    fn int(&mut self, value: BigInt);

    /// This creates a float literal from the given value.
    ///
    /// For example:
    ///
    /// ```ignore
    /// builder.float(1.2)
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// 1.2.
    /// ```
    ///
    fn float(&mut self, value: f64);

    /// This creates a literal atom with the given name.
    ///
    /// For example:
    ///
    /// ```ignore
    /// builder.atom("wibble")
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// wibble.
    /// ```
    ///
    fn atom(&mut self, name: &str);
}

/// A structure that implements the `ErlangBuilder` trait and produces a nice
/// and readable Erlang source string.
#[derive(Debug)]
pub struct ErlangSourceBuilder {
    code: String,
    /// This keeps track of what we're generating
    position: Vec<ErlangSourceBuilderPosition>,
    /// The current indentation to use when generating stuff like case
    /// expressions, block statements, etc.
    indentation: usize,
}

/// This is used to keep track of the current position when generating pretty
/// printed code from an `ErlangSourceBuilder`.
#[derive(Debug)]
enum ErlangSourceBuilderPosition {
    /// We're generating a function spec like `-spec wibble(atom()) -> atom()`.
    FunctionSpec,

    /// We're generating code for a type spec like
    /// `-type wibble() :: {ok, integer()}`.
    TypeSpec { expected: TypeSpecExpectedItem },

    /// We're generating a function type, there's a couple of things that make
    /// it up that we will need to generate: its arguments and the return type.
    /// Which one we're expecting to see is described by the `expected` field.
    FunctionType {
        expected: ExpectedFunctionTypeItem,
        /// If a function type doesn't appear at the top level as a spec
        /// annotation, then we must wrap it in a `fun(...)`. For example:
        ///
        /// ```erl
        /// % in a spec annotation it simply comes after the function name:
        /// -spec wibble () -> integer().
        /// wibble() -> 11.
        ///
        /// % but inside another type it has to be wrapped in `fun(...)`:
        /// -spec wobble () -> fun(() -> integer())
        /// wobble() -> fun wibble/0.
        /// ```
        ///
        /// This is `true` if the type has to be wrapped in `fun(...)`
        needs_wrapping: bool,
    },

    /// We're generating a named type like `integer()`, or `list(atom())`.
    NamedType {
        /// This is `true` is the first type argument of the named type has not
        /// been generated yet.
        first: bool,
    },

    /// We're generating a union type like `integer() | atom()`.
    UnionType {
        /// This is `true` is the first alternative of the union type has not
        /// been generated yet.
        first: bool,
    },

    /// We're generating a tuple type like `{integer(), atom()}`.
    TupleType {
        /// This is `true` is the first item of the tuple type has not been
        /// generated yet.
        first: bool,
    },

    /// We're generating the statements of a function.
    FunctionStatement {
        /// This is `true` if the first statement has not been generated yet.
        first: bool,
    },

    /// We're generating the statements of an anonymous function.
    AnonymousFunctionStatement {
        /// This is `true` if the first statement has not been generated yet.
        first: bool,
    },

    /// We're generating code for a match operator like `X = 1`.
    /// This is something that happens in multiple steps: first the pattern on
    /// the left hand side, second the expression on the right.
    MatchOperator {
        /// This keeps track of what we need to generate next.
        expected: ExpectedMatchSide,
    },

    /// We're generating code for a match pattern like `[1, A | _] = List`.
    /// This is something that happens in multiple steps: first the pattern on
    /// the left hand side, second the pattern on the right hand side.
    MatchPattern {
        /// This keeps track of what we need to generate next.
        expected: ExpectedMatchPatternSide,
    },

    /// We're generating a list.
    List {
        /// This is telling us if we're generating a list expression, or a list
        /// pattern.
        kind: ListKind,
        /// The list item we're expecting to see next.
        expected: ExpectedListItem,
    },

    /// We're generating code for a unary operator. We're waiting for the
    /// expression to apply the operator to.
    UnaryOperator,

    /// We're generating a tuple.
    Tuple {
        /// This is `true` is the first tuple item has not been generated yet.
        first: bool,
    },

    /// We're generating a tuple pattern.
    TuplePattern {
        /// This is `true` if the first pattern of the tuple has not been
        /// generated ywt.
        first: bool,
    },

    /// We're generating the segments of a bit array.
    BitArray {
        /// Whether we're dealing with a bit array pattern, or a bit array
        /// expression.
        kind: BitArrayKind,

        /// This is `true` if the first segment has not been generated yet.
        first: bool,
    },

    /// We're generating code for a segment of a bit array, like `10:1/signed`.
    BitArraySegment {
        expected: BitArraySegmentExpectedItem,
        /// This is `true` if the value of the bit array segment needs to be wrapped
        /// in parentheses. For example function calls need to be wrapped, or they
        /// would result in invalid Erlang being produced.
        ///
        /// ```erl
        /// <<x()/binary>>   % This is a syntax error!
        /// <<(x())/binary>> % This is fine.
        /// ```
        ///
        segment_value_needs_wrapping: bool,
        segment_size_needs_wrapping: bool,
    },

    /// We're generating a `begin ... end` block.
    Block {
        /// This is `true` is the first statement of the block has not been
        /// generated yet.
        first: bool,
    },

    /// We're generating code for a function call like `wibble(1, 2)`.
    /// This needs to happen in steps: first we generate the function being
    /// called (that could be any arbitrary expression after all), then we
    /// generate the arguments it's being called with.
    FunctionCall {
        expected: ExpectedCallItem,
        /// This is `true` if the thing that is being called needs to be wrapped
        /// in parentheses. This appears to be needed in just one case: if we
        /// are calling another function call expression. For example:
        ///
        /// ```erl
        /// wibble()()   % this is invalid Erlang
        /// (wibble())() % this is valid Erlang
        /// ```
        ///
        /// Actually this seems to be needed for OTP versions up to 28, in OTP
        /// 29 we can simply write `wibble()()`. However, since we have to
        /// support OTP 28 we will add the wrapping when needed.
        ///
        called_item_needs_wrapping: bool,
    },

    /// We're generating code for a binary operator like `1 + 3`.
    BinaryOperator {
        expected: ExpectedBinaryOperatorSide,
        /// Wether this binary operation needs to be wrapped in parentheses or
        /// not.
        needs_wrapping: bool,
        operator: &'static str,
    },

    /// We're generating code for a case expression.
    Case { expected: ExpectedCaseItem },
    /// We're generating code for a case clause.
    CaseClause { expected: ExpectedCaseClauseItem },
    /// We're generating the key-value pairs of a map.
    Map {
        /// This is `true` if no key-value pair has been generated yet.
        first: bool,
    },
    /// We're generating a key-value pair inside a map.
    MapField { expected: MapFieldExpectedItem },
    /// We're generating the fields of a record attribute.
    RecordAttribute {
        /// This is `true` if no field has been generated yet.
        first: bool,
    },
    /// We're generating the field of a record attribute. That needs to happen
    /// in two steps: first we generate the name, second we generate the type of
    /// the field.
    RecordField { expected: ExpectedRecordFieldItem },
    /// We're generating a guard in a possible series of guards in a clause.
    ClauseGuard,
}

#[derive(Debug, Eq, PartialEq)]
enum ListKind {
    /// We're generating a list pattern.
    Pattern,
    /// We're generating a list expression.
    Expression,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum BitArrayKind {
    /// We're generating a bit array pattern.
    Pattern,
    /// We're generating a bit array expression.
    Expression,
}

/// A map field is made of two things: a key, and a value. It's not an item that
/// is closed explicitly with a `end_map_field` function. It is implicitly over
/// after two expressions are generated. So we need to keep track of what we're
/// expecting to be generated next.
#[derive(Debug)]
enum MapFieldExpectedItem {
    Key,
    Value,
}

/// A record field is made of two things: a name, and a type. It's not an item
/// that is closed explicitly with a `end_record_field` function.
/// It is implicitly over after those two things are generated.
/// So we need to keep track of which we're expecting to be generated next.
#[derive(Debug)]
enum ExpectedRecordFieldItem {
    Name,
    Type,
}

/// Generating a case clause is done in three separate steps: first we generate
/// a single pattern, then we have to generate the guards for the clause,
/// finally we will be generating the statements making up the clause's body.
///
#[derive(Debug)]
enum ExpectedCaseClauseItem {
    Pattern,
    Guards {
        /// This is true if no guard has been generated yet.
        first: bool,
    },
    Body {
        /// This is true if no body statement has been generated yet.
        first: bool,
    },
}

/// Generating a case expression is done in two steps: first we generate the
/// subject being matched on, then we generate the branches of the case
/// expression.
///
#[derive(Debug)]
enum ExpectedCaseItem {
    /// We're waiting for the expression to be matched on to be generated.
    Subject,
    /// We've generated the expression to be matched on, and now are waiting for
    /// the case branches to be generated.
    Branches {
        /// This is `true` is no branch has been generated yet.
        first: bool,
    },
}

/// When generating a binary operator, that is made of three parts: the operator
/// and the left and right hand sides.
/// The way the Erlang Abstract Format works, we first generate the operator,
/// and then the two sides.
/// This is used to keep track which side we're expecting to see and properly
/// pretty print the output.
///
#[derive(Debug)]
enum ExpectedBinaryOperatorSide {
    Left,
    Right,
    BinaryOperatorIsOver,
}

/// When generating a bit array segment we need to generate exactly three
/// things: the value, the size, and the type specifiers.
/// This keeps track of which one we're expecting to be generated next.
///
#[derive(Debug)]
enum BitArraySegmentExpectedItem {
    Value {
        /// This is telling us if the value of the segment has to be a pattern
        /// or an expression.
        kind: BitArrayKind,
    },
    Size,
    Specifiers,
}

/// When generating a function call, that is made of two parts: the function to
/// be called (that could be a simple literal atom, denoting a function from the
/// current module, or any expression), and its arguments.
///
#[derive(Debug)]
enum ExpectedCallItem {
    /// We're waiting for the function to be called to be generated
    FunctionToBeCalled,
    /// The function to be called was generated, now we're waiting for its
    /// arguments.
    Arguments { first: bool },
}

/// When generating a function type, that is made of two parts: the type
/// arguments of the function, and the return type.
///
#[derive(Debug)]
enum ExpectedFunctionTypeItem {
    Arguments { first: bool },
    ReturnType,
}

/// Type specs don't have a "end_" function. They are implicitly over once a
/// type is generated. This is used to keep track of what we're expecting to
/// see.
///
#[derive(Debug)]
enum TypeSpecExpectedItem {
    TypeDefinition,
    TypeSpecIsOver,
}

/// A match operator is made of two sides: `X = 1`. A pattern and an expression.
///
#[derive(Debug)]
enum ExpectedMatchSide {
    /// We're waiting for the pattern on the left hand side of an assignment to
    /// be generated.
    Pattern,
    /// We're waiting for the expression on the right hand side of an assignment
    /// to be generated.
    Expression,
}

/// An as pattern is made of two sides: `[1, _ | _] = List`.
/// A pattern and a variable pattern for the name.
///
#[derive(Debug)]
enum ExpectedMatchPatternSide {
    /// We're waiting for the pattern on the left hand side of the match pattern
    /// to be generated.
    Left,
    /// We're waiting for the pattern on the right hand side of the match
    /// pattern to be generated.
    Right,
}

/// Lists are built by cons cells, so when building a list we will do something
/// like this: `[a | [b | []]]`.
/// This is telling us if we're expecting the first item, the second list, or if
/// the list is actually over.
///
#[derive(Debug)]
enum ExpectedListItem {
    First,
    Rest,
    ListIsOver,
}

static UNICODE_ESCAPE_SEQUENCE_PATTERN: OnceLock<Regex> = OnceLock::new();

/// How does pretty printing work? Here's a high level overview of how it works:
///
/// - When a new element is generated we call the `new_x` method.
///   If I'm generating an integer (or any expression) I call `new_expression`;
///   if I'm generating a pattern I call `new_pattern`.
/// - The `new_x` functions make sure that we're allowed to generate that
///   element in the current context (for example if I'm generating a tuple type
///   I can't start generating expression)!
/// - The `new_x` functions also make sure to add any code that is needed before
///   this new expression we're about to generate given the current context.
///   For example, say we're generating the items of a tuple, and we add another
///   one: first the call to `new_expression` is going to make sure to add
///   a comma to separate the previous item from the new one.
///
/// - Then we can start pushing the code needed to generate whatever it is we
///   are generating. If it's something simple like an integer we can just push
///   its string representation.
/// - There's also plenty of elements that are not "self-closing" and will be
///   generated in multiple steps (like function calls, tuples with multiple
///   items, binary operators, ...). In that case we can push a new `position`
///   to update the current context and keep track of what we're doing!
/// - Whenever we reach a `end_x` function we can pop the position we pushed on
///   the stack. That's the moment we can add whatever is needed to "close" one
///   of those complex elements. For example if I'm done generating a function's
///   body I can add a full stop at the end of it; if I'm done generating a
///   tuple I can add the final `}` after all the elements, and so on...
///
/// - There's one final tricky bit. Not all elements that are generated in
///   multiple steps have a `end_x` function (for example binary operators and
///   assignments). Those will end after a specific sequence of elements is
///   generated.
///   For example, if I call `self.match_operator` I know that it will be over
///   after the next pattern and expression are generated:
///
///   ```ignore
///   // X = 1
///   builder.match_operator()
///   builder.variable_pattern("X")
///   builder.int(1)
///   ```
///
///   Notice how here we don't have a `start_match_operator` and
///   `end_match_operator`. As you'll see in the implementation these will
///   require a bit of extra book-keeping in the `new_x` functions.
///
impl ErlangBuilder<String> for ErlangSourceBuilder {
    type BitArray = ();
    type BitArrayPattern = ();
    type Block = ();
    type CalledExpression = ();
    type Call = ();
    type Case = ();
    type Guard = ();
    type CaseSubject = ();
    type ClauseBody = ();
    type ClauseGuards = ();
    type ClausePattern = ();
    type Function = ();
    type FunctionSpec = ();
    type FunctionType = ();
    type FunctionTypeArguments = ();
    type Map = ();
    type NamedType = ();
    type RemoteNamedType = ();
    type RecordAttribute = ();
    type Tuple = ();
    type TuplePattern = ();
    type TupleType = ();
    type UnionType = ();
    type TypeSpec = ();

    fn new(module: Option<ErlangModuleName>) -> Self {
        Self {
            code: if let Some(module) = module {
                format!("-module({}).\n", quote_atom_name(&module.0))
            } else {
                String::new()
            },
            indentation: 0,
            position: vec![],
        }
    }

    fn into_output(mut self) -> String {
        self.close_currently_open_item();
        self.code.push('\n');
        self.code
    }

    fn export_attribute<'a, Name: AsRef<str>>(
        &mut self,
        exported: impl IntoIterator<Item = (Name, usize)>,
    ) {
        // If there's no item in the iterator we don't add the attribute at all.
        let mut exported = exported.into_iter().peekable();
        if exported.peek().is_none() {
            return;
        }

        self.new_top_level_form();

        self.code.push_str("-export([");
        let mut first = true;
        for (name, arity) in exported {
            if first {
                first = false;
            } else {
                self.code.push_str(", ");
            }

            self.code.push_str(&quote_atom_name(name.as_ref()));
            self.code.push('/');
            self.code.push_str(&arity.to_string())
        }
        self.code.push_str("]).\n");
    }

    fn export_type_attribute<'a, Name: AsRef<str>>(
        &mut self,
        exported: impl IntoIterator<Item = (Name, usize)>,
    ) {
        // If there's no item in the iterator we don't add the attribute at all.
        let mut exported = exported.into_iter().peekable();
        if exported.peek().is_none() {
            return;
        }

        self.new_top_level_form();

        self.code.push_str("-export_type([");
        let mut first = true;
        for (name, arity) in exported {
            if first {
                first = false;
            } else {
                self.code.push_str(", ");
            }

            self.code.push_str(&quote_atom_name(name.as_ref()));
            self.code.push('/');
            self.code.push_str(&arity.to_string())
        }
        self.code.push_str("]).\n");
    }

    fn doc_attribute(&mut self, content: DocContent<'_>) {
        self.new_top_level_form();
        self.code.push_str("-doc(");
        self.doc_content(content);
        self.code.push_str(").\n")
    }

    fn moduledoc_attribute(&mut self, content: DocContent<'_>) {
        self.new_top_level_form();
        self.code.push_str("-moduledoc(");
        self.doc_content(content);
        self.code.push_str(").\n")
    }

    fn compile_attribute<'a>(&mut self, arguments: impl IntoIterator<Item = &'a str>) {
        self.new_top_level_form();

        self.code.push_str("-compile([");

        let mut first = true;
        for argument in arguments {
            if first {
                first = false;
            } else {
                self.code.push_str(", ");
            }

            self.code.push_str(argument);
        }
        self.code.push_str("]).\n");
    }

    fn file_attribute(&mut self, file: &str, line: u32) {
        self.new_top_level_form();
        self.code.push_str("\n-file(\"");
        self.code.push_str(file);
        self.code.push_str("\", ");
        self.code.push_str(&line.to_string());
        self.code.push_str(").");
    }

    fn start_record_attribute(&mut self, record_name: &str) -> Self::RecordAttribute {
        self.new_top_level_form();
        self.code.push_str("-record(");
        self.code.push_str(&quote_atom_name(record_name));
        self.code.push_str(", {");
        self.indentation += INDENT;
        self.position
            .push(ErlangSourceBuilderPosition::RecordAttribute { first: true });
    }

    fn end_record_attribute(&mut self, _record: Self::RecordAttribute) {
        self.close_currently_open_item();
    }

    fn record_field(&mut self) {
        self.new_record_field();
        self.position
            .push(ErlangSourceBuilderPosition::RecordField {
                expected: ExpectedRecordFieldItem::Name,
            });
    }

    fn start_function_spec(&mut self, name: &str, _arity: usize) -> Self::FunctionSpec {
        self.new_top_level_form();
        self.code.push_str("\n-spec ");
        self.code.push_str(&quote_atom_name(name));
        self.position
            .push(ErlangSourceBuilderPosition::FunctionSpec);
    }

    fn end_function_spec(&mut self, _function_spec: Self::FunctionSpec) {
        self.close_currently_open_item();
    }

    fn start_type_spec<Name: AsRef<str>>(
        &mut self,
        opaque: bool,
        name: &str,
        type_variables: impl IntoIterator<Item = Name>,
    ) -> Self::TypeSpec {
        self.new_top_level_form();
        self.code.push('\n');
        self.code
            .push_str(if opaque { "-opaque " } else { "-type " });
        self.code.push_str(&quote_atom_name(name));
        self.code.push('(');

        let mut first = true;
        for type_variable in type_variables {
            if first {
                first = false
            } else {
                self.code.push_str(", ")
            }
            self.code.push_str(type_variable.as_ref())
        }
        self.code.push_str(") :: ");
        self.position.push(ErlangSourceBuilderPosition::TypeSpec {
            expected: TypeSpecExpectedItem::TypeDefinition,
        });
    }

    fn end_type_spec(&mut self, _type_spec: Self::TypeSpec) {
        // For the pretty printer type specs do not need any extra book keeping.
        // Ending one does nothing because the spec is already over and doesn't
        // change the builder's position.
    }

    fn start_function_type(&mut self) -> Self::FunctionTypeArguments {
        self.new_type();

        let needs_wrapping =
            if let Some(ErlangSourceBuilderPosition::FunctionSpec) = self.position.last() {
                self.code.push('(');
                false
            } else {
                self.code.push_str("fun((");
                true
            };

        self.position
            .push(ErlangSourceBuilderPosition::FunctionType {
                expected: ExpectedFunctionTypeItem::Arguments { first: true },
                needs_wrapping,
            });
    }

    fn end_function_type_arguments(
        &mut self,
        _function_type: Self::FunctionTypeArguments,
    ) -> Self::FunctionType {
        self.close_currently_open_item();
    }

    fn end_function_type(&mut self, _function_type: Self::FunctionType) {
        self.close_currently_open_item();
    }

    fn start_named_type(&mut self, name: &str) -> Self::NamedType {
        self.new_type();
        self.position
            .push(ErlangSourceBuilderPosition::NamedType { first: true });
        self.code.push_str(&quote_atom_name(name));
        self.code.push('(');
    }

    fn end_named_type(&mut self, _named_type: Self::NamedType) {
        self.close_currently_open_item();
    }

    fn start_remote_named_type(&mut self, module: ErlangModuleName, name: &str) -> Self::NamedType {
        self.new_type();
        self.position
            .push(ErlangSourceBuilderPosition::NamedType { first: true });
        self.code.push_str(&quote_atom_name(&module.0));
        self.code.push(':');
        self.code.push_str(&quote_atom_name(name));
        self.code.push('(');
    }

    fn end_remote_named_type(&mut self, _named_type: Self::RemoteNamedType) {
        self.close_currently_open_item();
    }

    fn start_tuple_type(&mut self) -> Self::TupleType {
        self.new_type();
        self.position
            .push(ErlangSourceBuilderPosition::TupleType { first: true });
        self.code.push('{');
    }

    fn end_tuple_type(&mut self, _tuple: Self::TupleType) {
        self.close_currently_open_item();
    }

    fn start_union_type(&mut self) -> Self::UnionType {
        self.new_type();
        self.position
            .push(ErlangSourceBuilderPosition::UnionType { first: true });
    }

    fn end_union_type(&mut self, _union_type: Self::UnionType) {
        self.close_currently_open_item();
    }

    fn type_variable(&mut self, name: &str) {
        self.new_type();
        self.code.push_str(name);
    }

    fn literal_atom_type(&mut self, name: &str) {
        self.new_type();
        self.code.push_str(&quote_atom_name(name));
    }

    fn start_function<Name: AsRef<str>>(
        &mut self,
        name: &str,
        _arity: usize,
        arguments_names: impl IntoIterator<Item = Name>,
    ) -> Self::Function {
        self.new_top_level_form();
        self.code.push_str(&quote_atom_name(name));
        self.code.push('(');

        let mut first = true;
        for argument in arguments_names {
            if !first {
                self.code.push_str(", ")
            } else {
                first = false;
            }
            self.code.push_str(argument.as_ref())
        }

        self.code.push_str(") ->");
        self.indentation += INDENT;
        self.position
            .push(ErlangSourceBuilderPosition::FunctionStatement { first: true });
    }

    fn start_anonymous_function<Name: AsRef<str>>(
        &mut self,
        arguments_names: impl IntoIterator<Item = Name>,
    ) -> Self::Function {
        self.new_expression();
        self.code.push_str("fun(");

        let mut first = true;
        for argument in arguments_names {
            if !first {
                self.code.push_str(", ")
            } else {
                first = false;
            }
            self.code.push_str(argument.as_ref())
        }

        self.code.push_str(") ->");
        self.indentation += INDENT;
        self.position
            .push(ErlangSourceBuilderPosition::AnonymousFunctionStatement { first: true });
    }

    fn end_function(&mut self, _function: Self::Function) {
        self.close_currently_open_item();
    }

    fn start_block(&mut self) -> Self::Block {
        self.new_expression();
        self.code.push_str("begin");
        self.indentation += INDENT;
        self.position
            .push(ErlangSourceBuilderPosition::Block { first: true });
    }

    fn end_block(&mut self, _block: Self::Block) {
        self.close_currently_open_item();
    }

    fn start_remote_call(&mut self, module: ErlangModuleName, function: &str) -> Self::Call {
        self.pop_leftover_items();

        // If this function call we're generating is itself being called then
        // it is going to need to be wrapped in parentheses to be valid in
        // OTP 28: it is not ok to write `wibble()()`, but we have to write
        // `(wibble())()`.
        if let Some(ErlangSourceBuilderPosition::FunctionCall {
            expected: ExpectedCallItem::FunctionToBeCalled,
            called_item_needs_wrapping,
        }) = self.position.last_mut()
        {
            *called_item_needs_wrapping = true;
        };

        self.new_expression();
        self.code.push_str(&quote_atom_name(&module.0));
        self.code.push(':');
        self.code.push_str(&quote_atom_name(function));
        self.position
            .push(ErlangSourceBuilderPosition::FunctionCall {
                expected: ExpectedCallItem::Arguments { first: true },
                called_item_needs_wrapping: false,
            });
    }

    fn start_call(&mut self) -> Self::Call {
        self.pop_leftover_items();

        // If this function call we're generating is itself being called then
        // it is going to need to be wrapped in parentheses to be valid in
        // OTP 28: it is not ok to write `wibble()()`, but we have to write
        // `(wibble())()`.
        if let Some(ErlangSourceBuilderPosition::FunctionCall {
            expected: ExpectedCallItem::FunctionToBeCalled,
            called_item_needs_wrapping,
        }) = self.position.last_mut()
        {
            *called_item_needs_wrapping = true;
        };

        self.new_expression();
        self.position
            .push(ErlangSourceBuilderPosition::FunctionCall {
                expected: ExpectedCallItem::FunctionToBeCalled,
                called_item_needs_wrapping: false,
            });
    }

    fn end_called_expression(&mut self, _called: Self::CalledExpression) -> Self::Call {
        // We need to make sure that just a single expression was generated as
        // the called expression.
        // So we can assert that we're no longer expecting the called expression
        // but the arguments, and none has been generated yet.
        self.pop_leftover_items();
        let Some(ErlangSourceBuilderPosition::FunctionCall {
            expected: ExpectedCallItem::Arguments { first: true },
            ..
        }) = self.position.last()
        else {
            invalid_code_for_position!(self, "end called expression")
        };
    }

    fn end_call(&mut self, _call: Self::Call) {
        self.close_currently_open_item();
    }

    fn start_tuple(&mut self) -> Self::Tuple {
        self.new_expression();
        self.code.push('{');
        self.position
            .push(ErlangSourceBuilderPosition::Tuple { first: true });
    }

    fn end_tuple(&mut self, _tuple: Self::Tuple) {
        self.close_currently_open_item();
    }

    fn start_map(&mut self) -> Self::Map {
        self.new_expression();
        self.code.push_str("#{");
        self.indentation += INDENT;
        self.position
            .push(ErlangSourceBuilderPosition::Map { first: true });
    }

    fn end_map(&mut self, _map: Self::Map) {
        self.close_currently_open_item();
    }

    fn map_field(&mut self) {
        self.new_map_field();
        self.position.push(ErlangSourceBuilderPosition::MapField {
            expected: MapFieldExpectedItem::Key,
        });
    }

    fn start_bit_array(&mut self) -> Self::BitArray {
        self.do_not_wrap_if_segment_value_or_size();
        self.new_expression();
        self.code.push_str("<<");
        self.position.push(ErlangSourceBuilderPosition::BitArray {
            kind: BitArrayKind::Expression,
            first: true,
        });
    }

    fn end_bit_array(&mut self, _bit_array: Self::BitArray) {
        self.close_currently_open_item();
    }

    fn bit_array_segment(&mut self) {
        let kind = self.new_bit_array_segment();
        self.position
            .push(ErlangSourceBuilderPosition::BitArraySegment {
                expected: BitArraySegmentExpectedItem::Value { kind },
                // We assume all values are going to have to be wrapped, better
                // be safe than sorry!
                // We will turn this off only for certain expressions we know
                // are safe to not wrap, like bare integers and strings.
                segment_value_needs_wrapping: true,
                segment_size_needs_wrapping: true,
            })
    }

    fn bit_array_segment_specifiers(
        &mut self,
        specifiers: impl IntoIterator<Item = BitArraySegmentSpecifier>,
    ) {
        self.pop_leftover_items();
        let Some(ErlangSourceBuilderPosition::BitArraySegment {
            expected: BitArraySegmentExpectedItem::Specifiers,
            segment_value_needs_wrapping,
            segment_size_needs_wrapping,
        }) = self.position.last_mut()
        else {
            invalid_code_for_position!(self, "bit array segment specifier");
        };

        if *segment_value_needs_wrapping {
            self.code.push(')');
            *segment_value_needs_wrapping = false;
        } else if *segment_size_needs_wrapping {
            self.code.push(')');
            *segment_size_needs_wrapping = false;
        }

        let mut first_specifier = true;
        let specifiers = specifiers.into_iter().sorted_by(|one, other| {
            if let BitArraySegmentSpecifier::Unit(_) = one {
                std::cmp::Ordering::Greater
            } else if let BitArraySegmentSpecifier::Unit(_) = other {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Equal
            }
        });

        for specifier in specifiers {
            let string = match specifier {
                BitArraySegmentSpecifier::Utf8 => "utf8",
                BitArraySegmentSpecifier::Utf16 => "utf16",
                BitArraySegmentSpecifier::Utf32 => "utf32",
                BitArraySegmentSpecifier::Integer => "integer",
                BitArraySegmentSpecifier::Float => "float",
                BitArraySegmentSpecifier::Binary => "binary",
                BitArraySegmentSpecifier::Bitstring => "bitstring",
                BitArraySegmentSpecifier::Signed => "signed",
                BitArraySegmentSpecifier::Unsigned => "unsigned",
                BitArraySegmentSpecifier::Little => "little",
                BitArraySegmentSpecifier::Big => "big",
                BitArraySegmentSpecifier::Native => "native",
                BitArraySegmentSpecifier::Unit(unit) => &format!("unit:{unit}"),
            };
            if first_specifier {
                first_specifier = false;
                self.code.push('/');
            } else {
                self.code.push('-')
            }
            self.code.push_str(string);
        }

        self.position.pop();
    }

    fn cons_list(&mut self) {
        self.cons_list_of_kind(ListKind::Expression);
    }

    fn empty_list(&mut self) {
        self.empty_list_of_kind(ListKind::Expression);
    }

    fn start_case(&mut self) -> Self::Case {
        self.new_expression();
        self.code.push_str("case ");
        self.position.push(ErlangSourceBuilderPosition::Case {
            expected: ExpectedCaseItem::Subject,
        });
    }

    fn end_case_subject(&mut self, _case: Self::CaseSubject) -> Self::Case {
        // We need to make sure that just a single expression was generated as
        // the subject and then this function was called.
        // So we can assert that we're no longer expecting the subject but the
        // clauses and none has been generated yet.
        self.pop_leftover_items();
        let Some(ErlangSourceBuilderPosition::Case {
            expected: ExpectedCaseItem::Branches { first: true },
        }) = self.position.last()
        else {
            invalid_code_for_position!(self, "end case subject")
        };
    }

    fn end_case(&mut self, _case: Self::Case) {
        self.close_currently_open_item();
    }

    fn start_case_clause(&mut self) -> Self::ClausePattern {
        self.new_case_clause();
        self.position.push(ErlangSourceBuilderPosition::CaseClause {
            expected: ExpectedCaseClauseItem::Pattern,
        });
    }

    fn end_clause_pattern(&mut self, _clause_pattern: Self::ClausePattern) -> Self::ClauseGuards {
        self.close_currently_open_item();
        self.position.push(ErlangSourceBuilderPosition::CaseClause {
            expected: ExpectedCaseClauseItem::Guards { first: true },
        });
    }

    fn start_clause_guard(&mut self) -> Self::Guard {
        self.new_clause_guard();
        self.position.push(ErlangSourceBuilderPosition::ClauseGuard);
    }

    fn end_clause_guard(&mut self, _clause_guard: Self::Guard) {
        self.close_currently_open_item();
    }

    fn end_clause_guards(&mut self, _clause_guards: Self::ClauseGuards) -> Self::ClauseBody {
        self.close_currently_open_item();
        self.indentation += INDENT;
        self.position.push(ErlangSourceBuilderPosition::CaseClause {
            expected: ExpectedCaseClauseItem::Body { first: true },
        });
    }

    fn end_clause_body(&mut self, _clause_body: Self::ClauseBody) {
        self.close_currently_open_item();
    }

    fn variable(&mut self, name: &str) {
        self.do_not_wrap_if_segment_value_or_size();
        self.new_expression();
        self.code.push_str(name);
    }

    fn unary_operator(&mut self, operator: &str) {
        self.new_expression();
        self.code.push_str(operator);
        self.code.push(' ');
        self.position
            .push(ErlangSourceBuilderPosition::UnaryOperator)
    }

    fn binary_operator(&mut self, operator: &'static str) {
        self.pop_leftover_items();

        // If this new binary operator we're generating is part of a bigger
        // binary operator (it doesn't matter if on the left or right-hand
        // side), then we want to wrap it in parentheses to avoid precedence
        // confusion!
        let needs_wrapping = match self.position.last() {
            Some(
                ErlangSourceBuilderPosition::BinaryOperator { .. }
                | ErlangSourceBuilderPosition::UnaryOperator,
            ) => true,
            Some(_) | None => false,
        };

        self.new_expression();
        if needs_wrapping {
            self.code.push('(');
        }
        self.position
            .push(ErlangSourceBuilderPosition::BinaryOperator {
                expected: ExpectedBinaryOperatorSide::Left,
                needs_wrapping,
                operator,
            })
    }

    fn function_reference(&mut self, module: Option<ErlangModuleName>, name: &str, arity: usize) {
        self.new_expression();
        self.code.push_str("fun ");
        if let Some(module) = module {
            self.code.push_str(&quote_atom_name(&module.0));
            self.code.push(':');
        }
        self.code.push_str(&quote_atom_name(name));
        self.code.push('/');
        self.code.push_str(&arity.to_string());
    }

    fn match_operator(&mut self) {
        self.new_expression();
        self.position
            .push(ErlangSourceBuilderPosition::MatchOperator {
                expected: ExpectedMatchSide::Pattern,
            });
    }

    fn match_pattern(&mut self) {
        self.new_pattern();
        self.position
            .push(ErlangSourceBuilderPosition::MatchPattern {
                expected: ExpectedMatchPatternSide::Left,
            })
    }

    fn variable_pattern(&mut self, name: &str) {
        self.do_not_wrap_if_segment_value_or_size();
        self.new_pattern();
        self.code.push_str(name);
    }

    fn discard_pattern(&mut self) {
        self.do_not_wrap_if_segment_value_or_size();
        self.new_pattern();
        self.code.push('_');
    }

    fn int_pattern(&mut self, number: BigInt) {
        self.do_not_wrap_if_segment_value_or_size();
        self.new_pattern();
        self.code.push_str(&number.to_string());
    }

    fn float_pattern(&mut self, number: f64) {
        self.do_not_wrap_if_segment_value_or_size();
        self.new_pattern();

        self.code.push_str(&format_float(number))
    }

    fn string_pattern(&mut self, content: &str) {
        self.do_not_wrap_if_segment_value_or_size();
        self.new_pattern();
        self.do_print_string_content(content);
    }

    fn atom_pattern(&mut self, name: &str) {
        self.new_pattern();
        self.code.push_str(&quote_atom_name(name));
    }

    fn start_tuple_pattern(&mut self) -> Self::TuplePattern {
        self.new_pattern();
        self.code.push('{');
        self.position
            .push(ErlangSourceBuilderPosition::TuplePattern { first: true });
    }

    fn end_tuple_pattern(&mut self, _tuple: Self::TuplePattern) {
        self.close_currently_open_item();
    }

    fn start_bit_array_pattern(&mut self) -> Self::BitArrayPattern {
        self.new_pattern();
        self.code.push_str("<<");
        self.position.push(ErlangSourceBuilderPosition::BitArray {
            kind: BitArrayKind::Pattern,
            first: true,
        });
    }

    fn end_bit_array_pattern(&mut self, _bit_array: Self::BitArrayPattern) {
        self.close_currently_open_item();
    }

    fn cons_list_pattern(&mut self) {
        self.cons_list_of_kind(ListKind::Pattern);
    }

    fn empty_list_pattern(&mut self) {
        self.empty_list_of_kind(ListKind::Pattern);
    }

    fn string(&mut self, content: &str) {
        self.do_not_wrap_if_segment_value_or_size();
        self.new_expression();
        self.do_print_string_content(content);
    }

    fn int(&mut self, number: BigInt) {
        self.do_not_wrap_if_segment_value_or_size();
        self.new_expression();
        self.code.push_str(&number.to_string());
    }

    fn float(&mut self, number: f64) {
        self.do_not_wrap_if_segment_value_or_size();
        self.new_expression();
        self.code.push_str(&format_float(number));
    }

    fn atom(&mut self, name: &str) {
        // There's one special case where we actually don't want to push the
        // atom's text at all. That's when we're generating the `default` atom
        // as the size of a bit array segment.
        // That's how we can tell in the Erlang Abstract Format that the size
        // should be the default value, but it's not actually spelled out in
        // textual Erlang code.
        if let Some(ErlangSourceBuilderPosition::BitArraySegment {
            expected: expected @ BitArraySegmentExpectedItem::Size,
            segment_value_needs_wrapping,
            segment_size_needs_wrapping,
        }) = self.position.last_mut()
        {
            // We are new expecting to see the specifiers list
            *expected = BitArraySegmentExpectedItem::Specifiers;
            if *segment_value_needs_wrapping {
                self.code.push(')');
                *segment_value_needs_wrapping = false;
            }
            // We have the default atom as a size, that means we don't have to
            // print anything at all! The size doesn't need any wrapping because
            // there's no size at all.
            *segment_size_needs_wrapping = false;
        } else {
            self.new_expression();
            self.code.push_str(&quote_atom_name(name));
        }
    }
}

fn format_float(number: f64) -> String {
    if number.is_zero() {
        if number.is_sign_negative() {
            String::from("-0.0")
        } else {
            String::from("+0.0")
        }
    } else if number.fract().is_zero() {
        format!("{number:.1}")
    } else {
        format!("{number}")
    }
}

const INDENT: usize = 4;

impl ErlangSourceBuilder {
    /// This has to be called before generating any new top level form: those
    /// are specs like `-spec`, `-type`, `-opaque`, doc attributes like
    /// `-doc` and `-moduledoc`, and function definitions.
    /// This allows the code generator to:
    /// - check for inconsistencies and panic (for example if we're generating
    ///   a top level form not at the top level scope).
    /// - update the current context.
    /// - add all the code that needs to go before this expression we're about
    ///   to generate, based on the current context.
    ///
    fn new_top_level_form(&mut self) {
        self.pop_leftover_items();
        if !self.position.is_empty() {
            invalid_code_for_position!(self, "top level form");
        }
    }

    /// This has to be called before generating any expression!
    /// This allows the code generator to:
    /// - check for inconsistencies and panic (for example if we're generating
    ///   expressions in the wrong place).
    /// - update the current context.
    /// - add all the code that needs to go before this expression we're about
    ///   to generate, based on the current context.
    ///
    fn new_expression(&mut self) {
        self.pop_leftover_items();
        let Some(position) = self.position.last_mut() else {
            invalid_code_for_position!(self, "expression");
        };

        match position {
            ErlangSourceBuilderPosition::RecordField {
                expected: expected @ ExpectedRecordFieldItem::Name,
            } => *expected = ExpectedRecordFieldItem::Type,

            ErlangSourceBuilderPosition::Case {
                expected: expected @ ExpectedCaseItem::Subject,
            } => *expected = ExpectedCaseItem::Branches { first: true },

            ErlangSourceBuilderPosition::FunctionCall {
                expected: expected @ ExpectedCallItem::FunctionToBeCalled,
                called_item_needs_wrapping,
            } => {
                if *called_item_needs_wrapping {
                    self.code.push('(');
                };
                *expected = ExpectedCallItem::Arguments { first: true }
            }

            ErlangSourceBuilderPosition::FunctionCall {
                expected: ExpectedCallItem::Arguments { first },
                called_item_needs_wrapping,
            } => {
                if *called_item_needs_wrapping {
                    *called_item_needs_wrapping = false;
                    self.code.push(')');
                }
                if *first {
                    self.code.push('(')
                } else {
                    self.code.push_str(", ");
                }
                *first = false;
            }
            ErlangSourceBuilderPosition::Tuple { first } => {
                if !*first {
                    self.code.push_str(", ");
                }
                *first = false;
            }

            // We're expecting the list's head. We don't have to add
            // anything!
            ErlangSourceBuilderPosition::List {
                expected: expected @ ExpectedListItem::First,
                kind: ListKind::Expression,
            } => *expected = ExpectedListItem::Rest,

            ErlangSourceBuilderPosition::List {
                expected: expected @ ExpectedListItem::Rest,
                kind: ListKind::Expression,
            } => {
                *expected = ExpectedListItem::ListIsOver;
                self.code.push_str(" | ")
            }

            ErlangSourceBuilderPosition::BinaryOperator {
                expected: expected @ ExpectedBinaryOperatorSide::Left,
                ..
            } => *expected = ExpectedBinaryOperatorSide::Right,

            ErlangSourceBuilderPosition::BinaryOperator {
                expected: expected @ ExpectedBinaryOperatorSide::Right,
                operator,
                ..
            } => {
                *expected = ExpectedBinaryOperatorSide::BinaryOperatorIsOver;
                self.code.push(' ');
                self.code.push_str(operator);
                self.code.push(' ');
            }

            ErlangSourceBuilderPosition::FunctionStatement { first }
            | ErlangSourceBuilderPosition::AnonymousFunctionStatement { first }
            | ErlangSourceBuilderPosition::Block { first }
            | ErlangSourceBuilderPosition::CaseClause {
                expected: ExpectedCaseClauseItem::Body { first },
            } => {
                if !*first {
                    self.code.push(',');
                }
                *first = false;
                self.code.push('\n');
                self.push_indentation();
            }

            ErlangSourceBuilderPosition::BitArraySegment {
                expected:
                    expected @ BitArraySegmentExpectedItem::Value {
                        kind: BitArrayKind::Expression,
                    },
                segment_value_needs_wrapping,
                ..
            } => {
                if *segment_value_needs_wrapping {
                    self.code.push('(');
                }
                *expected = BitArraySegmentExpectedItem::Size
            }

            ErlangSourceBuilderPosition::BitArraySegment {
                expected: expected @ BitArraySegmentExpectedItem::Size,
                segment_value_needs_wrapping,
                segment_size_needs_wrapping,
            } => {
                if *segment_value_needs_wrapping {
                    self.code.push(')');
                    // We've finished wrapping the value, we set this to
                    // false so we don't add other parentheses when we get
                    // to the specifiers list!
                    *segment_value_needs_wrapping = false;
                }
                *expected = BitArraySegmentExpectedItem::Specifiers;
                self.code.push(':');
                if *segment_size_needs_wrapping {
                    self.code.push('(');
                }
            }

            ErlangSourceBuilderPosition::ClauseGuard => {
                self.code.push_str(" when ");
            }

            ErlangSourceBuilderPosition::MapField { expected } => match expected {
                MapFieldExpectedItem::Key => *expected = MapFieldExpectedItem::Value,
                // We've generated a key and now the value is being generated.
                // So we need to add the `=>` separating key and value and we
                // can pop this position that is now complete.
                MapFieldExpectedItem::Value => {
                    self.code.push_str(" => ");
                    self.position.pop();
                }
            },

            // We were expecting an expression and someone is about to generate
            // it, we can pop this off the stack and need to add the ` = `
            // separating the previous pattern from this new expression.
            ErlangSourceBuilderPosition::MatchOperator {
                expected: ExpectedMatchSide::Expression,
            } => {
                self.code.push_str(" = ");
                self.position.pop();
            }
            // We were expecting an expression and someone generated it, we can
            // now pop this off the stack.
            ErlangSourceBuilderPosition::UnaryOperator => {
                self.position.pop();
            }

            // Expressions are not allowed in any of these positions.
            ErlangSourceBuilderPosition::Case {
                expected: ExpectedCaseItem::Branches { .. },
            }
            | ErlangSourceBuilderPosition::List {
                expected: ExpectedListItem::ListIsOver,
                kind: ListKind::Expression,
            }
            | ErlangSourceBuilderPosition::BitArray { .. }
            | ErlangSourceBuilderPosition::MatchOperator {
                expected: ExpectedMatchSide::Pattern,
            }
            | ErlangSourceBuilderPosition::CaseClause {
                expected: ExpectedCaseClauseItem::Pattern,
            }
            | ErlangSourceBuilderPosition::TuplePattern { .. }
            | ErlangSourceBuilderPosition::MatchPattern { .. }
            | ErlangSourceBuilderPosition::FunctionSpec
            | ErlangSourceBuilderPosition::TypeSpec { .. }
            | ErlangSourceBuilderPosition::NamedType { .. }
            | ErlangSourceBuilderPosition::FunctionType { .. }
            | ErlangSourceBuilderPosition::UnionType { .. }
            | ErlangSourceBuilderPosition::RecordField {
                expected: ExpectedRecordFieldItem::Type,
            }
            | ErlangSourceBuilderPosition::TupleType { .. }
            | ErlangSourceBuilderPosition::Map { .. }
            | ErlangSourceBuilderPosition::RecordAttribute { .. }
            | ErlangSourceBuilderPosition::CaseClause {
                expected: ExpectedCaseClauseItem::Guards { .. },
            }
            | ErlangSourceBuilderPosition::BitArraySegment {
                expected: BitArraySegmentExpectedItem::Specifiers,
                ..
            }
            | ErlangSourceBuilderPosition::BitArraySegment {
                expected:
                    BitArraySegmentExpectedItem::Value {
                        kind: BitArrayKind::Pattern,
                    },
                ..
            }
            | ErlangSourceBuilderPosition::BinaryOperator {
                expected: ExpectedBinaryOperatorSide::BinaryOperatorIsOver,
                ..
            }
            | ErlangSourceBuilderPosition::List {
                kind: ListKind::Pattern,
                ..
            } => invalid_code_for_position!(self, "expression"),
        }
    }

    /// This has to be called before generating any type!
    /// This allows the code generator to:
    /// - check for inconsistencies and panic (for example if we're generating
    ///   types in the wrong place).
    /// - update the current context.
    /// - add all the code that needs to go before this type we're about
    ///   to generate, based on the current context.
    ///
    fn new_type(&mut self) {
        self.pop_leftover_items();
        let Some(position) = self.position.last_mut() else {
            invalid_code_for_position!(self, "type");
        };

        match position {
            ErlangSourceBuilderPosition::FunctionSpec => {}
            ErlangSourceBuilderPosition::FunctionType {
                expected: ExpectedFunctionTypeItem::ReturnType,
                needs_wrapping: _,
            } => {}

            ErlangSourceBuilderPosition::TypeSpec {
                expected: expected @ TypeSpecExpectedItem::TypeDefinition,
            } => {
                *expected = TypeSpecExpectedItem::TypeSpecIsOver;
            }

            ErlangSourceBuilderPosition::FunctionType {
                expected: ExpectedFunctionTypeItem::Arguments { first },
                needs_wrapping: _,
            }
            | ErlangSourceBuilderPosition::TupleType { first }
            | ErlangSourceBuilderPosition::NamedType { first } => {
                if !*first {
                    self.code.push_str(", ")
                }
                *first = false
            }

            ErlangSourceBuilderPosition::UnionType { first } => {
                if !*first {
                    self.code.push_str(" | ")
                }
                *first = false
            }

            ErlangSourceBuilderPosition::RecordField {
                expected: ExpectedRecordFieldItem::Type,
            } => {
                self.code.push_str(" :: ");
                self.position.pop();
            }

            ErlangSourceBuilderPosition::FunctionCall { .. }
            | ErlangSourceBuilderPosition::Block { .. }
            | ErlangSourceBuilderPosition::Tuple { .. }
            | ErlangSourceBuilderPosition::BitArray { .. }
            | ErlangSourceBuilderPosition::BitArraySegment { .. }
            | ErlangSourceBuilderPosition::List { .. }
            | ErlangSourceBuilderPosition::FunctionStatement { .. }
            | ErlangSourceBuilderPosition::AnonymousFunctionStatement { .. }
            | ErlangSourceBuilderPosition::UnaryOperator
            | ErlangSourceBuilderPosition::Case { .. }
            | ErlangSourceBuilderPosition::ClauseGuard
            | ErlangSourceBuilderPosition::BinaryOperator { .. }
            | ErlangSourceBuilderPosition::CaseClause { .. }
            | ErlangSourceBuilderPosition::Map { .. }
            | ErlangSourceBuilderPosition::MapField { .. }
            | ErlangSourceBuilderPosition::RecordField {
                expected: ExpectedRecordFieldItem::Name,
            }
            | ErlangSourceBuilderPosition::MatchOperator {
                expected: ExpectedMatchSide::Expression,
            }
            | ErlangSourceBuilderPosition::RecordAttribute { .. }
            | ErlangSourceBuilderPosition::TypeSpec {
                expected: TypeSpecExpectedItem::TypeSpecIsOver,
            }
            | ErlangSourceBuilderPosition::MatchOperator {
                expected: ExpectedMatchSide::Pattern,
            }
            | ErlangSourceBuilderPosition::MatchPattern { .. }
            | ErlangSourceBuilderPosition::TuplePattern { .. } => {
                invalid_code_for_position!(self, "type")
            }
        }
    }

    /// This has to be called before generating any pattern!
    /// This allows the code generator to:
    /// - check for inconsistencies and panic (for example if we're generating
    ///   patterns in the wrong place).
    /// - update the current context.
    /// - add all the code that needs to go before this type we're about
    ///   to generate, based on the current context.
    ///
    fn new_pattern(&mut self) {
        self.pop_leftover_items();
        let Some(position) = self.position.last_mut() else {
            invalid_code_for_position!(self, "pattern");
        };

        match position {
            // We're expecting the list's head. We don't have to add
            // anything!
            ErlangSourceBuilderPosition::List {
                kind: ListKind::Pattern,
                expected: expected @ ExpectedListItem::First,
            } => *expected = ExpectedListItem::Rest,

            ErlangSourceBuilderPosition::List {
                kind: ListKind::Pattern,
                expected: expected @ ExpectedListItem::Rest,
            } => {
                *expected = ExpectedListItem::ListIsOver;
                self.code.push_str(" | ")
            }

            ErlangSourceBuilderPosition::BitArraySegment {
                segment_value_needs_wrapping,
                segment_size_needs_wrapping: _,
                expected:
                    expected @ BitArraySegmentExpectedItem::Value {
                        kind: BitArrayKind::Pattern,
                    },
            } => {
                if *segment_value_needs_wrapping {
                    self.code.push('(');
                }
                *expected = BitArraySegmentExpectedItem::Size
            }

            // We were waiting for the pattern to be generated, now we're done
            // and can start generating an expression for the right-hand side.
            ErlangSourceBuilderPosition::MatchOperator {
                expected: expected @ ExpectedMatchSide::Pattern,
            } => {
                *expected = ExpectedMatchSide::Expression;
            }

            // We were waiting for the pattern and a pattern has been generated.
            // We don't change this ourselves since the pattern has to be closed
            // explicitly calling `end_clause_pattern`.
            // We don't have to do anything here!
            ErlangSourceBuilderPosition::CaseClause {
                expected: ExpectedCaseClauseItem::Pattern,
            } => (),
            ErlangSourceBuilderPosition::TuplePattern { first } => {
                if *first {
                    *first = false;
                } else {
                    self.code.push_str(", ")
                }
            }
            ErlangSourceBuilderPosition::MatchPattern { expected } => match expected {
                ExpectedMatchPatternSide::Left => *expected = ExpectedMatchPatternSide::Right,
                // The right hand side is about to be generated so we have to
                // push the ` = ` to separate it from the left hand side, and we
                // can remove this position since the match pattern has now been
                // completed.
                ExpectedMatchPatternSide::Right => {
                    self.code.push_str(" = ");
                    self.position.pop();
                }
            },

            ErlangSourceBuilderPosition::FunctionCall { .. }
            | ErlangSourceBuilderPosition::Block { .. }
            | ErlangSourceBuilderPosition::FunctionStatement { .. }
            | ErlangSourceBuilderPosition::AnonymousFunctionStatement { .. }
            | ErlangSourceBuilderPosition::List {
                kind: ListKind::Expression,
                ..
            }
            | ErlangSourceBuilderPosition::BitArray { .. }
            | ErlangSourceBuilderPosition::BitArraySegment {
                expected:
                    BitArraySegmentExpectedItem::Size
                    | BitArraySegmentExpectedItem::Specifiers
                    | BitArraySegmentExpectedItem::Value {
                        kind: BitArrayKind::Expression,
                    },
                ..
            }
            | ErlangSourceBuilderPosition::UnaryOperator
            | ErlangSourceBuilderPosition::BinaryOperator { .. }
            | ErlangSourceBuilderPosition::Case { .. }
            | ErlangSourceBuilderPosition::ClauseGuard
            | ErlangSourceBuilderPosition::Map { .. }
            | ErlangSourceBuilderPosition::MapField { .. }
            | ErlangSourceBuilderPosition::MatchOperator {
                expected: ExpectedMatchSide::Expression,
            }
            | ErlangSourceBuilderPosition::Tuple { .. }
            | ErlangSourceBuilderPosition::FunctionType { .. }
            | ErlangSourceBuilderPosition::FunctionSpec
            | ErlangSourceBuilderPosition::TypeSpec { .. }
            | ErlangSourceBuilderPosition::NamedType { .. }
            | ErlangSourceBuilderPosition::UnionType { .. }
            | ErlangSourceBuilderPosition::TupleType { .. }
            | ErlangSourceBuilderPosition::RecordField { .. }
            | ErlangSourceBuilderPosition::RecordAttribute { .. }
            | ErlangSourceBuilderPosition::List {
                kind: ListKind::Pattern,
                expected: ExpectedListItem::ListIsOver,
            }
            | ErlangSourceBuilderPosition::CaseClause {
                expected: ExpectedCaseClauseItem::Guards { .. },
            }
            | ErlangSourceBuilderPosition::CaseClause {
                expected: ExpectedCaseClauseItem::Body { .. },
            } => {
                invalid_code_for_position!(self, "pattern");
            }
        }
    }

    fn close_currently_open_item(&mut self) {
        self.pop_leftover_items();
        let Some(position) = self.position.pop() else {
            return;
        };

        match position {
            // When we're done generating statements for a function we need to
            // add one final `.` to the last statement. Then we also want to
            // add an empty line to make our code breath a bit better.
            ErlangSourceBuilderPosition::FunctionStatement { .. } => {
                self.indentation -= INDENT;
                self.code.push_str(".\n")
            }
            // When we're done generating statements for an anonymous function
            // we need to add the closing `end` after the last statement on a
            // new line.
            ErlangSourceBuilderPosition::AnonymousFunctionStatement { .. } => {
                self.indentation -= INDENT;
                self.code.push('\n');
                self.push_indentation();
                self.code.push_str("end")
            }
            // When we're done generating statements for a block we need to add
            // the closing `end`, and reduce the nesting level.
            ErlangSourceBuilderPosition::Block { .. } => {
                self.indentation -= INDENT;
                self.code.push('\n');
                self.push_indentation();
                self.code.push_str("end");
            }
            // When we're done generating code for a function spec we want to
            // add a `.` and go to a new line so we can start generating the
            // function itself.
            ErlangSourceBuilderPosition::FunctionSpec => self.code.push_str(".\n"),
            // When we're done generating the arguments of a function we need
            // to add the closed parentheses for the function call!
            ErlangSourceBuilderPosition::FunctionCall {
                expected: ExpectedCallItem::Arguments { first },
                called_item_needs_wrapping,
            } => {
                if called_item_needs_wrapping {
                    self.code.push(')')
                }

                // If the function is closed with no arguments being generated
                // then we will need to add both the open and closed
                // parentheses. That's because the open paren is added when the
                // first argument is generated.
                if first {
                    self.code.push_str("()")
                } else {
                    self.code.push(')')
                }
            }

            // When we're done generating the items of a tuple we need
            // to add the closed curly brace to actually close the tuple.
            ErlangSourceBuilderPosition::TupleType { .. }
            | ErlangSourceBuilderPosition::Tuple { .. }
            | ErlangSourceBuilderPosition::TuplePattern { .. } => self.code.push('}'),
            // When we're done generating a bit array we can add its closing
            // element.
            ErlangSourceBuilderPosition::BitArray { .. } => self.code.push_str(">>"),

            // When the function type arguments are over we add what we need for
            // the return type.
            ErlangSourceBuilderPosition::FunctionType {
                expected: ExpectedFunctionTypeItem::Arguments { .. },
                needs_wrapping,
            } => {
                // After popping the argument, we now need to wait for the
                // return type!
                self.position
                    .push(ErlangSourceBuilderPosition::FunctionType {
                        expected: ExpectedFunctionTypeItem::ReturnType,
                        needs_wrapping,
                    });
                self.code.push_str(") -> ")
            }
            // When a named type is over we need to add the closing parentheses.
            ErlangSourceBuilderPosition::NamedType { .. } => self.code.push(')'),
            // If we close a function type we need to check what the current
            // state is. If we were generating this for a type spec we're done.
            // But if we were generating this as a type inside another we need to
            // add one further parentheses to close the `fun(...)` around the
            // function type.
            //
            // ```erl
            // % in a spec annotation it simply comes after the function name:
            // -spec wibble () -> integer().
            // wibble() -> 11.
            //
            // % but inside another type it has to be wrapped in `fun(...)`:
            // -spec wobble () -> fun(() -> integer())
            // %                                     ^ We're adding this bit here!
            // wobble() -> fun wibble/0.
            // ```
            ErlangSourceBuilderPosition::FunctionType {
                expected: ExpectedFunctionTypeItem::ReturnType,
                needs_wrapping,
            } => {
                if needs_wrapping {
                    self.code.push(')')
                }
            }
            // There's nothing left to do when a union type ends.
            ErlangSourceBuilderPosition::UnionType { .. } => (),
            // When a case expression is over, we need to add the closing `end`.
            ErlangSourceBuilderPosition::Case {
                expected: ExpectedCaseItem::Branches { .. },
            } => {
                self.indentation -= INDENT;
                self.code.push('\n');
                self.push_indentation();
                self.code.push_str("end");
            }

            ErlangSourceBuilderPosition::ClauseGuard => (),
            ErlangSourceBuilderPosition::CaseClause { expected } => match expected {
                ExpectedCaseClauseItem::Pattern => (),
                // When the guards of a case clause are over we need to add the
                // arrow before the body is generated.
                ExpectedCaseClauseItem::Guards { .. } => self.code.push_str(" ->"),
                ExpectedCaseClauseItem::Body { .. } => {
                    self.indentation -= INDENT;
                }
            },
            // We're done with a map, we can add the closed parentheses and
            // reduce nesting.
            ErlangSourceBuilderPosition::Map { .. } => {
                self.indentation -= INDENT;
                self.code.push('\n');
                self.push_indentation();
                self.code.push('}');
            }
            ErlangSourceBuilderPosition::RecordAttribute { .. } => {
                self.indentation -= INDENT;
                self.code.push('\n');
                self.push_indentation();
                self.code.push_str("}).");
            }

            ErlangSourceBuilderPosition::MapField { .. }
            | ErlangSourceBuilderPosition::RecordField { .. }
            | ErlangSourceBuilderPosition::MatchPattern { .. }
            | ErlangSourceBuilderPosition::UnaryOperator
            | ErlangSourceBuilderPosition::List { .. }
            | ErlangSourceBuilderPosition::BinaryOperator { .. }
            | ErlangSourceBuilderPosition::MatchOperator { .. }
            | ErlangSourceBuilderPosition::TypeSpec { .. }
            | ErlangSourceBuilderPosition::BitArraySegment { .. }
            | ErlangSourceBuilderPosition::Case {
                expected: ExpectedCaseItem::Subject,
            }
            | ErlangSourceBuilderPosition::FunctionCall {
                // If we try and close a function for which no called item was
                // generated, then that's an error!
                expected: ExpectedCallItem::FunctionToBeCalled,
                ..
            } => invalid_code_for_position!(self, "pop leftover item"),
        }
    }

    /// Given the content of something that has to end up inside an Erlang
    /// literal string `~"..."` this escapes its content based on
    /// position-specific rules: depending where we will be putting the string
    /// (and so depending where it comes from) it might need different things to
    /// be escaped!
    ///
    /// For example, if we're given the content of a literal Gleam string, we
    /// know that some charachters like double quotes are going to be escaped
    /// already:
    ///
    /// ```gleam
    /// let some_gleam_string = "wibble\"wobble"
    /// ```
    ///
    /// After all, if those weren't escaped our Gleam program would have a
    /// syntax error and not compile!
    ///
    /// However, there's other pieces of syntax that might be turned into
    /// literal Erlang strings: doc comments. In that case those can contain
    /// unescaped characters. For example:
    ///
    /// ```gleam
    /// /// This is a doc comment that has unescaped double quotes ""
    /// ```
    ///
    /// If we were to just take that comment's content and put it in an Erlang
    /// string with no escaping that would produce invalid code!
    ///
    fn escape_string_content(&self, content: &str) -> String {
        let position = self
            .position
            .last()
            .expect("escaping string in the top level scope");

        match position {
            ErlangSourceBuilderPosition::FunctionSpec
            | ErlangSourceBuilderPosition::TypeSpec { .. }
            | ErlangSourceBuilderPosition::FunctionType { .. }
            | ErlangSourceBuilderPosition::NamedType { .. }
            | ErlangSourceBuilderPosition::UnionType { .. }
            | ErlangSourceBuilderPosition::RecordAttribute { .. }
            | ErlangSourceBuilderPosition::TupleType { .. } => {
                invalid_code_for_position!(self, "escaping string")
            }

            ErlangSourceBuilderPosition::FunctionCall { .. }
            | ErlangSourceBuilderPosition::FunctionStatement { .. }
            | ErlangSourceBuilderPosition::AnonymousFunctionStatement { .. }
            | ErlangSourceBuilderPosition::List { .. }
            | ErlangSourceBuilderPosition::UnaryOperator
            | ErlangSourceBuilderPosition::Tuple { .. }
            | ErlangSourceBuilderPosition::TuplePattern { .. }
            | ErlangSourceBuilderPosition::BitArray { .. }
            | ErlangSourceBuilderPosition::BitArraySegment { .. }
            | ErlangSourceBuilderPosition::Block { .. }
            | ErlangSourceBuilderPosition::BinaryOperator { .. }
            | ErlangSourceBuilderPosition::Case { .. }
            | ErlangSourceBuilderPosition::CaseClause { .. }
            | ErlangSourceBuilderPosition::ClauseGuard
            | ErlangSourceBuilderPosition::Map { .. }
            | ErlangSourceBuilderPosition::MapField { .. }
            | ErlangSourceBuilderPosition::MatchPattern { .. }
            | ErlangSourceBuilderPosition::RecordField { .. }
            | ErlangSourceBuilderPosition::MatchOperator { .. } => {
                // When pretty printing we want the resulting code to be regular
                // executable Erlang code.
                // If we're tasked with escaping the content of a literal string
                // expression we need to turn Gleam's `\u` sequences into
                // Erlang's `\x`.
                UNICODE_ESCAPE_SEQUENCE_PATTERN
                    .get_or_init(|| {
                        Regex::new(r#"(\\+)(u)"#)
                            .expect("Unicode escape sequence regex cannot be constructed")
                    })
                    // `\\u`-s should not be affected, so that "\\u..." is not converted to
                    // "\\x...". That's why capturing groups is used to exclude cases that
                    // shouldn't be replaced.
                    .replace_all(content, |caps: &regex::Captures<'_>| {
                        let slashes = caps.get(1).map_or("", |match_| match_.as_str());
                        if slashes.len().is_multiple_of(2) {
                            format!("{slashes}u")
                        } else {
                            format!("{slashes}x")
                        }
                    })
                    .into()
            }
        }
    }

    #[must_use]
    fn new_bit_array_segment(&mut self) -> BitArrayKind {
        self.pop_leftover_items();
        let Some(ErlangSourceBuilderPosition::BitArray { first, kind }) = self.position.last_mut()
        else {
            invalid_code_for_position!(self, "bit array segment")
        };

        if *first {
            *first = false;
        } else {
            self.code.push_str(", ");
        }

        *kind
    }

    fn new_case_clause(&mut self) {
        self.pop_leftover_items();
        let Some(ErlangSourceBuilderPosition::Case {
            expected: ExpectedCaseItem::Branches { first },
        }) = self.position.last_mut()
        else {
            invalid_code_for_position!(self, "case clause")
        };

        if *first {
            *first = false;
            self.code.push_str(" of\n");
            self.indentation += INDENT;
        } else {
            self.code.push_str(";\n\n");
        }
        self.code.push_str(&" ".repeat(self.indentation))
    }

    fn new_map_field(&mut self) {
        self.pop_leftover_items();
        let Some(ErlangSourceBuilderPosition::Map { first }) = self.position.last_mut() else {
            invalid_code_for_position!(self, "map field");
        };

        if *first {
            *first = false;
            self.code.push('\n');
        } else {
            self.code.push_str(",\n")
        }
        self.code.push_str(&" ".repeat(self.indentation))
    }

    fn new_clause_guard(&mut self) {
        self.pop_leftover_items();
        let Some(ErlangSourceBuilderPosition::CaseClause {
            expected:
                ExpectedCaseClauseItem::Guards {
                    first: first @ true,
                },
        }) = self.position.last_mut()
        else {
            invalid_code_for_position!(self, "new clause guard");
        };

        *first = false;
    }

    /// You can call this before generating any expression (before the
    /// `new_expression` call) if we can skip wrapping it in parentheses when it
    /// appears as a bitstring segment value.
    /// This is opt-out rather than opt-in: it's always safe to wrap everything,
    /// but it leads to slightly uglier code for some values like bare integers.
    /// For example:
    ///
    /// ```erl
    /// <<(1)/signed>>
    /// % ^ ^ Those are not needed at all!
    /// ```
    ///
    /// So for those values that you know are safe and would look better you can
    /// use this function.
    ///
    fn do_not_wrap_if_segment_value_or_size(&mut self) {
        self.pop_leftover_items();
        // If we're about to generate a bit array segment value, or a size,
        // then we can skip the wrapping, otherwise this is not needed at all!
        match self.position.last_mut() {
            Some(ErlangSourceBuilderPosition::BitArraySegment {
                segment_value_needs_wrapping,
                expected: BitArraySegmentExpectedItem::Value { .. },
                ..
            }) => *segment_value_needs_wrapping = false,
            Some(ErlangSourceBuilderPosition::BitArraySegment {
                segment_size_needs_wrapping,
                expected: BitArraySegmentExpectedItem::Size,
                ..
            }) => *segment_size_needs_wrapping = false,
            _ => (),
        }
    }

    /// This can be used to output the content of a string wether that is a
    /// pattern or an expression!
    fn do_print_string_content(&mut self, content: &str) {
        let content = self.escape_string_content(content);
        // If we're generating a string as a bit array segment value we want to
        // output slightly different code: rather than a bit array we want to
        // output a regular string with no modifiers which are going to be added
        // later as the segment is created
        if let Some(ErlangSourceBuilderPosition::BitArraySegment {
            // the call to `new_expression` at the beginning is going to advance
            // the state to expect the `::Size`, that means the code we are
            // outputting now was expected to be the the `::Value` of the bit
            // array segment!
            // We can't move the `new_expression` call down, that _must_ be the
            // first thing we do, so we check to see if the size if the next
            // thing we're expecting to see.
            expected: BitArraySegmentExpectedItem::Size,
            segment_value_needs_wrapping: _,
            segment_size_needs_wrapping: _,
        }) = self.position.last()
        {
            self.code.push('"');
            self.code.push_str(&content);
            self.code.push('"');
        } else {
            self.code.push_str("~\"");
            self.code.push_str(&content);
            self.code.push('"');
        }
    }

    fn cons_list_of_kind(&mut self, list_kind: ListKind) {
        self.pop_leftover_items();
        match self.position.last_mut() {
            // If we were expecting the rest of a list and we generate a new
            // cons cell we can keep adding commas to separate items: we want
            // to show `[1, 2, 3]` rather than `[1 | [2 | [3 | []]]]`
            Some(ErlangSourceBuilderPosition::List {
                kind,
                expected: expected @ ExpectedListItem::Rest,
            }) if *kind == list_kind => {
                *expected = ExpectedListItem::First;
                self.code.push_str(", ");
            }
            // Otherwise we have to properly start a new list: push the `[` and
            // wait for the first item to be generated.
            Some(_) | None => {
                match list_kind {
                    ListKind::Pattern => self.new_pattern(),
                    ListKind::Expression => self.new_expression(),
                }
                self.code.push('[');
                self.position.push(ErlangSourceBuilderPosition::List {
                    kind: list_kind,
                    expected: ExpectedListItem::First,
                });
            }
        }
    }

    fn empty_list_of_kind(&mut self, list_kind: ListKind) {
        self.pop_leftover_items();
        match self.position.last_mut() {
            // If we were building a cons list and were expecting the end of the
            // list then we have to special case this: we don't want to render
            // it as: `[1, 2 | []]`, but as `[1, 2]`.
            Some(ErlangSourceBuilderPosition::List {
                kind,
                expected: ExpectedListItem::Rest,
            }) if *kind == list_kind => {
                // Crucially we don't want to call `new_expression` here: we
                // don't want the default handling.
                self.code.push(']');
                // The list is over, so we pop it away.
                self.position.pop();
            }
            Some(_) | None => {
                match list_kind {
                    ListKind::Pattern => self.new_pattern(),
                    ListKind::Expression => self.new_expression(),
                }
                self.code.push_str("[]");
            }
        }
    }

    /// This has to be called before any `new_x` function. This takes care of
    /// removing all leftover items that do not self close.
    ///
    /// That happens when there's some node that requires code to be pushed
    /// _after_ it is over and it is not closed manually with a `end_x`
    /// function.
    fn pop_leftover_items(&mut self) {
        let Some(position) = self.position.last() else {
            return;
        };

        match position {
            ErlangSourceBuilderPosition::TypeSpec {
                expected: TypeSpecExpectedItem::TypeSpecIsOver,
            } => {
                self.code.push_str(".\n");
                self.position.pop();
                self.pop_leftover_items();
            }

            // The expression we're generating is preceded by a list
            // that has been closed. Lists are a bit tricky because they
            // are built as a list of cons cells, so we can't really
            // tell when a list is over until we get to the next
            // expression.
            ErlangSourceBuilderPosition::List {
                expected: ExpectedListItem::ListIsOver,
                ..
            } => {
                self.code.push(']');
                // We remove this leftover list...
                self.position.pop();
                // ...and then we can keep going until there's no leftover
                // items left.
                self.pop_leftover_items();
            }

            // Like with lists, we don't really know when a binary operator is
            // over until we get to the following operation (or we try closing
            // the current item and notice there's a closed operator left).
            // Also, just like lists, we might still need to add some
            // parentheses _after_ the operator is over.
            ErlangSourceBuilderPosition::BinaryOperator {
                expected: ExpectedBinaryOperatorSide::BinaryOperatorIsOver,
                needs_wrapping,
                ..
            } => {
                // If needed add the closing parentheses...
                if *needs_wrapping {
                    self.code.push(')');
                }
                // ...we remove this leftover binary operator...
                self.position.pop();
                // ...and then we can keep going until there's no leftover items
                // left.
                self.pop_leftover_items();
            }

            // All of these items are not leftovers. They are either still open,
            // require manual closing, or things that don't need closing at all!
            ErlangSourceBuilderPosition::NamedType { .. }
            | ErlangSourceBuilderPosition::UnionType { .. }
            | ErlangSourceBuilderPosition::TupleType { .. }
            | ErlangSourceBuilderPosition::FunctionStatement { .. }
            | ErlangSourceBuilderPosition::AnonymousFunctionStatement { .. }
            | ErlangSourceBuilderPosition::FunctionSpec
            | ErlangSourceBuilderPosition::Tuple { .. }
            | ErlangSourceBuilderPosition::TuplePattern { .. }
            | ErlangSourceBuilderPosition::BitArray { .. }
            | ErlangSourceBuilderPosition::Map { .. }
            | ErlangSourceBuilderPosition::Block { .. }
            | ErlangSourceBuilderPosition::ClauseGuard
            | ErlangSourceBuilderPosition::UnaryOperator
            | ErlangSourceBuilderPosition::FunctionType {
                expected:
                    ExpectedFunctionTypeItem::Arguments { .. } | ExpectedFunctionTypeItem::ReturnType,
                ..
            }
            | ErlangSourceBuilderPosition::TypeSpec {
                expected: TypeSpecExpectedItem::TypeDefinition,
            }
            | ErlangSourceBuilderPosition::MapField {
                expected: MapFieldExpectedItem::Key | MapFieldExpectedItem::Value,
            }
            | ErlangSourceBuilderPosition::List {
                expected: ExpectedListItem::First | ExpectedListItem::Rest,
                ..
            }
            | ErlangSourceBuilderPosition::BitArraySegment {
                expected:
                    BitArraySegmentExpectedItem::Size
                    | BitArraySegmentExpectedItem::Specifiers
                    | BitArraySegmentExpectedItem::Value { .. },
                ..
            }
            | ErlangSourceBuilderPosition::FunctionCall {
                expected: ExpectedCallItem::Arguments { .. } | ExpectedCallItem::FunctionToBeCalled,
                ..
            }
            | ErlangSourceBuilderPosition::MatchPattern {
                expected: ExpectedMatchPatternSide::Left | ExpectedMatchPatternSide::Right,
            }
            | ErlangSourceBuilderPosition::Case {
                expected: ExpectedCaseItem::Subject | ExpectedCaseItem::Branches { .. },
            }
            | ErlangSourceBuilderPosition::CaseClause {
                expected:
                    ExpectedCaseClauseItem::Pattern
                    | ExpectedCaseClauseItem::Guards { .. }
                    | ExpectedCaseClauseItem::Body { .. },
            }
            | ErlangSourceBuilderPosition::BinaryOperator {
                expected: ExpectedBinaryOperatorSide::Left | ExpectedBinaryOperatorSide::Right,
                ..
            }
            | ErlangSourceBuilderPosition::RecordAttribute { .. }
            | ErlangSourceBuilderPosition::MatchOperator {
                expected: ExpectedMatchSide::Expression | ExpectedMatchSide::Pattern,
            }
            | ErlangSourceBuilderPosition::RecordField { .. } => (),
        }
    }

    fn new_record_field(&mut self) {
        self.pop_leftover_items();
        let Some(ErlangSourceBuilderPosition::RecordAttribute { first }) = self.position.last_mut()
        else {
            panic!("tried generating record field outside of record attribute");
        };

        if *first {
            *first = false;
        } else {
            self.code.push(',');
        }
        self.code.push('\n');
        self.push_indentation();
    }

    fn push_indentation(&mut self) {
        for _ in 0..self.indentation {
            self.code.push(' ');
        }
    }

    /// This produces a pretty printed error message including the current
    /// position.
    fn error_with_position(&self, expected: &str) -> String {
        let position = self.position.last();
        format!("tried {expected}, position: {position:?}")
    }

    fn doc_content(&mut self, content: DocContent<'_>) {
        match content {
            DocContent::False => self.code.push_str("false"),
            // Escaping strings generated inside doc attributes is a little
            // different: since their content doesn't come from a Gleam
            // literal string but freeform text, their content might contain
            // all sorts of unescaped characters.
            DocContent::String(content) => {
                self.code.push_str("~\"");
                self.code
                    .push_str(&content.replace("\\", "\\\\").replace("\"", "\\\""));
                self.code.push('"');
            }
        }
    }
}

/// This wraps an atom name in between single quotes if needed.
fn quote_atom_name(name: &str) -> String {
    if is_erlang_reserved_word(name) {
        // Escape because of keyword collision
        format!("'{name}'")
    } else if atom_name_regex().is_match(name) {
        String::from(name)
    } else {
        // Escape because of characters contained
        format!("'{name}'")
    }
}

static ATOM_NAME_REGEX: OnceLock<Regex> = OnceLock::new();

fn atom_name_regex() -> &'static Regex {
    ATOM_NAME_REGEX.get_or_init(|| Regex::new(r"^[a-z][a-z0-9_@]*$").expect("atom RE regex"))
}

fn is_erlang_reserved_word(name: &str) -> bool {
    matches!(
        name,
        "!" | "receive"
            | "bnot"
            | "div"
            | "rem"
            | "band"
            | "bor"
            | "bxor"
            | "bsl"
            | "bsr"
            | "not"
            | "and"
            | "or"
            | "xor"
            | "orelse"
            | "andalso"
            | "when"
            | "end"
            | "fun"
            | "try"
            | "catch"
            | "after"
            | "begin"
            | "let"
            | "query"
            | "cond"
            | "if"
            | "of"
            | "case"
            | "maybe"
            | "else"
    )
}
