// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::Zero;
use regex::Regex;
use std::sync::OnceLock;

#[must_use]
/// Represents an open function that has yet to be closed.
/// A function definition is started with `Eaf::start_function` and _must_ be
/// closed using `Eaf::end_function`.
pub struct Function {
    clauses: erlang_term_format::List,
    statements: erlang_term_format::List,
}

#[must_use]
/// Represents an open function call that has yet to be closed.
pub struct Call {
    arguments: erlang_term_format::List,
}

#[must_use]
/// Represents an open tuple that has yet to be closed.
pub struct Tuple {
    items: erlang_term_format::List,
}

#[must_use]
/// Represents an open tuple type that has yet to be closed.
pub struct TupleType {
    items: erlang_term_format::List,
}

#[must_use]
/// Represents an open doc/moduledoc attribute.
pub struct DocAttribute {
    items: erlang_term_format::List,
}

#[must_use]
/// Represents an open function type annotation that has yet to be closed after
/// generating the arguments types and the return type.
pub struct FunctionType {
    types: erlang_term_format::List,
}

#[must_use]
/// Represents an open named type that has yet to be closed after generating
/// the types it takes as an argument (if any).
pub struct NamedType {
    types: erlang_term_format::List,
}

#[must_use]
/// Represents an open alternative type that has yet to be closed after
/// generating all of its alternatives.
pub struct UnionType {
    alternatives: erlang_term_format::List,
}

#[must_use]
/// Represents an open function type annotation that has yet to be closed after
/// generating the arguments types and the return type.
pub struct FunctionSpec {
    representations: erlang_term_format::List,
}

#[must_use]
/// Represents an open block that has yet to be closed after generating the
/// statements that go inside it.
pub struct Block {
    statements: erlang_term_format::List,
}

#[must_use]
/// Represents an open list of arguments' types in a function type annotation
/// that has yet to be closed.
pub struct FunctionTypeArguments {
    types: erlang_term_format::List,
    arguments: erlang_term_format::List,
}

/// Defines the operations to describe the content of an Erlang module.
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
pub trait Eaf<Output> {
    /// Creates a new `Eaf` data structure to generate code for a single module
    /// with the given name.
    ///
    fn new(module_name: &str) -> Self;

    /// Consumes the given `Eaf` turning it into some other representation.
    /// For example that might be a binary representation, or a textual pretty
    /// printed one.
    ///
    fn into_output(self) -> Output;

    /// Adds to the module an export attribute for the given exported functions.
    ///
    /// For example:
    ///
    /// ```ignore
    /// eaf.export_attribute(vec![("wibble", 1), ("wobble", 2)]);
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
    /// eaf.export_attribute(vec![("wibble", 1), ("wobble", 2)]);
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

    /// Starts a `-doc` attribute.
    /// What is generated after calling this function will end up inside the
    /// `-doc` attribute.
    /// You'll most likely always put a string or the atom "false" inside it.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let doc = eaf.start_doc_attribute();
    /// eaf.atom("false");
    /// eaf.close_doc_attribute(doc);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -doc(false).
    /// ```
    ///
    fn start_doc_attribute(&mut self) -> DocAttribute;

    /// Starts a `-moduledoc` attribute.
    /// What is generated after calling this function will end up inside the
    /// `-moduledoc` attribute.
    /// You'll most likely always put a string or the atom "false" inside it.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let doc = eaf.start_moduledoc_attribute();
    /// eaf.atom("false");
    /// eaf.close_doc_attribute(doc);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -moduledoc(false).
    /// ```
    ///
    fn start_moduledoc_attribute(&mut self) -> DocAttribute;

    /// This closes the currently open doc/moduledoc attribute.
    /// Code generated after this is not gonna be part of it.
    ///
    fn end_doc_attribute(&mut self, attribute: DocAttribute);

    /// This generates the code for a `-compile([]).` attribute where all the
    /// strings produces by the given iterator are going to be passed as atom
    /// literals.
    ///
    /// For example:
    ///
    /// ```ignore
    /// eaf.compile_attribute(vec!["no_warn", "inline"]);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -compile([no_warn, inline]).
    /// ```
    ///
    fn compile_attribute<'a>(&mut self, arguments: impl IntoIterator<Item = &'a str>);

    /// This starts a function type spec.
    /// Everything that is generated after this call is interpreted as the
    /// annotated type of the function. So this should be followed by a single
    /// function type.
    ///
    /// After that is complete, this has to be closed using `end_function_spec`.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let spec = eaf.start_function_spec("wibble", 1)
    /// let function_type = eaf.start_function_type();
    /// eaf.int_type();
    /// let function_type eaf.end_function_type_arguments(function_type);
    /// eaf.variable_type("A");
    /// eaf.end_function_type(function_type);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -spec wibble(integer(), A) -> A.
    /// ```
    ///
    fn start_function_spec(&mut self, name: &str, arity: usize) -> FunctionSpec;

    /// This closes the currently open function spec.
    /// Code generated after this is not gonna be part of this function spec.
    ///
    fn end_function_spec(&mut self, function_spec: FunctionSpec);

    /// This starts a function type.
    /// Any code generated after this is gonna be an argument type of the open
    /// function type until `end_function_type_arguments` is called.
    /// After that you should generate a single type that's gonna be the return
    /// type, and then end the function.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let function_type = eaf.start_function_type();
    /// eaf.int_type();
    /// eaf.variable_type("A");
    /// let function_type eaf.end_function_type_arguments(function_type);
    /// eaf.variable_type("A");
    /// eaf.end_function_type(function_type);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// (integer(), A) -> A.
    /// ```
    ///
    fn start_function_type(&mut self) -> FunctionTypeArguments;

    /// This closes the currently open function type arguments list.
    /// This means that the next type that is generated is going to be the
    /// return type of the open function type.
    ///
    /// After that you should call `end_function_type` to close the function
    /// type.
    ///
    fn end_function_type_arguments(&mut self, function_type: FunctionTypeArguments)
    -> FunctionType;

    /// This takes a function type and closes it.
    /// Code generated after this is not gonna be part of this function type.
    ///
    fn end_function_type(&mut self, function_type: FunctionType);

    /// This starts a named type (either defined previously in this module, or
    /// a built-in Erlang type) with the given name.
    /// Any code generated after this is gonna be an argument of the open
    /// named type type until `end_named_type` is called.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let integer = eaf.start_named_type("integer");
    /// eaf.end_named_type(integer);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// integer().
    /// ```
    ///
    fn start_named_type(&mut self, name: &str) -> NamedType;

    /// This starts a remote named type with the given module and name.
    /// Any code generated after this is gonna be an argument of the open
    /// named type type until `end_named_type` is called.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let type_ = eaf.start_remote_named_type("wibble", "wobble");
    /// eaf.end_named_type(type_);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// wibble:wobble().
    /// ```
    ///
    fn start_remote_named_type(&mut self, module: &str, name: &str) -> NamedType;

    /// This takes a named type and closes it.
    /// Code generated after this is not gonna be part of this named type.
    ///
    fn end_named_type(&mut self, named_type: NamedType);

    /// This starts a tuple type.
    /// Any code generated after this is gonna be one of the tuple items.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let tuple = eaf.start_tuple_type();
    /// eaf.literal_atom_type("nil");
    /// eaf.literal_atom_type("ok");
    /// eaf.end_tuple_type(tuple);
    /// ```
    ///
    /// Corresponds to the following Erlang type:
    ///
    /// ```erl
    /// {nil, ok}.
    /// ```
    ///
    fn start_tuple_type(&mut self) -> TupleType;

    /// This takes a tuple type and closes it.
    /// Code generated after this is not gonna be part of this tuple type.
    ///
    fn end_tuple_type(&mut self, tuple: TupleType);

    /// This starts a union type.
    /// Any code generated after this is gonna be a possible alternative of this
    /// type.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let ok_or_error = eaf.start_union_type();
    /// eaf.literal_atom_type("ok");
    /// eaf.literal_atom_type("error");
    /// eaf.end_union_type(ok_or_error);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// ok | error.
    /// ```
    ///
    fn start_union_type(&mut self) -> UnionType;

    /// This takes a union type and closes it.
    /// Code generated after this is not gonna be part of this union type.
    ///
    fn end_union_type(&mut self, union_type: UnionType);

    /// This generated the code for a type variable with the given name.
    ///
    /// For example, if we were to define the type of the identity function we
    /// could do it like this:
    ///
    /// ```ignore
    /// let function = eaf.start_function_type();
    /// eaf.type_variable("A");
    /// let function = eaf.end_function_type_arguments();
    /// eaf.type_variable("A");
    /// eaf.end_function(function);
    /// ```
    ///
    /// And it corresponds to:
    ///
    /// ```erl
    /// (A) -> A.
    /// ```
    ///
    fn type_variable(&mut self, name: &str);

    /// This generated the code for a literal atom type.
    ///
    /// For example, the annotation of a function returning the atom `nil` is
    /// be defined like this:
    ///
    /// ```ignore
    /// let function = eaf.start_function_type();
    /// let function = eaf.end_function_type_arguments();
    /// eaf.literal_atom_type("nil");
    /// eaf.end_function(function);
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
    /// let function = eaf.start_function("first_name", 0, vec![]);
    /// eaf.string("Giacomo");
    /// eaf.end_function(function);
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
    ) -> Function;

    /// This takes a function and closes it.
    /// Code generated after this is not gonna be part of this function.
    ///
    fn end_function(&mut self, function: Function);

    /// This starts a block expression.
    /// Any code generated after this is gonna be a statement inside the open
    /// block.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let block = eaf.start_block();
    /// eaf.string("Giacomo");
    /// eaf.integer(1);
    /// eaf.end_block(block);
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
    fn start_block(&mut self) -> Block;

    /// This takes a block and closes it.
    /// Code generated after this is not gonna be part of this block.
    ///
    fn end_block(&mut self, block: Block);

    /// This starts a remote call.
    /// Any code generated after this is gonna be an argument of the open
    /// function call `end_call` is called.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let call = eaf.start_remote_call("io", "format");
    /// eaf.string("Giacomo");
    /// eaf.end_call(call);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// io:format(~"Giacomo").
    /// ```
    ///
    fn start_remote_call(&mut self, module: &str, function: &str) -> Call;

    /// This starts a function call.
    /// The expression generated immediately after this is going to be the thing
    /// that is called, followed by its arguments.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let call = eaf.start_call();
    /// eaf.atom("wibble")
    /// eaf.string("Hello");
    /// eaf.string("Giacomo");
    /// eaf.end_call(call);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// wibble(~"Hello", ~"Giacomo").
    /// ```
    ///
    fn start_call(&mut self) -> Call;

    /// This takes an open call and closes it.
    /// Code generated after this is not gonna be an argument to this call.
    ///
    fn end_call(&mut self, call: Call);

    /// This starts a tuple.
    /// Any code generated after this is gonna be an item of the tuple.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let tuple = eaf.start_tuple();
    /// eaf.string("Hello");
    /// eaf.integer(1);
    /// eaf.end_tuple(tuple);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// { ~"Hello", 1 }.
    /// ```
    ///
    fn start_tuple(&mut self) -> Tuple;

    /// This takes an open tuple and closes it.
    /// Code generated after this is not gonna be an item of the tuple.
    ///
    fn end_tuple(&mut self, tuple: Tuple);

    /// This creates a list.
    /// The next two generated values are going to be respectively the first
    /// item and the tail of the list.
    ///
    /// For example:
    ///
    /// ```ignore
    /// eaf.cons_list();
    /// eaf.variable("Hello");
    /// eaf.cons_list();
    /// eaf.string("Giacomo");
    /// eaf.empty_list();
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// [Hello | [ ~"Giacomo" | []]].
    /// % Which, with some syntax sugar, is how we represent a list with two
    /// % elements:
    /// % [Hello, ~"Giacomo"]
    /// ```
    ///
    fn cons_list(&mut self);

    /// This creates an empty list.
    ///
    fn empty_list(&mut self);

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
    /// eaf.unary_operator("-");
    /// eaf.variable("X");
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
    /// eaf.binary_operator("+");
    /// eaf.variable("X");
    /// eaf.integer(1);
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
    /// eaf.function_reference(None, "wibble", 1);
    /// eaf.function_reference(Some("io"), "format", 2);
    /// ```
    ///
    /// Correspond to:
    ///
    /// ```erl
    /// fun wibble/1,
    /// fun io:format/2.
    /// ```
    ///
    fn function_reference(&mut self, module: Option<&str>, name: &str, arity: usize);

    /// This is used to create the code that corresponds to an assignment.
    /// A call to this function should always be followed by the generation of
    /// a pattern (the left-hand side of the assignment), and of an expression
    /// (the right-hand side of the assignment).
    ///
    /// For example:
    ///
    /// ```ignore
    /// eaf.match_operator();
    /// eaf.variable_pattern("X");
    /// eaf.integer(1);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// X = 1.
    /// ```
    ///
    fn match_operator(&mut self);

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

    /// This creates a string literal, where the string is represented as a
    /// bit array with the utf8 bytes making up the string.
    /// This is how Gleam string literals are represented in Erlang.
    ///
    /// For example:
    ///
    /// ```ignore
    /// eaf.string("ksiąskę");
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// ~"ksiąskę".
    /// % Which is the same as <<"ksiąskę"/utf8>>
    /// % Or the same as writing the bytes directly:
    /// % <<107, 115, 105, 196, 133, 115, 107, 196, 153>>
    /// ```
    ///
    fn string(&mut self, string: &str);

    /// This creates an integer literal from the given value.
    ///
    /// For example:
    ///
    /// ```ignore
    /// eaf.integer(BigInt::from(2))
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// 2.
    /// ```
    ///
    fn integer(&mut self, value: BigInt);

    /// This creates a float literal from the given value.
    ///
    /// For example:
    ///
    /// ```ignore
    /// eaf.float(1.2)
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
    /// eaf.atom("wibble")
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

/// A structure that implements the EAF trait but rather than producing the
/// Erlang abstract format binary, it will produce a nice and readable Erlang
/// source string that can be used for testing.
#[derive(Debug)]
pub struct PrettyEaf {
    code: String,
    /// This keeps track of what we're generating
    position: Vec<PrettyEafPosition>,
    /// The current indentation to use when generating stuff like case
    /// expressions, block statements, etc.
    indentation: usize,
}

/// This is used to keep track of the current position when generating pretty
/// printed code from an `Eaf`.
#[derive(Debug)]
pub enum PrettyEafPosition {
    /// We're generating a top level documentation attribute like `-doc(false)`,
    /// or `-moduledoc(~"wibble wobble")`.
    DocAttribute,

    /// We're generating a function spec like `-spec wibble(atom()) -> atom()`.
    FunctionSpec,

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

    /// We're generating code for a match operator like `X = 1`.
    /// This is something that happens in multiple steps: first the pattern on
    /// the left hand side, second the expression on the right.
    MatchOperator {
        /// This keeps track of what we need to generate next.
        expected: ExpectedMatchSide,
    },

    /// We're generating a list.
    List {
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
    FunctionCall { expected: ExpectedCallItem },

    /// We're generating code for a binary operator like `1 + 3`.
    BinaryOperator {
        expected: ExpectedBinaryOperatorSide,
        /// Wether this binary operation needs to be wrapped in parentheses or
        /// not.
        needs_to_be_wrapped: bool,
        operator: &'static str,
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
pub enum ExpectedBinaryOperatorSide {
    Left,
    Right,
    BinaryOperatorIsOver,
}

/// When generating a function call, that is made of two parts: the function to
/// be called (that could be a simple literal atom, denoting a function from the
/// current module, or any expression), and its arguments.
///
#[derive(Debug)]
pub enum ExpectedCallItem {
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
pub enum ExpectedFunctionTypeItem {
    Arguments { first: bool },
    ReturnType,
}

/// A match operator is made of two sides: `X = 1`. A pattern and an expression.
///
#[derive(Debug)]
pub enum ExpectedMatchSide {
    /// We're waiting for the pattern on the left hand side of an assignment to
    /// be generated.
    Pattern,
    /// We're waiting for the expression on the right hand side of an assignment
    /// to be generated.
    Expression,
}

/// Lists are built by cons cells, so when building a list we will do something
/// like this: `[a | [b | []]]`.
/// This is telling us if we're expecting the first item, the second list, or if
/// the list is actually over.
///
#[derive(Debug)]
pub enum ExpectedListItem {
    First,
    Rest,
    ListIsOver,
}

static UNICODE_ESCAPE_SEQUENCE_PATTERN: OnceLock<Regex> = OnceLock::new();

/// How does pretty printing work? Here's a high level overview of how it works:
///
/// - when a new element is generated we call the `new_x` method.
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
///   eaf.match_operator()
///   eaf.variable_pattern("X")
///   eaf.integer(1)
///   ```
///
///   Notice how here we don't have a `start_match_operator` and
///   `end_match_operator`. As you'll see in the implementation these will
///   require a bit of extra book-keeping in the `new_x` functions.
///
impl Eaf<String> for PrettyEaf {
    fn new(module_name: &str) -> Self {
        Self {
            code: format!("-module({module_name}).\n"),
            indentation: 0,
            position: vec![],
        }
    }

    fn into_output(mut self) -> String {
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

        self.code.push_str(&format!(
            "-export([{}]).\n",
            exported
                .into_iter()
                .map(|(name, arity)| { format!("{}/{}", quote_atom_name(name.as_ref()), arity) })
                .join(", ")
        ));
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

        self.code.push_str(&format!(
            "-export_type([{}]).\n",
            exported
                .into_iter()
                .map(|(name, arity)| { format!("{}/{}", quote_atom_name(name.as_ref()), arity) })
                .join(", ")
        ));
    }

    fn compile_attribute<'a>(&mut self, arguments: impl IntoIterator<Item = &'a str>) {
        self.code.push_str(&format!(
            "-compile([{}]).\n",
            arguments.into_iter().join(", ")
        ))
    }

    fn start_doc_attribute(&mut self) -> DocAttribute {
        self.code.push_str("-doc(");
        self.position.push(PrettyEafPosition::DocAttribute);
        DocAttribute {
            items: PrettyEaf::dummy_list(),
        }
    }

    fn start_moduledoc_attribute(&mut self) -> DocAttribute {
        self.code.push_str("-moduledoc(");
        self.position.push(PrettyEafPosition::DocAttribute);
        DocAttribute {
            items: PrettyEaf::dummy_list(),
        }
    }

    fn end_doc_attribute(&mut self, attribute: DocAttribute) {
        self.close_currently_open_item();
        attribute.items.consume();
    }

    fn start_function<Name: AsRef<str>>(
        &mut self,
        name: &str,
        _arity: usize,
        arguments_names: impl IntoIterator<Item = Name>,
    ) -> Function {
        self.code.push_str(&quote_atom_name(name));
        self.code.push('(');
        let mut arguments_names = arguments_names.into_iter().peekable();
        while let Some(name) = arguments_names.next() {
            self.code.push_str(name.as_ref());
            // If there's a name coming after this we will add a separator comma
            // as well.
            if !arguments_names.peek().is_none() {
                self.code.push_str(", ");
            }
        }
        self.code.push_str(") ->");

        self.indentation += INDENT;
        self.position
            .push(PrettyEafPosition::FunctionStatement { first: true });

        Function {
            clauses: PrettyEaf::dummy_list(),
            statements: PrettyEaf::dummy_list(),
        }
    }

    fn start_remote_call(&mut self, module: &str, function: &str) -> Call {
        self.new_expression();
        self.code.push_str(&format!(
            "{}:{}",
            quote_atom_name(module),
            quote_atom_name(function),
        ));
        self.position.push(PrettyEafPosition::FunctionCall {
            expected: ExpectedCallItem::Arguments { first: true },
        });
        Call {
            arguments: PrettyEaf::dummy_list(),
        }
    }

    fn variable(&mut self, name: &str) {
        self.new_expression();
        self.code.push_str(name);
    }

    fn variable_pattern(&mut self, name: &str) {
        self.new_pattern();
        self.code.push_str(name);
    }

    fn discard_pattern(&mut self) {
        self.new_pattern();
        self.code.push('_');
    }

    fn string(&mut self, content: &str) {
        self.new_expression();
        let content = self.escape_string_content(content);
        self.code.push_str(&format!("~\"{content}\"",));
    }

    fn integer(&mut self, number: BigInt) {
        self.new_expression();
        self.code.push_str(&format!("{number}"));
    }

    fn float(&mut self, number: f64) {
        self.new_expression();
        if number.is_zero() {
            self.code.push_str("+0.0")
        } else if number.fract().is_zero() {
            self.code.push_str(&format!("{number:.1}"))
        } else {
            self.code.push_str(&format!("{number}"))
        }
    }

    fn start_call(&mut self) -> Call {
        self.new_expression();
        self.position.push(PrettyEafPosition::FunctionCall {
            expected: ExpectedCallItem::FunctionToBeCalled,
        });
        Call {
            arguments: PrettyEaf::dummy_list(),
        }
    }

    fn end_call(&mut self, call: Call) {
        self.close_currently_open_item();
        call.arguments.consume();
    }

    fn end_function(&mut self, function: Function) {
        self.close_currently_open_item();
        function.clauses.consume();
        function.statements.consume();
    }

    fn cons_list(&mut self) {
        match self.position.last_mut() {
            // If we were expecting the rest of a list and we generate a new
            // cons cell we can keep adding commas to separate items: we want
            // to show `[1, 2, 3]` rather than `[1 | [2 | [3 | []]]]`
            Some(PrettyEafPosition::List {
                expected: expected @ ExpectedListItem::Rest,
            }) => {
                *expected = ExpectedListItem::First;
                self.code.push_str(", ");
            }
            // Otherwise we have to properly start a new list: push the `[` and
            // wait for the first item to be generated.
            Some(_) | None => {
                self.new_expression();
                self.code.push('[');
                self.position.push(PrettyEafPosition::List {
                    expected: ExpectedListItem::First,
                });
            }
        }
    }

    fn empty_list(&mut self) {
        match self.position.last_mut() {
            // If we were building a cons list and were expecting the end of the
            // list then we have to special case this: we don't want to render
            // it as: `[1, 2 | []]`, but as `[1, 2]`.
            Some(PrettyEafPosition::List {
                expected: ExpectedListItem::Rest,
            }) => {
                // Crucially we don't want to call `new_expression` here: we
                // don't want the default handling.
                self.code.push(']');
                // The list is over, so we pop it away.
                self.position.pop();
            }
            Some(_) | None => {
                self.new_expression();
                self.code.push_str("[]");
            }
        }
    }

    fn unary_operator(&mut self, operator: &str) {
        self.new_expression();
        self.code.push_str(operator);
        self.code.push(' ');
        self.position.push(PrettyEafPosition::UnaryOperator)
    }

    fn binary_operator(&mut self, operator: &'static str) {
        self.new_expression();

        // If this new binary operator we're generating is part of a bigger
        // binary operator (it doesn't matter if on the left or right-hand
        // side), then we want to wrap it in parentheses to avoid precedence
        // confusion!
        let needs_to_be_wrapped = match self.position.last() {
            Some(PrettyEafPosition::BinaryOperator { .. }) => {
                self.code.push('(');
                true
            }
            Some(_) | None => false,
        };

        self.position.push(PrettyEafPosition::BinaryOperator {
            expected: ExpectedBinaryOperatorSide::Left,
            needs_to_be_wrapped,
            operator,
        })
    }

    fn match_operator(&mut self) {
        self.new_expression();
        self.position.push(PrettyEafPosition::MatchOperator {
            expected: ExpectedMatchSide::Pattern,
        });
    }

    fn start_tuple(&mut self) -> Tuple {
        self.new_expression();
        self.code.push('{');
        self.position.push(PrettyEafPosition::Tuple { first: true });

        Tuple {
            items: PrettyEaf::dummy_list(),
        }
    }

    fn end_tuple(&mut self, tuple: Tuple) {
        self.close_currently_open_item();
        tuple.items.consume();
    }

    fn start_function_type(&mut self) -> FunctionTypeArguments {
        self.new_type();

        let needs_wrapping = if let Some(PrettyEafPosition::FunctionSpec) = self.position.last() {
            self.code.push('(');
            false
        } else {
            self.code.push_str("fun((");
            true
        };

        self.position.push(PrettyEafPosition::FunctionType {
            expected: ExpectedFunctionTypeItem::Arguments { first: true },
            needs_wrapping,
        });

        FunctionTypeArguments {
            types: PrettyEaf::dummy_list(),
            arguments: PrettyEaf::dummy_list(),
        }
    }

    fn end_function_type_arguments(
        &mut self,
        function_type: FunctionTypeArguments,
    ) -> FunctionType {
        self.close_currently_open_item();
        function_type.arguments.consume();
        FunctionType {
            types: function_type.types,
        }
    }

    fn end_function_type(&mut self, function_type: FunctionType) {
        self.close_currently_open_item();
        function_type.types.consume();
    }

    fn start_function_spec(&mut self, name: &str, _arity: usize) -> FunctionSpec {
        self.code
            .push_str(&format!("\n-spec {}", quote_atom_name(name)));
        self.position.push(PrettyEafPosition::FunctionSpec);
        FunctionSpec {
            representations: PrettyEaf::dummy_list(),
        }
    }

    fn end_function_spec(&mut self, function_spec: FunctionSpec) {
        self.close_currently_open_item();
        function_spec.representations.consume();
    }

    fn type_variable(&mut self, name: &str) {
        self.new_type();
        self.code.push_str(name);
    }

    fn literal_atom_type(&mut self, name: &str) {
        self.new_type();
        self.code.push_str(&quote_atom_name(name));
    }

    fn start_named_type(&mut self, name: &str) -> NamedType {
        self.new_type();
        self.position
            .push(PrettyEafPosition::NamedType { first: true });
        self.code.push_str(name);
        self.code.push('(');

        NamedType {
            types: PrettyEaf::dummy_list(),
        }
    }

    fn start_remote_named_type(&mut self, module: &str, name: &str) -> NamedType {
        self.new_type();
        self.position
            .push(PrettyEafPosition::NamedType { first: true });
        self.code.push_str(module);
        self.code.push(':');
        self.code.push_str(name);
        self.code.push('(');

        NamedType {
            types: PrettyEaf::dummy_list(),
        }
    }

    fn end_named_type(&mut self, named_type: NamedType) {
        self.close_currently_open_item();
        named_type.types.consume();
    }

    fn start_union_type(&mut self) -> UnionType {
        self.new_type();
        self.position
            .push(PrettyEafPosition::UnionType { first: true });

        UnionType {
            alternatives: PrettyEaf::dummy_list(),
        }
    }

    fn end_union_type(&mut self, union_type: UnionType) {
        self.close_currently_open_item();
        union_type.alternatives.consume();
    }

    fn start_tuple_type(&mut self) -> TupleType {
        self.new_type();
        self.position
            .push(PrettyEafPosition::TupleType { first: true });
        self.code.push('{');

        TupleType {
            items: PrettyEaf::dummy_list(),
        }
    }

    fn end_tuple_type(&mut self, tuple: TupleType) {
        self.close_currently_open_item();
        tuple.items.consume();
    }

    fn atom(&mut self, name: &str) {
        self.new_expression();
        self.code.push_str(&quote_atom_name(name));
    }

    fn start_block(&mut self) -> Block {
        self.new_expression();
        self.code.push_str("begin");
        self.indentation += INDENT;
        self.position.push(PrettyEafPosition::Block { first: true });

        Block {
            statements: PrettyEaf::dummy_list(),
        }
    }

    fn end_block(&mut self, block: Block) {
        self.close_currently_open_item();
        block.statements.consume();
    }

    fn function_reference(&mut self, module: Option<&str>, name: &str, arity: usize) {
        self.new_expression();
        self.code.push_str("fun ");
        if let Some(module) = module {
            self.code.push_str(&quote_atom_name(module));
            self.code.push(':');
        }
        self.code.push_str(&quote_atom_name(name));
        self.code.push('/');
        self.code.push_str(&format!("{arity}"));
    }
}

const INDENT: usize = 4;

impl PrettyEaf {
    fn dummy_list() -> erlang_term_format::List {
        erlang_term_format::List::new(0)
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
        let Some(position) = self.position.last_mut() else {
            panic!("new expression at the module top level");
        };

        match position {
            PrettyEafPosition::DocAttribute => (),

            PrettyEafPosition::FunctionCall {
                expected: expected @ ExpectedCallItem::FunctionToBeCalled,
            } => *expected = ExpectedCallItem::Arguments { first: true },

            PrettyEafPosition::FunctionCall {
                expected: ExpectedCallItem::Arguments { first },
            } => {
                if *first {
                    self.code.push('(')
                } else {
                    self.code.push_str(", ");
                }
                *first = false;
            }
            PrettyEafPosition::Tuple { first } => {
                if !*first {
                    self.code.push_str(", ");
                }
                *first = false;
            }
            PrettyEafPosition::List { expected } => match expected {
                // We're expecting the list's head. We don't have to add
                // anything!
                ExpectedListItem::First => *expected = ExpectedListItem::Rest,
                ExpectedListItem::Rest => {
                    *expected = ExpectedListItem::ListIsOver;
                    self.code.push_str(" | ")
                }
                // The expression we're generating is preceded by a list
                // that has been closed. Lists are a bit tricky because they
                // are built as a list of cons cells, so we can't really
                // tell when a list is over until we get to the next
                // expression.
                ExpectedListItem::ListIsOver => {
                    self.code.push(']');
                    // We remove this leftover list...
                    self.position.pop();
                    // ...and then we can properly deal with this new
                    // expression.
                    self.new_expression();
                }
            },

            PrettyEafPosition::BinaryOperator {
                expected,
                needs_to_be_wrapped,
                operator,
            } => {
                match expected {
                    ExpectedBinaryOperatorSide::Left => {
                        *expected = ExpectedBinaryOperatorSide::Right
                    }
                    ExpectedBinaryOperatorSide::Right => {
                        *expected = ExpectedBinaryOperatorSide::BinaryOperatorIsOver;
                        self.code.push(' ');
                        self.code.push_str(operator);
                        self.code.push(' ');
                    }
                    // Like with lists, we don't really know when a binary
                    // operator is over until we get to the following operation
                    // (or we try closing the current item and notice there's a
                    // closed operator left).
                    // Also, just like lists, we might still need to add some
                    // parentheses _after_ the operator is over.
                    ExpectedBinaryOperatorSide::BinaryOperatorIsOver => {
                        // If needed add the closing parentheses...
                        if *needs_to_be_wrapped {
                            self.code.push(')');
                        }
                        // ...we remove this leftover binary operator...
                        self.position.pop();
                        // ...and then we can properly deal with this new
                        // expression.
                        self.new_expression();
                    }
                }
            }

            PrettyEafPosition::FunctionStatement { first } | PrettyEafPosition::Block { first } => {
                if !*first {
                    self.code.push(',');
                }
                self.code.push('\n');
                self.code.push_str(&" ".repeat(self.indentation));
                *first = false;
            }

            PrettyEafPosition::MatchOperator {
                expected: ExpectedMatchSide::Pattern,
            } => {
                panic!("tried generating expression inside a pattern")
            }

            PrettyEafPosition::FunctionSpec
            | PrettyEafPosition::NamedType { .. }
            | PrettyEafPosition::FunctionType { .. }
            | PrettyEafPosition::UnionType { .. }
            | PrettyEafPosition::TupleType { .. } => {
                panic!("tried generating expression inside a type")
            }

            // We were expecting an expression and someone is about to generate
            // it, we can pop this off the stack and need to add the ` = `
            // separating the previous pattern from this new expression.
            PrettyEafPosition::MatchOperator {
                expected: ExpectedMatchSide::Expression,
            } => {
                self.code.push_str(" = ");
                self.position.pop();
            }
            // We were expecting an expression and someone generated it, we can
            // now pop this off the stack.
            PrettyEafPosition::UnaryOperator => {
                self.position.pop();
            }
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
        let Some(position) = self.position.last_mut() else {
            panic!("new type at the module top level")
        };

        match position {
            PrettyEafPosition::FunctionSpec => {}
            PrettyEafPosition::FunctionType {
                expected: ExpectedFunctionTypeItem::ReturnType,
                needs_wrapping: _,
            } => {}

            PrettyEafPosition::FunctionType {
                expected: ExpectedFunctionTypeItem::Arguments { first },
                needs_wrapping: _,
            }
            | PrettyEafPosition::TupleType { first }
            | PrettyEafPosition::NamedType { first } => {
                if !*first {
                    self.code.push_str(", ")
                }
                *first = false
            }

            PrettyEafPosition::UnionType { first } => {
                if !*first {
                    self.code.push_str(" | ")
                }
                *first = false
            }

            PrettyEafPosition::MatchOperator {
                expected: ExpectedMatchSide::Pattern,
            } => {
                panic!("tried generating a type inside a pattern")
            }

            PrettyEafPosition::FunctionCall { .. }
            | PrettyEafPosition::Block { .. }
            | PrettyEafPosition::Tuple { .. }
            | PrettyEafPosition::List { .. }
            | PrettyEafPosition::FunctionStatement { .. }
            | PrettyEafPosition::DocAttribute
            | PrettyEafPosition::UnaryOperator
            | PrettyEafPosition::BinaryOperator { .. }
            | PrettyEafPosition::MatchOperator {
                expected: ExpectedMatchSide::Expression,
            } => {
                panic!("tried generating a type inside an expression")
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
        let Some(position) = self.position.last_mut() else {
            panic!("new pattern at the module top level")
        };

        match position {
            PrettyEafPosition::FunctionCall { .. }
            | PrettyEafPosition::Block { .. }
            | PrettyEafPosition::FunctionStatement { .. }
            | PrettyEafPosition::List { .. }
            | PrettyEafPosition::UnaryOperator
            | PrettyEafPosition::BinaryOperator { .. }
            | PrettyEafPosition::MatchOperator {
                expected: ExpectedMatchSide::Expression,
            }
            | PrettyEafPosition::Tuple { .. } => {
                panic!("tried generating pattern in expression")
            }

            PrettyEafPosition::FunctionType { .. }
            | PrettyEafPosition::FunctionSpec
            | PrettyEafPosition::NamedType { .. }
            | PrettyEafPosition::UnionType { .. }
            | PrettyEafPosition::TupleType { .. } => panic!("tried generating pattern in type"),

            PrettyEafPosition::DocAttribute => panic!("tried generating pattern in attribute"),

            // We were waiting for the pattern to be generated, now we're done
            // and can start generating an expression for the right-hand side.
            PrettyEafPosition::MatchOperator {
                expected: expected @ ExpectedMatchSide::Pattern,
            } => {
                *expected = ExpectedMatchSide::Expression;
            }
        }
    }

    fn close_currently_open_item(&mut self) {
        let Some(position) = self.position.pop() else {
            return;
        };

        match position {
            PrettyEafPosition::UnaryOperator => panic!("popped unary operator"),
            PrettyEafPosition::List { expected } => match expected {
                ExpectedListItem::First => panic!("popped list expecting first item"),
                ExpectedListItem::Rest => panic!("popped list expecting tail"),
                // We want to close the current item but we first have to deal
                // with a possibly leftover list.
                // Lists are a bit tricky because they are built as a list of
                // cons cells, so we can't really tell when a list is over until
                // we get to the next expression.
                ExpectedListItem::ListIsOver => {
                    // We close the list...
                    self.code.push(']');
                    // ...and then we can properly deal with the open item we
                    // want to close.
                    self.close_currently_open_item();
                }
            },
            PrettyEafPosition::BinaryOperator {
                expected,
                needs_to_be_wrapped,
                ..
            } => match expected {
                ExpectedBinaryOperatorSide::Left => {
                    panic!("popped operator expecting left hand side")
                }
                ExpectedBinaryOperatorSide::Right => {
                    panic!("popped operator expecting right hand side")
                }
                // We want to close the current item but we first have to deal
                // with a possibly leftover binary operator.
                // Operators are a bit tricky because we can't really tell when
                // one is over until we get to the next expression.
                ExpectedBinaryOperatorSide::BinaryOperatorIsOver => {
                    if needs_to_be_wrapped {
                        self.code.push(')')
                    }
                    self.close_currently_open_item();
                }
            },
            PrettyEafPosition::MatchOperator { expected } => match expected {
                ExpectedMatchSide::Pattern => panic!("popped match expecting pattern"),
                ExpectedMatchSide::Expression => panic!("popped match expecting expression"),
            },
            // When we're done generating statements for a function we need to
            // add one final `.` to the last statement. Then we also want to
            // add an empty line to make our code breath a bit better.
            PrettyEafPosition::FunctionStatement { .. } => {
                self.indentation -= INDENT;
                self.code.push_str(".\n")
            }
            // When we're done generating statements for a block we need to add
            // the closing `end`, and reduce the nesting level.
            PrettyEafPosition::Block { .. } => {
                self.indentation -= INDENT;
                self.code.push('\n');
                self.code.push_str(&" ".repeat(self.indentation));
                self.code.push_str("end");
            }
            // When we're done generating code for a function spec we want to
            // add a `.` and go to a new line so we can start generating the
            // function itself.
            PrettyEafPosition::FunctionSpec => self.code.push_str(".\n"),
            // When we're done generating the arguments of a function we need
            // to add the closed parentheses for the function call!
            PrettyEafPosition::FunctionCall {
                expected: ExpectedCallItem::Arguments { first },
            } => {
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
            // If we try and close a function for which no called item was
            // generated, then that's an error!
            PrettyEafPosition::FunctionCall {
                expected: ExpectedCallItem::FunctionToBeCalled,
            } => panic!("popped call with no called function"),
            // When we're done generating the items of a tuple we need
            // to add the closed curly brace to actually close the tuple.
            PrettyEafPosition::TupleType { .. } | PrettyEafPosition::Tuple { .. } => {
                self.code.push('}')
            }
            // When the function type arguments are over we add what we need for
            // the return type.
            PrettyEafPosition::FunctionType {
                expected: ExpectedFunctionTypeItem::Arguments { .. },
                needs_wrapping,
            } => {
                // After popping the argument, we now need to wait for the
                // return type!
                self.position.push(PrettyEafPosition::FunctionType {
                    expected: ExpectedFunctionTypeItem::ReturnType,
                    needs_wrapping,
                });
                self.code.push_str(") -> ")
            }
            // When a named type is over we need to add the closing parentheses.
            PrettyEafPosition::NamedType { .. } => self.code.push(')'),
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
            PrettyEafPosition::FunctionType {
                expected: ExpectedFunctionTypeItem::ReturnType,
                needs_wrapping,
            } => {
                if needs_wrapping {
                    self.code.push(')')
                }
            }
            // There's nothing left to do when a union type ends.
            PrettyEafPosition::UnionType { .. } => (),
            // When a doc attribute is closed we need to add the closed
            // parentheses and a newline.
            PrettyEafPosition::DocAttribute => self.code.push_str(").\n"),
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
            PrettyEafPosition::FunctionSpec
            | PrettyEafPosition::FunctionType { .. }
            | PrettyEafPosition::NamedType { .. }
            | PrettyEafPosition::UnionType { .. }
            | PrettyEafPosition::TupleType { .. } => panic!("escaping string inside a type"),

            PrettyEafPosition::FunctionCall { .. }
            | PrettyEafPosition::FunctionStatement { .. }
            | PrettyEafPosition::List { .. }
            | PrettyEafPosition::UnaryOperator
            | PrettyEafPosition::Tuple { .. }
            | PrettyEafPosition::Block { .. }
            | PrettyEafPosition::BinaryOperator { .. }
            | PrettyEafPosition::MatchOperator { .. } => {
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

            PrettyEafPosition::DocAttribute => {
                // Escaping strings generated inside doc attributes is a little
                // different: since their content doesn't come from a Gleam
                // literal string but freeform text, their content might contain
                // all sorts of unescaped characters.
                content.replace("\\", "\\\\").replace("\"", "\\\"")
            }
        }
    }
}

/// This wraps an atom name in between single quotes if needed.
fn quote_atom_name(name: &str) -> String {
    if is_erlang_reserved_word(name) {
        // Escape because of keyword collision
        format!("'{name}'")
    } else if atom_pattern().is_match(name) {
        String::from(name)
    } else {
        // Escape because of characters contained
        format!("'{name}'")
    }
}

static ATOM_PATTERN: OnceLock<Regex> = OnceLock::new();

fn atom_pattern() -> &'static Regex {
    ATOM_PATTERN.get_or_init(|| Regex::new(r"^[a-z][a-z0-9_@]*$").expect("atom RE regex"))
}

fn is_erlang_reserved_word(name: &str) -> bool {
    match name {
        "!" | "receive" | "bnot" | "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr"
        | "not" | "and" | "or" | "xor" | "orelse" | "andalso" | "when" | "end" | "fun" | "try"
        | "catch" | "after" | "begin" | "let" | "query" | "cond" | "if" | "of" | "case"
        | "maybe" | "else" => true,
        _ => false,
    }
}
