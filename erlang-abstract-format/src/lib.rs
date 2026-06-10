use std::ops::Neg;

use erlang_term_format::Etf;
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::Signed;

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
/// Represents an open function type annotation that has yet to be closed after
/// generating the arguments types and the return type.
pub struct FunctionType {
    types: erlang_term_format::List,
}

#[must_use]
/// Represents an open function type annotation that has yet to be closed after
/// generating the arguments types and the return type.
pub struct FunctionSpec {
    representations: erlang_term_format::List,
}

#[must_use]
/// Represents an open list of arguments' types in a function type annotation
/// that has yet to be closed.
pub struct FunctionTypeArguments {
    types: erlang_term_format::List,
    arguments: erlang_term_format::List,
}

/// Defines the operations to describe the content of an Erlang module.
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

    /// Adds to the module a doc attribute with the given content.
    /// The doc comment will refer to the form immediately following it.
    ///
    /// For example:
    ///
    /// ```ignore
    /// eaf.doc_attribute("Some fancy docs");
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -doc(~"Some fancy docs").
    /// ```
    ///
    fn doc_attribute(&mut self, docs: &str);

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

    /// This generates the code for an integer type.
    ///
    /// For example:
    ///
    /// ```ignore
    /// eaf.integer_type();
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```
    /// integer().
    /// ```
    ///
    /// This can be used in erlang type annotations and to build more complex
    /// types!
    ///
    fn int_type(&mut self);

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
    /// Any code generated after this is gonna be an argument of the open
    /// function call `end_call` is called.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let call = eaf.start_call("wibble");
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
    fn start_call(&mut self, function: &str) -> Call;

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

    /// This generated the code that is going to apply the unary negation
    /// operator to the expression that is going to be generated next.
    /// For example:
    ///
    /// ```ignore
    /// eaf.unary_negation();
    /// eaf.variable("X");
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// -X.
    /// ```
    ///
    fn unary_negation(&mut self);

    /// This creates the code that is going to assign the expression that is
    /// generated next to a variable with the given name.
    ///
    /// For example:
    ///
    /// ```ignore
    /// eaf.variable_assignment("X");
    /// eaf.integer(1);
    /// ```
    ///
    /// Corresponds to:
    ///
    /// ```erl
    /// X = 1.
    /// ```
    ///
    fn variable_assignment(&mut self, name: &str);

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
    /// {integer, Annotation, 2}.
    /// ```
    ///
    fn integer(&mut self, value: BigInt);
}

/// This is a data structure that can be used to produce the Erlang abstract
/// format binary representation of an Erlang module.
#[derive(Debug)]
pub struct BinaryEaf {
    etf: Etf,
    open_module: erlang_term_format::List,
    /// This is a stack keeping track of the position we find ourselves in.
    /// This is used to keep track of how many statements are in a function
    /// body, in a list, or in other places with a variable number of values.
    position: Vec<BinaryEafPosition>,
    module_forms: u32,
}

#[derive(Debug)]
enum BinaryEafPosition {
    /// We're generating code for a function's body.
    /// Any expression that is generated in this position is going to end up
    /// inside the function's body as a statement.
    ///
    FunctionBody { statements: u32 },

    /// We're generating code for a function call.
    /// Any expression that is generated in this position is going to end up
    /// as one of the function's arguments.
    ///
    FunctionCall { arguments: u32 },

    /// We're generating code for a tuple.
    /// Any expression that is generated in this position is going to end up as
    /// one of the tuple's items.
    Tuple { items: u32 },

    /// We're generating code that describes a list, the next expression that is
    /// generated is going to be the list's head.
    ///
    ListHead,

    /// We're generating code that describes a list, the next expression that is
    /// generated is going to be the list's tail; the head has been generated
    /// already.
    ///
    ListTail,

    /// We've generated code for the unary negation operator and are expecting
    /// the expression that should come after it.
    ///
    UnaryNegation,

    /// We've generated code to assign to a variable, and are expecting the
    /// expression that should be assigned to it.
    ///
    VariableAssignment,
}

impl BinaryEaf {
    fn module_attribute(&mut self, module_name: &str) {
        self.ensure_top_level();
        self.module_forms += 1;

        self.etf.small_tuple(4);
        self.etf.atom("attribute");
        self.annotation();
        self.etf.atom("module");
        self.etf.atom(module_name);
    }

    fn annotation(&mut self) {
        let list = self.etf.start_list();
        self.etf.small_tuple(2);
        self.etf.atom("location");
        self.etf.usize(1);
        self.etf.end_list(list, 1);
    }

    fn variable_pattern(&mut self, argument_name: &str) {
        self.etf.small_tuple(3);
        self.etf.atom("var");
        self.annotation();
        self.etf.atom(argument_name);
    }

    /// Generates the eaf representation of an atom literal.
    fn atom_literal(&mut self, atom: &str) {
        self.etf.small_tuple(3);
        self.etf.atom("atom");
        self.annotation();
        self.etf.atom(atom);
    }

    /// Generates the eaf representation of an Erlang charlist literal with the
    /// given content.
    fn charlist(&mut self, content: &str) {
        self.etf.small_tuple(3);
        self.etf.atom("string");
        self.annotation();
        let characters = self.etf.start_list();
        for char in content.bytes() {
            self.etf.usize(char as usize);
        }
        self.etf.end_list(characters, content.len() as u32);
    }

    fn ensure_top_level(&self) {
        assert!(
            self.position.is_empty(),
            "called not at the top level position"
        )
    }

    /// This is called before generating code for any expression. It is going to
    /// update the current position based on where we are updating the number
    /// of tracked expressions/function arguments/etc.
    fn new_expression(&mut self) {
        match self.position.last_mut() {
            None => unreachable!("generating expression at the top level"),
            Some(BinaryEafPosition::FunctionBody { statements }) => *statements += 1,
            Some(BinaryEafPosition::FunctionCall { arguments }) => *arguments += 1,
            Some(BinaryEafPosition::Tuple { items }) => *items += 1,
            Some(BinaryEafPosition::ListHead) => {
                self.position.pop();
                self.position.push(BinaryEafPosition::ListTail)
            }
            Some(BinaryEafPosition::ListTail)
            | Some(BinaryEafPosition::UnaryNegation)
            | Some(BinaryEafPosition::VariableAssignment) => {
                self.position.pop();
            }
        }
    }

    /// This is called before generating code for any assignment. It is going to
    /// update the current position based on where we are updating the number
    /// of tracked statements.
    fn new_assignment(&mut self) {
        match self.position.last_mut() {
            None => unreachable!("generating an assignment at the top level"),
            Some(position) => match position {
                BinaryEafPosition::FunctionBody { statements } => *statements += 1,
                BinaryEafPosition::FunctionCall { .. } => {
                    unreachable!("new assignment as function argument")
                }
                BinaryEafPosition::Tuple { .. } => {
                    unreachable!("new assignment as tuple item")
                }
                BinaryEafPosition::ListHead => unreachable!("new assignment as list head"),
                BinaryEafPosition::ListTail => unreachable!("new assignment as list tail"),
                BinaryEafPosition::UnaryNegation => {
                    unreachable!("new assignment as unary negation")
                }
                BinaryEafPosition::VariableAssignment => {
                    unreachable!("new assignment as assignment")
                }
            },
        }
    }
}

impl Eaf<Vec<u8>> for BinaryEaf {
    fn new(module_name: &str) -> Self {
        let mut etf = Etf::new();
        let open_module = etf.start_list();
        let mut this = Self {
            etf,
            open_module,
            position: vec![],
            module_forms: 0,
        };
        this.module_attribute(module_name);
        this
    }

    fn into_output(mut self) -> Vec<u8> {
        self.etf.end_list(self.open_module, self.module_forms);
        self.etf.into_vec()
    }

    fn export_attribute<Name: AsRef<str>>(
        &mut self,
        exported: impl IntoIterator<Item = (Name, usize)>,
    ) {
        self.ensure_top_level();

        // If there's no item in the iterator we don't add the attribute at all.
        let mut exported = exported.into_iter().peekable();
        if exported.peek().is_none() {
            return;
        }

        self.module_forms += 1;

        self.etf.small_tuple(4);
        self.etf.atom("attribute");
        self.annotation();
        self.etf.atom("export");

        let mut count = 0;
        let list = self.etf.start_list();
        for (name, arity) in exported {
            count += 1;
            self.etf.small_tuple(2);
            self.etf.atom(name.as_ref());
            self.etf.usize(arity);
        }
        self.etf.end_list(list, count);
    }

    fn export_type_attribute<Name: AsRef<str>>(
        &mut self,
        exported: impl IntoIterator<Item = (Name, usize)>,
    ) {
        self.ensure_top_level();

        // If there's no item in the iterator we don't add the attribute at all.
        let mut exported = exported.into_iter().peekable();
        if exported.peek().is_none() {
            return;
        }

        self.module_forms += 1;

        self.etf.small_tuple(4);
        self.etf.atom("attribute");
        self.annotation();
        self.etf.atom("export_type");

        let mut count = 0;
        let list = self.etf.start_list();
        for (name, arity) in exported {
            count += 1;
            self.etf.small_tuple(2);
            self.etf.atom(name.as_ref());
            self.etf.usize(arity);
        }
        self.etf.end_list(list, count);
    }

    fn doc_attribute(&mut self, content: &str) {
        self.ensure_top_level();
        self.module_forms += 1;

        self.etf.small_tuple(4);
        self.etf.atom("attribute");
        self.annotation();
        self.etf.atom("doc");
        self.etf.binary(content.len() as u32, content.bytes());
    }

    fn start_function<Name: AsRef<str>>(
        &mut self,
        function_name: &str,
        arity: usize,
        arguments_names: impl IntoIterator<Item = Name>,
    ) -> Function {
        self.ensure_top_level();
        self.module_forms += 1;
        self.position
            .push(BinaryEafPosition::FunctionBody { statements: 0 });

        // A function is represented as a tuple that looks like this:
        //
        //  {function,ANNO,Name,Arity,[Rep(Fc_1), ...,Rep(Fc_k)]}
        //
        // Where `Rep(Fc_1)`, ..., `Rep(Fc_k)` are the representation of the
        // function clauses.
        // Each Gleam function is always compiled down to a function with a
        // single clause. So that list should always contain a single item.
        //
        // A clause is the represented as this:
        //
        //  {clause,ANNO,Rep(Ps),[],Rep(B)}
        //
        // - `Rep(Ps)` is the representation of the arguments of the clause.
        //   In Gleam the generated function's arguments are always gonna be
        //   simple variables (and not contain any pattern).
        // - `Rep(B)` is the representation of the statements of the clause.
        //   That is, a list with the representation of each individual
        //   statement.
        //
        // So, given how a Gleam function is compiled to Erlang we can see that
        // its representation is gonna be:
        //
        //  {
        //     function, ANNO, Name, Arity, [{
        //        clause, ANNO, [{var, ANNO, arg0}, ...], [], [
        //          Rep(Statement0),
        //          Rep(Statement1),
        //          ...
        //        ]
        //     }]
        //  }

        self.etf.small_tuple(5);
        self.etf.atom("function");
        self.annotation();
        self.etf.atom(function_name);
        self.etf.usize(arity);

        // This list is gonna contain just a single clause! And we immediately
        // write the clause tuple.
        let clauses = self.etf.start_list();
        self.etf.small_tuple(5);
        self.etf.atom("clause");
        self.annotation();

        // Now we add a list with the arguments to the clause tuple.
        let arguments = self.etf.start_list();
        let mut count = 0;
        for argument_name in arguments_names {
            count += 1;
            self.variable_pattern(argument_name.as_ref())
        }
        self.etf.end_list(arguments, count);

        // The spec now wants an empty list.
        self.etf.empty_list();

        // Finally we open the list that's gonna contain the function
        // statements.
        // We can't tell how long that's gonna be yet, and we'll set that length
        // only when the function body gets closed and we know how many
        // statements we've actually generated.
        let statements = self.etf.start_list();
        // Only once the caller is done adding statements we can close the
        // statements list and then the outer clauses list containing the
        // function clause.
        Function {
            clauses,
            statements,
        }
    }

    fn end_function(&mut self, function: Function) {
        let Some(BinaryEafPosition::FunctionBody { statements }) = self.position.pop() else {
            unreachable!("ending a function while not in function position")
        };
        self.etf.end_list(function.statements, statements);
        self.etf.end_list(function.clauses, 1);
    }

    fn start_tuple(&mut self) -> Tuple {
        self.new_expression();
        self.position.push(BinaryEafPosition::Tuple { items: 0 });

        self.etf.small_tuple(3);
        self.etf.atom("tuple");
        self.annotation();
        let items = self.etf.start_list();
        Tuple { items }
    }

    fn end_tuple(&mut self, tuple: Tuple) {
        let Some(BinaryEafPosition::Tuple { items }) = self.position.pop() else {
            unreachable!("ending a tuple while not in tuple position")
        };
        self.etf.end_list(tuple.items, items);
    }

    fn start_remote_call(&mut self, module: &str, name: &str) -> Call {
        self.new_expression();
        self.position
            .push(BinaryEafPosition::FunctionCall { arguments: 0 });

        // A remote call is represented by the following tuple:
        //
        //   {
        //     call, ANNO, {remote, ANNO, Rep(E_m), Rep(E_0)}, [
        //       Rep(E_1),
        //       ...,
        //       Rep(E_k)
        //     ]
        //   }
        //
        self.etf.small_tuple(4);
        self.etf.atom("call");
        self.annotation();

        self.etf.small_tuple(4);
        self.etf.atom("remote");
        self.annotation();
        self.atom_literal(module);
        self.atom_literal(name);

        let arguments = self.etf.start_list();
        Call { arguments }
    }

    fn start_call(&mut self, function: &str) -> Call {
        self.new_expression();
        self.position
            .push(BinaryEafPosition::FunctionCall { arguments: 0 });

        // {call,ANNO,Rep(E_0),[Rep(E_1), ..., Rep(E_k)]}
        self.etf.small_tuple(4);
        self.etf.atom("call");
        self.annotation();
        self.atom_literal(function);
        let arguments = self.etf.start_list();
        Call { arguments }
    }

    fn end_call(&mut self, call: Call) {
        let Some(BinaryEafPosition::FunctionCall { arguments }) = self.position.pop() else {
            unreachable!("tried closing call not in call position");
        };
        self.etf.end_list(call.arguments, arguments);
    }

    fn variable(&mut self, variable: &str) {
        self.new_expression();
        self.variable_pattern(variable);
    }

    fn string(&mut self, content: &str) {
        self.new_expression();

        // A gleam literal string is encoded as a bitarray in Erlang.
        // So a string is a bitarray with a single segment that's the bytes of
        // the utf8-encoded literal string.
        //
        // {bin,ANNO,
        // [{bin_element,ANNO,Rep(E_1),Rep(Size_1),Rep(TSL_1)}, ..., {bin_element,ANNO,Rep(E_k),Rep(Size_k),Rep(TSL_k)}]}
        self.etf.small_tuple(3);
        self.etf.atom("bin");
        self.annotation();
        let segments = self.etf.start_list();

        self.etf.small_tuple(5);
        self.etf.atom("bin_element");
        self.annotation();

        self.charlist(content);

        self.etf.atom("default");
        self.etf.atom("default");

        self.etf.end_list(segments, 1);
    }

    fn integer(&mut self, mut value: BigInt) {
        self.new_expression();

        // If the value is negative that's gonna be represented as the unary
        // negation followed by the positive number. So we start by adding the
        // unary negation tuple and then we'll push the number representation.
        if value.is_negative() {
            value = value.neg();
            self.unary_negation();
        }

        self.etf.small_tuple(3);
        self.etf.atom("integer");
        self.annotation();
        self.etf.bigint(value);
    }

    fn cons_list(&mut self) {
        self.new_expression();
        self.position.push(BinaryEafPosition::ListHead);

        self.etf.small_tuple(4);
        self.etf.atom("cons");
        self.annotation();
    }

    fn empty_list(&mut self) {
        self.new_expression();

        self.etf.small_tuple(2);
        self.etf.atom("nil");
        self.annotation();
    }

    fn unary_negation(&mut self) {
        self.new_expression();
        self.position.push(BinaryEafPosition::UnaryNegation);

        self.etf.small_tuple(4);
        self.etf.atom("op");
        self.annotation();
        self.etf.atom("-");
    }

    fn variable_assignment(&mut self, name: &str) {
        self.new_assignment();
        self.position.push(BinaryEafPosition::VariableAssignment);

        self.etf.small_tuple(4);
        self.etf.atom("match");
        self.annotation();
        self.variable_pattern(name);
    }

    fn start_function_type(&mut self) -> FunctionTypeArguments {
        todo!()
    }

    fn end_function_type_arguments(
        &mut self,
        function_type: FunctionTypeArguments,
    ) -> FunctionType {
        todo!()
    }

    fn end_function_type(&mut self, function_type: FunctionType) {
        todo!()
    }

    fn start_function_spec(&mut self, name: &str, arity: usize) -> FunctionSpec {
        todo!()
    }

    fn end_function_spec(&mut self, function_spec: FunctionSpec) {
        todo!()
    }

    fn int_type(&mut self) {
        todo!()
    }
}

/// A structure that implements the EAF trait but rather than producing the
/// Erlang abstract format binary, it will produce a nice and readable Erlang
/// source string that can be used for testing.
#[derive(Debug)]
pub struct PrettyEaf {
    code: String,
    /// This keeps track of what we're generating
    position: Vec<PrettyEafPosition>,
}

/// This is used to keep track of the current position when generating pretty
/// printed code from an `Eaf`.
#[derive(Debug)]
pub enum PrettyEafPosition {
    FunctionArgument {
        first: bool,
    },
    FunctionStatement {
        indentation: usize,
        first: bool,
    },
    List {
        expected: ExpectedListItem,
    },
    /// We've generated `-` and now we're waiting for the expression to follow
    /// that.
    UnaryNegation,
    TupleItem {
        first: bool,
    },
    VariableAssignment,
    FunctionTypeArguments {
        first: bool,
    },
    FunctionType,
    FunctionSpec,
}

/// Lists are built by cons cells, so when building a list we will do something
/// like this: `[a | [b | []]]`.
/// This is telling us if we're expecting the first item, the second list.
///
#[derive(Debug)]
pub enum ExpectedListItem {
    First,
    Rest,
}

impl Eaf<String> for PrettyEaf {
    fn new(module_name: &str) -> Self {
        Self {
            code: format!("-module({module_name}).\n"),
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
                .map(|(name, arity)| { format!("{}/{}", name.as_ref(), arity) })
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
                .map(|(name, arity)| { format!("{}/{}", name.as_ref(), arity) })
                .join(", ")
        ));
    }

    fn doc_attribute(&mut self, docs: &str) {
        self.code.push_str(&format!("\n-doc(~\"{docs}\")."));
    }

    fn start_function<Name: AsRef<str>>(
        &mut self,
        name: &str,
        _arity: usize,
        arguments_names: impl IntoIterator<Item = Name>,
    ) -> Function {
        self.code.push_str(name);
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

        self.position.push(PrettyEafPosition::FunctionStatement {
            first: true,
            indentation: INDENT,
        });
        Function {
            clauses: PrettyEaf::dummy_list(),
            statements: PrettyEaf::dummy_list(),
        }
    }

    fn start_remote_call(&mut self, module: &str, function: &str) -> Call {
        self.new_expression();
        self.code.push_str(&format!("{module}:{function}("));
        self.position
            .push(PrettyEafPosition::FunctionArgument { first: true });
        Call {
            arguments: PrettyEaf::dummy_list(),
        }
    }

    fn variable(&mut self, name: &str) {
        self.new_expression();
        self.code.push_str(name);
    }

    fn string(&mut self, content: &str) {
        self.new_expression();
        self.code.push_str(&format!("~\"{content}\"",));
    }

    fn integer(&mut self, number: BigInt) {
        self.new_expression();
        self.code.push_str(&format!("{number}"));
    }

    fn start_call(&mut self, function: &str) -> Call {
        self.new_expression();
        self.code.push_str(&format!("{function}("));
        self.position
            .push(PrettyEafPosition::FunctionArgument { first: true });
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
            Some(PrettyEafPosition::List {
                expected: expected @ ExpectedListItem::Rest,
            }) => {
                *expected = ExpectedListItem::First;
                self.code.push_str(", ");
            }
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
            Some(PrettyEafPosition::List {
                expected: ExpectedListItem::Rest,
            }) => {
                self.code.push(']');
                self.position.pop();
            }
            Some(_) | None => {
                self.new_expression();
                self.code.push_str("[]");
            }
        }
    }

    fn unary_negation(&mut self) {
        self.new_expression();
        self.position.push(PrettyEafPosition::UnaryNegation)
    }

    fn variable_assignment(&mut self, name: &str) {
        self.new_expression();
        self.code.push_str(&format!("{name} = "));
        self.position.push(PrettyEafPosition::VariableAssignment);
    }

    fn start_tuple(&mut self) -> Tuple {
        self.new_expression();
        self.code.push('{');
        self.position
            .push(PrettyEafPosition::TupleItem { first: true });

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
        self.code.push('(');
        self.position
            .push(PrettyEafPosition::FunctionTypeArguments { first: true });

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
        self.position.push(PrettyEafPosition::FunctionType);
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
        self.code.push_str(&format!("-spec {name}"));
        self.position.push(PrettyEafPosition::FunctionSpec);
        FunctionSpec {
            representations: PrettyEaf::dummy_list(),
        }
    }

    fn end_function_spec(&mut self, function_spec: FunctionSpec) {
        self.close_currently_open_item();
        function_spec.representations.consume();
    }

    fn int_type(&mut self) {
        self.new_type();
        self.code.push_str("integer()");
    }
}

const INDENT: usize = 4;

impl PrettyEaf {
    fn dummy_list() -> erlang_term_format::List {
        erlang_term_format::List::new(0)
    }

    fn new_expression(&mut self) {
        let Some(position) = self.position.last_mut() else {
            return;
        };

        match position {
            PrettyEafPosition::FunctionArgument { first }
            | PrettyEafPosition::TupleItem { first } => {
                if !*first {
                    self.code.push_str(", ");
                }
                *first = false;
            }
            PrettyEafPosition::List { expected } => match expected {
                // We're expecting the list's head. We don't have to add
                // anything!
                ExpectedListItem::First => *expected = ExpectedListItem::Rest,
                ExpectedListItem::Rest => panic!("generated improper list"),
            },
            PrettyEafPosition::FunctionStatement { indentation, first } => {
                if !*first {
                    self.code.push(',');
                }
                self.code.push('\n');
                self.code.push_str(&" ".repeat(*indentation));
                *first = false;
            }
            PrettyEafPosition::UnaryNegation | PrettyEafPosition::VariableAssignment => {
                self.position.pop();
            }

            PrettyEafPosition::FunctionSpec
            | PrettyEafPosition::FunctionTypeArguments { .. }
            | PrettyEafPosition::FunctionType => {
                panic!("tried generating expression inside a type")
            }
        }
    }

    fn new_type(&mut self) {
        let Some(position) = self.position.last_mut() else {
            return;
        };

        match position {
            PrettyEafPosition::FunctionSpec => {}
            PrettyEafPosition::FunctionType => {}
            PrettyEafPosition::FunctionTypeArguments { first } => {
                if !*first {
                    self.code.push_str(", ")
                }
                *first = false
            }

            PrettyEafPosition::FunctionArgument { .. }
            | PrettyEafPosition::TupleItem { .. }
            | PrettyEafPosition::List { .. }
            | PrettyEafPosition::FunctionStatement { .. }
            | PrettyEafPosition::UnaryNegation
            | PrettyEafPosition::VariableAssignment => {
                panic!("tried generating an expression where a type is expected")
            }
        }
    }

    fn close_currently_open_item(&mut self) {
        let Some(position) = self.position.pop() else {
            return;
        };

        match position {
            PrettyEafPosition::VariableAssignment => {
                panic!("tried closing variable assignment")
            }
            PrettyEafPosition::UnaryNegation => panic!("tried closing unary negation"),
            PrettyEafPosition::List { expected } => match expected {
                ExpectedListItem::First => panic!("popped list expecting first item"),
                ExpectedListItem::Rest => panic!("popped list expecting tail"),
            },
            // When we're done generating statements for a function we need to
            // add one final `.` to the last statement. Then we also want to
            // add an empty line to make our code breath a bit better.
            PrettyEafPosition::FunctionStatement { .. } => self.code.push_str(".\n"),
            // When we're done generating code for a function spec we want to
            // add a `.` and go to a new line so we can start generating the
            // function itself.
            PrettyEafPosition::FunctionSpec => self.code.push_str(".\n"),
            // When we're done generating the arguments of a function we need
            // to add the closed parentheses for the function call!
            PrettyEafPosition::FunctionArgument { .. } => self.code.push(')'),
            // When we're done generating the items of a tuple we need
            // to add the closed parentheses for the function call!
            PrettyEafPosition::TupleItem { .. } => self.code.push('}'),

            // When the function type arguments are over we add what we need for
            // the return type.
            PrettyEafPosition::FunctionTypeArguments { .. } => self.code.push_str(") -> "),
            // There's nothing left to add after a function type.
            PrettyEafPosition::FunctionType => (),
        }
    }
}

pub fn wibble<Output>(eaf: &mut impl Eaf<Output>) {
    eaf.export_attribute(vec![("add_one", 1)]);

    let function = eaf.start_function("add_one", 1, vec!["N"]);
    eaf.variable_assignment("One");
    eaf.integer(1.into());

    let call = eaf.start_call("sum");
    eaf.variable("One");
    eaf.variable("N");
    eaf.end_call(call);

    eaf.end_function(function);
}

// Possible ways to test this:
// 1. more integration tests!!
// 2. ETF parser and then read the bytes into sexpr.
//

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pretty_wibble_test() {
        let mut eaf = PrettyEaf::new("wibble");
        wibble(&mut eaf);
        println!("{}", eaf.into_output());
    }

    #[test]
    fn binary_wibble_test() {
        let mut eaf = BinaryEaf::new("wibble");
        wibble(&mut eaf);
        println!(
            r#"begin
  {{ok, _, Binary}} = compile:forms(binary_to_term(<<{}>>), [report]),
  ok = file:write_file("wibble.beam", Binary)
end."#,
            eaf.into_output().iter().join(",")
        );
    }

    #[test]
    fn binary_wibble_listing_test() {
        let mut eaf = BinaryEaf::new("wibble");
        wibble(&mut eaf);
        println!(
            "compile:forms(binary_to_term(<<{}>>), [report, 'P']).",
            eaf.into_output().iter().join(",")
        );
    }
}
