// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2018 The Gleam contributors

mod pattern;
#[cfg(test)]
mod tests;

use crate::build::{Target, module_erlang_name};
use crate::erlang::pattern::{PatternPrinter, StringPatternAssignment};
use crate::strings::{convert_string_escape_chars, to_snake_case};
use crate::type_::is_prelude_module;
use crate::{
    Result,
    ast::{Function, *},
    docvec,
    line_numbers::LineNumbers,
    pretty::*,
    type_::{
        ModuleValueConstructor, PatternConstructor, Type, TypeVar, TypedCallArg, ValueConstructor,
        ValueConstructorVariant,
    },
};
use camino::Utf8Path;
use ecow::{EcoString, eco_format};
use erlang_abstract_format::{Eaf, PrettyEaf};
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::{ConstZero, Signed};
use regex::{Captures, Regex};
use std::sync::OnceLock;
use std::{collections::HashMap, ops::Deref, sync::Arc};
use vec1::Vec1;

const INDENT: isize = 4;
const MAX_COLUMNS: isize = 80;

fn module_name_atom(module: &str) -> Document<'static> {
    atom_string(module.replace('/', "@").into())
}

fn module_name_atom_str(module: &str) -> EcoString {
    escape_atom_string(module.replace('/', "@").into())
}

/// This describes how an expression that is used in a Gleam's function call
/// should be called in the Erlang generated code.
enum FunctionCall<'a> {
    /// We're calling a function from the given module.
    /// It might be the same module we're generating code for, so the
    /// qualification might not be needed at all; remember to check that!
    ///
    /// ```erl
    /// io:println("wibble")
    /// ```
    ///
    Call { module: &'a str, name: &'a str },

    /// The expression is not a module level function and can be called directly
    /// like thie:
    ///
    /// ```erl
    /// SomeVariable("wibble"),
    /// fun() -> nil end().
    /// ```
    DirectCall,

    /// This is actually not a call but rather needs to build a tuple with the
    /// given tag.
    /// This is needed for records: those are function calls in Gleam, but
    /// simple tuples on the Erlang side.
    BuildRecord { name: &'a str },
}

/// This is a structure used to generate code for an Erlang module.
#[derive(Debug)]
pub struct Generator<'a> {
    /// The module for which we're currently generating Erlang code.
    module: &'a TypedModule,
    line_numbers: &'a LineNumbers,

    /// The relative source path to the module that's gonna be used in `-file`
    /// attributes in the generated Erlang code.
    module_source_path: EcoString,

    /// Wether `echo` has been used in this module, we're gonna need to know
    /// this in order to add the code needed by the pretty printing.
    echo_used: bool,
}

/// This is a generator that takes care of generating the code for a single
/// function, taking care of things like the scope, variable renaming, and
/// generating the function's attributes and statements.
struct FunctionGenerator<'a, 'generator> {
    /// The name of the function we're generating code for.
    function_name: &'a str,

    /// A reference to the module generator, this is needed to take care of some
    /// global state shared by all the functions.
    module_generator: &'generator mut Generator<'a>,

    /// This maps from variable origin in the Gleam code to the name it was
    /// assigned to it in the generated Erlang code.
    ///
    /// Erlang doesn't allow shadowing existing variables, so it's not always
    /// the case that a variable named `wibble` in Gleam is going to correspond
    /// to the Erlang `Wibble` variable. For example:
    ///
    /// ```gleam
    /// let a = 1
    /// let a = a + 1
    /// ```
    ///
    /// In Erlang this would become:
    ///
    /// ```erl
    /// A = 1,
    /// A@1 = A + 1,
    /// ```
    ///
    /// So variables might need renaming.
    /// Whenever we find a variable usage in Gleam we have to check "what is
    /// the name that was given to the variable that comes from this location?"
    /// Only then we'll know what's the correct name to use for it.
    ///
    variable_names: im::HashMap<SrcSpan, EcoString>,

    /// This keeps track of the number of throwaway variables that have already
    /// been generated in the current function.
    /// For example if this is `2` it means we've already generated:
    ///
    /// ```erl
    /// _value
    /// _value@1
    /// _value@2
    /// ```
    ///
    /// We need this to make sure that every time we generate a new throwaway
    /// variable it has a unique name not shadowing anything else.
    ///
    throwaway_variables: usize,

    /// This keeps track of all the names that are taken for the current
    /// function and can't be used when defining new variables.
    /// For example if this is `hash_map![("wibble", 2), ("wobble", 1)]`
    /// this means that all of these variables have already been defined
    /// somewhere in the current function:
    ///
    /// ```
    /// Wibble = ...,
    /// Wibble@1 = ...,
    /// Wibble@2 = ...,
    ///
    /// Wobble = ...,
    /// Wobble@1 = ...,
    /// ```
    ///
    /// This is handy whenever we run into a new variable assignment and have to
    /// generate a new name for it in Erlang.
    ///
    taken_names: im::HashMap<String, usize>,
}

impl<'a> Generator<'a> {
    pub fn new(
        module: &'a TypedModule,
        line_numbers: &'a LineNumbers,
        module_root: &'a Utf8Path,
    ) -> Self {
        let module_source_path = module
            .type_info
            .src_path
            .strip_prefix(module_root)
            .unwrap_or(&module.type_info.src_path)
            .as_str()
            .replace("\\", "\\\\")
            .into();

        Self {
            module,
            module_source_path,
            line_numbers,
            echo_used: false,
        }
    }

    fn module_document<Output>(&mut self, eaf: &mut impl Eaf<Output>) -> Result<Document<'a>> {
        // We need to know which private functions are referenced in importable
        // constants so that we can export them anyway in the generated Erlang.
        // This is because otherwise when the constant is used in another module it
        // would result in an error as it tries to reference this private function.
        let overridden_publicity =
            find_private_functions_referenced_in_importable_constants(self.module);

        // We add a `-compile` attribute at the top of each module to instruct
        // the Erlang compiler.
        eaf.compile_attribute([
            "no_auto_import",
            "nowarn_unused_vars",
            "nowarn_unused_function",
            "nowarn_nomatch",
            "inline",
        ]);

        // We then need to add an `-export` attribute for all the module's
        // public functions.
        eaf.export_attribute(
            (self.module.definitions.functions.iter())
                .filter_map(|function| function_export(function, &overridden_publicity)),
        );
        // We do the same but with types.
        eaf.export_type_attribute(
            (self.module.definitions.custom_types.iter())
                .map(|custom_type| type_export(custom_type)),
        );

        // We also add a `-module_doc` comment at the beginning of the module with its
        // documentation.
        self.module_documentation(eaf);

        // Then we generate the definitions for types defined inside this module.
        // type definitions -type spec

        // And finally generate all the functions that the module defined.
        for function in &self.module.definitions.functions {
            FunctionGenerator::new(function, self).module_function(eaf, function);
        }

        // TODO: How do we deal with echo???
        // if self.echo_used { ... }

        Ok(nil())
    }

    fn module_documentation<Output>(&mut self, eaf: &mut impl Eaf<Output>) {
        if self.module.type_info.is_internal {
            // The module is internal so we need to add a `-moduledoc(false).`
            // attribute to make sure its documentation is hidden.
            let doc = eaf.start_moduledoc_attribute();
            eaf.atom("false");
            eaf.end_doc_attribute(doc);
        } else if self.module.documentation.is_empty() {
            // The module is not internal, but it has no docs.
            // We don't have to do anything.
            return;
        } else {
            // The module has some documentation that we're going to include
            // with a `-moduledoc` attribute.
            let doc = eaf.start_moduledoc_attribute();
            let documentation = &self.module.documentation.iter().join("\n");
            eaf.string(documentation);
            eaf.end_doc_attribute(doc);
        };
    }
}

impl<'a, 'generator> FunctionGenerator<'a, 'generator> {
    pub fn new(
        function: &'a TypedFunction,
        module_generator: &'generator mut Generator<'a>,
    ) -> Self {
        let function_name = match function.name.as_ref() {
            Some((_, function_name)) => function_name,
            None => panic!("Module functions should have a name"),
        };

        Self {
            function_name,
            module_generator,
            taken_names: im::HashMap::new(),
            variable_names: im::HashMap::new(),
            throwaway_variables: 0,
        }
    }

    /// Given a variable name this returns a document with the name used to
    /// reference such variable (names can change if a variable were to shadow
    /// something with the same name!).
    ///
    /// ## Panics
    /// This will panic if the variable is not in scope as that is most likely
    /// the result of a bug in the compiler.
    pub fn local_var_name(&self, variable_origin: &SrcSpan) -> EcoString {
        self.variable_names
            .get(variable_origin)
            .expect("variable not in scope")
            .clone()
    }

    /// Assigns a name to this new variable making sure it's not shadowing any
    /// existing one.
    ///
    /// - `name` is the name of the variable as defined in the Gleam source code
    /// - `location` is where that variable comes from, and it is used to then
    ///   get this newly generated name back.
    ///
    /// For example:
    ///
    /// ```gleam
    /// let wibble = 1
    /// ```
    ///
    /// When we run into this Gleam assignment we will need to decide how to
    /// call it on the Erlang side. So we would call:
    ///
    /// ```ignore
    /// let location = todo!("the location of this variable")
    /// new_erlang_variable("wibble", location)
    /// // and later we can tell what name was picked by calling
    /// // `local_variable_name`
    /// local_variable_name(location) // "Wibble"
    /// ```
    ///
    ///
    pub fn new_erlang_variable(&mut self, name: &str, location: SrcSpan) -> EcoString {
        let next = self.taken_names.get(name).map_or(0, |i| i + 1);
        let _ = self.taken_names.insert(name.to_string(), next);
        let erlang_name = match next {
            0 => variable_name(name),
            _ => eco_format!("{}@{}", variable_name(name), next),
        };
        let _ = self.variable_names.insert(location, erlang_name.clone());
        erlang_name
    }

    /// Sometimes during code generation we might need to create new variables
    /// that were not accounted for during analysis.
    /// Those variables don't really have an origin in the source code and are
    /// usually generated and immediately used.
    ///
    /// For example:
    ///
    /// ```erl
    /// _denominator = ...,
    /// 1 / _denominator.
    /// ```
    ///
    /// Any time you need one such variable you can create it with this method
    /// instead of `new_erlang_variable` which is meant to be used for variables
    /// generated from Gleam code (and so wants the source location of the
    /// variable).
    ///
    /// The generated name is guaranteed to always be unique for the given
    /// function.
    ///
    fn new_throwaway_variable(&mut self) -> EcoString {
        let name = if self.throwaway_variables == 0 {
            EcoString::from("_value")
        } else {
            eco_format!("_value@{}", self.throwaway_variables)
        };
        self.throwaway_variables += 1;
        name
    }

    /// Generates code for an Erlang module function. This might return None
    /// if there's no code to be generated at all!
    /// For example if the function is unused, or if the function is a private
    /// Erlang external (in which case, it would be inlined instead).
    fn module_function<Output>(&mut self, eaf: &mut impl Eaf<Output>, function: &'a TypedFunction) {
        // We don't generate any code for unused functions.
        if self
            .module_generator
            .module
            .unused_definition_positions
            .contains(&function.location.start)
        {
            return;
        }

        // Private external functions don't need to render anything, the
        // underlying Erlang implementation is used directly at the call site.
        if function.external_erlang.is_some() && function.publicity.is_private() {
            return;
        }

        // If the function has no suitable Erlang implementation then there is
        // nothing to generate for it.
        if !function.implementations.supports(Target::Erlang) {
            return;
        }

        let function_name = EcoString::from(escape_erlang_existing_name(self.function_name));

        // Then we add the function's documentation and type annotation.
        self.function_spec_attribute(eaf, &function_name, function);
        self.function_doc_attribute(eaf, function);

        // Finally we start generating code for the function itself, how we do
        // it depends if the function is external or not.
        let arity = function.arguments.len();
        match function.external_erlang.as_ref() {
            // If the function is not external we generate the code for all of
            // its statements.
            None => {
                let arguments = self.function_arguments_names(&function.arguments, false);
                let open_function = eaf.start_function(&function_name, arity, arguments);
                let _ = self.statement_sequence(eaf, &function.body);
                eaf.end_function(open_function);
            }

            // An external function consists of just a remote call being
            // passed all of the function's arguments.
            Some((module, external_function_name, _location)) => {
                let arguments = self
                    .function_arguments_names(&function.arguments, true)
                    .collect_vec();
                let open_function = eaf.start_function(&function_name, arity, arguments.clone());
                let call =
                    eaf.start_remote_call(&module_erlang_name(module), external_function_name);
                for argument in arguments {
                    eaf.variable(&argument);
                }
                eaf.end_call(call);
                eaf.end_function(open_function);
            }
        }
    }

    /// This generates the `-spec` attribute for a function with the given name.
    ///
    fn function_spec_attribute<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        function_name: &EcoString,
        function: &'a Function<Arc<Type>, TypedExpr>,
    ) {
        // We start by getting all the type variable usages from this function,
        // both in the argument types and return type.
        let module_name = &self.module_generator.module.name;
        let var_usages = &collect_type_var_usages(
            HashMap::new(),
            function
                .arguments
                .iter()
                .map(|argument| &argument.type_)
                .chain(std::iter::once(&function.return_type)),
        );
        let generator = TypeGenerator::new(module_name).with_var_usages(var_usages);

        // We can then start generating the function spec.
        let spec = eaf.start_function_spec(function_name, function.arguments.len());
        let function_type = eaf.start_function_type();
        for argument in &function.arguments {
            generator.type_(eaf, &argument.type_)
        }
        let function_type = eaf.end_function_type_arguments(function_type);
        generator.type_(eaf, &function.return_type);
        eaf.end_function_type(function_type);
        eaf.end_function_spec(spec);
    }

    fn function_doc_attribute<Output>(&self, eaf: &mut impl Eaf<Output>, function: &TypedFunction) {
        // If a function is marked as internal or comes from an internal module
        // we want to hide its documentation in the Erlang shell!
        // So the doc directive will look like this: `-doc(false).`
        let is_internal =
            self.module_generator.module.type_info.is_internal || function.publicity.is_internal();

        if is_internal {
            let attribute = eaf.start_doc_attribute();
            eaf.atom("false");
            eaf.end_doc_attribute(attribute);
        } else if let Some((_, documentation)) = &function.documentation
            && !documentation.is_empty()
        {
            let attribute = eaf.start_doc_attribute();
            eaf.string(&documentation);
            eaf.end_doc_attribute(attribute);
        }
    }

    /// Given a function, this will return the names of the arguments to be used
    /// in this function's definition. This will also update the current scope
    /// to add those names to the available local variables.
    fn function_arguments_names(
        &mut self,
        arguments: &[TypedArg],
        is_external: bool,
    ) -> impl Iterator<Item = EcoString> {
        arguments.iter().map(move |argument| match &argument.names {
            // When the function is external we need to be careful with discarded
            // arguments. _All_ of the function arguments are always used in an
            // external function, regardless of them being discarded in Gleam:
            //
            // ```gleam
            // @external(erlang, "io", "format")
            // fn format(_string: String, _args: List(String)) -> Nil
            // ```
            //
            // Becomes:
            //
            // ```erl
            // format(_string, _args) ->
            //   io:format(_string, _args).
            // ```
            //
            // If an argument is made of just underscores, then that would result
            // in a syntax error in the generated Erlang, where the external
            // function is called with a discard `io:format(_, _)`!
            // So in this case we use a throwaway name to make sure the external
            // function can be called correctly.
            ArgNames::Discard { name, .. } | ArgNames::LabelledDiscard { name, .. }
                if is_external =>
            {
                if name.chars().all(|char| char == '_') {
                    self.new_throwaway_variable()
                } else {
                    self.new_erlang_variable(name, argument.location)
                }
            }
            ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => EcoString::from("_"),
            ArgNames::Named { name, .. } | ArgNames::NamedLabelled { name, .. } => {
                self.new_erlang_variable(name, argument.location)
            }
        })
    }

    fn statement_sequence<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        statements: &'a [TypedStatement],
    ) -> Document<'a> {
        for statement in statements {
            let _ = match statement {
                Statement::Expression(expression) => self.expr(eaf, expression),
                Statement::Assignment(assignment) => self.assignment(eaf, assignment),
                Statement::Use(use_) => self.expr(eaf, &use_.call),
                Statement::Assert(assert) => self.assert(eaf, assert),
            };
        }

        nil()
    }

    fn expr<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        expression: &'a TypedExpr,
    ) -> Document<'a> {
        match expression {
            TypedExpr::Todo {
                message: label,
                location,
                ..
            } => self.todo(eaf, label.as_deref(), *location),

            TypedExpr::Panic {
                location, message, ..
            } => self.panic(eaf, *location, message.as_deref()),

            TypedExpr::Echo {
                expression,
                location,
                message,
                ..
            } => {
                let expression = expression
                    .as_ref()
                    .expect("echo with no expression outside of pipe");
                let expression = self.maybe_block_expr(eaf, expression);
                self.echo(eaf, nil(), message.as_deref(), location)
            }

            TypedExpr::Int { int_value, .. } => {
                eaf.integer(int_value.clone());
                nil()
            }
            TypedExpr::Float { float_value, .. } => {
                eaf.float(float_value.value());
                nil()
            }
            TypedExpr::String { value, .. } => {
                eaf.string(value);
                nil()
            }

            TypedExpr::Pipeline {
                first_value,
                assignments,
                finally,
                ..
            } => self.pipeline(eaf, first_value, assignments, finally),

            TypedExpr::Block { statements, .. } => {
                // If the block has a single expression we don't bother wrapping
                // it in an additional `begin ... end` block.
                // It's going to be added only if strictly needed.
                if statements.len() == 1
                    && let Statement::Expression(expression) = statements.first()
                {
                    self.maybe_block_expr(eaf, expression);
                } else {
                    let block = eaf.start_block();
                    let _ = self.statement_sequence(eaf, statements);
                    eaf.end_block(block)
                }

                nil()
            }

            TypedExpr::TupleIndex { tuple, index, .. } => {
                self.tuple_index(eaf, tuple, *index);
                nil()
            }
            TypedExpr::RecordAccess { record, index, .. } => {
                self.tuple_index(eaf, record, index + 1);
                nil()
            }
            TypedExpr::PositionalAccess { record, index, .. } => {
                self.tuple_index(eaf, record, index + 1);
                nil()
            }

            TypedExpr::Var {
                name, constructor, ..
            } => {
                self.var(eaf, name, constructor);
                nil()
            }

            TypedExpr::Fn {
                arguments, body, ..
            } => self.fun(eaf, arguments, body),

            TypedExpr::NegateBool { value, .. } => {
                eaf.unary_operator("not");
                let _ = self.maybe_block_expr(eaf, value);
                nil()
            }

            TypedExpr::NegateInt { value, .. } => {
                eaf.unary_operator("-");
                let _ = self.maybe_block_expr(eaf, value);
                nil()
            }

            TypedExpr::List { elements, tail, .. } => {
                // We generate all the items of the list as cons cells.
                for element in elements {
                    eaf.cons_list();
                    let _ = self.maybe_block_expr(eaf, element);
                }
                // Finally we close the list with the tail, or an empty list
                // (so that we're sure we're building proper Erlang lists).
                if let Some(tail) = tail {
                    let _ = self.maybe_block_expr(eaf, tail);
                } else {
                    eaf.empty_list();
                }

                nil()
            }

            TypedExpr::Call { fun, arguments, .. } => {
                self.call(eaf, fun, arguments);
                nil()
            }

            TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Record { name, arity: 0, .. },
                ..
            } => atom_string(to_snake_case(name)),

            TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Constant { literal, .. },
                ..
            } => self.const_inline(eaf, literal),

            TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Record { name, arity, .. },
                ..
            } => record_constructor_function(name.clone(), *arity as usize),

            TypedExpr::ModuleSelect {
                type_,
                constructor:
                    ModuleValueConstructor::Fn {
                        external_erlang: Some((module, name)),
                        ..
                    }
                    | ModuleValueConstructor::Fn { module, name, .. },
                ..
            } => module_select_fn(eaf, type_.clone(), module, name),

            TypedExpr::RecordUpdate {
                updated_record_assigned_name,
                updated_record,
                constructor,
                arguments,
                ..
            } => {
                // If the record value itself needs to be bound to a variable
                // before the update, we define it.
                if let Some(name) = updated_record_assigned_name.as_ref() {
                    eaf.match_operator();
                    eaf.variable_pattern(
                        &self.new_erlang_variable(name, updated_record.location()),
                    );
                    self.maybe_block_expr(eaf, updated_record);
                }
                // Then a record update is simply a call!
                self.call(eaf, constructor, arguments);
                nil()
            }

            TypedExpr::Case {
                subjects, clauses, ..
            } => self.case(eaf, subjects, clauses),

            TypedExpr::BinOp {
                operator,
                left,
                right,
                ..
            } => {
                self.bin_op(eaf, operator, left, right);
                nil()
            }

            TypedExpr::Tuple { elements, .. } => {
                let tuple = eaf.start_tuple();
                for element in elements {
                    let _ = self.maybe_block_expr(eaf, element);
                }
                eaf.end_tuple(tuple);
                nil()
            }

            TypedExpr::BitArray { segments, .. } => bit_array(
                segments
                    .iter()
                    .map(|segment| self.bit_array_expression_segment(eaf, segment)),
            ),

            TypedExpr::Invalid { .. } => {
                panic!("invalid expressions should not reach code generation")
            }
        }
    }

    fn todo<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        message: Option<&'a TypedExpr>,
        location: SrcSpan,
    ) -> Document<'a> {
        let message = match message {
            Some(message) => self.expr(eaf, message),
            None => string("`todo` expression evaluated. This code has not yet been implemented."),
        };
        self.erlang_error("todo", &message, location, vec![])
    }

    fn panic<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        location: SrcSpan,
        message: Option<&'a TypedExpr>,
    ) -> Document<'a> {
        let message = match message {
            Some(message) => self.expr(eaf, message),
            None => string("`panic` expression evaluated."),
        };
        self.erlang_error("panic", &message, location, vec![])
    }

    fn erlang_error(
        &self,
        name: &'a str,
        message: &Document<'a>,
        location: SrcSpan,
        fields: Vec<(&'a str, Document<'a>)>,
    ) -> Document<'a> {
        let mut fields_doc = docvec![
            "gleam_error => ",
            name,
            ",",
            line(),
            "message => ",
            message.clone(),
            ",",
            line(),
            "file => <<?FILEPATH/utf8>>,",
            line(),
            "module => ",
            self.module_generator
                .module
                .name
                .clone()
                .to_doc()
                .surround("<<\"", "\"/utf8>>"),
            ",",
            line(),
            "function => ",
            string(self.function_name),
            ",",
            line(),
            "line => ",
            self.module_generator
                .line_numbers
                .line_number(location.start),
        ];

        for (key, value) in fields {
            fields_doc = fields_doc
                .append(",")
                .append(line())
                .append(key)
                .append(" => ")
                .append(value);
        }

        let error = docvec!["#{", fields_doc.group().nest(INDENT), "}"];
        docvec!["erlang:error", wrap_arguments([error.group()])]
    }

    fn echo<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        body: Document<'a>,
        message: Option<&'a TypedExpr>,
        location: &SrcSpan,
    ) -> Document<'a> {
        self.module_generator.echo_used = true;

        let message = message
            .as_ref()
            .map(|message| {
                self.maybe_block_expr(eaf, message);
                nil()
            })
            .unwrap_or("nil".to_doc());

        "echo".to_doc().append(wrap_arguments(vec![
            body,
            message,
            self.module_generator
                .line_numbers
                .line_number(location.start)
                .to_doc(),
        ]))
    }

    fn maybe_block_expr<Output>(&mut self, eaf: &mut impl Eaf<Output>, expression: &'a TypedExpr) {
        if needs_begin_end_wrapping(expression) {
            let block = eaf.start_block();
            let _ = self.expr(eaf, expression);
            eaf.end_block(block);
        } else {
            let _ = self.expr(eaf, expression);
        }
    }

    fn assignment<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        assignment: &'a TypedAssignment,
    ) -> Document<'a> {
        match &assignment.kind {
            AssignmentKind::Let | AssignmentKind::Generated => {
                self.let_(eaf, &assignment.value, &assignment.pattern);
                nil()
            }
            AssignmentKind::Assert {
                message, location, ..
            } => self.let_assert(
                eaf,
                &assignment.value,
                &assignment.pattern,
                message.as_ref(),
                *location,
            ),
        }
    }

    fn let_<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        value: &'a TypedExpr,
        pattern: &'a TypedPattern,
    ) {
        eaf.match_operator();
        let _ = PatternPrinter::new(self).print(eaf, pattern);
        self.maybe_block_expr(eaf, value)
    }

    fn let_assert<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        value: &'a TypedExpr,
        pattern: &'a TypedPattern,
        message: Option<&'a TypedExpr>,
        location: SrcSpan,
    ) -> Document<'a> {
        // If the pattern will never fail, like a tuple or a simple variable, we
        // simply treat it as if it were a `let` assignment.
        if pattern.always_matches() {
            self.let_(eaf, value, pattern);
            return nil();
        }

        let message = match message {
            Some(message) => self.expr(eaf, message),
            None => string("Pattern match failed, no pattern matched the value."),
        };

        let subject = {
            self.maybe_block_expr(eaf, value);
            nil()
        };

        // The code we generated for a `let assert` assignment looks something like
        // this. For this Gleam code:
        //
        // ```gleam
        // let assert [a, b, c] = [1, 2, 3]
        // ```
        //
        // We generate (roughly) the following Erlang:
        //
        // ```erlang
        // {A, B, C} = case [1, 2, 3] of
        //   [A, B, C] -> {A, B, C};
        //   _ -> erlang:error(...)
        // end.
        // ```
        // This is the most efficient way to properly extract all the required
        // variables from the pattern. However, if the `let assert` assignment is
        // the last in a block, like this:
        //
        // ```gleam
        // let x = {
        //   let assert [a, b, c] = [1, 2, 3]
        // }
        // ```
        //
        // The generated Erlang code will end up assigning the value `#(1, 2, 3)`
        // to the variable `x`, instead of `[1, 2, 3]`. In this case, we must
        // generate slightly different code. Since we know we won't be using the
        // bound variables anywhere (there is nothing else in this scope to
        // reference them), we can safely remove the assignment from the generated
        // code, and generate the following:
        //
        // ```erlang
        // X = begin
        //   _assert_subject = [1, 2, 3]
        //   case _assert_subject of
        //     [A, B, C] -> _assert_subject;
        //     _ -> erlang:error(...)
        //   end
        // end.
        // ```
        //
        // That correctly assigns `[1, 2, 3]` to the `x` variable.
        //
        let is_tail = false;

        let (subject_assignment, subject) = if is_tail && !value.is_var() {
            let variable = self.new_throwaway_variable();
            let assignment = docvec![variable.clone(), " = ", subject, ",", line()];
            (assignment, variable.to_doc())
        } else {
            (nil(), nil())
        };

        let mut pattern_printer = PatternPrinter::new(self);
        let pattern_document = pattern_printer.print(eaf, pattern);
        let PatternPrinter {
            generator: _,
            variables,
            guards,
            assignments,
        } = pattern_printer;

        let assignments_map = assignments
            .iter()
            .map(|assignment| (assignment.gleam_name.clone(), assignment))
            .collect();
        let clause_guard = self.optional_clause_guard(eaf, None, guards, &assignments_map);

        let value_document = nil();
        let assignment = nil();

        let clauses = docvec![
            pattern_document,
            clause_guard,
            " -> ",
            value_document,
            ";",
            line(),
            "todo",
            " ->",
            docvec![
                line(),
                self.erlang_error(
                    "let_assert",
                    &message,
                    location,
                    vec![
                        ("value", "todo".to_doc()),
                        ("start", location.start.to_doc()),
                        ("'end'", value.location().end.to_doc()),
                        ("pattern_start", pattern.location().start.to_doc()),
                        ("pattern_end", pattern.location().end.to_doc()),
                    ],
                )
                .nest(INDENT)
            ]
            .nest(INDENT)
        ];

        let assignments = if assignments.is_empty() {
            nil()
        } else {
            docvec![
                ",",
                line(),
                join(
                    assignments
                        .iter()
                        .map(|assignment| assignment.to_assignment_doc()),
                    ",".to_doc().append(line())
                )
            ]
        };

        docvec![
            subject_assignment,
            assignment,
            "case ",
            subject,
            " of",
            docvec![line(), clauses].nest(INDENT),
            line(),
            "end",
            assignments,
        ]
    }

    fn pipeline<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        first_value: &'a TypedPipelineAssignment,
        assignments: &'a [(TypedPipelineAssignment, PipelineAssignmentKind)],
        finally: &'a TypedExpr,
    ) -> Document<'a> {
        let mut documents = Vec::with_capacity((assignments.len() + 1) * 3);
        let all_assignments = std::iter::once(first_value)
            .chain(assignments.iter().map(|(assignment, _kind)| assignment));

        // A pipeline is desugared as a sequence of assignments:
        //
        // ```erl
        // Step1 = fun()
        // Step2 = fun(Step1)
        // Step3 = fun(Step2)
        // % ...
        // ```
        //
        // So we need to keep around the name the prevopis pipeline step had
        // to pass it as an argument to the following call. This is what this
        // variable is for.
        let mut previous_step_variable_name = None;
        for assignment in all_assignments {
            // An echo in a pipeline won't result in an assignment, instead it
            // just prints the previous variable assigned in the pipeline.
            if let TypedExpr::Echo {
                expression: None,
                message,
                location,
                ..
            } = assignment.value.as_ref()
            {
                let previous_step_variable_name = previous_step_variable_name
                    .to_owned()
                    .expect("echo with no previous step in a pipe");
                documents.push(self.echo(
                    eaf,
                    previous_step_variable_name,
                    message.as_deref(),
                    location,
                ));
            } else {
                // Otherwise we assign the intermediate pipe value to a variable.
                self.maybe_block_expr(eaf, &assignment.value);
                let body = nil();
                let name = self.new_erlang_variable(&assignment.name, assignment.location);
                previous_step_variable_name = Some(name.clone().to_doc());
                documents.push(docvec![name, " = ", body]);
            };
            documents.push(",".to_doc());
            documents.push(line());
        }

        // We also need to do the same thing for the final step of the pipeline.
        // It's slightly different compared to the other ones so we have to do
        // that separately.
        if let TypedExpr::Echo {
            expression: None,
            message,
            location,
            ..
        } = finally
        {
            let previous_step_variable_name = previous_step_variable_name
                .to_owned()
                .expect("echo with no previous step in a pipe");
            documents.push(self.echo(
                eaf,
                previous_step_variable_name,
                message.as_deref(),
                location,
            ));
        } else {
            documents.push(self.expr(eaf, finally))
        }

        documents.to_doc()
    }

    fn assert<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        assert: &'a TypedAssert,
    ) -> Document<'a> {
        let Assert {
            value,
            location,
            message,
        } = assert;

        let message = match message {
            Some(message) => self.expr(eaf, message),
            None => string("Assertion failed."),
        };

        let mut assignments = Vec::new();

        let (subject, mut fields) = match value {
            TypedExpr::Call { fun, arguments, .. } => {
                self.assert_call(eaf, fun, arguments, &mut assignments)
            }
            TypedExpr::BinOp {
                operator,
                left,
                right,
                ..
            } => {
                let operator_document = match operator {
                    BinOp::And => {
                        return self.assert_and(eaf, left, right, message, *location);
                    }
                    BinOp::Or => {
                        return self.assert_or(eaf, left, right, message, *location);
                    }
                    BinOp::Eq => "=:=",
                    BinOp::NotEq => "/=",
                    BinOp::LtInt | BinOp::LtFloat => "<",
                    BinOp::LtEqInt | BinOp::LtEqFloat => "=<",
                    BinOp::GtInt | BinOp::GtFloat => ">",
                    BinOp::GtEqInt | BinOp::GtEqFloat => ">=",
                    BinOp::AddInt
                    | BinOp::AddFloat
                    | BinOp::SubInt
                    | BinOp::SubFloat
                    | BinOp::MultInt
                    | BinOp::MultFloat
                    | BinOp::DivInt
                    | BinOp::DivFloat
                    | BinOp::RemainderInt
                    | BinOp::Concatenate => {
                        panic!("Non-boolean operators cannot appear here in well-typed code")
                    }
                };

                let left_document = self.assign_to_variable(eaf, left, &mut assignments);
                let right_document = self.assign_to_variable(eaf, right, &mut assignments);
                (
                    binop_documents(
                        left_document.clone(),
                        operator_document,
                        right_document.clone(),
                    ),
                    vec![
                        ("kind", atom("binary_operator")),
                        ("operator", atom(operator.name())),
                        (
                            "left",
                            asserted_expression(
                                AssertExpression::from_expression(left),
                                Some(left_document),
                                left.location(),
                            ),
                        ),
                        (
                            "right",
                            asserted_expression(
                                AssertExpression::from_expression(right),
                                Some(right_document),
                                right.location(),
                            ),
                        ),
                    ],
                )
            }

            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => (
                {
                    self.maybe_block_expr(eaf, value);
                    nil()
                },
                vec![
                    ("kind", atom("expression")),
                    (
                        "expression",
                        asserted_expression(
                            AssertExpression::from_expression(value),
                            Some("false".to_doc()),
                            value.location(),
                        ),
                    ),
                ],
            ),
        };

        fields.push(("start", location.start.to_doc()));
        fields.push(("'end'", value.location().end.to_doc()));
        fields.push(("expression_start", value.location().start.to_doc()));

        let clauses = docvec![
            line(),
            "true -> nil;",
            line(),
            "false -> ",
            self.erlang_error("assert", &message, *location, fields),
        ];

        docvec![
            assignments,
            "case ",
            subject,
            " of",
            clauses.nest(INDENT),
            line(),
            "end"
        ]
    }

    /// In Gleam, the `&&` operator is short-circuiting, meaning that we can't
    /// pre-evaluate both sides of it, and use them in the exception that is
    /// thrown.
    /// Instead, we need to implement this short-circuiting logic ourself.
    ///
    /// If we short-circuit, we must leave the second expression unevaluated,
    /// and signal that using the `unevaluated` variant, as detailed in the
    /// exception format. For the first expression, we know it must be `false`,
    /// otherwise we would have continued by evaluating the second expression.
    ///
    /// Similarly, if we do evaluate the second expression and fail, we know
    /// that the first expression must have evaluated to `true`, and the second
    /// to `false`. This way, we avoid needing to evaluate either expression
    /// twice.
    ///
    /// The generated code then looks something like this:
    /// ```erlang
    /// case expr1 of
    ///   true -> case expr2 of
    ///     true -> true;
    ///     false -> <throw exception>
    ///   end;
    ///   false -> <throw exception>
    /// end
    /// ```
    ///
    fn assert_and<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        message: Document<'a>,
        location: SrcSpan,
    ) -> Document<'a> {
        let left_kind = AssertExpression::from_expression(left);
        let right_kind = AssertExpression::from_expression(right);

        let fields_if_short_circuiting = vec![
            ("kind", atom("binary_operator")),
            ("operator", atom("&&")),
            (
                "left",
                asserted_expression(left_kind, Some("false".to_doc()), left.location()),
            ),
            (
                "right",
                asserted_expression(AssertExpression::Unevaluated, None, right.location()),
            ),
            ("start", location.start.to_doc()),
            ("'end'", right.location().end.to_doc()),
            ("expression_start", left.location().start.to_doc()),
        ];

        let fields = vec![
            ("kind", atom("binary_operator")),
            ("operator", atom("&&")),
            (
                "left",
                asserted_expression(left_kind, Some("true".to_doc()), left.location()),
            ),
            (
                "right",
                asserted_expression(right_kind, Some("false".to_doc()), right.location()),
            ),
            ("start", location.start.to_doc()),
            ("'end'", right.location().end.to_doc()),
            ("expression_start", left.location().start.to_doc()),
        ];

        let right_clauses = docvec![
            line(),
            "true -> nil;",
            line(),
            "false -> ",
            self.erlang_error("assert", &message, location, fields),
        ];

        let left_clauses = docvec![
            line(),
            "true -> ",
            docvec![
                "case ",
                {
                    self.maybe_block_expr(eaf, right);
                    nil()
                },
                " of",
                right_clauses.nest(INDENT),
                line(),
                "end"
            ]
            .nest(INDENT),
            ";",
            line(),
            "false -> ",
            self.erlang_error("assert", &message, location, fields_if_short_circuiting,),
        ];

        docvec![
            "case ",
            {
                self.maybe_block_expr(eaf, left);
                nil()
            },
            " of",
            left_clauses.nest(INDENT),
            line(),
            "end"
        ]
    }

    /// Similar to `&&`, `||` is also short-circuiting in Gleam. However, if `||`
    /// short-circuits, that's because the first expression evaluated to `true`,
    /// meaning the whole assertion succeeds. This allows us to directly use Erlang's
    /// `orelse` operator as the subject of the `case` expression.
    ///
    /// The only difference is that due to the nature of `||`, if the assertion fails,
    /// we know that both sides must have evaluated to `false`, so we don't
    /// need to store the values of them in variables beforehand.
    fn assert_or<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        message: Document<'a>,
        location: SrcSpan,
    ) -> Document<'a> {
        let fields = vec![
            ("kind", atom("binary_operator")),
            ("operator", atom("||")),
            (
                "left",
                asserted_expression(
                    AssertExpression::from_expression(left),
                    Some("false".to_doc()),
                    left.location(),
                ),
            ),
            (
                "right",
                asserted_expression(
                    AssertExpression::from_expression(right),
                    Some("false".to_doc()),
                    right.location(),
                ),
            ),
            ("start", location.start.to_doc()),
            ("'end'", right.location().end.to_doc()),
            ("expression_start", left.location().start.to_doc()),
        ];

        let clauses = docvec![
            line(),
            "true -> nil;",
            line(),
            "false -> ",
            self.erlang_error("assert", &message, location, fields),
        ];

        docvec![
            "case ",
            docvec![
                {
                    self.maybe_block_expr(eaf, left);
                    nil()
                },
                " orelse ",
                {
                    self.maybe_block_expr(eaf, right);
                    nil()
                }
            ]
            .nest(INDENT),
            " of",
            clauses.nest(INDENT),
            line(),
            "end"
        ]
    }

    fn block<Output>(&mut self, eaf: &mut impl Eaf<Output>, statements: &'a Vec1<TypedStatement>) {
        if statements.len() == 1
            && let Statement::Expression(expression) = statements.first()
            && !needs_begin_end_wrapping(expression)
        {
            let _ = self.expr(eaf, expression);
        } else {
            let block = eaf.start_block();
            let _ = self.statement_sequence(eaf, statements);
            eaf.end_block(block)
        }
    }

    fn tuple_index<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        tuple: &'a TypedExpr,
        index: u64,
    ) {
        let call = eaf.start_remote_call("erlang", "element");
        eaf.integer((index + 1).into());
        let _ = self.maybe_block_expr(eaf, tuple);
        eaf.end_call(call);
    }

    fn var<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        name: &'a str,
        constructor: &'a ValueConstructor,
    ) {
        match &constructor.variant {
            ValueConstructorVariant::Record {
                name: record_name, ..
            } => match constructor.type_.deref() {
                Type::Fn { arguments, .. } => {
                    let chars = incrementing_arguments_list(arguments.len());
                    let _ = "fun("
                        .to_doc()
                        .append(chars.clone())
                        .append(") -> {")
                        .append(atom_string(to_snake_case(record_name)))
                        .append(", ")
                        .append(chars)
                        .append("} end");
                }
                Type::Named { .. } | Type::Var { .. } | Type::Tuple { .. } => {
                    eaf.atom(&to_snake_case(record_name))
                }
            },

            ValueConstructorVariant::LocalVariable { location, .. } => {
                eaf.variable(&self.local_var_name(location))
            }

            ValueConstructorVariant::ModuleConstant { literal, .. } => {
                let _ = self.const_inline(eaf, literal);
            }

            ValueConstructorVariant::ModuleFn {
                arity,
                external_erlang: Some((module, name)),
                ..
            } if *module == self.module_generator.module.name => {
                eaf.function_reference(None, name, *arity)
            }

            ValueConstructorVariant::ModuleFn {
                arity,
                external_erlang: Some((module, name)),
                ..
            } => eaf.function_reference(Some(module), name, *arity),

            ValueConstructorVariant::ModuleFn { arity, module, .. }
                if *module == self.module_generator.module.name =>
            {
                eaf.function_reference(None, name, *arity)
            }

            ValueConstructorVariant::ModuleFn {
                arity,
                module,
                name,
                ..
            } => eaf.function_reference(Some(module), name, *arity),
        }
    }

    /// Generates the document for the arguments' list of a function, bringing
    /// all those variable names into scope (that's needed to avoid accidentally
    /// shadowing a variable, that will result in an exception in Erlang)!
    fn fun_arguments(&mut self, arguments: &'a [TypedArg]) -> Document<'a> {
        wrap_arguments(arguments.iter().map(|argument| match &argument.names {
            ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => "_".to_doc(),
            ArgNames::Named { name, location }
            | ArgNames::NamedLabelled {
                name,
                name_location: location,
                ..
            } => self.new_erlang_variable(name, *location).to_doc(),
        }))
    }

    fn fun<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        arguments: &'a [TypedArg],
        body: &'a [TypedStatement],
    ) -> Document<'a> {
        let outer_scope = self.taken_names.clone();
        let doc = "fun"
            .to_doc()
            .append(self.fun_arguments(arguments).append(" ->"))
            .append(
                break_("", " ")
                    .append(self.statement_sequence(eaf, body))
                    .nest(INDENT),
            )
            .append(break_("", " "))
            .append("end")
            .group();
        self.taken_names = outer_scope;
        doc
    }

    fn call<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        fun: &'a TypedExpr,
        arguments: &'a [TypedCallArg],
    ) {
        match how_to_call(fun) {
            FunctionCall::BuildRecord { name } if arguments.is_empty() => {
                eaf.atom(&to_snake_case(name))
            }
            FunctionCall::BuildRecord { name } => {
                let tuple = eaf.start_tuple();
                eaf.atom(&to_snake_case(name));
                for argument in arguments {
                    let _ = self.maybe_block_expr(eaf, &argument.value);
                }
                eaf.end_tuple(tuple)
            }
            FunctionCall::Call { module, name } => {
                let call = if module != self.module_generator.module.name {
                    eaf.start_remote_call(
                        &module_erlang_name(module),
                        escape_erlang_existing_name(name),
                    )
                } else {
                    let call = eaf.start_call();
                    eaf.atom(escape_erlang_existing_name(name));
                    call
                };
                for argument in arguments {
                    let _ = self.maybe_block_expr(eaf, &argument.value);
                }
                eaf.end_call(call)
            }
            FunctionCall::DirectCall => todo!(),
        }
    }

    fn docs_arguments_call<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        fun: &'a TypedExpr,
        mut arguments: Vec<Document<'a>>,
    ) -> Document<'a> {
        match fun {
            TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Record { name, .. },
                ..
            }
            | TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        variant: ValueConstructorVariant::Record { name, .. },
                        ..
                    },
                ..
            } => tuple(std::iter::once(atom_string(to_snake_case(name))).chain(arguments)),

            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        variant:
                            ValueConstructorVariant::ModuleFn {
                                external_erlang: Some((module, name)),
                                ..
                            }
                            | ValueConstructorVariant::ModuleFn { module, name, .. },
                        ..
                    },
                ..
            } => todo!(),

            // Match against a Constant::Var that contains a function.
            // We want this to be emitted like a normal function call, not a function variable
            // substitution.
            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        variant:
                            ValueConstructorVariant::ModuleConstant {
                                literal:
                                    Constant::Var {
                                        constructor: Some(constructor),
                                        ..
                                    },
                                ..
                            },
                        ..
                    },
                ..
            } if constructor.variant.is_module_fn() => match &constructor.variant {
                ValueConstructorVariant::ModuleFn {
                    external_erlang: Some((module, name)),
                    ..
                }
                | ValueConstructorVariant::ModuleFn { module, name, .. } => {
                    todo!()
                }
                ValueConstructorVariant::LocalVariable { .. }
                | ValueConstructorVariant::ModuleConstant { .. }
                | ValueConstructorVariant::Record { .. } => {
                    unreachable!("The above clause guard ensures that this is a module fn")
                }
            },

            TypedExpr::ModuleSelect {
                constructor:
                    ModuleValueConstructor::Fn {
                        external_erlang: Some((module, name)),
                        ..
                    }
                    | ModuleValueConstructor::Fn { module, name, .. },
                ..
            } => {
                let arguments = wrap_arguments(arguments);
                let name = escape_erlang_existing_name(name);
                // We use the constructor Fn variant's `module` and function `name`.
                // It would also be valid to use the module and label as in the
                // Gleam code, but using the variant can result in an optimisation
                // in which the target function is used for `external fn`s, removing
                // one layer of wrapping.
                // This also enables an optimisation in the Erlang compiler in which
                // some Erlang BIFs can be replaced with literals if their arguments
                // are literals, such as `binary_to_atom`.
                atom_string(module_erlang_name(module))
                    .append(":")
                    .append(atom_string(name.into()))
                    .append(arguments)
            }

            TypedExpr::Fn { kind, body, .. } if kind.is_capture() => {
                if let Statement::Expression(TypedExpr::Call {
                    fun,
                    arguments: inner_arguments,
                    ..
                }) = body.first()
                {
                    let mut merged_arguments = Vec::with_capacity(inner_arguments.len());
                    for arg in inner_arguments {
                        if let TypedExpr::Var { name, .. } = &arg.value
                            && name == CAPTURE_VARIABLE
                        {
                            merged_arguments.push(arguments.swap_remove(0))
                        } else {
                            merged_arguments.push({
                                self.maybe_block_expr(eaf, &arg.value);
                                nil()
                            })
                        }
                    }
                    self.docs_arguments_call(eaf, fun, merged_arguments)
                } else {
                    panic!("Erl printing: Capture was not a call")
                }
            }

            TypedExpr::Fn { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::TupleIndex { .. } => {
                let arguments = wrap_arguments(arguments);
                self.expr(eaf, fun).surround("(", ")").append(arguments)
            }

            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::List { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => {
                let arguments = wrap_arguments(arguments);
                self.maybe_block_expr(eaf, fun);
                nil()
            }
        }
    }

    fn case<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        subjects: &'a [TypedExpr],
        cs: &'a [TypedClause],
    ) -> Document<'a> {
        let subjects_doc = if subjects.len() == 1 {
            let subject = subjects
                .first()
                .expect("erl case printing of single subject");
            self.maybe_block_expr(eaf, subject);
            nil()
        } else {
            tuple(subjects.iter().map(|element| {
                self.maybe_block_expr(eaf, element);
                nil()
            }))
        };
        "case "
            .to_doc()
            .append(subjects_doc)
            .append(" of")
            .append(line().append(self.clauses(eaf, cs)).nest(INDENT))
            .append(line())
            .append("end")
            .group()
    }

    fn clauses<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        cs: &'a [TypedClause],
    ) -> Document<'a> {
        join(
            cs.iter().map(|c| {
                let outer_scope = self.taken_names.clone();
                let erl = self.clause(eaf, c);
                // Reset the known variables now the clauses' scope has ended
                self.taken_names = outer_scope;
                erl
            }),
            ";".to_doc().append(lines(2)),
        )
    }

    fn clause<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        clause: &'a TypedClause,
    ) -> Document<'a> {
        let Clause {
            guard,
            pattern,
            alternative_patterns,
            then,
            ..
        } = clause;

        // These are required to get the alternative patterns working properly.
        // Simply rendering the duplicate erlang clauses breaks the variable
        // rewriting because each pattern would define different (rewritten)
        // variables names.
        let initial_taken_names = self.taken_names.clone();

        let mut branches_docs = Vec::with_capacity(alternative_patterns.len() + 1);
        for patterns in std::iter::once(pattern).chain(alternative_patterns) {
            // Erlang doesn't support alternative patterns, so we turn each
            // alternative into a branch of its own.
            // For each alternative, before generating the body, we need to reset
            // the variables in scope to what they are before the case expression,
            // so that a branch will not interfere with the other ones!
            self.taken_names = initial_taken_names.clone();
            let mut pattern_printer = PatternPrinter::new(self);

            let pattern = match patterns.as_slice() {
                [pattern] => pattern_printer.print(eaf, pattern),
                _ => tuple(patterns.iter().map(|pattern| {
                    pattern_printer.reset_variables();
                    pattern_printer.print(eaf, pattern)
                })),
            };

            let PatternPrinter {
                generator: _,
                variables: _,
                guards,
                assignments,
            } = pattern_printer;

            let assignments_map = assignments
                .iter()
                .map(|assignment| (assignment.gleam_name.clone(), assignment))
                .collect();

            let guard = self.optional_clause_guard(eaf, guard.as_ref(), guards, &assignments_map);
            let then = self.clause_consequence(eaf, then, assignments).group();
            branches_docs.push(docvec![
                pattern,
                guard,
                " ->",
                docvec![line(), then].nest(INDENT),
            ]);
        }

        join(branches_docs, ";".to_doc().append(lines(2)))
    }

    fn clause_consequence<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        consequence: &'a TypedExpr,
        // Further assignments that the pattern might need to introduce at the start
        // of the new block.
        assignments: Vec<StringPatternAssignment<'a>>,
    ) -> Document<'a> {
        let assignment_doc = if assignments.is_empty() {
            nil()
        } else {
            let separator = ",".to_doc().append(line());
            join(
                assignments
                    .iter()
                    .map(|assignment| assignment.to_assignment_doc()),
                separator.clone(),
            )
            .append(separator)
        };

        let consequence = if let TypedExpr::Block { statements, .. } = consequence {
            self.statement_sequence(eaf, statements)
        } else {
            self.expr(eaf, consequence)
        };
        assignment_doc.append(consequence)
    }

    fn const_inline<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        literal: &'a TypedConstant,
    ) -> Document<'a> {
        match literal {
            Constant::Int {
                value, int_value, ..
            } => {
                eaf.integer(int_value.clone());
                int(value)
            }
            Constant::Float {
                value, float_value, ..
            } => {
                eaf.float(float_value.value());
                float(value)
            }
            Constant::String { value, .. } => {
                eaf.string(value);
                string(value)
            }
            Constant::Tuple { elements, .. } => tuple(
                elements
                    .iter()
                    .map(|element| self.const_inline(eaf, element)),
            ),

            Constant::List { elements, tail, .. } => {
                match tail {
                    // There's no tail in the list, we join all the elements and
                    // call it a day.
                    None => join(
                        elements
                            .iter()
                            .map(|element| self.const_inline(eaf, element)),
                        break_(",", ", "),
                    ),
                    Some(tail) => match tail.list_elements() {
                        // There's a tail in the list whose elements are all known at
                        // compile time. In this case we replace the tail with those
                        // elements and create a single flat list.
                        Some(tail_elements) => join(
                            elements
                                .iter()
                                .chain(tail_elements)
                                .map(|element| self.const_inline(eaf, element)),
                            break_(",", ", "),
                        ),
                        // There's a tail in the list but we can't really tell what its
                        // elements are at compile time. This means we have to use
                        // erlang's syntax to append to a list.
                        None => {
                            let elements = join(
                                elements
                                    .iter()
                                    .map(|element| self.const_inline(eaf, element)),
                                break_(",", ", "),
                            );
                            docvec![elements, " | ", self.const_inline(eaf, tail)]
                        }
                    },
                }
                .nest(INDENT)
                .surround("[", "]")
                .group()
            }

            Constant::BitArray { segments, .. } => bit_array(
                segments
                    .iter()
                    .map(|s| self.const_segment(eaf, &s.value, &s.options)),
            ),

            Constant::Record {
                type_, arguments, ..
            } if arguments.is_none() => {
                let tag = literal
                    .constant_record_tag()
                    .expect("record without inferred constructor made it to code generation");

                match type_.deref() {
                    Type::Fn { arguments, .. } => record_constructor_function(tag, arguments.len()),
                    Type::Named { .. } | Type::Var { .. } | Type::Tuple { .. } => {
                        atom_string(to_snake_case(&tag))
                    }
                }
            }

            Constant::Record { arguments, .. } => {
                let tag = literal
                    .constant_record_tag()
                    .expect("record without inferred constructor made it to code generation");

                // Record updates are fully expanded during type checking, so we just handle arguments
                let arguments_doc = arguments
                    .iter()
                    .flatten()
                    .map(|argument| self.const_inline(eaf, &argument.value));
                let tag = atom_string(to_snake_case(&tag));
                tuple(std::iter::once(tag).chain(arguments_doc))
            }

            Constant::Var {
                name, constructor, ..
            } => {
                self.var(
                    eaf,
                    name,
                    constructor
                        .as_ref()
                        .expect("This is guaranteed to hold a value."),
                );
                nil()
            }

            Constant::StringConcatenation { left, right, .. } => {
                self.const_string_concatenate(eaf, left, right)
            }

            Constant::RecordUpdate { .. } => {
                panic!("record updates should not reach code generation")
            }
            Constant::Todo { .. } => panic!("todo constants should not reach code generation"),
            Constant::Invalid { .. } => {
                panic!("invalid constants should not reach code generation")
            }
        }
    }

    fn const_string_concatenate<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        left: &'a TypedConstant,
        right: &'a TypedConstant,
    ) -> Document<'a> {
        let left = self.const_string_concatenate_argument(eaf, left);
        let right = self.const_string_concatenate_argument(eaf, right);
        const_string_concatenate_bit_array([left, right])
    }

    fn const_string_concatenate_inner<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        left: &'a TypedConstant,
        right: &'a TypedConstant,
    ) -> Document<'a> {
        let left = self.const_string_concatenate_argument(eaf, left);
        let right = self.const_string_concatenate_argument(eaf, right);
        join([left, right], break_(",", ", "))
    }

    fn const_string_concatenate_argument<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        value: &'a TypedConstant,
    ) -> Document<'a> {
        match value {
            Constant::String { value, .. } => docvec!['"', string_inner(value), "\"/utf8"],

            Constant::Var {
                constructor: Some(constructor),
                ..
            } => match &constructor.variant {
                ValueConstructorVariant::ModuleConstant {
                    literal: Constant::String { value, .. },
                    ..
                } => docvec!['"', string_inner(value), "\"/utf8"],
                ValueConstructorVariant::ModuleConstant {
                    literal: Constant::StringConcatenation { left, right, .. },
                    ..
                } => self.const_string_concatenate_inner(eaf, left, right),
                ValueConstructorVariant::LocalVariable { .. }
                | ValueConstructorVariant::ModuleConstant { .. }
                | ValueConstructorVariant::ModuleFn { .. }
                | ValueConstructorVariant::Record { .. } => self.const_inline(eaf, value),
            },

            Constant::StringConcatenation { left, right, .. } => {
                self.const_string_concatenate_inner(eaf, left, right)
            }

            Constant::Int { .. }
            | Constant::Float { .. }
            | Constant::Tuple { .. }
            | Constant::List { .. }
            | Constant::Record { .. }
            | Constant::RecordUpdate { .. }
            | Constant::BitArray { .. }
            | Constant::Var { .. }
            | Constant::Todo { .. }
            | Constant::Invalid { .. } => self.const_inline(eaf, value),
        }
    }

    fn string_concatenate<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
    ) -> Document<'a> {
        let left = self.string_concatenate_argument(eaf, left);
        let right = self.string_concatenate_argument(eaf, right);
        bit_array([left, right])
    }

    fn string_concatenate_argument<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        value: &'a TypedExpr,
    ) -> Document<'a> {
        match value {
            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        variant:
                            ValueConstructorVariant::ModuleConstant {
                                literal: Constant::String { value, .. },
                                ..
                            },
                        ..
                    },
                ..
            }
            | TypedExpr::String { value, .. } => {
                docvec!['"', string_inner(value), "\"/utf8"]
            }

            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        variant: ValueConstructorVariant::LocalVariable { location, .. },
                        ..
                    },
                ..
            } => docvec![self.local_var_name(location), "/binary"],

            TypedExpr::BinOp {
                operator: BinOp::Concatenate,
                ..
            } => docvec![self.expr(eaf, value), "/binary"],

            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => {
                docvec![
                    "(",
                    {
                        self.maybe_block_expr(eaf, value);
                        nil()
                    },
                    ")/binary"
                ]
            }
        }
    }

    fn const_segment<Output, Gen: Eaf<Output>>(
        &mut self,
        eaf: &mut Gen,
        value: &'a TypedConstant,
        options: &'a [TypedConstantBitArraySegmentOption],
    ) -> Document<'a> {
        let value_is_a_string_literal = matches!(value, Constant::String { .. });

        let create_document = |eaf: &mut Gen, this: &mut Self| {
            match value {
                // Skip the normal <<value/utf8>> surrounds
                Constant::String { value, .. } => value.to_doc().surround("\"", "\""),

                // As normal
                Constant::Int { .. } | Constant::Float { .. } | Constant::BitArray { .. } => {
                    this.const_inline(eaf, value)
                }

                // Wrap anything else in parentheses
                Constant::Tuple { .. }
                | Constant::List { .. }
                | Constant::Record { .. }
                | Constant::RecordUpdate { .. }
                | Constant::Var { .. }
                | Constant::StringConcatenation { .. }
                | Constant::Todo { .. }
                | Constant::Invalid { .. } => this.const_inline(eaf, value).surround("(", ")"),
            }
        };

        let size = |eaf: &mut Gen, value: &'a TypedConstant, this: &mut Self| {
            if let Constant::Int { .. } = value {
                Some(":".to_doc().append(this.const_inline(eaf, value)))
            } else {
                Some(
                    ":".to_doc()
                        .append(this.const_inline(eaf, value).surround("(", ")")),
                )
            }
        };

        let unit = |_eaf: &mut Gen, value: &'a u8| Some(eco_format!("unit:{value}").to_doc());

        bit_array_segment(
            eaf,
            create_document,
            options,
            size,
            unit,
            value_is_a_string_literal,
            false,
            self,
        )
    }

    fn assign_to_variable<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        value: &'a TypedExpr,
        assignments: &mut Vec<Document<'a>>,
    ) -> Document<'a> {
        if value.is_var() {
            self.expr(eaf, value)
        } else {
            let value = {
                self.maybe_block_expr(eaf, value);
                nil()
            };
            let variable = self.new_throwaway_variable();
            let definition = docvec![variable.clone(), " = ", value, ",", line()];
            assignments.push(definition);
            variable.to_doc()
        }
    }

    fn assert_call<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        function: &'a TypedExpr,
        arguments: &'a Vec<CallArg<TypedExpr>>,
        assignments: &mut Vec<Document<'a>>,
    ) -> (Document<'a>, Vec<(&'static str, Document<'a>)>) {
        let argument_variables = arguments
            .iter()
            .map(|argument| self.assign_to_variable(eaf, &argument.value, assignments))
            .collect_vec();

        let arguments = join(
            argument_variables
                .iter()
                .zip(arguments)
                .map(|(variable, argument)| {
                    asserted_expression(
                        AssertExpression::from_expression(&argument.value),
                        Some(variable.clone()),
                        argument.location(),
                    )
                }),
            break_(",", ", "),
        )
        .nest(INDENT)
        .surround("[", "]");

        (
            self.docs_arguments_call(eaf, function, argument_variables),
            vec![("kind", atom("function_call")), ("arguments", arguments)],
        )
    }

    fn bin_op<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        name: &'a BinOp,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
    ) {
        let operator = match name {
            BinOp::And => "andalso",
            BinOp::Or => "orelse",
            BinOp::LtInt | BinOp::LtFloat => "<",
            BinOp::LtEqInt | BinOp::LtEqFloat => "=<",
            BinOp::Eq => "=:=",
            BinOp::NotEq => "/=",
            BinOp::GtInt | BinOp::GtFloat => ">",
            BinOp::GtEqInt | BinOp::GtEqFloat => ">=",
            BinOp::AddInt | BinOp::AddFloat => "+",
            BinOp::SubInt | BinOp::SubFloat => "-",
            BinOp::MultInt | BinOp::MultFloat => "*",

            // Division needs some extra case, in Gleam dividing by 0 results
            // in 0; while in Erlang that's an exception.
            BinOp::DivFloat => return self.float_div(eaf, left, right),
            BinOp::DivInt => return self.int_div(eaf, left, right, "div"),
            BinOp::RemainderInt => return self.int_div(eaf, left, right, "rem"),

            // String concatenation is not a binop at all! It's just building a
            // bit array.
            BinOp::Concatenate => {
                let _ = self.string_concatenate(eaf, left, right);
                return;
            }
        };

        eaf.binary_operator(operator);
        self.maybe_block_expr(eaf, left);
        self.maybe_block_expr(eaf, right);
    }

    fn float_div<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
    ) {
        if right.is_non_zero_compile_time_number() {
            eaf.binary_operator("/");
            self.maybe_block_expr(eaf, left);
            self.maybe_block_expr(eaf, right);
        } else if left.is_literal() && right.is_zero_compile_time_number() {
            eaf.float(0.0);
        } else {
            let left = self.expr(eaf, left);
            let right = self.expr(eaf, right);
            let denominator = self.new_throwaway_variable();
            let clauses = docvec![
                line(),
                "+0.0 -> +0.0;",
                line(),
                "-0.0 -> -0.0;",
                line(),
                denominator.clone(),
                " -> ",
                binop_documents(left, "/", denominator.to_doc())
            ];
            let _ = docvec!["case ", right, " of", clauses.nest(INDENT), line(), "end"];
        }
    }

    fn int_div<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        op: &'static str,
    ) {
        if right.is_non_zero_compile_time_number() {
            eaf.binary_operator(op);
            self.maybe_block_expr(eaf, left);
            self.maybe_block_expr(eaf, right);
        } else if left.is_literal() && right.is_zero_compile_time_number() {
            // If we have a constant value divided by zero then it's safe to
            // replace it directly with 0.
            eaf.integer(BigInt::ZERO);
        } else {
            let left = self.expr(eaf, left);
            let right = self.expr(eaf, right);
            let denominator = self.new_throwaway_variable();
            let clauses = docvec![
                line(),
                "0 -> 0;",
                line(),
                denominator.clone(),
                " -> ",
                binop_documents(left, op, denominator.to_doc())
            ];
            let _ = docvec!["case ", right, " of", clauses.nest(INDENT), line(), "end"];
        }
    }

    /// This is used to print segments of a bit array expression.
    /// Those are different enough from the constant and pattern ones that it would
    /// no longer make sense to try and adapt the `bit_array_segment` generic
    /// function to work with the three of them.
    /// So you should use this one for printing expression segments, and the generic
    /// `bit_array_segment` function for constant and pattern segments instead.
    ///
    fn bit_array_expression_segment<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        segment: &'a TypedExprBitArraySegment,
    ) -> Document<'a> {
        // Literal strings can have the `utf8`, `utf16`, or `utf32` options just
        // fine, and that would be no issue on the Erlang side:
        //
        // ```erl
        // <<"wibble"/utf8>>
        // <<"wibble"/utf16>>
        // <<"wibble"/utf32>>
        // ```
        //
        // However there's issues when we try and use those options with _variables_
        // with the string type. That will result in errors on the Erlang target:
        //
        // ```erl
        // % These are all runtime errors!!
        // <<SomeString/utf8>>
        // <<SomeString/utf16>>
        // <<SomeString/utf32>>
        // ```
        //
        // In Gleam we support those options for all string values, not just
        // literals. So we need to do something about them:
        //
        // - `utf8`: strings are already `utf8` binaries in Gleam, so if we have a
        //   string value with that option we can put it in the bit array like any
        //   other binary value:
        //   ```gleam
        //   <<some_string:utf8>>
        //   // becomes <<SomeString/binary>>
        //   ```
        // - `utf16` and `utf32`: these are a bit tricker since they will require
        //   some conversion (which is what we also do on the JavaScript target!).
        //   So in this case we need to use the `unicode:characters_to_binary`
        //   function that will return a binary value we can then put in the bit
        //   array:
        //   ```gleam
        //   <<some_string:utf16-little>>
        //   // becomes
        //   // <<(unicode:characters_to_binary(
        //   //     SomeString,
        //   //     utf8,               the current encoding
        //   //     {utf16, little})    the encoding we want
        //   //  )/binary>>
        //   ```
        //
        if segment.type_.is_string()
            && !segment.value.is_literal_string()
            && let Some(encoding) = expression_segment_string_encoding(segment)
        {
            match encoding {
                // Gleam strings are utf8 encoded binaries, so we just need to add
                // the binary option
                ExpressionSegmentStringEncoding::Utf8 => {
                    docvec![
                        self.bit_array_expression_segment_value(eaf, &segment.value),
                        "/binary"
                    ]
                }

                // For utf16 and utf32 we need an explicit conversion using erlang's
                // `unicode:characters_to_binary`
                ExpressionSegmentStringEncoding::Utf16 { endiannes } => {
                    let value = {
                        self.maybe_block_expr(eaf, &segment.value);
                        nil()
                    };
                    let encoding = match endiannes {
                        Endianness::Big => "{utf16, big}",
                        Endianness::Little => "{utf16, little}",
                    };
                    docvec![
                        "(unicode:characters_to_binary",
                        wrap_arguments([value, "utf8".to_doc(), encoding.to_doc()]),
                        ")/binary"
                    ]
                }
                ExpressionSegmentStringEncoding::Utf32 { endiannes } => {
                    let value = {
                        self.maybe_block_expr(eaf, &segment.value);
                        nil()
                    };
                    let encoding = match endiannes {
                        Endianness::Big => "{utf32, big}",
                        Endianness::Little => "{utf32, little}",
                    };

                    docvec![
                        "(unicode:characters_to_binary",
                        wrap_arguments([value, "utf8".to_doc(), encoding.to_doc()]),
                        ")/binary"
                    ]
                }
            }
        } else {
            // If the bit array segment doesn't need any special handling we use the
            // regular printing functions to format its value and options.
            docvec![
                self.bit_array_expression_segment_value(eaf, &segment.value),
                self.bit_array_expression_options(eaf, &segment.options)
            ]
        }
    }

    fn bit_array_expression_options<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        options: &'a [BitArrayOption<TypedExpr>],
    ) -> Document<'a> {
        // The size and unit options are a bit special: if present size must come
        // first, and the unit must come last. So we keep them separate from all the
        // other options.
        //
        // ```erl
        // <<Segment:Size/Option1-Option2-unit:UnitValue>>
        // %        ^^^^^ Size is first immediately after `:`
        // %             ^^^^^^^^^^^^^^^^ All other options come after `/`
        // %                             ^^^^^ And unit is always the last one of
        // %                                   those written like this: `unit:Value`
        // ```
        let mut size: Option<Document<'a>> = None;
        let mut unit: Option<Document<'a>> = None;
        let mut others = Vec::new();

        for option in options {
            match option {
                BitArrayOption::Utf8 { .. } => others.push("utf8".to_doc()),
                BitArrayOption::Utf16 { .. } => others.push("utf16".to_doc()),
                BitArrayOption::Utf32 { .. } => others.push("utf32".to_doc()),
                BitArrayOption::Int { .. } => others.push("integer".to_doc()),
                BitArrayOption::Float { .. } => others.push("float".to_doc()),
                BitArrayOption::Bytes { .. } => others.push("binary".to_doc()),
                BitArrayOption::Bits { .. } => others.push("bitstring".to_doc()),
                BitArrayOption::Utf8Codepoint { .. } => others.push("utf8".to_doc()),
                BitArrayOption::Utf16Codepoint { .. } => others.push("utf16".to_doc()),
                BitArrayOption::Utf32Codepoint { .. } => others.push("utf32".to_doc()),
                BitArrayOption::Signed { .. } => others.push("signed".to_doc()),
                BitArrayOption::Unsigned { .. } => others.push("unsigned".to_doc()),
                BitArrayOption::Big { .. } => others.push("big".to_doc()),
                BitArrayOption::Little { .. } => others.push("little".to_doc()),
                BitArrayOption::Native { .. } => others.push("native".to_doc()),
                BitArrayOption::Unit { value, .. } => {
                    unit = Some(eco_format!("unit:{value}").to_doc())
                }
                BitArrayOption::Size { value, .. } => {
                    // Sizes need some care: in Erlang, having a negative segment size
                    // results in a runtime error. We can't do that in Gleam! So any
                    // negative value must be turned to zero instead:
                    size = Some(if let TypedExpr::Int { int_value, .. } = value.as_ref() {
                        // For literals we can easily replace negative values with
                        // the literal zero.
                        let value = if int_value.is_negative() {
                            &BigInt::ZERO
                        } else {
                            int_value
                        };
                        docvec![":", value.clone()]
                    } else {
                        // For any other non constant expression we need to use
                        // `erlang:max(0, <Value>)` to ensure the value is never
                        // zero at runtime!
                        docvec![
                            ":(erlang:max(0, ",
                            {
                                self.maybe_block_expr(eaf, value);
                                nil()
                            },
                            "))"
                        ]
                    });
                }
            }
        }

        // The unit must always be the last option, if present.
        if let Some(unit) = unit {
            others.push(unit)
        }

        let options = if !others.is_empty() {
            docvec!["/", join(others, "-".to_doc())]
        } else {
            nil()
        };

        // Size comes before all the other options.
        docvec![size, options]
    }

    /// The document for the value of a bit array segment expression.
    /// Segment values can't be produced using a simple `expr` call but need special
    /// handling in some cases which this function takes care of!
    fn bit_array_expression_segment_value<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        value: &'a TypedExpr,
    ) -> Document<'a> {
        match value {
            // Skip the normal <<value/utf8>> surrounds
            TypedExpr::String { value, .. } => string_inner(value).surround("\"", "\""),

            // As normal
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::BitArray { .. } => self.expr(eaf, value),

            // Anything else needs to be wrapped in parentheses
            TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => self.expr(eaf, value).surround("(", ")"),
        }
    }

    fn optional_clause_guard<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        guard: Option<&'a TypedClauseGuard>,
        additional_guards: Vec<Document<'a>>,
        assignments: &HashMap<EcoString, &StringPatternAssignment<'a>>,
    ) -> Document<'a> {
        let guard_doc = guard.map(|guard| self.bare_clause_guard(eaf, guard, assignments));

        let guards_count = guard_doc.iter().len() + additional_guards.len();
        let guards_docs = additional_guards.into_iter().chain(guard_doc).map(|guard| {
            if guards_count > 1 {
                guard.surround("(", ")")
            } else {
                guard
            }
        });
        let doc = join(guards_docs, " andalso ".to_doc());
        if doc.is_empty() {
            doc
        } else {
            " when ".to_doc().append(doc)
        }
    }

    fn bare_clause_guard<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        guard: &'a TypedClauseGuard,
        assignments: &HashMap<EcoString, &StringPatternAssignment<'a>>,
    ) -> Document<'a> {
        match guard {
            ClauseGuard::Invalid { .. } => unreachable!("invalid guard made it to code generation"),

            ClauseGuard::Block { value, .. } => self
                .bare_clause_guard(eaf, value, assignments)
                .surround("(", ")"),

            ClauseGuard::Not { expression, .. } => {
                docvec!["not ", self.bare_clause_guard(eaf, expression, assignments)]
            }

            ClauseGuard::BinaryOperator {
                operator,
                left,
                right,
                ..
            } => {
                let left_document = self.clause_guard(eaf, left, assignments);
                let right_document = self.clause_guard(eaf, right, assignments);

                let operator = match operator {
                    BinOp::Or => "orelse",
                    BinOp::And => "andalso",
                    BinOp::Eq => "=:=",
                    BinOp::NotEq => "=/=",
                    BinOp::GtInt | BinOp::GtFloat => ">",
                    BinOp::GtEqInt | BinOp::GtEqFloat => ">=",
                    BinOp::LtInt | BinOp::LtFloat => "<",
                    BinOp::LtEqInt | BinOp::LtEqFloat => "=<",
                    BinOp::AddInt | BinOp::AddFloat => "+",
                    BinOp::SubInt | BinOp::SubFloat => "-",
                    BinOp::MultInt | BinOp::MultFloat => "*",
                    BinOp::DivFloat => "/",
                    BinOp::DivInt => "div",
                    BinOp::RemainderInt => "rem",
                    BinOp::Concatenate => {
                        return self.clause_guard_string_concatenate(eaf, left, right, assignments);
                    }
                };

                docvec![left_document, " ", operator, " ", right_document]
            }

            // Only local variables are supported and the typer ensures that all
            // ClauseGuard::Vars are local variables
            ClauseGuard::Var {
                name,
                definition_location,
                ..
            } => {
                // If we're referencing a variable introduced by a string pattern
                // assignment we need to replace it with its actual literal value:
                // in the generated code the variable is only defined later, so
                // just referencing its name would result in an error.
                assignments
                    .get(name)
                    .map(|assignment| assignment.literal_value.clone())
                    .unwrap_or_else(|| self.local_var_name(definition_location).to_doc())
            }

            ClauseGuard::TupleIndex { tuple, index, .. } => {
                self.tuple_index_inline(eaf, tuple, *index)
            }

            ClauseGuard::FieldAccess {
                container, index, ..
            } => self.tuple_index_inline(eaf, container, index.expect("Unable to find index") + 1),

            ClauseGuard::ModuleSelect { literal, .. } => self.const_inline(eaf, literal),

            ClauseGuard::Constant(constant) => self.const_inline(eaf, constant),
        }
    }

    fn clause_guard<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        guard: &'a TypedClauseGuard,
        assignments: &HashMap<EcoString, &StringPatternAssignment<'a>>,
    ) -> Document<'a> {
        match guard {
            ClauseGuard::Invalid { .. } => unreachable!("invalid guard made it to code generation"),
            // Binary operators are wrapped in parens
            ClauseGuard::BinaryOperator { .. } => "("
                .to_doc()
                .append(self.bare_clause_guard(eaf, guard, assignments))
                .append(")"),

            // Other expressions are not
            ClauseGuard::Constant(_)
            | ClauseGuard::Not { .. }
            | ClauseGuard::Var { .. }
            | ClauseGuard::TupleIndex { .. }
            | ClauseGuard::FieldAccess { .. }
            | ClauseGuard::ModuleSelect { .. }
            | ClauseGuard::Block { .. } => self.bare_clause_guard(eaf, guard, assignments),
        }
    }

    fn tuple_index_inline<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        tuple: &'a TypedClauseGuard,
        index: u64,
    ) -> Document<'a> {
        let index_doc = eco_format!("{}", (index + 1)).to_doc();
        let tuple_doc = self.bare_clause_guard(eaf, tuple, &HashMap::new());
        "erlang:element"
            .to_doc()
            .append(wrap_arguments([index_doc, tuple_doc]))
    }

    fn clause_guard_string_concatenate<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        left: &'a TypedClauseGuard,
        right: &'a TypedClauseGuard,
        assignments: &HashMap<EcoString, &StringPatternAssignment<'a>>,
    ) -> Document<'a> {
        let left = self.clause_guard_string_concatenate_argument(eaf, left, assignments);
        let right = self.clause_guard_string_concatenate_argument(eaf, right, assignments);
        bit_array([left, right])
    }

    fn clause_guard_string_concatenate_argument<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        guard: &'a TypedClauseGuard,
        assignments: &HashMap<EcoString, &StringPatternAssignment<'a>>,
    ) -> Document<'a> {
        match guard {
            ClauseGuard::Invalid { .. } => unreachable!("invalid guard made it to code generation"),

            ClauseGuard::Constant(Constant::String { value, .. }) => {
                docvec!['"', string_inner(value), "\"/utf8"]
            }

            ClauseGuard::Constant(Constant::StringConcatenation { left, right, .. }) => {
                self.const_string_concatenate_inner(eaf, left, right)
            }

            ClauseGuard::ModuleSelect { literal, .. } => match literal {
                Constant::String { value, .. } => docvec!['"', string_inner(value), "\"/utf8"],
                Constant::StringConcatenation { left, right, .. } => {
                    self.const_string_concatenate_inner(eaf, left, right)
                }
                Constant::Int { .. }
                | Constant::Float { .. }
                | Constant::Tuple { .. }
                | Constant::List { .. }
                | Constant::Record { .. }
                | Constant::RecordUpdate { .. }
                | Constant::BitArray { .. }
                | Constant::Var { .. }
                | Constant::Todo { .. }
                | Constant::Invalid { .. } => {
                    docvec!["(", self.const_inline(eaf, literal), ")/binary"]
                }
            },

            ClauseGuard::Var {
                name,
                definition_location,
                ..
            } => assignments
                .get(name)
                .map(|assignment| docvec![assignment.literal_value.clone(), "/binary"])
                .unwrap_or_else(|| docvec![self.local_var_name(definition_location), "/binary"]),

            ClauseGuard::BinaryOperator {
                operator: BinOp::Concatenate,
                left,
                right,
                ..
            } => docvec![
                self.clause_guard_string_concatenate(eaf, left, right, assignments),
                "/binary"
            ],

            ClauseGuard::Block { .. }
            | ClauseGuard::BinaryOperator { .. }
            | ClauseGuard::Not { .. }
            | ClauseGuard::TupleIndex { .. }
            | ClauseGuard::FieldAccess { .. }
            | ClauseGuard::Constant(_) => docvec![
                self.clause_guard(eaf, guard, assignments)
                    .surround("(", ")"),
                "/binary"
            ],
        }
    }
}

pub fn records(module: &TypedModule) -> Vec<(&str, String)> {
    module
        .definitions
        .custom_types
        .iter()
        .filter(|custom_type| {
            custom_type.publicity.is_public()
                && !module
                    .unused_definition_positions
                    .contains(&custom_type.location.start)
        })
        .flat_map(|custom_type| &custom_type.constructors)
        .filter(|constructor| !constructor.arguments.is_empty())
        .filter_map(|constructor| {
            constructor
                .arguments
                .iter()
                .map(
                    |RecordConstructorArg {
                         label,
                         ast: _,
                         location: _,
                         type_,
                         ..
                     }| {
                        label
                            .as_ref()
                            .map(|(_, label)| (label.as_str(), type_.clone()))
                    },
                )
                .collect::<Option<Vec<_>>>()
                .map(|fields| (constructor.name.as_str(), fields))
        })
        .map(|(name, fields)| (name, record_definition(name, &fields)))
        .collect()
}

/// Given an expression, this tells us how we should be calling it as a
/// function in the generated erlang code.
fn how_to_call<'a>(function: &'a TypedExpr) -> FunctionCall<'a> {
    match function {
        // This is a record constructor from the current module.
        // For example:
        //
        // ```gleam
        // pub type Wibble { Wibble(Int) }
        // pub fn main() {
        //   Wibble(1)
        // //^^^^^^^^^ This!
        // }
        // ```
        //
        // On the Erlang side we have to build a tagged tuple
        //
        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, .. },
            ..
        } => FunctionCall::BuildRecord { name },

        // Notice how whenever we have a function that has an erlang
        // external definition we will always directly call that and not go
        // through the Gleam function. For example:
        //
        // ```gleam
        // pub fn main() {
        //   format("hello", [])
        // }
        //
        // @external(erlang, "io", "format")
        // fn format(string: String, args: List(String)) -> Nil
        // ```
        //
        // Will result in:
        //
        // ```erl
        // main() ->
        //   io:format(~"hello", []).
        // ```
        //
        // This enables the Erlang compiler to further optimise those calls.
        //
        TypedExpr::ModuleSelect {
            constructor:
                ModuleValueConstructor::Fn {
                    external_erlang: Some((module, name)),
                    ..
                }
                | ModuleValueConstructor::Fn { module, name, .. },
            ..
        } => FunctionCall::Call { module, name },

        // We're calling a variable as a function.
        TypedExpr::Var { constructor, .. } => match &constructor.variant {
            // The variable is the constructor for a record.
            // That's a tagged tuple.
            ValueConstructorVariant::Record { name, .. } => FunctionCall::BuildRecord { name },
            // The variable is a module function, we can call that as usual
            // just like we did for `TypedExpr::ModuleSelect`.
            ValueConstructorVariant::ModuleFn {
                external_erlang: Some((module, name)),
                ..
            }
            | ValueConstructorVariant::ModuleFn { module, name, .. } => {
                FunctionCall::Call { module, name }
            }
            // The variable is a variable defined inside the function, we
            // can call it directly:
            //
            // ```erl
            // SomeVariable = fun() -> ... end,
            // SomeVariable()
            // ```
            ValueConstructorVariant::LocalVariable { .. } => FunctionCall::DirectCall,
            // The variable is a module constant, if it refers to a module
            // function we want to call it directly.
            ValueConstructorVariant::ModuleConstant { literal, .. } => {
                if let Constant::Var {
                    constructor: Some(constructor),
                    ..
                } = literal
                    && let ValueConstructorVariant::ModuleFn {
                        external_erlang: Some((module, name)),
                        ..
                    }
                    | ValueConstructorVariant::ModuleFn { module, name, .. } =
                        &constructor.variant
                {
                    FunctionCall::Call { module, name }
                } else {
                    FunctionCall::DirectCall
                }
            }
        },

        //TypedExpr::Fn { kind, body, .. } if kind.is_capture() => {
        //    if let Statement::Expression(TypedExpr::Call {
        //        fun,
        //        arguments: inner_arguments,
        //        ..
        //    }) = body.first()
        //    {
        //        let mut merged_arguments = Vec::with_capacity(inner_arguments.len());
        //        for arg in inner_arguments {
        //            if let TypedExpr::Var { name, .. } = &arg.value
        //                && name == CAPTURE_VARIABLE
        //            {
        //                merged_arguments.push(arguments.swap_remove(0))
        //            } else {
        //                merged_arguments.push(self.maybe_block_expr(eaf, &arg.value))
        //            }
        //        }
        //        self.docs_arguments_call(eaf, fun, merged_arguments)
        //    } else {
        //        panic!("Erl printing: Capture was not a call")
        //    }
        //}
        TypedExpr::Fn { .. }
        | TypedExpr::Call { .. }
        | TypedExpr::Todo { .. }
        | TypedExpr::Panic { .. }
        | TypedExpr::RecordAccess { .. }
        | TypedExpr::TupleIndex { .. }
        | TypedExpr::Int { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::String { .. }
        | TypedExpr::Block { .. }
        | TypedExpr::Pipeline { .. }
        | TypedExpr::List { .. }
        | TypedExpr::BinOp { .. }
        | TypedExpr::Case { .. }
        | TypedExpr::PositionalAccess { .. }
        | TypedExpr::ModuleSelect { .. }
        | TypedExpr::Tuple { .. }
        | TypedExpr::Echo { .. }
        | TypedExpr::BitArray { .. }
        | TypedExpr::RecordUpdate { .. }
        | TypedExpr::NegateBool { .. }
        | TypedExpr::NegateInt { .. }
        | TypedExpr::Invalid { .. } => FunctionCall::DirectCall,
    }
}

pub fn record_definition(name: &str, fields: &[(&str, Arc<Type>)]) -> String {
    let name = to_snake_case(name);
    let type_printer = TypePrinter::new("").var_as_any();
    let fields = fields.iter().map(move |(name, type_)| {
        let type_ = type_printer.print(type_);
        docvec![atom_string((*name).into()), " :: ", type_.group()]
    });
    let fields = break_("", "")
        .append(join(fields, break_(",", ", ")))
        .nest(INDENT)
        .append(break_("", ""))
        .group();
    docvec!["-record(", atom_string(name), ", {", fields, "}).", line()]
        .to_pretty_string(MAX_COLUMNS)
}

pub fn module<'a>(
    module: &'a TypedModule,
    line_numbers: &'a LineNumbers,
    root: &'a Utf8Path,
) -> Result<String> {
    let mut generator = Generator::new(module, line_numbers, root);
    let mut eaf = PrettyEaf::new(&module.erlang_name());
    let _document = generator
        .module_document(&mut eaf)?
        .to_pretty_string(MAX_COLUMNS);

    Ok(eaf.into_output())
}

/// If the given function should be exported from the current Erlang module then
/// this function will return its name and arity to be used when exporting it.
/// For example: `pub fn wibble(a, b)` will produce `Some(("wibble", 2))`, so
/// we can export `wibble/2`.
fn function_export<'a>(
    function: &'a TypedFunction,
    overridden_publicity: &im::HashSet<EcoString>,
) -> Option<(&'a str, usize)> {
    let (_, name) = function
        .name
        .as_ref()
        .expect("module function with no name");

    // If the function is not implemented for this target, don't attempt to
    // export it.
    if !function.implementations.supports(Target::Erlang) {
        return None;
    }

    // If the function is not importable and it's publicity has not been
    // overridden, don't attempt to export it.
    if !function.publicity.is_importable() && !overridden_publicity.contains(name) {
        return None;
    }

    let name = escape_erlang_existing_name(name);
    Some((name, function.arguments.len()))
}

/// Given a custom type this returns the name it should be used to export it and
/// its arity. For example: `pub type Wibble(a, b)` will produce `("wibble", 2)`,
/// so we can export `wibble/2`.
fn type_export<'a>(custom_type: &'a TypedCustomType) -> (EcoString, usize) {
    let name = erl_safe_type_name(to_snake_case(&custom_type.name));
    let arity = custom_type.typed_parameters.len();
    (name, arity)
}

fn register_custom_type_exports<'a>(
    custom_type: &TypedCustomType,
    type_exports: &mut Vec<Document<'a>>,
    type_defs: &mut Vec<Document<'a>>,
    module_name: &'a str,
) {
    let TypedCustomType {
        name,
        constructors,
        opaque,
        typed_parameters,
        external_erlang,
        ..
    } = custom_type;

    // Erlang doesn't allow phantom type variables in type definitions but gleam does
    // so we check the type declaratinon against its constroctors and generate a phantom
    // value that uses the unused type variables.
    let type_var_usages = collect_type_var_usages(HashMap::new(), typed_parameters);
    let mut constructor_var_usages = HashMap::new();
    for c in constructors {
        constructor_var_usages =
            collect_type_var_usages(constructor_var_usages, c.arguments.iter().map(|a| &a.type_));
    }
    let phantom_vars: Vec<_> = type_var_usages
        .keys()
        .filter(|&id| !constructor_var_usages.contains_key(id))
        .sorted()
        .map(|&id| Type::Var {
            type_: Arc::new(std::cell::RefCell::new(TypeVar::Generic { id })),
        })
        .collect();
    let phantom_vars_constructor = if !phantom_vars.is_empty() {
        let type_printer = TypePrinter::new(module_name);
        Some(tuple(
            std::iter::once("gleam_phantom".to_doc())
                .chain(phantom_vars.iter().map(|pv| type_printer.print(pv))),
        ))
    } else {
        None
    };
    // Type Exports
    type_exports.push(
        erl_safe_type_name(to_snake_case(name))
            .to_doc()
            .append("/")
            .append(typed_parameters.len()),
    );
    // Type definitions
    let definition = if constructors.is_empty() {
        if let Some((module, external_type, _location)) = external_erlang {
            let printer = TypePrinter::new(module_name);
            docvec![
                module,
                ":",
                external_type,
                "(",
                join(
                    typed_parameters
                        .iter()
                        .map(|parameter| printer.print(parameter)),
                    ", ".to_doc()
                ),
                ")"
            ]
        } else {
            let constructors = std::iter::once("any()".to_doc()).chain(phantom_vars_constructor);
            join(constructors, break_(" |", " | "))
        }
    } else {
        let constructors = constructors
            .iter()
            .map(|constructor| {
                let name = atom_string(to_snake_case(&constructor.name));
                if constructor.arguments.is_empty() {
                    name
                } else {
                    let type_printer = TypePrinter::new(module_name);
                    let arguments = constructor
                        .arguments
                        .iter()
                        .map(|argument| type_printer.print(&argument.type_));
                    tuple(std::iter::once(name).chain(arguments))
                }
            })
            .chain(phantom_vars_constructor);
        join(constructors, break_(" |", " | "))
    }
    .nest(INDENT);
    let type_printer = TypePrinter::new(module_name);
    let params = join(
        typed_parameters
            .iter()
            .map(|type_| type_printer.print(type_)),
        ", ".to_doc(),
    );
    let doc = if *opaque { "-opaque " } else { "-type " }
        .to_doc()
        .append(erl_safe_type_name(to_snake_case(name)))
        .append("(")
        .append(params)
        .append(") :: ")
        .append(definition)
        .group()
        .append(".");
    type_defs.push(doc);
}

fn wrap_arguments<'a, I>(arguments: I) -> Document<'a>
where
    I: IntoIterator<Item = Document<'a>>,
{
    break_("", "")
        .append(join(arguments, break_(",", ", ")))
        .nest(INDENT)
        .append(break_("", ""))
        .surround("(", ")")
        .group()
}

fn atom_string(value: EcoString) -> Document<'static> {
    escape_atom_string(value).to_doc()
}

static ATOM_PATTERN: OnceLock<Regex> = OnceLock::new();

fn atom_pattern() -> &'static Regex {
    ATOM_PATTERN.get_or_init(|| Regex::new(r"^[a-z][a-z0-9_@]*$").expect("atom RE regex"))
}

fn atom(value: &str) -> Document<'_> {
    if is_erlang_reserved_word(value) {
        // Escape because of keyword collision
        eco_format!("'{value}'").to_doc()
    } else if atom_pattern().is_match(value) {
        // No need to escape
        EcoString::from(value).to_doc()
    } else {
        // Escape because of characters contained
        eco_format!("'{value}'").to_doc()
    }
}

pub fn escape_atom_string(value: EcoString) -> EcoString {
    if is_erlang_reserved_word(&value) {
        // Escape because of keyword collision
        eco_format!("'{value}'")
    } else if atom_pattern().is_match(&value) {
        value
    } else {
        // Escape because of characters contained
        eco_format!("'{value}'")
    }
}

static PATTERN: OnceLock<Regex> = OnceLock::new();

fn unicode_escape_sequence_pattern() -> &'static Regex {
    PATTERN.get_or_init(|| {
        Regex::new(r#"(\\+)(u)"#).expect("Unicode escape sequence regex cannot be constructed")
    })
}

fn string_inner(value: &str) -> Document<'_> {
    let content = unicode_escape_sequence_pattern()
        // `\\u`-s should not be affected, so that "\\u..." is not converted to
        // "\\x...". That's why capturing groups is used to exclude cases that
        // shouldn't be replaced.
        .replace_all(value, |caps: &Captures<'_>| {
            let slashes = caps.get(1).map_or("", |m| m.as_str());

            if slashes.len().is_multiple_of(2) {
                format!("{slashes}u")
            } else {
                format!("{slashes}x")
            }
        });
    EcoString::from(content).to_doc()
}

fn string(value: &str) -> Document<'_> {
    string_inner(value).surround("<<\"", "\"/utf8>>")
}

fn string_length_utf8_bytes(str: &EcoString) -> usize {
    convert_string_escape_chars(str).len()
}

fn tuple<'a>(elements: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    join(elements, break_(",", ", "))
        .nest(INDENT)
        .surround("{", "}")
        .group()
}

fn const_string_concatenate_bit_array<'a>(
    elements: impl IntoIterator<Item = Document<'a>>,
) -> Document<'a> {
    join(elements, break_(",", ", "))
        .nest(INDENT)
        .surround("<<", ">>")
        .group()
}

fn bit_array<'a>(elements: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    join(elements, break_(",", ", "))
        .nest(INDENT)
        .surround("<<", ">>")
        .group()
}

enum ExpressionSegmentStringEncoding {
    Utf8,
    Utf16 { endiannes: Endianness },
    Utf32 { endiannes: Endianness },
}

fn expression_segment_string_encoding(
    segment: &TypedExprBitArraySegment,
) -> Option<ExpressionSegmentStringEncoding> {
    let endiannes = segment.endianness();
    segment.options.iter().find_map(|option| match option {
        BitArrayOption::Utf8 { .. } => Some(ExpressionSegmentStringEncoding::Utf8),
        BitArrayOption::Utf16 { .. } => Some(ExpressionSegmentStringEncoding::Utf16 { endiannes }),
        BitArrayOption::Utf32 { .. } => Some(ExpressionSegmentStringEncoding::Utf32 { endiannes }),

        BitArrayOption::Bytes { .. }
        | BitArrayOption::Int { .. }
        | BitArrayOption::Float { .. }
        | BitArrayOption::Bits { .. }
        | BitArrayOption::Utf8Codepoint { .. }
        | BitArrayOption::Utf16Codepoint { .. }
        | BitArrayOption::Utf32Codepoint { .. }
        | BitArrayOption::Signed { .. }
        | BitArrayOption::Unsigned { .. }
        | BitArrayOption::Big { .. }
        | BitArrayOption::Little { .. }
        | BitArrayOption::Native { .. }
        | BitArrayOption::Size { .. }
        | BitArrayOption::Unit { .. } => None,
    })
}

fn bit_array_segment<
    'a,
    Value: 'a,
    CreateDoc,
    SizeToDoc,
    UnitToDoc,
    State,
    Output,
    Gen: Eaf<Output>,
>(
    eaf: &mut Gen,
    mut create_document: CreateDoc,
    options: &'a [BitArrayOption<Value>],
    mut size_to_doc: SizeToDoc,
    mut unit_to_doc: UnitToDoc,
    value_is_a_string_literal: bool,
    value_is_a_discard: bool,
    state: &mut State,
) -> Document<'a>
where
    CreateDoc: FnMut(&'_ mut Gen, &mut State) -> Document<'a>,
    SizeToDoc: FnMut(&'_ mut Gen, &'a Value, &mut State) -> Option<Document<'a>>,
    UnitToDoc: FnMut(&'_ mut Gen, &'a u8) -> Option<Document<'a>>,
{
    let mut size: Option<Document<'a>> = None;
    let mut unit: Option<Document<'a>> = None;
    let mut others = Vec::new();

    // Erlang only allows valid codepoint integers to be used as values for utf segments
    // We want to support <<string_var:utf8>> for all string variables, but <<StringVar/utf8>> is invalid
    // To work around this we use the binary type specifier for these segments instead
    let override_type = if !value_is_a_string_literal && !value_is_a_discard {
        Some("binary")
    } else {
        None
    };

    for option in options {
        use BitArrayOption as Opt;
        if !others.is_empty() && !matches!(option, Opt::Size { .. } | Opt::Unit { .. }) {
            others.push("-".to_doc());
        }
        match option {
            Opt::Utf8 { .. } => others.push(override_type.unwrap_or("utf8").to_doc()),
            Opt::Utf16 { .. } => others.push(override_type.unwrap_or("utf16").to_doc()),
            Opt::Utf32 { .. } => others.push(override_type.unwrap_or("utf32").to_doc()),
            Opt::Int { .. } => others.push("integer".to_doc()),
            Opt::Float { .. } => others.push("float".to_doc()),
            Opt::Bytes { .. } => others.push("binary".to_doc()),
            Opt::Bits { .. } => others.push("bitstring".to_doc()),
            Opt::Utf8Codepoint { .. } => others.push("utf8".to_doc()),
            Opt::Utf16Codepoint { .. } => others.push("utf16".to_doc()),
            Opt::Utf32Codepoint { .. } => others.push("utf32".to_doc()),
            Opt::Signed { .. } => others.push("signed".to_doc()),
            Opt::Unsigned { .. } => others.push("unsigned".to_doc()),
            Opt::Big { .. } => others.push("big".to_doc()),
            Opt::Little { .. } => others.push("little".to_doc()),
            Opt::Native { .. } => others.push("native".to_doc()),
            Opt::Size { value, .. } => size = size_to_doc(eaf, value, state),
            Opt::Unit { value, .. } => unit = unit_to_doc(eaf, value),
        }
    }

    let mut document = create_document(eaf, state);

    document = document.append(size);
    let others_is_empty = others.is_empty();

    if !others_is_empty {
        document = document.append("/").append(others);
    }

    if unit.is_some() {
        if !others_is_empty {
            document = document.append("-").append(unit)
        } else {
            document = document.append("/").append(unit)
        }
    }

    document
}

fn binop_documents<'a>(left: Document<'a>, op: &'static str, right: Document<'a>) -> Document<'a> {
    left.append(break_("", " "))
        .append(op)
        .group()
        .append(" ")
        .append(right)
}

fn float<'a>(value: &str) -> Document<'a> {
    let mut value = value.replace('_', "");
    if value.ends_with('.') {
        value.push('0')
    }

    match value.split('.').collect_vec().as_slice() {
        ["0", "0"] => "+0.0".to_doc(),
        [before_dot, after_dot] if after_dot.starts_with('e') => {
            eco_format!("{before_dot}.0{after_dot}").to_doc()
        }
        _ => EcoString::from(value).to_doc(),
    }
}

fn int<'a>(value: &str) -> Document<'a> {
    let mut value = value.replace('_', "");
    if value.starts_with("0x") {
        value.replace_range(..2, "16#");
    } else if value.starts_with("0o") {
        value.replace_range(..2, "8#");
    } else if value.starts_with("0b") {
        value.replace_range(..2, "2#");
    }

    EcoString::from(value).to_doc()
}

fn record_constructor_function<'a>(tag: EcoString, arity: usize) -> Document<'a> {
    let chars = incrementing_arguments_list(arity);
    "fun("
        .to_doc()
        .append(chars.clone())
        .append(") -> {")
        .append(atom_string(to_snake_case(&tag)))
        .append(", ")
        .append(chars)
        .append("} end")
}

fn needs_begin_end_wrapping(expression: &TypedExpr) -> bool {
    match expression {
        // Record updates are 1 expression if there's no assignment, multiple otherwise.
        TypedExpr::RecordUpdate {
            updated_record_assigned_name,
            ..
        } => updated_record_assigned_name.is_some(),

        TypedExpr::Pipeline { .. } => true,

        TypedExpr::Int { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::String { .. }
        | TypedExpr::Var { .. }
        | TypedExpr::Fn { .. }
        | TypedExpr::List { .. }
        | TypedExpr::Call { .. }
        | TypedExpr::BinOp { .. }
        | TypedExpr::Case { .. }
        | TypedExpr::RecordAccess { .. }
        | TypedExpr::PositionalAccess { .. }
        | TypedExpr::Block { .. }
        | TypedExpr::ModuleSelect { .. }
        | TypedExpr::Tuple { .. }
        | TypedExpr::TupleIndex { .. }
        | TypedExpr::Todo { .. }
        | TypedExpr::Echo { .. }
        | TypedExpr::Panic { .. }
        | TypedExpr::BitArray { .. }
        | TypedExpr::NegateBool { .. }
        | TypedExpr::NegateInt { .. }
        | TypedExpr::Invalid { .. } => false,
    }
}

#[derive(Debug, Clone, Copy)]
enum AssertExpression {
    Literal,
    Expression,
    Unevaluated,
}

impl AssertExpression {
    fn from_expression(expression: &TypedExpr) -> Self {
        if expression.is_literal() {
            Self::Literal
        } else {
            Self::Expression
        }
    }
}

fn asserted_expression(
    kind: AssertExpression,
    value: Option<Document<'_>>,
    location: SrcSpan,
) -> Document<'_> {
    let kind = match kind {
        AssertExpression::Literal => atom("literal"),
        AssertExpression::Expression => atom("expression"),
        AssertExpression::Unevaluated => atom("unevaluated"),
    };

    let start = location.start.to_doc();
    let end = location.end.to_doc();

    let value_field = if let Some(value) = value {
        docvec!["value => ", value, ",", line()]
    } else {
        nil()
    };

    let fields_doc = docvec![
        "kind => ",
        kind,
        ",",
        line(),
        value_field,
        "start => ",
        start,
        ",",
        line(),
        // `end` is a keyword in Erlang, so we have to quote it
        "'end' => ",
        end,
        line(),
    ];

    "#{".to_doc()
        .append(fields_doc.group().nest(INDENT))
        .append("}")
}

fn module_select_fn<'a, Output>(
    eaf: &mut impl Eaf<Output>,
    type_: Arc<Type>,
    module_name: &'a str,
    label: &'a str,
) -> Document<'a> {
    match crate::type_::collapse_links(type_).as_ref() {
        Type::Fn { arguments, .. } => {
            eaf.function_reference(Some(module_name), label, arguments.len());
            nil()
        }

        Type::Named { .. } | Type::Var { .. } | Type::Tuple { .. } => module_name_atom(module_name)
            .append(":")
            .append(atom(label))
            .append("()"),
    }
}

fn incrementing_arguments_list(arity: usize) -> EcoString {
    let arguments = (0..arity).map(|c| format!("Field@{c}"));
    Itertools::intersperse(arguments, ", ".into())
        .collect::<String>()
        .into()
}

fn variable_name(name: &str) -> EcoString {
    let mut chars = name.chars();
    let first_char = chars.next();
    let first_uppercased = first_char.into_iter().flat_map(char::to_uppercase);
    first_uppercased.chain(chars).collect()
}

/// When rendering a type variable to an erlang type spec we need all type variables with the
/// same id to end up with the same name in the generated erlang.
/// This function converts a usize into base 26 A-Z for this purpose.
fn id_to_type_var(id: u64) -> Document<'static> {
    if id < 26 {
        let mut name = EcoString::from("");
        name.push(char::from_u32((id % 26 + 65) as u32).expect("id_to_type_var 0"));
        return name.to_doc();
    }
    let mut name = vec![];
    let mut last_char = id;
    while last_char >= 26 {
        name.push(char::from_u32((last_char % 26 + 65) as u32).expect("id_to_type_var 1"));
        last_char /= 26;
    }
    name.push(char::from_u32((last_char % 26 + 64) as u32).expect("id_to_type_var 2"));
    name.reverse();
    name.into_iter().collect::<EcoString>().to_doc()
}

/// When rendering a type variable to an erlang type spec we need all type
/// variables with the same id to end up with the same name in the generated
/// Erlang.
/// This function converts a usize into base 26 A-Z for this purpose.
fn id_to_type_var_str(id: u64) -> EcoString {
    if id < 26 {
        let mut name = EcoString::from("");
        name.push(char::from_u32((id % 26 + 65) as u32).expect("id_to_type_var 0"));
        return name;
    }
    let mut name = vec![];
    let mut last_char = id;
    while last_char >= 26 {
        name.push(char::from_u32((last_char % 26 + 65) as u32).expect("id_to_type_var 1"));
        last_char /= 26;
    }
    name.push(char::from_u32((last_char % 26 + 64) as u32).expect("id_to_type_var 2"));
    name.reverse();
    name.into_iter().collect()
}

pub fn is_erlang_reserved_word(name: &str) -> bool {
    match name {
        "!" | "receive" | "bnot" | "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr"
        | "not" | "and" | "or" | "xor" | "orelse" | "andalso" | "when" | "end" | "fun" | "try"
        | "catch" | "after" | "begin" | "let" | "query" | "cond" | "if" | "of" | "case"
        | "maybe" | "else" => true,
        _ => false,
    }
}

// Includes shell_default & user_default which are looked for by the erlang shell
pub fn is_erlang_standard_library_module(name: &str) -> bool {
    match name {
        "array" | "base64" | "beam_lib" | "binary" | "c" | "calendar" | "dets" | "dict"
        | "digraph" | "digraph_utils" | "epp" | "erl_anno" | "erl_eval" | "erl_expand_records"
        | "erl_id_trans" | "erl_internal" | "erl_lint" | "erl_parse" | "erl_pp" | "erl_scan"
        | "erl_tar" | "ets" | "file_sorter" | "filelib" | "filename" | "gb_sets" | "gb_trees"
        | "gen_event" | "gen_fsm" | "gen_server" | "gen_statem" | "io" | "io_lib" | "lists"
        | "log_mf_h" | "maps" | "math" | "ms_transform" | "orddict" | "ordsets" | "pool"
        | "proc_lib" | "proplists" | "qlc" | "queue" | "rand" | "random" | "re" | "sets"
        | "shell" | "shell_default" | "shell_docs" | "slave" | "sofs" | "string" | "supervisor"
        | "supervisor_bridge" | "sys" | "timer" | "unicode" | "uri_string" | "user_default"
        | "win32reg" | "zip" => true,
        _ => false,
    }
}

// Includes the functions that are autogenerated by Erlang itself
pub fn escape_erlang_existing_name(name: &str) -> &str {
    match name {
        "module_info" => "moduleInfo",
        _ => name,
    }
}

/// A TypeVar can either be rendered as an actual type variable such as `A` or `B`,
/// or it can be rendered as `any()` depending on how many usages it has. If it
/// has only 1 usage it is an `any()` type. If it has more than 1 usage it is a
/// type variable. This function gathers usages for this determination.
///
///   Examples:
///     fn(a) -> String       // `a` is `any()`
///     fn() -> Result(a, b)  // `a` and `b` are `any()`
///     fn(a) -> a            // `a` is a type var
fn collect_type_var_usages<'a>(
    mut ids: HashMap<u64, u64>,
    types: impl IntoIterator<Item = &'a Arc<Type>>,
) -> HashMap<u64, u64> {
    for type_ in types {
        type_var_ids(type_, &mut ids);
    }
    ids
}

fn result_type_var_ids(ids: &mut HashMap<u64, u64>, arg_ok: &Type, arg_err: &Type) {
    let mut ok_ids = HashMap::new();
    type_var_ids(arg_ok, &mut ok_ids);

    let mut err_ids = HashMap::new();
    type_var_ids(arg_err, &mut err_ids);

    let mut result_counts = ok_ids;
    for (id, count) in err_ids {
        let _ = result_counts
            .entry(id)
            .and_modify(|current_count| {
                if *current_count < count {
                    *current_count = count;
                }
            })
            .or_insert(count);
    }
    for (id, count) in result_counts {
        let _ = ids
            .entry(id)
            .and_modify(|current_count| {
                *current_count += count;
            })
            .or_insert(count);
    }
}

fn type_var_ids(type_: &Type, ids: &mut HashMap<u64, u64>) {
    match type_ {
        Type::Var { type_ } => match type_.borrow().deref() {
            TypeVar::Generic { id, .. } | TypeVar::Unbound { id, .. } => {
                let count = ids.entry(*id).or_insert(0);
                *count += 1;
            }
            TypeVar::Link { type_ } => type_var_ids(type_, ids),
        },
        Type::Named {
            arguments,
            module,
            name,
            ..
        } => match arguments[..] {
            [ref arg_ok, ref arg_err] if is_prelude_module(module) && name == "Result" => {
                result_type_var_ids(ids, arg_ok, arg_err)
            }
            _ => {
                for argument in arguments {
                    type_var_ids(argument, ids)
                }
            }
        },
        Type::Fn { arguments, return_ } => {
            for argument in arguments {
                type_var_ids(argument, ids)
            }
            type_var_ids(return_, ids);
        }
        Type::Tuple { elements } => {
            for element in elements {
                type_var_ids(element, ids)
            }
        }
    }
}

fn erl_safe_type_name(mut name: EcoString) -> EcoString {
    if matches!(
        name.as_str(),
        "any"
            | "arity"
            | "atom"
            | "binary"
            | "bitstring"
            | "boolean"
            | "byte"
            | "char"
            | "dynamic"
            | "float"
            | "function"
            | "identifier"
            | "integer"
            | "iodata"
            | "iolist"
            | "list"
            | "map"
            | "maybe_improper_list"
            | "mfa"
            | "module"
            | "neg_integer"
            | "nil"
            | "no_return"
            | "node"
            | "non_neg_integer"
            | "none"
            | "nonempty_improper_list"
            | "nonempty_list"
            | "nonempty_string"
            | "number"
            | "pid"
            | "port"
            | "pos_integer"
            | "reference"
            | "string"
            | "term"
            | "timeout"
            | "tuple"
    ) {
        name.push('_');
        name
    } else {
        escape_atom_string(name)
    }
}

#[derive(Debug)]
struct TypeGenerator<'a> {
    /// If this is true, all types that are generic or unbound are going to be
    /// treated as `any()`.
    ///
    var_as_any: bool,
    /// A TypeVar can either be rendered as an actual type variable such as `A`
    /// or `B`, or it can be rendered as `any()` depending on how many times it
    /// is used.
    /// If it is only ever used once, it is an `any()` type.
    /// If it has more than 1 usage it is a regular type variable.
    ///
    /// For example:
    ///
    /// ```gleam
    /// fn(a) -> String       // `a` is turned into `any()`
    /// fn() -> Result(a, b)  // `a` and `b` are turned into `any()`
    /// fn(a) -> a            // `a` is a type var
    /// ```
    ///
    /// If present, this is a map telling us from generic variable id, to number
    /// of times that variable is used.
    ///
    var_usages: Option<&'a HashMap<u64, u64>>,
    current_module: &'a str,
}

impl<'a> TypeGenerator<'a> {
    fn new(current_module: &'a str) -> Self {
        Self {
            current_module,
            var_usages: None,
            var_as_any: false,
        }
    }

    /// Records the how type variables are used in order to correctly print
    /// the type
    pub fn with_var_usages(mut self, var_usages: &'a HashMap<u64, u64>) -> Self {
        self.var_usages = Some(var_usages);
        self
    }

    /// Print any type variable as `any()` rather than a generic type (like `A`,
    /// `B`, ...).
    fn var_as_any(mut self) -> Self {
        self.var_as_any = true;
        self
    }

    pub fn type_<Output>(&self, eaf: &mut impl Eaf<Output>, type_: &Type) {
        match type_ {
            Type::Var { type_ } => self.type_variable(eaf, &type_.borrow()),
            Type::Named {
                name,
                module,
                arguments,
                ..
            } if is_prelude_module(module) => self.prelude_type(eaf, name, arguments),
            Type::Named {
                name,
                module,
                arguments,
                ..
            } => self.named_type(eaf, module, name, arguments),
            Type::Fn { arguments, return_ } => {
                let function_type = eaf.start_function_type();
                for argument in arguments {
                    self.type_(eaf, argument);
                }
                let function_type = eaf.end_function_type_arguments(function_type);
                self.type_(eaf, return_);
                eaf.end_function_type(function_type);
            }
            Type::Tuple { elements } => {
                let tuple = eaf.start_tuple_type();
                for element in elements {
                    self.type_(eaf, element)
                }
                eaf.end_tuple_type(tuple);
            }
        }
    }

    fn type_variable<Output>(&self, eaf: &mut impl Eaf<Output>, type_: &TypeVar) {
        match type_ {
            TypeVar::Link { type_ } => self.type_(eaf, type_),
            TypeVar::Generic { id, .. } | TypeVar::Unbound { id, .. } => {
                if self.var_as_any || self.type_variable_is_used_exactly_once(*id) {
                    let any = eaf.start_named_type("any");
                    eaf.end_named_type(any);
                } else {
                    eaf.type_variable(&id_to_type_var_str(*id))
                }
            }
        }
    }

    /// Given a type variable id, this returns true if the `var_usages` field
    /// is set and the variable is used exactly once.
    ///
    #[must_use]
    fn type_variable_is_used_exactly_once(&self, id: u64) -> bool {
        match self.var_usages {
            Some(usages) => usages.get(&id) == Some(&1),
            None => false,
        }
    }

    fn prelude_type<Output>(
        &self,
        eaf: &mut impl Eaf<Output>,
        name: &str,
        arguments: &[Arc<Type>],
    ) {
        match name {
            "Nil" => eaf.literal_atom_type("nil"),
            "Int" | "UtfCodepoint" => {
                let integer = eaf.start_named_type("integer");
                eaf.end_named_type(integer);
            }
            "String" => {
                let string = eaf.start_named_type("binary");
                eaf.end_named_type(string);
            }
            "Bool" => {
                let boolean = eaf.start_named_type("boolean");
                eaf.end_named_type(boolean);
            }
            "Float" => {
                let float = eaf.start_named_type("float");
                eaf.end_named_type(float);
            }
            "BitArray" => {
                let bitstring = eaf.start_named_type("bitstring");
                eaf.end_named_type(bitstring);
            }
            "List" => {
                let list = eaf.start_named_type("list");
                let list_item = arguments
                    .first()
                    .expect("prelude type list with no argument");
                self.type_(eaf, list_item);
                eaf.end_named_type(list);
            }
            "Result" => {
                let [ok_type, error_type] = arguments else {
                    panic!("result type with no ok and err types")
                };

                let result = eaf.start_union_type();

                let ok = eaf.start_tuple_type();
                eaf.literal_atom_type("ok");
                self.type_(eaf, ok_type);
                eaf.end_tuple_type(ok);

                let error = eaf.start_tuple_type();
                eaf.literal_atom_type("error");
                self.type_(eaf, error_type);
                eaf.end_tuple_type(error);

                eaf.end_union_type(result);
            }

            // Getting here should mean we either forgot a built-in type or there is a
            // compiler error
            name => panic!("{name} is not a prelude type."),
        }
    }

    fn named_type<Output>(
        &self,
        eaf: &mut impl Eaf<Output>,
        module: &str,
        name: &str,
        arguments: &[Arc<Type>],
    ) {
        let name = erl_safe_type_name(to_snake_case(name));
        let type_ = if self.current_module == module {
            eaf.start_named_type(&name)
        } else {
            eaf.start_remote_named_type(&module_name_atom_str(module), &name)
        };

        for argument in arguments {
            self.type_(eaf, argument)
        }

        eaf.end_named_type(type_);
    }
}

#[derive(Debug)]
struct TypePrinter<'a> {
    var_as_any: bool,
    current_module: &'a str,
    var_usages: Option<&'a HashMap<u64, u64>>,
}

impl<'a> TypePrinter<'a> {
    fn new(current_module: &'a str) -> Self {
        Self {
            current_module,
            var_usages: None,
            var_as_any: false,
        }
    }

    pub fn with_var_usages(mut self, var_usages: &'a HashMap<u64, u64>) -> Self {
        self.var_usages = Some(var_usages);
        self
    }

    pub fn print(&self, type_: &Type) -> Document<'static> {
        match type_ {
            Type::Var { type_ } => self.print_var(&type_.borrow()),

            Type::Named {
                name,
                module,
                arguments,
                ..
            } if is_prelude_module(module) => self.print_prelude_type(name, arguments),

            Type::Named {
                name,
                module,
                arguments,
                ..
            } => self.print_type_app(module, name, arguments),

            Type::Fn { arguments, return_ } => self.print_fn(arguments, return_),

            Type::Tuple { elements } => tuple(elements.iter().map(|element| self.print(element))),
        }
    }

    fn print_var(&self, type_: &TypeVar) -> Document<'static> {
        match type_ {
            TypeVar::Generic { .. } | TypeVar::Unbound { .. } if self.var_as_any => {
                "any()".to_doc()
            }
            TypeVar::Generic { id, .. } | TypeVar::Unbound { id, .. } => match &self.var_usages {
                Some(usages) => match usages.get(id) {
                    Some(&0) => nil(),
                    Some(&1) => "any()".to_doc(),
                    _ => id_to_type_var(*id),
                },
                None => id_to_type_var(*id),
            },
            TypeVar::Link { type_ } => self.print(type_),
        }
    }

    fn print_prelude_type(&self, name: &str, arguments: &[Arc<Type>]) -> Document<'static> {
        match name {
            "Nil" => "nil".to_doc(),
            "Int" | "UtfCodepoint" => "integer()".to_doc(),
            "String" => "binary()".to_doc(),
            "Bool" => "boolean()".to_doc(),
            "Float" => "float()".to_doc(),
            "BitArray" => "bitstring()".to_doc(),
            "List" => {
                let arg0 = self.print(arguments.first().expect("print_prelude_type list"));
                "list(".to_doc().append(arg0).append(")")
            }
            "Result" => match arguments {
                [arg_ok, arg_err] => {
                    let ok = tuple(["ok".to_doc(), self.print(arg_ok)]);
                    let error = tuple(["error".to_doc(), self.print(arg_err)]);
                    docvec![ok, break_(" |", " | "), error].nest(INDENT).group()
                }
                _ => panic!("print_prelude_type result expects ok and err"),
            },
            // Getting here should mean we either forgot a built-in type or there is a
            // compiler error
            name => panic!("{name} is not a built-in type."),
        }
    }

    fn print_type_app(
        &self,
        module: &str,
        name: &str,
        arguments: &[Arc<Type>],
    ) -> Document<'static> {
        let arguments = join(
            arguments.iter().map(|argument| self.print(argument)),
            ", ".to_doc(),
        );
        let name = erl_safe_type_name(to_snake_case(name)).to_doc();
        if self.current_module == module {
            docvec![name, "(", arguments, ")"]
        } else {
            docvec![module_name_atom(module), ":", name, "(", arguments, ")"]
        }
    }

    fn print_fn(&self, arguments: &[Arc<Type>], return_: &Type) -> Document<'static> {
        let arguments = join(
            arguments.iter().map(|argument| self.print(argument)),
            ", ".to_doc(),
        );
        let return_ = self.print(return_);
        "fun(("
            .to_doc()
            .append(arguments)
            .append(") -> ")
            .append(return_)
            .append(")")
    }

    /// Print type vars as `any()`.
    fn var_as_any(mut self) -> Self {
        self.var_as_any = true;
        self
    }
}

fn find_private_functions_referenced_in_importable_constants(
    module: &TypedModule,
) -> im::HashSet<EcoString> {
    let mut overridden_publicity = im::HashSet::new();

    for constant in &module.definitions.constants {
        if constant.publicity.is_importable() {
            find_referenced_private_functions(&constant.value, &mut overridden_publicity)
        }
    }
    overridden_publicity
}

fn find_referenced_private_functions(
    constant: &TypedConstant,
    already_found: &mut im::HashSet<EcoString>,
) {
    match constant {
        Constant::Todo { .. } => panic!("todo constants should not reach code generation"),
        Constant::Invalid { .. } => panic!("invalid constants should not reach code generation"),
        Constant::RecordUpdate { .. } => {
            panic!("record updates should not reach code generation")
        }

        Constant::Int { .. }
        | Constant::Float { .. }
        | Constant::String { .. }
        | Constant::BitArray { .. } => (),

        TypedConstant::Var {
            name, constructor, ..
        } => {
            if let Some(ValueConstructor { type_, .. }) = constructor.as_deref()
                && let Type::Fn { .. } = **type_
            {
                let _ = already_found.insert(name.clone());
            }
        }

        TypedConstant::Record { arguments, .. } => arguments
            .iter()
            .flatten()
            .for_each(|argument| find_referenced_private_functions(&argument.value, already_found)),

        TypedConstant::StringConcatenation { left, right, .. } => {
            find_referenced_private_functions(left, already_found);
            find_referenced_private_functions(right, already_found);
        }

        Constant::Tuple { elements, .. } => elements
            .iter()
            .for_each(|element| find_referenced_private_functions(element, already_found)),

        Constant::List { elements, tail, .. } => {
            elements
                .iter()
                .for_each(|element| find_referenced_private_functions(element, already_found));

            if let Some(tail) = tail {
                find_referenced_private_functions(tail, already_found);
            }
        }
    }
}
