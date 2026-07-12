// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2018 The Gleam contributors

mod pattern;
#[cfg(test)]
mod tests;

use crate::build::Target;
use crate::erlang::pattern::{AliasedLiteral, PatternGenerator};
use crate::strings::to_snake_case;
use crate::type_::{self, is_prelude_module};
use crate::{
    ast::*,
    line_numbers::LineNumbers,
    type_::{
        ModuleValueConstructor, PatternConstructor, Type, TypeVar, TypedCallArg, ValueConstructor,
        ValueConstructorVariant,
    },
};
use camino::Utf8Path;
use ecow::{EcoString, eco_format};
use erlang_generation::{
    BitArraySegmentSpecifier, ErlangBuilder, ErlangModuleName, ErlangSourceBuilder,
};
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::Signed;
use regex::Regex;
use std::collections::VecDeque;
use std::sync::OnceLock;
use std::{collections::HashMap, ops::Deref, sync::Arc};

/// This is an open runtime error to which more fields can still be added.
#[must_use]
struct RuntimeError<Map, Call> {
    /// This is the map that is going to be thrown by the `erlang:error` call.
    error_map: Map,
    /// This is the call to `erlang:error` that will throw the error, with the
    /// map as an argument.
    erlang_error_call: Call,
}

/// Represents all the different kind of runtime errors that Gleam can raise.
enum RuntimeErrorKind {
    Todo,
    Panic,
    Assert,
    LetAssert,
}

impl RuntimeErrorKind {
    fn default_error_message(&self) -> &'static str {
        match self {
            RuntimeErrorKind::Panic => "`panic` expression evaluated.",
            RuntimeErrorKind::Assert => "Assertion failed.",
            RuntimeErrorKind::LetAssert => "Pattern match failed, no pattern matched the value.",
            RuntimeErrorKind::Todo => {
                "`todo` expression evaluated. This code has not yet been implemented."
            }
        }
    }
}

enum EchoPrintedValue<'a> {
    /// We're printing the result of a pipeline step.
    PipeStep {
        /// This is the name that was given to the variable holding the value
        /// we have to print.
        name: EcoString,
    },
    /// We're printing any arbitrary expression.
    Expression { value: &'a TypedExpr },
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

    /// The relative source path to the module that's gonna be used in error
    /// messages in the generated Erlang code.
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

    /// This keeps track of the number of generated variables that have already
    /// been generated in the current function.
    /// For example if this is `2` it means we've already generated:
    ///
    /// ```erl
    /// _value
    /// _value@1
    /// _value@2
    /// ```
    ///
    /// We need this to make sure that every time we generate a new generated
    /// variable it has a unique name not shadowing anything else.
    ///
    generated_variables: usize,

    /// This keeps track of all the names that are taken for the current
    /// function and can't be used when defining new variables.
    /// For example if this is `hash_map![("wibble", 2), ("wobble", 1)]`
    /// this means that all of these variables have already been defined
    /// somewhere in the current function:
    ///
    /// ```erl
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

    fn module_document<Output>(&mut self, builder: &mut impl ErlangBuilder<Output>) {
        // We need to know which private functions are referenced in importable
        // constants so that we can export them anyway in the generated Erlang.
        // This is because otherwise when the constant is used in another module it
        // would result in an error as it tries to reference this private function.
        let overridden_publicity =
            find_private_functions_referenced_in_importable_constants(self.module);

        // We add a `-compile` attribute at the top of each module to instruct
        // the Erlang compiler.
        builder.compile_attribute([
            "no_auto_import",
            "nowarn_ignored",
            "nowarn_unused_vars",
            "nowarn_unused_function",
            "nowarn_nomatch",
            "inline",
        ]);

        // We then need to add an `-export` attribute for all the module's
        // public functions.
        builder.export_attribute(
            (self.module.definitions.functions.iter())
                .filter_map(|function| function_export(function, &overridden_publicity)),
        );
        // We do the same but with types.
        builder.export_type_attribute(self.module.definitions.custom_types.iter().map(type_export));

        // We also add a `-module_doc` comment at the beginning of the module
        // with its documentation.
        self.module_documentation(builder);

        // Then we generate `-type` definitions for the module's types.
        for custom_type in &self.module.definitions.custom_types {
            self.type_definition(builder, custom_type);
        }

        // And finally generate all the functions that the module defined.
        for function in &self.module.definitions.functions {
            FunctionGenerator::new(function, self).module_function(builder, function);
        }
    }

    fn module_documentation<Output>(&mut self, builder: &mut impl ErlangBuilder<Output>) {
        if self.module.type_info.is_internal {
            // The module is internal so we need to add a `-moduledoc(false).`
            // attribute to make sure its documentation is hidden.
            let doc = builder.start_moduledoc_attribute();
            builder.atom("false");
            builder.end_doc_attribute(doc);
        } else if self.module.documentation.is_empty() {
            // The module is not internal, but it has no docs.
            // We don't have to do anything.
        } else {
            // The module has some documentation that we're going to include
            // with a `-moduledoc` attribute.
            let doc = builder.start_moduledoc_attribute();
            let documentation = &self.module.documentation.iter().join("\n");
            builder.string(documentation);
            builder.end_doc_attribute(doc);
        }
    }

    fn type_definition<Output>(
        &self,
        builder: &mut impl ErlangBuilder<Output>,
        custom_type: &TypedCustomType,
    ) {
        let TypedCustomType {
            name,
            constructors,
            opaque,
            typed_parameters,
            external_erlang,
            ..
        } = custom_type;

        let name = erl_safe_type_name(to_snake_case(name));

        // We start the type spec.
        builder.type_spec(
            *opaque,
            &name,
            typed_parameters
                .iter()
                .map(|type_| type_parameter_name(type_)),
        );

        // Now we need to generate the type definition.
        // Erlang doesn't allow to have phantom type variables, so if there's
        // any type variable that is not used we will need to add one variant to
        // the resulting type that is using all those phantom variables to avoid
        // errors!
        let phantom_type_variables = phantom_type_variables(custom_type);
        let has_phantom_type_variables = !phantom_type_variables.is_empty();
        match (constructors.as_slice(), has_phantom_type_variables) {
            // This is an external type with an annotation telling us what type
            // it corresponds to in Erlang.
            // In that case all type variables are phantom type variables!
            ([], _) if let Some((module, type_name, _)) = external_erlang => {
                let type_ =
                    builder.start_remote_named_type(ErlangModuleName::new(module), type_name);
                for type_variable in phantom_type_variables {
                    builder.type_variable(&type_variable);
                }
                builder.end_named_type(type_);
            }
            // This is an external type with no external annotation and no
            // phantom type variables. It is just `any()`.
            ([], false) => {
                let any = builder.start_named_type("any");
                builder.end_named_type(any);
            }
            // This is an external type with no external annotation and some
            // phantom type variables, we need to add an alternative to use
            // them: `any() | {gleam_phantom, A, B, ...}`
            ([], true) => {
                let union = builder.start_union_type();
                let any = builder.start_named_type("any");
                builder.end_named_type(any);
                self.phantom_type(builder, phantom_type_variables);
                builder.end_union_type(union);
            }
            // This is an external type with a single constructor, no need to
            // make it a union.
            ([constructor], false) => self.constructor_type(builder, constructor),
            // This is an external type with multiple constructors, we have to
            // turn it into a union!
            (constructors, has_phantom_type_variables) => {
                let union = builder.start_union_type();
                for constructor in constructors {
                    self.constructor_type(builder, constructor);
                }
                if has_phantom_type_variables {
                    self.phantom_type(builder, phantom_type_variables);
                }
                builder.end_union_type(union);
            }
        }
    }

    /// Given a constructor this generates its type. For example:
    ///
    /// ```gleam
    /// Wibble(Int, String)
    /// ```
    ///
    /// Would be turned into:
    ///
    /// ```erl
    /// {wibble, integer(), binary()}.
    /// ```
    ///
    fn constructor_type<Output>(
        &self,
        builder: &mut impl ErlangBuilder<Output>,
        constructor: &RecordConstructor<Arc<Type>>,
    ) {
        let constructor_atom = to_snake_case(&constructor.name);
        if constructor.arguments.is_empty() {
            // A constructor with no fields becomes a regular atom on the Erlang
            // target.
            builder.literal_atom_type(&constructor_atom);
        } else {
            // Othwerwise, it is a tuple tagged with the atom with the
            // constructor name.
            let generator = TypeGenerator::new(&self.module.name);
            let tuple = builder.start_tuple_type();
            builder.literal_atom_type(&constructor_atom);
            for argument in &constructor.arguments {
                generator.type_(builder, &argument.type_);
            }
            builder.end_tuple_type(tuple);
        }
    }

    /// Given a list of phantom type variabes, this generates a type using all
    /// of those.
    ///
    /// Erlang doesn't allow having phantom type variables in type annotations,
    /// so whenever there's any we need to manually add a type that uses them to
    /// make sure we get no errors. For example:
    ///
    /// ```gleam
    /// pub type Wibble(a, phantom) {
    ///   Wibble(a)
    /// }
    /// ```
    ///
    /// Will have to be turned into:
    ///
    /// ```erl
    /// -type wibble(A) :: {wibble, A} | {gleam_phantom, Phantom}.
    /// ```
    ///
    /// So the phantom type is nothing more than a tuple tagged with
    /// `gleam_phantom`.
    ///
    fn phantom_type<Output>(
        &self,
        builder: &mut impl ErlangBuilder<Output>,
        phantom_type_variables: Vec<EcoString>,
    ) {
        let phantom_tuple = builder.start_tuple_type();
        builder.literal_atom_type("gleam_phantom");
        for phantom_type_variable in phantom_type_variables {
            builder.type_variable(&phantom_type_variable);
        }
        builder.end_tuple_type(phantom_tuple);
    }
}

/// Given a custom type, this will return a vector with the names of all the
/// phantom type varaibles that it has. The names returned are the names we can
/// use in Erlang!
fn phantom_type_variables(custom_type: &CustomType<Arc<Type>>) -> Vec<EcoString> {
    // We first find all the variables that appear in the type definition
    // itself: any of those that isn't used by any of the constructors is going
    // to be a phantom type variable.
    let mut definition_type_variables =
        collect_type_var_usages(HashMap::new(), custom_type.typed_parameters.iter());

    // So we need to gather all the type variables referenced by all the
    // constructors.
    let mut constructors_type_variables = HashMap::new();
    for constructor in &custom_type.constructors {
        constructors_type_variables = collect_type_var_usages(
            constructors_type_variables,
            constructor.arguments.iter().map(|argument| &argument.type_),
        );
    }

    // The phantom ones are the ones in the definition that are not referenced
    // by any constructor:
    for used_type_variable in constructors_type_variables.keys() {
        let _ = definition_type_variables.remove(used_type_variable);
    }

    definition_type_variables
        .into_keys()
        .map(id_to_type_var_str)
        .sorted()
        .collect_vec()
}

/// Given a custom type's type parameter (that is expected to be generic or
/// unbound), this will return the name the corresponding type variable should
/// have in the generated erlang code.
///
/// If the type passed is not generic this will panic!
fn type_parameter_name(type_: &Type) -> EcoString {
    let Type::Var { type_ } = type_ else {
        panic!("non generic type as type parameter")
    };
    match &*type_.borrow() {
        TypeVar::Unbound { id } | TypeVar::Generic { id } => id_to_type_var_str(*id),
        TypeVar::Link { type_ } => type_parameter_name(type_),
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
            generated_variables: 0,
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
    fn new_generated_variable(&mut self) -> EcoString {
        let name = if self.generated_variables == 0 {
            EcoString::from("_value")
        } else {
            eco_format!("_value@{}", self.generated_variables)
        };
        self.generated_variables += 1;
        name
    }

    /// Generates code for an Erlang module function. This might return None
    /// if there's no code to be generated at all!
    /// For example if the function is unused, or if the function is a private
    /// Erlang external (in which case, it would be inlined instead).
    fn module_function<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        function: &'a TypedFunction,
    ) {
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
        builder.file_attribute(
            &self.module_generator.module_source_path,
            self.module_generator
                .line_numbers
                .line_number(function.location.start),
        );
        self.function_spec_attribute(builder, &function_name, function);
        self.function_doc_attribute(builder, function);

        // Finally we start generating code for the function itself, how we do
        // it depends if the function is external or not.
        let arity = function.arguments.len();
        match function.external_erlang.as_ref() {
            // If the function is not external we generate the code for all of
            // its statements.
            None => {
                let arguments = self.function_arguments_names(&function.arguments, false);
                let open_function = builder.start_function(&function_name, arity, arguments);
                self.statement_sequence(builder, &function.body);
                builder.end_function(open_function);
            }

            // An external function consists of just a remote call being
            // passed all of the function's arguments.
            Some((module, external_function_name, _location)) => {
                let arguments = self
                    .function_arguments_names(&function.arguments, true)
                    .collect_vec();
                let open_function =
                    builder.start_function(&function_name, arity, arguments.clone());
                let call = builder
                    .start_remote_call(ErlangModuleName::new(module), external_function_name);
                for argument in arguments {
                    builder.variable(&argument);
                }
                builder.end_call(call);
                builder.end_function(open_function);
            }
        }
    }

    /// This generates the `-spec` attribute for a function with the given name.
    ///
    fn function_spec_attribute<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
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
        let spec = builder.start_function_spec(function_name, function.arguments.len());
        let function_type = builder.start_function_type();
        for argument in &function.arguments {
            generator.type_(builder, &argument.type_);
        }
        let function_type = builder.end_function_type_arguments(function_type);
        generator.type_(builder, &function.return_type);
        builder.end_function_type(function_type);
        builder.end_function_spec(spec);
    }

    fn function_doc_attribute<Output>(
        &self,
        builder: &mut impl ErlangBuilder<Output>,
        function: &TypedFunction,
    ) {
        // If a function is marked as internal or comes from an internal module
        // we want to hide its documentation in the Erlang shell!
        // So the doc directive will look like this: `-doc(false).`
        let is_internal =
            self.module_generator.module.type_info.is_internal || function.publicity.is_internal();

        if is_internal {
            let attribute = builder.start_doc_attribute();
            builder.atom("false");
            builder.end_doc_attribute(attribute);
        } else if let Some((_, documentation)) = &function.documentation
            && !documentation.is_empty()
        {
            let attribute = builder.start_doc_attribute();
            builder.string(documentation);
            builder.end_doc_attribute(attribute);
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
            // So in this case we use a generated name to make sure the external
            // function can be called correctly.
            ArgNames::Discard { name, location }
            | ArgNames::LabelledDiscard {
                name,
                name_location: location,
                ..
            } if is_external => {
                if name.chars().all(|char| char == '_') {
                    self.new_generated_variable()
                } else {
                    self.new_erlang_variable(name, *location)
                }
            }
            ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => EcoString::from("_"),
            ArgNames::Named { name, location }
            | ArgNames::NamedLabelled {
                name,
                name_location: location,
                ..
            } => self.new_erlang_variable(name, *location),
        })
    }

    fn statement_sequence<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        statements: &'a [TypedStatement],
    ) {
        // We go over each statement one by one and produce the code they need.
        for i in 0..statements.len() {
            match statements.get(i).expect("statement in range") {
                Statement::Expression(expression) => self.expression(builder, expression),
                Statement::Use(use_) => self.expression(builder, &use_.call),
                Statement::Assert(assert) => self.assert(builder, assert),
                Statement::Assignment(assignment) => match &assignment.kind {
                    AssignmentKind::Let | AssignmentKind::Generated => {
                        self.let_(builder, &assignment.value, &assignment.pattern);
                    }
                    // Let asserts are slightly different from everything else:
                    // A let assert is compiled to a case expression where we
                    // have two branches:
                    //
                    // ```gleam
                    // let assert [a, b] = some_list
                    // // ... the remaining statements
                    // ```
                    //
                    // It will turn into something that looks like this:
                    //
                    // ```erl
                    // case SomeList of
                    //   [a, b] ->
                    //     % ... the remaining statements;
                    //   _ ->
                    //    erlang:error(...)
                    // end.
                    // ```
                    //
                    // So in case we find a let assert we need to break out of
                    // this cycle and pass it all the remaining statements so
                    // that it can put those under the correct branch of the
                    // case expression it's going to produce.
                    AssignmentKind::Assert {
                        message, location, ..
                    } => {
                        return self.let_assert(
                            builder,
                            &assignment.value,
                            &assignment.pattern,
                            message.as_ref(),
                            *location,
                            statements.get(i + 1..).unwrap_or_default(),
                        );
                    }
                },
            }
        }
    }

    fn expression<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        expression: &'a TypedExpr,
    ) {
        match expression {
            //
            // Simple scalar values, and blocks.
            //
            TypedExpr::Int { int_value, .. } => builder.int(int_value.clone()),
            TypedExpr::Float { float_value, .. } => builder.float(float_value.value()),
            TypedExpr::String { value, .. } => builder.string(value),
            TypedExpr::Var {
                name, constructor, ..
            } => self.var(builder, name, constructor),
            TypedExpr::Block { statements, .. } => {
                // If the block has a single expression we don't bother wrapping
                // it in an additional `begin ... end` block.
                // It's going to be added only if strictly needed.
                if statements.len() == 1
                    && let Statement::Expression(expression) = statements.first()
                {
                    self.maybe_block_expr(builder, expression);
                } else {
                    let block = builder.start_block();
                    self.statement_sequence(builder, statements);
                    builder.end_block(block);
                }
            }

            //
            // Operators.
            //
            TypedExpr::NegateBool { value, .. } => {
                builder.unary_operator("not");
                self.maybe_block_expr(builder, value);
            }
            TypedExpr::NegateInt { value, .. } => {
                builder.unary_operator("-");
                self.maybe_block_expr(builder, value);
            }
            TypedExpr::BinOp {
                operator,
                left,
                right,
                ..
            } => self.binary_operator(builder, operator, left, right),

            //
            // BitArrays, Lists, and Tuples.
            //
            TypedExpr::BitArray { segments, .. } => {
                let bit_array = builder.start_bit_array();
                for segment in segments {
                    self.bit_array_expression_segment(builder, segment);
                }
                builder.end_bit_array(bit_array);
            }
            TypedExpr::List { elements, tail, .. } => {
                // We generate all the items of the list as cons cells.
                for element in elements {
                    builder.cons_list();
                    self.maybe_block_expr(builder, element);
                }
                // Finally we close the list with the tail, or an empty list
                // (so that we're sure we're building proper Erlang lists).
                if let Some(tail) = tail {
                    self.maybe_block_expr(builder, tail);
                } else {
                    builder.empty_list();
                }
            }
            TypedExpr::Tuple { elements, .. } => {
                let tuple = builder.start_tuple();
                for element in elements {
                    self.maybe_block_expr(builder, element);
                }
                builder.end_tuple(tuple);
            }

            //
            // Accessing data inside tuples, and records.
            // They're all tuple accesses at the end of the day!
            //
            TypedExpr::TupleIndex { tuple, index, .. } => self.tuple_index(builder, tuple, *index),
            TypedExpr::RecordAccess { record, index, .. }
            | TypedExpr::PositionalAccess { record, index, .. } => {
                self.tuple_index(builder, record, index + 1);
            }

            //
            // Records and record updates.
            //
            TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Record { name, arity: 0, .. },
                ..
            } => builder.atom(&to_snake_case(name)),
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
                    builder.match_operator();
                    builder.variable_pattern(
                        &self.new_erlang_variable(name, updated_record.location()),
                    );
                    self.maybe_block_expr(builder, updated_record);
                }
                // Then a record update is simply a call!
                self.call(builder, constructor, arguments);
            }

            //
            // All kinds of anonymous functions.
            //
            TypedExpr::Fn {
                arguments, body, ..
            } => {
                let outer_scope = self.taken_names.clone();
                let argument_names = self.function_arguments_names(arguments, false);
                let function = builder.start_anonymous_function(argument_names);
                self.statement_sequence(builder, body);
                builder.end_function(function);
                self.taken_names = outer_scope;
            }
            TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Record { name, arity, .. },
                ..
            } => self.record_builder_anonymous_function(builder, name, *arity as usize),
            TypedExpr::ModuleSelect {
                type_,
                constructor:
                    ModuleValueConstructor::Fn {
                        external_erlang: Some((module, name)),
                        ..
                    }
                    | ModuleValueConstructor::Fn { module, name, .. },
                ..
            } => match type_::collapse_links(type_.clone()).as_ref() {
                Type::Fn { arguments, .. } => builder.function_reference(
                    Some(ErlangModuleName::new(module)),
                    escape_erlang_existing_name(name),
                    arguments.len(),
                ),

                Type::Named { .. } | Type::Var { .. } | Type::Tuple { .. } => {
                    let name = escape_erlang_existing_name(name);
                    let call = builder.start_remote_call(ErlangModuleName::new(module), name);
                    builder.end_call(call);
                }
            },

            //
            // Calling functions.
            //
            TypedExpr::Call { fun, arguments, .. } => self.call(builder, fun, arguments),
            TypedExpr::Pipeline {
                first_value,
                assignments,
                finally,
                ..
            } => self.pipeline(builder, first_value, assignments, finally),

            //
            // Todo, panic, and echo.
            //
            TypedExpr::Todo {
                message, location, ..
            } => {
                let error = self.start_runtime_error(
                    builder,
                    RuntimeErrorKind::Todo,
                    *location,
                    message.as_deref(),
                );
                self.end_runtime_error(builder, error);
            }
            TypedExpr::Panic {
                location, message, ..
            } => {
                let error = self.start_runtime_error(
                    builder,
                    RuntimeErrorKind::Panic,
                    *location,
                    message.as_deref(),
                );
                self.end_runtime_error(builder, error);
            }
            TypedExpr::Echo {
                expression,
                location,
                message,
                ..
            } => self.echo(
                builder,
                *location,
                message.as_deref(),
                EchoPrintedValue::Expression {
                    value: expression
                        .as_ref()
                        .expect("echo with no expression outside of pipe"),
                },
            ),

            //
            // Module constants.
            //
            TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Constant { literal, .. },
                ..
            } => self.inlined_constant(builder, literal),

            //
            // Control flow.
            //
            TypedExpr::Case {
                subjects, clauses, ..
            } => self.case(builder, subjects, clauses),

            //
            // Something went wrong!
            //
            TypedExpr::Invalid { .. } => {
                panic!("invalid expressions should not reach code generation")
            }
        }
    }

    fn echo<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        echo_location: SrcSpan,
        message: Option<&'a TypedExpr>,
        printed_value: EchoPrintedValue<'a>,
    ) {
        self.module_generator.echo_used = true;

        let call = builder.start_call();
        builder.atom("echo");

        // Echo has 4 arguments: the expression to print...
        match printed_value {
            EchoPrintedValue::PipeStep { name } => builder.variable(&name),
            EchoPrintedValue::Expression { value } => self.maybe_block_expr(builder, value),
        }
        // ...the message to print (or nil if there's no message)...
        if let Some(message) = message {
            self.maybe_block_expr(builder, message);
        } else {
            builder.atom("nil");
        }

        // ...the filepath of this module...
        builder.string(&self.module_generator.module_source_path);

        // ...and the line number of the expression.
        builder.int(
            self.module_generator
                .line_numbers
                .line_number(echo_location.start)
                .into(),
        );

        builder.end_call(call);
    }

    /// This starts a call to `erlang:error` with a map representing a Gleam
    /// runtime error of the given kind.
    /// Some fields are mandatory and always added, but if you need to add more
    /// fields you can still do so by calling `builder.map_field()`.
    ///
    /// After you're done generating those additional fields remember you _must_
    /// call `end_runtime_error` before generating any other piece of code!
    ///
    fn start_runtime_error<Output, Builder: ErlangBuilder<Output>>(
        &mut self,
        builder: &mut Builder,
        error_kind: RuntimeErrorKind,
        location: SrcSpan,
        message: Option<&'a TypedExpr>,
    ) -> RuntimeError<Builder::Map, Builder::Call> {
        let call = builder.start_remote_call(ErlangModuleName::erlang(), "error");
        let map = builder.start_map();

        builder.map_field();
        builder.atom("gleam_error");
        builder.atom(match error_kind {
            RuntimeErrorKind::Todo => "todo",
            RuntimeErrorKind::Panic => "panic",
            RuntimeErrorKind::Assert => "assert",
            RuntimeErrorKind::LetAssert => "let_assert",
        });

        builder.map_field();
        builder.atom("message");
        if let Some(message) = message {
            self.maybe_block_expr(builder, message);
        } else {
            builder.string(error_kind.default_error_message());
        }

        builder.map_field();
        builder.atom("file");
        builder.string(&self.module_generator.module_source_path);

        builder.map_field();
        builder.atom("module");
        builder.string(&self.module_generator.module.name);

        builder.map_field();
        builder.atom("function");
        builder.string(self.function_name);

        builder.map_field();
        builder.atom("line");
        builder.int(
            self.module_generator
                .line_numbers
                .line_number(location.start)
                .into(),
        );

        RuntimeError {
            error_map: map,
            erlang_error_call: call,
        }
    }

    /// This closes an open runtime error.
    fn end_runtime_error<Output, Builder: ErlangBuilder<Output>>(
        &self,
        builder: &mut Builder,
        runtime_error: RuntimeError<Builder::Map, Builder::Call>,
    ) {
        builder.end_map(runtime_error.error_map);
        builder.end_call(runtime_error.erlang_error_call);
    }

    fn maybe_block_expr<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        expression: &'a TypedExpr,
    ) {
        if needs_begin_end_wrapping(expression) {
            let block = builder.start_block();
            self.expression(builder, expression);
            builder.end_block(block);
        } else {
            self.expression(builder, expression);
        }
    }

    fn let_<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        value: &'a TypedExpr,
        pattern: &'a TypedPattern,
    ) {
        builder.match_operator();
        PatternGenerator::new(self).pattern(builder, pattern);
        self.maybe_block_expr(builder, value);
    }

    fn let_assert<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        value: &'a TypedExpr,
        pattern: &'a TypedPattern,
        message: Option<&'a TypedExpr>,
        location: SrcSpan,
        following_statements: &'a [TypedStatement],
    ) {
        // If the pattern will never fail, like a tuple or a simple variable, we
        // simply treat it as if it were a `let` assignment.
        if pattern.always_matches() {
            self.let_(builder, value, pattern);
            self.statement_sequence(builder, following_statements);
            return;
        }

        // Otherwise we turn the let assert into a case expression with two
        // branches: one for the asserted pattern, and one catch all to throw an
        // exception in case the pattern doesn't match.
        let case = builder.start_case();
        self.maybe_block_expr(builder, value);

        // This is the first branch for when the asserted pattern matches: it's
        // going to run all the remaining statements in its body.
        if !following_statements.is_empty() {
            // If there's statements after this let assert we want to generate
            // them.
            let clause = builder.start_case_clause();
            let mut generator = PatternGenerator::new(self);
            generator.pattern(builder, pattern);
            let clause = builder.end_clause_pattern(clause);
            let clause = builder.end_clause_guards(clause);
            let variables_to_add_later = generator.variables_to_add_later;
            self.pattern_assignments(builder, variables_to_add_later);
            self.statement_sequence(builder, following_statements);
            builder.end_clause_body(clause);
        } else {
            // If there's no statements following the let assert, that means
            // that it's the last statement in the block and we need to return
            // the value being matched on.
            // It will look something like this:
            //
            // ```erl
            // case MatchedValue of
            //   [_, A | _] = _value -> _value;
            // %              ^^^^^^ We bind the pattern to a variable
            // %                     and return it.
            //   _ -> erlang:error(...)
            // end
            // ```
            let clause = builder.start_case_clause();
            let matched_value_name = self.new_generated_variable();
            builder.match_pattern();
            let mut generator = PatternGenerator::new(self);
            generator.pattern(builder, pattern);
            builder.variable_pattern(&matched_value_name);

            let clause = builder.end_clause_pattern(clause);
            let clause = builder.end_clause_guards(clause);
            builder.variable(&matched_value_name);
            builder.end_clause_body(clause);
        }

        // This is the catch all branch to throw an error otherwise.
        let clause = builder.start_case_clause();
        let value_name = self.new_generated_variable();
        builder.variable_pattern(&value_name);
        let clause = builder.end_clause_pattern(clause);
        let clause = builder.end_clause_guards(clause);
        let error =
            self.start_runtime_error(builder, RuntimeErrorKind::LetAssert, location, message);

        // We want to add some additional fields to the error map:
        builder.map_field();
        builder.atom("value");
        builder.variable(&value_name);

        builder.map_field();
        builder.atom("start");
        builder.int(location.start.into());

        builder.map_field();
        builder.atom("end");
        builder.int(value.location().end.into());

        builder.map_field();
        builder.atom("pattern_start");
        builder.int(pattern.location().start.into());

        builder.map_field();
        builder.atom("pattern_end");
        builder.int(pattern.location().end.into());

        self.end_runtime_error(builder, error);
        builder.end_clause_body(clause);

        builder.end_case(case);
    }

    fn pipeline<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        first_value: &'a TypedPipelineAssignment,
        assignments: &'a [(TypedPipelineAssignment, PipelineAssignmentKind)],
        finally: &'a TypedExpr,
    ) {
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
        let mut previous_step_variable_name: Option<EcoString> = None;
        for assignment in std::iter::once(first_value)
            .chain(assignments.iter().map(|(assignment, _kind)| assignment))
        {
            // A pipeline step always ends up assigned to a variable.
            // So we start by generating `_pipe = ...`, followed by the
            // expression.
            builder.match_operator();
            let name = self.new_erlang_variable(&assignment.name, assignment.location);
            builder.variable_pattern(&name);

            // In case of a pipe we need to manually pass the previous step to
            // echo as an argument.
            if let TypedExpr::Echo {
                expression: None,
                message,
                location,
                ..
            } = assignment.value.as_ref()
            {
                self.echo(
                    builder,
                    *location,
                    message.as_deref(),
                    EchoPrintedValue::PipeStep {
                        name: previous_step_variable_name
                            .to_owned()
                            .expect("echo with no previous step in a pipe"),
                    },
                );
            } else {
                self.maybe_block_expr(builder, &assignment.value);
                previous_step_variable_name = Some(name);
            }
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
            self.echo(
                builder,
                *location,
                message.as_deref(),
                EchoPrintedValue::PipeStep {
                    name: previous_step_variable_name
                        .expect("echo with no previous step in a pipe"),
                },
            );
        } else {
            self.expression(builder, finally);
        }
    }

    fn assert<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        assert: &'a TypedAssert,
    ) {
        let Assert {
            value,
            location,
            message,
        } = assert;

        match value {
            // We're asserting on a binary operator. We want to show the result
            // of each side in the error that is produced.
            // So we will bind the two sides of the operator to variables and
            // shove them in the error map too!
            TypedExpr::BinOp {
                operator,
                left,
                right,
                ..
            } => {
                let erlang_operator = match operator {
                    // Writing asserts on binops requires some extra care, check
                    // out their docs!
                    BinOp::And => {
                        return self.assert_and(builder, left, right, message.as_ref(), *location);
                    }
                    BinOp::Or => {
                        return self.assert_or(builder, left, right, message.as_ref(), *location);
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

                // If the left or right hand side are not simple variables we'll
                // need to first assign those to generated variables and keep
                // track of those names.
                let left = if !left.is_var() {
                    let name = self.new_generated_variable();
                    builder.match_operator();
                    builder.variable_pattern(&name);
                    self.maybe_block_expr(builder, left);
                    AssertionExpression::from_generated_variable(name, left)
                } else {
                    AssertionExpression::from_expression(left)
                };

                let right = if !right.is_var() {
                    let name = self.new_generated_variable();
                    builder.match_operator();
                    builder.variable_pattern(&name);
                    self.maybe_block_expr(builder, right);
                    AssertionExpression::from_generated_variable(name, right)
                } else {
                    AssertionExpression::from_expression(right)
                };

                let case = builder.start_case();

                // Then we need to apply the operator. If any of the two sides
                // has been bound to a variable we can use that name directly!
                builder.binary_operator(erlang_operator);
                self.runtime_value(builder, &left);
                self.runtime_value(builder, &right);

                // If the operator evaluates to true the assertion succeeded.
                // We can just return nil.
                let clause = builder.start_case_clause();
                builder.atom_pattern("true");
                let clause = builder.end_clause_pattern(clause);
                let clause = builder.end_clause_guards(clause);
                builder.atom("nil");
                builder.end_clause_body(clause);

                // Otherwise we want to throw a runtime error!
                let clause = builder.start_case_clause();
                builder.atom_pattern("false");
                let clause = builder.end_clause_pattern(clause);
                let clause = builder.end_clause_guards(clause);
                self.assert_binary_operator_error(
                    builder,
                    *operator,
                    left,
                    right,
                    message.as_ref(),
                    *location,
                );
                builder.end_clause_body(clause);

                builder.end_case(case);
            }

            TypedExpr::Call { fun, arguments, .. } => {
                // When asserting on a call, we want to include the values of
                // each argument in the assertion error in case of failure.
                // This means we first have to evaluate each argument and bind
                // it to a variable so that we can later reference them from the
                // error message without evaluating each argument twice!
                let mut call_arguments = Vec::with_capacity(arguments.len());
                for argument in arguments {
                    let argument = if !argument.value.is_var() {
                        let name = self.new_generated_variable();
                        builder.match_operator();
                        builder.variable_pattern(&name);
                        self.maybe_block_expr(builder, &argument.value);
                        AssertionExpression::from_generated_variable(name, &argument.value)
                    } else {
                        AssertionExpression::from_expression(&argument.value)
                    };
                    call_arguments.push(argument);
                }

                let case = builder.start_case();
                self.call_in_assert(builder, fun, &call_arguments);

                // If the operator evaluates to true the assertion succeeded.
                // We can just return nil.
                let clause = builder.start_case_clause();
                builder.atom_pattern("true");
                let clause = builder.end_clause_pattern(clause);
                let clause = builder.end_clause_guards(clause);
                builder.atom("nil");
                builder.end_clause_body(clause);

                // Otherwise we want to throw a runtime error!
                let clause = builder.start_case_clause();
                builder.atom_pattern("false");
                let clause = builder.end_clause_pattern(clause);
                let clause = builder.end_clause_guards(clause);
                self.assert_call_error(
                    builder,
                    value,
                    &call_arguments,
                    message.as_ref(),
                    *location,
                );
                builder.end_clause_body(clause);

                builder.end_case(case);
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
            | TypedExpr::Invalid { .. } => {
                let case = builder.start_case();
                self.maybe_block_expr(builder, value);

                // If the expression evaluates to true the assertion succeeded.
                // We can just return nil.
                let clause = builder.start_case_clause();
                builder.atom_pattern("true");
                let clause = builder.end_clause_pattern(clause);
                let clause = builder.end_clause_guards(clause);
                builder.atom("nil");
                builder.end_clause_body(clause);

                // Otherwise we want to throw a runtime error!
                let clause = builder.start_case_clause();
                builder.atom_pattern("false");
                let clause = builder.end_clause_pattern(clause);
                let clause = builder.end_clause_guards(clause);
                self.assert_expression_error(
                    builder,
                    AssertionExpression::from_expression(value).evaluated_to_bool(false),
                    message.as_ref(),
                    *location,
                );
                builder.end_clause_body(clause);

                builder.end_case(case);
            }
        }
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
        builder: &mut impl ErlangBuilder<Output>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        message: Option<&'a TypedExpr>,
        location: SrcSpan,
    ) {
        let case = builder.start_case();
        self.maybe_block_expr(builder, left);

        // In case the first expression is true, we get to evaluate the second
        // one as well, then we will be able to tell if the assertion failed or
        // not!
        let clause = builder.start_case_clause();
        builder.atom_pattern("true");
        let clause = builder.end_clause_pattern(clause);
        let clause = builder.end_clause_guards(clause);
        {
            // Now we have to match on the right hand side!
            let case = builder.start_case();
            self.maybe_block_expr(builder, right);

            // If it's true the assertion succeded! We can return `nil`.
            let clause = builder.start_case_clause();
            builder.atom_pattern("true");
            let clause = builder.end_clause_pattern(clause);
            let clause = builder.end_clause_guards(clause);
            builder.atom("nil");
            builder.end_clause_body(clause);

            // If it's false the assertion failed! The left hand side was true
            // but this one evaluated to false :(
            let clause = builder.start_case_clause();
            builder.atom_pattern("false");
            let clause = builder.end_clause_pattern(clause);
            let clause = builder.end_clause_guards(clause);
            self.assert_binary_operator_error(
                builder,
                BinOp::And,
                AssertionExpression::from_expression(left).evaluated_to_bool(true),
                AssertionExpression::from_expression(right).evaluated_to_bool(false),
                message,
                location,
            );
            builder.end_clause_body(clause);
            builder.end_case(case);
        }
        builder.end_clause_body(clause);

        // In case the first expression is false, we want to fail fast. We are
        // short circuiting without evaluating the right hand side! This side
        // just build an error.
        let clause = builder.start_case_clause();
        builder.atom_pattern("false");
        let clause = builder.end_clause_pattern(clause);
        let clause = builder.end_clause_guards(clause);
        self.assert_binary_operator_error(
            builder,
            BinOp::And,
            AssertionExpression::from_expression(left).evaluated_to_bool(false),
            AssertionExpression::from_expression(right).was_unevaluated(),
            message,
            location,
        );
        builder.end_clause_body(clause);

        builder.end_case(case);
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
        builder: &mut impl ErlangBuilder<Output>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        message: Option<&'a TypedExpr>,
        location: SrcSpan,
    ) {
        let case = builder.start_case();
        builder.binary_operator("orelse");
        self.maybe_block_expr(builder, left);
        self.maybe_block_expr(builder, right);

        // If the result is true, then the assertion succeeded, we can return
        // nil.
        let clause = builder.start_case_clause();
        builder.atom_pattern("true");
        let clause = builder.end_clause_pattern(clause);
        let clause = builder.end_clause_guards(clause);
        builder.atom("nil");
        builder.end_clause_body(clause);

        // But if it fails we know that both sides of the assertion resulted in
        // a false value. In that case we throw an error.

        let clause = builder.start_case_clause();
        builder.atom_pattern("false");
        let clause = builder.end_clause_pattern(clause);
        let clause = builder.end_clause_guards(clause);
        self.assert_binary_operator_error(
            builder,
            BinOp::Or,
            AssertionExpression::from_expression(left).evaluated_to_bool(false),
            AssertionExpression::from_expression(right).evaluated_to_bool(false),
            message,
            location,
        );
        builder.end_clause_body(clause);

        builder.end_case(case);
    }

    /// This generates the code that throws a runtime error whan an `assert`
    /// that is checking the result of a binary operator fails.
    fn assert_binary_operator_error<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        operator: BinOp,
        left: AssertionExpression<'a>,
        right: AssertionExpression<'a>,
        message: Option<&'a TypedExpr>,
        location: SrcSpan,
    ) {
        let error = self.start_runtime_error(builder, RuntimeErrorKind::Assert, location, message);

        builder.map_field();
        builder.atom("kind");
        builder.atom("binary_operator");

        builder.map_field();
        builder.atom("operator");
        builder.atom(operator.name());

        builder.map_field();
        builder.atom("left");
        self.assertion_expression_map(builder, &left);

        builder.map_field();
        builder.atom("right");
        self.assertion_expression_map(builder, &right);

        builder.map_field();
        builder.atom("start");
        builder.int(location.start.into());

        builder.map_field();
        builder.atom("end");
        builder.int(right.location.end.into());

        builder.map_field();
        builder.atom("expression_start");
        builder.int(left.location.start.into());

        self.end_runtime_error(builder, error);
    }

    /// This generates the code that throws a runtime error whan an `assert`
    /// that is checking the result of an arbitrary expression fails.
    fn assert_expression_error<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        expression: AssertionExpression<'a>,
        message: Option<&'a TypedExpr>,
        location: SrcSpan,
    ) {
        let error = self.start_runtime_error(builder, RuntimeErrorKind::Assert, location, message);

        builder.map_field();
        builder.atom("kind");
        builder.atom("expression");

        // If assert fails on an expression's result then we know it must have
        // evaluated to false!
        builder.map_field();
        builder.atom("expression");
        self.assertion_expression_map(builder, &expression);

        builder.map_field();
        builder.atom("start");
        builder.int(location.start.into());

        builder.map_field();
        builder.atom("end");
        builder.int(expression.location.end.into());

        builder.map_field();
        builder.atom("expression_start");
        builder.int(expression.location.start.into());

        self.end_runtime_error(builder, error);
    }

    fn assert_call_error<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        call: &'a TypedExpr,
        arguments: &[AssertionExpression<'a>],
        message: Option<&'a TypedExpr>,
        location: SrcSpan,
    ) {
        let error = self.start_runtime_error(builder, RuntimeErrorKind::Assert, location, message);

        builder.map_field();
        builder.atom("kind");
        builder.atom("function_call");

        builder.map_field();
        builder.atom("arguments");
        for argument in arguments {
            builder.cons_list();
            self.assertion_expression_map(builder, argument);
        }
        builder.empty_list();

        builder.map_field();
        builder.atom("start");
        builder.int(location.start.into());

        builder.map_field();
        builder.atom("end");
        builder.int(call.location().end.into());

        builder.map_field();
        builder.atom("expression_start");
        builder.int(call.location().start.into());

        self.end_runtime_error(builder, error);
    }

    /// Given an expression being asserted on. This generates the code for an
    /// Erlang map that describes it: with a field for its kind, its value, and
    /// its location in the source code.
    fn assertion_expression_map<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        expression: &AssertionExpression<'a>,
    ) {
        let AssertionExpression {
            kind,
            runtime_value,
            location,
        } = expression;

        let map = builder.start_map();

        builder.map_field();
        builder.atom("kind");
        builder.atom(match kind {
            AssertedExpressionKind::Literal => "literal",
            AssertedExpressionKind::Expression => "expression",
            AssertedExpressionKind::Unevaluated => "unevaluated",
        });

        if runtime_value.is_some() {
            builder.map_field();
            builder.atom("value");
            self.runtime_value(builder, expression);
        }

        builder.map_field();
        builder.atom("start");
        builder.int(location.start.into());

        builder.map_field();
        builder.atom("end");
        builder.int(location.end.into());

        builder.end_map(map);
    }

    /// This takes a value that is in an assertion (and might have been bound
    /// to a variable somewhere) and produces the code that will reference
    /// such value.
    ///
    fn runtime_value<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        expression: &AssertionExpression<'a>,
    ) {
        match expression
            .runtime_value
            .as_ref()
            .expect("trying to reference unevaluated assert value")
        {
            AssertedExpressionRuntimeValue::KnownBool(true) => builder.atom("true"),
            AssertedExpressionRuntimeValue::KnownBool(false) => builder.atom("false"),
            AssertedExpressionRuntimeValue::Variable(name) => builder.variable(name),
            AssertedExpressionRuntimeValue::Expression(expr) => {
                self.maybe_block_expr(builder, expr);
            }
        }
    }

    fn tuple_index<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        tuple: &'a TypedExpr,
        index: u64,
    ) {
        let call = builder.start_remote_call(ErlangModuleName::erlang(), "element");
        builder.int((index + 1).into());
        self.maybe_block_expr(builder, tuple);
        builder.end_call(call);
    }

    fn var<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        name: &'a str,
        constructor: &'a ValueConstructor,
    ) {
        match &constructor.variant {
            ValueConstructorVariant::Record {
                name: record_name, ..
            } => match constructor.type_.deref() {
                // We have a variable referencing a record: we are either
                // referencing a record constructor function, or building a
                // record that has no fields:
                //
                // ```gleam
                // type Wibble {
                //   Wibble
                //   Wobble(Int)
                // }
                //
                // pub fn main() {
                //   Wibble
                // //^^^^^^ Building record with no fields
                //   Wobble
                // //^^^^^^ Referencing record constructor function
                // }
                // ```
                Type::Named { .. } | Type::Var { .. } | Type::Tuple { .. } => {
                    builder.atom(&to_snake_case(record_name));
                }
                Type::Fn { arguments, .. } => {
                    self.record_builder_anonymous_function(builder, record_name, arguments.len());
                }
            },

            ValueConstructorVariant::LocalVariable { location, .. } => {
                builder.variable(&self.local_var_name(location));
            }

            ValueConstructorVariant::ModuleConstant { literal, .. } => {
                self.inlined_constant(builder, literal);
            }

            ValueConstructorVariant::ModuleFn {
                arity,
                external_erlang: Some((module, name)),
                ..
            } => {
                let name = escape_erlang_existing_name(name);
                if *module == self.module_generator.module.name {
                    builder.function_reference(None, name, *arity);
                } else {
                    builder.function_reference(Some(ErlangModuleName::new(module)), name, *arity);
                }
            }

            ValueConstructorVariant::ModuleFn { arity, module, .. }
                if *module == self.module_generator.module.name =>
            {
                builder.function_reference(None, escape_erlang_existing_name(name), *arity);
            }

            ValueConstructorVariant::ModuleFn {
                arity,
                module,
                name,
                ..
            } => builder.function_reference(
                Some(ErlangModuleName::new(module)),
                escape_erlang_existing_name(name),
                *arity,
            ),
        }
    }

    fn call<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        fun: &'a TypedExpr,
        arguments: &'a [TypedCallArg],
    ) {
        match how_to_call(fun) {
            // If we're building a record then we want to just output a
            // tagged tuple, there's no function call at all!
            FunctionCall::BuildRecord { name } => self.build_record(builder, name, arguments),
            // If we're calling some module function like `io.println`, `main`,
            // `list.map` then we can call the function using its name (and
            // module name if it comes from a different module).
            FunctionCall::Call { module, name } => {
                let call = if module != self.module_generator.module.name {
                    builder.start_remote_call(
                        ErlangModuleName::new(module),
                        escape_erlang_existing_name(name),
                    )
                } else {
                    let call = builder.start_call();
                    builder.atom(escape_erlang_existing_name(name));
                    call
                };
                for argument in arguments {
                    self.maybe_block_expr(builder, &argument.value);
                }
                builder.end_call(call);
            }
            // If we're calling anything else (like an anonymous function, or
            // the result of another function call) we generate its code and
            // call that result directly.
            FunctionCall::DirectCall => {
                let call = builder.start_call();
                self.maybe_block_expr(builder, fun);
                for argument in arguments {
                    self.maybe_block_expr(builder, &argument.value);
                }
                builder.end_call(call);
            }
        }
    }

    /// This generates the code for a call that happens in an `assert`.
    /// For example: `assert wibble.wobble(a, b)`.
    ///
    /// This is a function separate from the regular `self.call` since the call
    /// arguments are not just TypedExpressions but values that might have been
    /// bound to variables in previous statements.
    /// Assert has to do it when a call is asserted so that those arguments can
    /// be referenced later in the error thrown at runtime!
    ///
    fn call_in_assert<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        fun: &'a TypedExpr,
        arguments: &[AssertionExpression<'a>],
    ) {
        match how_to_call(fun) {
            // What comes after `assert` has to produce a boolean, so type
            // checking should make it impossible to build a record here.
            FunctionCall::BuildRecord { .. } => {
                panic!("type checking should make it impossible to call a record in an assert")
            }

            // If we're calling some module function like `io.println`, `main`,
            // `list.map` then we can call the function using its name (and
            // module name if it comes from a different module).
            FunctionCall::Call { module, name } => {
                let call = if module != self.module_generator.module.name {
                    builder.start_remote_call(
                        ErlangModuleName::new(module),
                        escape_erlang_existing_name(name),
                    )
                } else {
                    let call = builder.start_call();
                    builder.atom(escape_erlang_existing_name(name));
                    call
                };
                for argument in arguments {
                    self.runtime_value(builder, argument);
                }
                builder.end_call(call);
            }

            // If we're calling anything else (like an anonymous function, or
            // the result of another function call) we generate its code and
            // call that result directly.
            FunctionCall::DirectCall => {
                let call = builder.start_call();
                self.maybe_block_expr(builder, fun);
                for argument in arguments {
                    self.runtime_value(builder, argument);
                }
                builder.end_call(call);
            }
        }
    }

    /// Given a Gleam record name and the arguments it's called with, this
    /// generates the code to build such record.
    /// For example: `Wibble(1, 2)` would be `record_builder("Wibble", [1, 2])`.
    /// It would result in a tuple like this: `{wibble, 1, 2}`.
    ///
    /// Notice how the name you have to specify is the _Gleam name_ of the
    /// record. This function will take care of turning it to snake case!
    fn build_record<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        record_name: &str,
        arguments: &'a [TypedCallArg],
    ) {
        if arguments.is_empty() {
            builder.atom(&to_snake_case(record_name));
        } else {
            let tuple = builder.start_tuple();
            builder.atom(&to_snake_case(record_name));
            for argument in arguments {
                self.maybe_block_expr(builder, &argument.value);
            }
            builder.end_tuple(tuple);
        }
    }

    fn case<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        subjects: &'a [TypedExpr],
        clauses: &'a [TypedClause],
    ) {
        let case = builder.start_case();

        // If there's more than a single subject we will need to wrap those in a
        // tuple and start matching on tuple patterns. That's because Erlang
        // doesn't support matching on multiple subjects like Gleam.
        match subjects {
            [subject] => self.maybe_block_expr(builder, subject),
            subjects => {
                let tuple = builder.start_tuple();
                for subject in subjects {
                    self.maybe_block_expr(builder, subject);
                }
                builder.end_tuple(tuple);
            }
        }

        for clause in clauses {
            let taken_names_before_clause = self.taken_names.clone();

            self.clause_branch(builder, &clause.pattern, clause);

            // Erlang doesn't support alternative patterns so we're gonna have
            // to turn those into separate branches!
            // Since those are going to have the exact same body we don't want
            // it to use different updated variable names.
            // So they should have the same scope that existed before generating
            // the first clause branch.
            // For example:
            //
            // ```gleam
            // case x {
            //   1 | 2 -> { let a = Nil }
            //   _ -> Nil
            // }
            // ```
            //
            // We want the generated code to look like this:
            //
            // ```erl
            // case x of
            //    1 -> A = nil;
            //    2 -> A = nil;
            // %       ^ We're still using `A`, not `A@1`!
            //    _ -> nil
            // end
            // ```
            //
            for pattern in &clause.alternative_patterns {
                self.taken_names = taken_names_before_clause.clone();
                self.clause_branch(builder, pattern, clause);
            }
        }

        builder.end_case(case);
    }

    /// Given a pattern and the branch it belongs to this generates an Erlang
    /// case clause for that pattern.
    fn clause_branch<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        patterns: &'a Vec<Pattern<Arc<Type>>>,
        clause: &'a Clause<TypedExpr, Arc<Type>>,
    ) {
        let clause_pattern = builder.start_case_clause();

        // We start by generating the case clause pattern. If we're matching on
        // multiple subjects (and so patterns has more that a single item) those
        // are gonna be wrapped in a tuple pattern.
        // That's how we match on multiple things on the Erlang target.
        let mut pattern_generator = PatternGenerator::new(self);
        match patterns.as_slice() {
            [pattern] => pattern_generator.pattern(builder, pattern),
            patterns => {
                let tuple = builder.start_tuple_pattern();
                for pattern in patterns {
                    pattern_generator.pattern(builder, pattern);
                }
                builder.end_tuple_pattern(tuple);
            }
        }

        let variables_to_add_later = pattern_generator.variables_to_add_later;

        let clause_guards = builder.end_clause_pattern(clause_pattern);
        if let Some(guard) = clause.guard.as_ref() {
            self.clause_guard(builder, guard, &variables_to_add_later);
        }

        // Finally we can generate the clause body. If the clause is
        // followed by a single block then we want it to be a statements
        // sequence (and not wrapped in a begin ... end block as we usually
        // would when generating code for a block expression).
        let clause_body = builder.end_clause_guards(clause_guards);
        self.pattern_assignments(builder, variables_to_add_later);
        if let TypedExpr::Block { statements, .. } = &clause.then {
            self.statement_sequence(builder, statements);
        } else {
            self.expression(builder, &clause.then);
        }
        builder.end_clause_body(clause_body);
    }

    /// Erlang doesn't have a special constant declaration syntax; so each Gleam
    /// constant is simply inlined anywhere it is used.
    ///
    /// This function produces the code of a constant expression.
    ///
    fn inlined_constant<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        literal: &'a TypedConstant,
    ) {
        match literal {
            Constant::Int { int_value, .. } => builder.int(int_value.clone()),
            Constant::Float { float_value, .. } => builder.float(float_value.value()),
            Constant::String { value, .. } => builder.string(value),
            Constant::Var {
                name, constructor, ..
            } => self.var(
                builder,
                name,
                constructor
                    .as_ref()
                    .expect("This is guaranteed to hold a value."),
            ),

            Constant::Tuple { elements, .. } => {
                let tuple = builder.start_tuple();
                for element in elements {
                    self.inlined_constant(builder, element);
                }
                builder.end_tuple(tuple);
            }

            Constant::List { elements, tail, .. } => {
                for element in elements {
                    builder.cons_list();
                    self.inlined_constant(builder, element);
                }
                match tail {
                    // If there's no tail we simply add an empty list cell to
                    // end the cons list.
                    None => builder.empty_list(),
                    Some(tail) => match tail.list_elements() {
                        // If there's a tail and we don't statically know the
                        // elements it's made of, we add it as a regular Erlang
                        // tail and it will be `[1, 2 | Tail]`.
                        None => self.inlined_constant(builder, tail),
                        // But if we can tell it has some fixed amount of
                        // constant elements, then those are inlined too!
                        Some(list_elements) => {
                            for element in list_elements {
                                builder.cons_list();
                                self.inlined_constant(builder, element);
                            }
                            builder.empty_list();
                        }
                    },
                }
            }

            Constant::BitArray { segments, .. } => {
                let bit_array = builder.start_bit_array();
                for segment in segments {
                    self.bit_array_constant_segment(builder, segment);
                }
                builder.end_bit_array(bit_array);
            }

            Constant::Record {
                type_, arguments, ..
            } => {
                let tag = literal
                    .constant_record_tag()
                    .expect("record without inferred constructor made it to code generation");

                match arguments {
                    // This is a regular record call, we're building a record as
                    // usual as a tagged tuple.
                    Some(arguments) => {
                        let tuple = builder.start_tuple();
                        builder.atom(&to_snake_case(&tag));
                        for argument in arguments {
                            self.inlined_constant(builder, &argument.value);
                        }
                        builder.end_tuple(tuple);
                    }
                    // Otherwise we are either referencing a record constructor
                    // function, or building a record that has no fields:
                    //
                    // ```gleam
                    // type Wibble {
                    //   Wibble
                    //   Wobble(Int)
                    // }
                    //
                    // const a = Wibble
                    // //        ^^^^^^ Building record with no fields
                    // const b = Wobble
                    // //        ^^^^^^ Referencing record constructor function
                    // ```
                    None => match type_::collapse_links(type_.clone()).deref() {
                        Type::Named { .. } | Type::Var { .. } | Type::Tuple { .. } => {
                            builder.atom(&to_snake_case(&tag));
                        }
                        Type::Fn { arguments, .. } => {
                            self.record_builder_anonymous_function(builder, &tag, arguments.len());
                        }
                    },
                }
            }

            Constant::StringConcatenation { left, right, .. } => {
                self.constant_string_concatenate(builder, left, right);
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

    fn bit_array_constant_segment<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        segment: &'a TypedConstantBitArraySegment,
    ) {
        builder.bit_array_segment();
        self.inlined_constant(builder, &segment.value);
        match segment.size() {
            Some(TypedConstant::Int { int_value, .. }) if int_value.is_negative() => {
                builder.int(BigInt::ZERO);
            }
            Some(size) => self.inlined_constant(builder, size),
            None => builder.atom("default"),
        }
        self.bit_array_segment_specifiers(builder, segment);
    }

    fn bit_array_segment_specifiers<Output, Expr>(
        &self,
        builder: &mut impl ErlangBuilder<Output>,
        segment: &'a BitArraySegment<Expr, Arc<Type>>,
    ) {
        let options = segment.options.iter();
        builder.bit_array_segment_specifiers(options.filter_map(|option| match option {
            BitArrayOption::Utf8 { .. } | BitArrayOption::Utf8Codepoint { .. } => {
                Some(BitArraySegmentSpecifier::Utf8)
            }
            BitArrayOption::Utf16 { .. } | BitArrayOption::Utf16Codepoint { .. } => {
                Some(BitArraySegmentSpecifier::Utf16)
            }
            BitArrayOption::Utf32 { .. } | BitArrayOption::Utf32Codepoint { .. } => {
                Some(BitArraySegmentSpecifier::Utf32)
            }
            BitArrayOption::Int { .. } => Some(BitArraySegmentSpecifier::Integer),
            BitArrayOption::Float { .. } => Some(BitArraySegmentSpecifier::Float),
            BitArrayOption::Bytes { .. } => Some(BitArraySegmentSpecifier::Binary),
            BitArrayOption::Bits { .. } => Some(BitArraySegmentSpecifier::Bitstring),
            BitArrayOption::Signed { .. } => Some(BitArraySegmentSpecifier::Signed),
            BitArrayOption::Unsigned { .. } => Some(BitArraySegmentSpecifier::Unsigned),
            BitArrayOption::Big { .. } => Some(BitArraySegmentSpecifier::Big),
            BitArrayOption::Little { .. } => Some(BitArraySegmentSpecifier::Little),
            BitArrayOption::Native { .. } => Some(BitArraySegmentSpecifier::Native),
            BitArrayOption::Unit { value, .. } => Some(BitArraySegmentSpecifier::Unit(*value)),
            BitArrayOption::Size { .. } => None,
        }));
    }

    fn constant_string_concatenate<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        left: &'a TypedConstant,
        right: &'a TypedConstant,
    ) {
        let mut items = VecDeque::new();
        items.push_back(left);
        items.push_back(right);

        let bit_array = builder.start_bit_array();
        while let Some(segment) = items.pop_front() {
            match segment {
                // When concatenating constant strings we flatten out all
                // strings that are being concatenated: so that
                // `"a" <> "b" <> "c"` becomes a single bitstring like this:
                // `<<~"a", ~"b", ~"c">>` rather than nested bitstrings:
                // `<<<<~"a", ~"b">>/binary, ~"c">>`.
                // If we find a string concatenation we push its separate items
                // to be printed next!
                Constant::StringConcatenation { left, right, .. } => {
                    items.push_front(right);
                    items.push_front(left);
                    continue;
                }
                // When concatenating constant strings we want all constant
                // variables to also be fully expanded, so that if we have
                //
                // ```gleam
                // const a = "one"
                // const b = a <> "two"
                // ```
                //
                // Any use of b will be replaced with `<<~"one", ~"two">>`.
                Constant::Var {
                    constructor: Some(constructor),
                    ..
                } if let ValueConstructorVariant::ModuleConstant { literal, .. } =
                    &constructor.variant =>
                {
                    items.push_front(literal);
                    continue;
                }

                Constant::Int { .. }
                | Constant::Float { .. }
                | Constant::String { .. }
                | Constant::Tuple { .. }
                | Constant::List { .. }
                | Constant::Record { .. }
                | Constant::RecordUpdate { .. }
                | Constant::BitArray { .. }
                | Constant::Var { .. }
                | Constant::Invalid { .. }
                | Constant::Todo { .. } => (),
            }

            builder.bit_array_segment();
            self.inlined_constant(builder, segment);
            builder.atom("default");
            builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Utf8]);
        }
        builder.end_bit_array(bit_array);
    }

    fn string_concatenate<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
    ) {
        let bit_array = builder.start_bit_array();
        self.string_concatenate_argument(builder, left);
        self.string_concatenate_argument(builder, right);
        builder.end_bit_array(bit_array);
    }

    fn string_concatenate_argument<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        value: &'a TypedExpr,
    ) {
        // String concatenation is basically building a bit array with two
        // elements. Anything is going to be simply added as a `/binary` segment
        // with one exception: if we're dealing with a literal string that needs
        // the `/utf8` specifier instead!
        // In both cases the size is alwaus automatic, so we generate the
        // `default` atom.
        builder.bit_array_segment();
        self.maybe_block_expr(builder, value);
        builder.atom("default");
        builder.bit_array_segment_specifiers(if produces_literal_string(value) {
            [BitArraySegmentSpecifier::Utf8]
        } else {
            [BitArraySegmentSpecifier::Binary]
        });
    }

    fn binary_operator<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
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
            BinOp::DivFloat => return self.float_division(builder, left, right),
            BinOp::DivInt => return self.int_division(builder, left, right, "div"),
            BinOp::RemainderInt => return self.int_division(builder, left, right, "rem"),

            // String concatenation is not a binop at all! It's just building a
            // bit array.
            BinOp::Concatenate => return self.string_concatenate(builder, left, right),
        };

        builder.binary_operator(operator);
        self.maybe_block_expr(builder, left);
        self.maybe_block_expr(builder, right);
    }

    fn float_division<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
    ) {
        match how_to_divide(left, right) {
            HowToDivide::ReplaceWithZero => builder.float(0.0),
            HowToDivide::EvaluateLeftAndReturnZero => {
                // We first evaluate the left hand side, and ignore its return
                // value, and then we return zero directly!
                self.maybe_block_expr(builder, left);
                builder.float(0.0);
            }
            HowToDivide::PlainErlangDivision => {
                builder.binary_operator("/");
                self.maybe_block_expr(builder, left);
                self.maybe_block_expr(builder, right);
            }
            HowToDivide::MatchOnRight {
                is_left_hand_side_pure,
            } => {
                // We first have to evaluate the left hand side, and store its
                // result in a variable to use later.
                let left_name = if !is_left_hand_side_pure {
                    let left_name = self.new_generated_variable();
                    builder.match_operator();
                    builder.variable_pattern(&left_name);
                    self.maybe_block_expr(builder, left);
                    Some(left_name)
                } else {
                    None
                };

                let case = builder.start_case();
                self.maybe_block_expr(builder, right);

                // +0.0 -> +0.0
                let clause = builder.start_case_clause();
                builder.float_pattern(0.0);
                let guards = builder.end_clause_pattern(clause);
                let body = builder.end_clause_guards(guards);
                builder.float(0.0);
                builder.end_clause_body(body);

                // -0.0 -> -0.0
                let clause = builder.start_case_clause();
                builder.float_pattern(-0.0);
                let guards = builder.end_clause_pattern(clause);
                let body = builder.end_clause_guards(guards);
                builder.float(-0.0);
                builder.end_clause_body(body);

                // _value -> left / _value
                let denominator = self.new_generated_variable();
                let clause = builder.start_case_clause();
                builder.variable_pattern(&denominator);
                let guards = builder.end_clause_pattern(clause);
                let body = builder.end_clause_guards(guards);
                builder.binary_operator("/");
                // If we had bound the left hand side to a variabe we just
                // reference it, otherwise we will generate the code for the
                // numerator.
                if let Some(left_name) = left_name {
                    builder.variable(&left_name);
                } else {
                    self.maybe_block_expr(builder, left);
                }
                builder.variable(&denominator);
                builder.end_clause_body(body);

                builder.end_case(case);
            }
        }
    }

    fn int_division<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        op: &'static str,
    ) {
        match how_to_divide(left, right) {
            HowToDivide::ReplaceWithZero => builder.int(BigInt::ZERO),
            HowToDivide::EvaluateLeftAndReturnZero => {
                // We first evaluate the left hand side, and ignore its return
                // value, and then we return zero directly!
                self.maybe_block_expr(builder, left);
                builder.int(BigInt::ZERO);
            }
            HowToDivide::PlainErlangDivision => {
                builder.binary_operator(op);
                self.maybe_block_expr(builder, left);
                self.maybe_block_expr(builder, right);
            }
            HowToDivide::MatchOnRight {
                is_left_hand_side_pure,
            } => {
                // If the left hand side is not a pure expression we will have
                // to evaluate it before the right hand side of the expression.
                // So we assign it to a generated variable that we will then
                // reference in the case expression's body.
                // It will look something like this:
                //
                // ```erl
                // _value = <left_hand_side>,
                // case <right_hand_side> of
                //   0 -> 0;
                //   _value@1 -> _value div _value@1
                // end
                // ```
                //
                let left_name = if !is_left_hand_side_pure {
                    let left_name = self.new_generated_variable();
                    builder.match_operator();
                    builder.variable_pattern(&left_name);
                    self.maybe_block_expr(builder, left);
                    Some(left_name)
                } else {
                    None
                };

                let case = builder.start_case();
                self.maybe_block_expr(builder, right);

                // 0 -> 0
                let clause = builder.start_case_clause();
                builder.int_pattern(BigInt::ZERO);
                let guards = builder.end_clause_pattern(clause);
                let body = builder.end_clause_guards(guards);
                builder.int(BigInt::ZERO);
                builder.end_clause_body(body);

                // _value -> left div _value
                let denominator = self.new_generated_variable();
                let clause = builder.start_case_clause();
                builder.variable_pattern(&denominator);
                let guards = builder.end_clause_pattern(clause);
                let body = builder.end_clause_guards(guards);
                builder.binary_operator(op);
                // If we had bound the left hand side to a variabe we just
                // reference it, otherwise we will generate the code for the
                // numerator.
                if let Some(left_name) = left_name {
                    builder.variable(&left_name);
                } else {
                    self.maybe_block_expr(builder, left);
                }
                builder.variable(&denominator);
                builder.end_clause_body(body);

                builder.end_case(case);
            }
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
        builder: &mut impl ErlangBuilder<Output>,
        segment: &'a TypedExprBitArraySegment,
    ) {
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
            let (size, endiannes) = match encoding {
                ExpressionSegmentStringEncoding::Utf16 { endiannes } => (16, endiannes),
                ExpressionSegmentStringEncoding::Utf32 { endiannes } => (32, endiannes),
                ExpressionSegmentStringEncoding::Utf8 => {
                    // Gleam strings are utf8 encoded binaries, so we just need
                    // to add the binary option and we can call it a day.
                    builder.bit_array_segment();
                    self.maybe_block_expr(builder, &segment.value);
                    builder.atom("default");
                    builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);
                    return;
                }
            };

            builder.bit_array_segment();

            // For utf16 and utf32 we need an explicit conversion using erlang's
            // `unicode:characters_to_binary`. The segment value will be
            // something like this:
            // ```erl
            // unicode:characters_to_binary(<segment_value>, utf8, {utf16, big})
            // ```
            let call =
                builder.start_remote_call(ErlangModuleName::unicode(), "characters_to_binary");
            {
                self.maybe_block_expr(builder, &segment.value);
                builder.atom("utf8");
                let tuple = builder.start_tuple();
                builder.atom(&format!("utf{size}"));
                match endiannes {
                    Endianness::Big => builder.atom("big"),
                    Endianness::Little => builder.atom("little"),
                }
                builder.end_tuple(tuple);
            }
            builder.end_call(call);

            builder.atom("default");
            builder.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);
        } else {
            // If the bit array segment doesn't need any special handling we use the
            // regular printing functions to format its value and options.
            builder.bit_array_segment();
            self.maybe_block_expr(builder, &segment.value);
            self.bit_array_expression_segment_size(builder, segment);
            self.bit_array_segment_specifiers(builder, segment);
        }
    }

    /// This generates the code that will produce the size expression of a bit
    /// array segment.
    ///
    /// Make sure to only call this when you're expected to generate a bit array
    /// size!
    fn bit_array_expression_segment_size<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        segment: &'a TypedExprBitArraySegment,
    ) {
        let Some(size) = segment.size() else {
            builder.atom("default");
            return;
        };

        // Sizes need some care: in Erlang, having a negative segment size
        // results in a runtime error. We can't do that in Gleam! So any
        // negative value must be turned to zero instead:
        if let TypedExpr::Int { int_value, .. } = &size {
            if int_value.is_negative() {
                builder.int(BigInt::ZERO);
            } else {
                builder.int(int_value.clone());
            }
        } else {
            let call = builder.start_remote_call(ErlangModuleName::erlang(), "max");
            builder.int(BigInt::ZERO);
            self.maybe_block_expr(builder, size);
            builder.end_call(call);
        }
    }

    fn clause_guard<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        guard: &'a TypedClauseGuard,
        assignments: &HashMap<EcoString, AliasedLiteral>,
    ) {
        match guard {
            ClauseGuard::Invalid { .. } => unreachable!("invalid guard made it to code generation"),

            ClauseGuard::ModuleSelect { literal, .. } => self.inlined_constant(builder, literal),
            ClauseGuard::Constant(constant) => self.inlined_constant(builder, constant),

            ClauseGuard::Block { value, .. } => self.clause_guard(builder, value, assignments),

            ClauseGuard::TupleIndex { tuple, index, .. } => {
                self.clause_guard_tuple_index(builder, tuple, *index);
            }

            ClauseGuard::FieldAccess {
                container, index, ..
            } => self.clause_guard_tuple_index(
                builder,
                container,
                index.expect("Unable to find index") + 1,
            ),
            ClauseGuard::Not { expression, .. } => {
                builder.unary_operator("not");
                self.clause_guard(builder, expression, assignments);
            }

            ClauseGuard::BinaryOperator {
                operator,
                left,
                right,
                ..
            } => {
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
                        return self.clause_guard_string_concatenate(
                            builder,
                            left,
                            right,
                            assignments,
                        );
                    }
                };

                builder.binary_operator(operator);
                self.clause_guard(builder, left, assignments);
                self.clause_guard(builder, right, assignments);
            }

            // Only local variables are supported and the typer ensures that all
            // ClauseGuard::Vars are local variables
            ClauseGuard::Var {
                name,
                definition_location,
                ..
            } => {
                // If we're referencing a variable introduced by an alias pattern
                // we need to replace it with its actual literal value: in the
                // generated code the variable is only defined later, so just
                // referencing its name would result in an error.
                match assignments.get(name) {
                    Some(AliasedLiteral::String { value, .. }) => builder.string(value),
                    Some(AliasedLiteral::Int { value, .. }) => builder.int(value.clone()),
                    Some(AliasedLiteral::Float { value, .. }) => builder.float(value.value()),
                    None => {
                        builder.variable(&self.local_var_name(definition_location));
                    }
                }
            }
        }
    }

    fn clause_guard_tuple_index<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        tuple: &'a TypedClauseGuard,
        index: u64,
    ) {
        let call = builder.start_remote_call(ErlangModuleName::erlang(), "element");
        builder.int((index + 1).into());
        self.clause_guard(builder, tuple, &HashMap::new());
        builder.end_call(call);
    }

    fn clause_guard_string_concatenate<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        left: &'a TypedClauseGuard,
        right: &'a TypedClauseGuard,
        assignments: &HashMap<EcoString, AliasedLiteral>,
    ) {
        let bit_array = builder.start_bit_array();
        self.clause_guard_string_concatenate_argument(builder, left, assignments);
        self.clause_guard_string_concatenate_argument(builder, right, assignments);
        builder.end_bit_array(bit_array);
    }

    fn clause_guard_string_concatenate_argument<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        guard: &'a TypedClauseGuard,
        assignments: &HashMap<EcoString, AliasedLiteral>,
    ) {
        // String concatenation is basically building a bit array with two
        // elements. Anything is going to be simply added as a `/binary` segment
        // with one exception: if we're dealing with a literal string that needs
        // the `/utf8` specifier instead!
        // In both cases the size is alwaus automatic, so we generate the
        // `default` atom.
        builder.bit_array_segment();
        self.clause_guard(builder, guard, assignments);
        builder.atom("default");
        builder.bit_array_segment_specifiers(if guard_produces_literal_string(guard) {
            [BitArraySegmentSpecifier::Utf8]
        } else {
            [BitArraySegmentSpecifier::Binary]
        });
    }

    /// Given a record name and the number of arguments it accepts, this outputs
    /// the code to generate an anonymous function that builds that record.
    ///
    /// For example, given:
    ///
    /// ```gleam
    /// pub type Wibble {
    ///   Wibble(Int, String)
    /// }
    ///
    /// pub fn main() {
    ///   Wibble
    /// //^^^^^^ This has to return the builder function!
    /// }
    /// ```
    ///
    /// We will produce the following Erlang code:
    ///
    /// ```erl
    /// main() ->
    ///   fun(_value, _value@1) ->
    ///     {wibble, _value, _value@1}
    ///   end.
    /// ```
    ///
    fn record_builder_anonymous_function<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        record_name: &str,
        arguments: usize,
    ) {
        let arguments = (0..arguments)
            .map(|_| self.new_generated_variable())
            .collect_vec();
        let function = builder.start_anonymous_function(&arguments);

        if arguments.is_empty() {
            builder.atom(&to_snake_case(record_name));
        } else {
            let tuple = builder.start_tuple();
            builder.atom(&to_snake_case(record_name));
            for argument in arguments {
                builder.variable(&argument);
            }
            builder.end_tuple(tuple);
        }

        builder.end_function(function);
    }

    /// After generating a pattern we might have to generate additional variable
    /// bindings in the body following a clause pattern.
    /// This adds those variable to the current body.
    fn pattern_assignments<Output>(
        &mut self,
        builder: &mut impl ErlangBuilder<Output>,
        variables_to_add_later: HashMap<EcoString, AliasedLiteral>,
    ) {
        let variables_to_add_later = variables_to_add_later
            .into_iter()
            .sorted_by(|(one, _), (other, _)| one.cmp(other));

        for (gleam_name, value) in variables_to_add_later {
            builder.match_operator();
            match value {
                AliasedLiteral::String { location, value } => {
                    builder.variable_pattern(&self.new_erlang_variable(&gleam_name, location));
                    builder.string(&value);
                }
                AliasedLiteral::Float { location, value } => {
                    builder.variable_pattern(&self.new_erlang_variable(&gleam_name, location));
                    builder.float(value.value());
                }
                AliasedLiteral::Int { location, value } => {
                    builder.variable_pattern(&self.new_erlang_variable(&gleam_name, location));
                    builder.int(value.clone());
                }
            }
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

/// This represents the different ways a Gleam division could be turned into an
/// Erlang division.
/// In Gleam dividing by zero results in a 0, not in an exception. This means
/// we can't always just use the plain Erlang division operator.
enum HowToDivide {
    /// Means we can divide two expression by just doing `One / Other`.
    PlainErlangDivision,

    /// The entire division can be safely replaced with a literal `0`.
    ReplaceWithZero,

    /// This means the left hand side could have side effects, but then we're
    /// dividing it by zero, so we can just ignore its result and directly
    /// return 0.
    EvaluateLeftAndReturnZero,

    /// This means we have to pattern match on the right hand side to make sure
    /// that it is not zero, otherwise we'll have to return zero, or the
    /// division operation would result in an exception.
    ///
    /// It will look something like this:
    ///
    /// ```erl
    /// case right_hand_side of
    ///   0 -> 0;
    ///   _denominator -> left_hand_side / _denominator
    /// ```
    ///
    MatchOnRight { is_left_hand_side_pure: bool },
}

fn how_to_divide(left: &TypedExpr, right: &TypedExpr) -> HowToDivide {
    if right.is_non_zero_compile_time_number() {
        // Right can't be zero, so it's safe to just divide!
        HowToDivide::PlainErlangDivision
    } else if left.is_pure_value_constructor() {
        if right.is_zero_compile_time_number() {
            // Left has no side effects and `right` is `0`, so we can just
            // replace the result with zero!
            HowToDivide::ReplaceWithZero
        } else {
            // Left has no side effects, but `right` could still be zero at
            // runtime, we need to match on it.
            HowToDivide::MatchOnRight {
                is_left_hand_side_pure: true,
            }
        }
    } else {
        // Left can have side effects, but the right hand side is zero!
        // In that case we have to evaluate `left`, but then we can directly
        // return 0.
        if right.is_zero_compile_time_number() {
            HowToDivide::EvaluateLeftAndReturnZero
        } else {
            // Otherwise we'll have to make sure things are evaluated in the
            // correct order.
            HowToDivide::MatchOnRight {
                is_left_hand_side_pure: false,
            }
        }
    }
}

pub fn record_definition(record_name: &str, fields: &[(&str, Arc<Type>)]) -> String {
    let mut builder = ErlangSourceBuilder::new(None);

    let attribute = builder.start_record_attribute(&to_snake_case(record_name));

    let type_printer = TypeGenerator::new("").var_as_any();
    for (field_name, field_type) in fields {
        builder.record_field();
        builder.atom(field_name);
        type_printer.type_(&mut builder, field_type);
    }

    builder.end_record_attribute(attribute);
    builder.into_output()
}

pub fn module<'a>(
    module: &'a TypedModule,
    line_numbers: &'a LineNumbers,
    root: &'a Utf8Path,
) -> String {
    let mut generator = Generator::new(module, line_numbers, root);
    let mut builder = ErlangSourceBuilder::new(Some(ErlangModuleName::new(&module.name)));
    generator.module_document(&mut builder);

    let mut output = builder.into_output();
    if generator.echo_used {
        output.push_str(std::include_str!("../templates/echo.erl"));
    }
    output
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
fn type_export(custom_type: &TypedCustomType) -> (EcoString, usize) {
    let name = erl_safe_type_name(to_snake_case(&custom_type.name));
    let arity = custom_type.typed_parameters.len();
    (name, arity)
}

/// This returns true if the given expression is going to be compiled to a
/// single literal Erlang string.
/// This is not true just for literal Gleam strings like `"abc"`, but also
/// variables referencing string constants (as those are inlined)
fn produces_literal_string(value: &TypedExpr) -> bool {
    match value {
        TypedExpr::String { .. }
        // Constants are inlined on the Erlang target, so we need to check if
        // those are literal strings too!
        | TypedExpr::ModuleSelect {
            constructor:
                ModuleValueConstructor::Constant {
                    literal: Constant::String { .. },
                    ..
                },
            ..
        }
        | TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant:
                        ValueConstructorVariant::ModuleConstant {
                            literal: Constant::String { .. },
                            ..
                        },
                    ..
                },
            ..
        } => true,

        TypedExpr::Int { .. }
        | TypedExpr::Var { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::Block { .. }
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
        | TypedExpr::BitArray { .. }
        | TypedExpr::RecordUpdate { .. }
        | TypedExpr::NegateBool { .. }
        | TypedExpr::NegateInt { .. }
        | TypedExpr::Invalid { .. } => false,
    }
}

/// This returns true if the given expression is going to be compiled to a
/// single literal Erlang string.
/// This is not true just for literal Gleam strings like `"abc"`, but also
/// variables referencing string constants (as those are inlined)
fn guard_produces_literal_string(guard: &ClauseGuard<Arc<Type>>) -> bool {
    match guard {
        ClauseGuard::Block { value, .. } => guard_produces_literal_string(value),

        ClauseGuard::ModuleSelect {
            literal: Constant::String { .. },
            ..
        }
        | ClauseGuard::Constant(Constant::String { .. }) => true,

        ClauseGuard::BinaryOperator { .. }
        | ClauseGuard::Constant(..)
        | ClauseGuard::ModuleSelect { .. }
        | ClauseGuard::Not { .. }
        | ClauseGuard::Var { .. }
        | ClauseGuard::TupleIndex { .. }
        | ClauseGuard::FieldAccess { .. }
        | ClauseGuard::Invalid { .. } => false,
    }
}

static ATOM_PATTERN: OnceLock<Regex> = OnceLock::new();

fn atom_pattern() -> &'static Regex {
    ATOM_PATTERN.get_or_init(|| Regex::new(r"^[a-z][a-z0-9_@]*$").expect("atom RE regex"))
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

fn needs_begin_end_wrapping(expression: &TypedExpr) -> bool {
    match expression {
        // Record updates are 1 expression if there's no assignment, multiple
        // otherwise.
        TypedExpr::RecordUpdate {
            updated_record_assigned_name,
            ..
        } => updated_record_assigned_name.is_some(),

        // Pipelines are always multiple assignments.
        TypedExpr::Pipeline { .. } => true,

        // Binary operations that require division might have to be turned into
        // multiple statements!
        TypedExpr::BinOp {
            operator: BinOp::DivFloat | BinOp::DivInt | BinOp::RemainderInt,
            left,
            right,
            ..
        } => match how_to_divide(left, right) {
            // In these cases we'll have to generate two statements: a variable
            // assignment for the left hand side, and one to return the value!
            HowToDivide::MatchOnRight {
                is_left_hand_side_pure: false,
            }
            | HowToDivide::EvaluateLeftAndReturnZero => true,
            // Here we just generate a single statement.
            HowToDivide::PlainErlangDivision
            | HowToDivide::ReplaceWithZero
            | HowToDivide::MatchOnRight {
                is_left_hand_side_pure: true,
            } => false,
        },

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

/// This represents an expression that appears in an expression, either because
/// it is part of some larger expression (like a binop: `assert a && b`, or a
/// call `assert wibble(wobble)`), or because it is being matched against
/// directly (like `assert wibble`).
struct AssertionExpression<'a> {
    /// This tells us the kind of expression we're dealing with: wether that's a
    /// literal, an expression that can't be known at compile time, or if it
    /// hasn't been evaluated at all!
    kind: AssertedExpressionKind,
    /// If the expression has been evaluated, this is going to tell us how we
    /// can reference its value.
    runtime_value: Option<AssertedExpressionRuntimeValue<'a>>,
    /// This is the location pointing to where in the Gleam source code this
    /// expression comes from.
    location: SrcSpan,
}

impl<'a> AssertionExpression<'a> {
    fn from_expression(expression: &'a TypedExpr) -> Self {
        Self {
            runtime_value: Some(AssertedExpressionRuntimeValue::Expression(expression)),
            kind: if expression.is_literal() {
                AssertedExpressionKind::Literal
            } else {
                AssertedExpressionKind::Expression
            },
            location: expression.location(),
        }
    }

    fn was_unevaluated(mut self) -> Self {
        self.kind = AssertedExpressionKind::Unevaluated;
        self.runtime_value = None;
        self
    }

    fn evaluated_to_bool(mut self, result: bool) -> Self {
        self.runtime_value = Some(AssertedExpressionRuntimeValue::KnownBool(result));
        self
    }

    fn from_generated_variable(name: EcoString, original_expression: &'a TypedExpr) -> Self {
        Self {
            runtime_value: Some(AssertedExpressionRuntimeValue::Variable(name)),
            kind: if original_expression.is_literal() {
                AssertedExpressionKind::Literal
            } else {
                AssertedExpressionKind::Expression
            },
            location: original_expression.location(),
        }
    }
}

/// This describes the kind of expression we're asserting against.
///
#[derive(Debug, Clone, Copy)]
enum AssertedExpressionKind {
    /// The expression being asserted against is a literal value. For example:
    ///
    /// ```gleam
    /// assert True && wibble
    /// //     ^^^^ This is a literal value.
    /// ```
    ///
    Literal,
    /// The expression being asserted against is anything but a literal, well
    /// known, value.
    /// For example:
    ///
    /// ```gleam
    /// assert True && wibble
    /// //             ^^^^^^ This is an expression.
    /// ```
    ///
    Expression,
    /// The expression being asserted against has not been evaluated yet,
    /// because of some short circuiting behaviour.
    ///
    /// ```gleam
    /// assert False && something_else()
    /// //              ^^^^^^^^^^^^^^^^ This will never be evaluated.
    /// ```
    Unevaluated,
}

/// This is telling us what the runtime value of some asserted value is.
/// Used to produce the code for such value in runtime errors if an assert
/// fails.
///
/// For example:
///
/// ```gleam
/// assert wibble()
/// ```
///
/// If the assertion fails we know that `wibble()` must be `false` at runtime.
/// It's `AssertedExpressionRuntimeValue` would be
/// `AssertedExpressionRuntimeValue::Bool(false)`.
///
#[derive(Debug)]
enum AssertedExpressionRuntimeValue<'a> {
    /// We can tell that the asserted value must be a known bool.
    /// That's because we know wether the assertion failed (it must be false),
    /// or not (it must be true).
    KnownBool(bool),
    /// The asserted value was bound to a generated variable with the given
    /// name, and we can reference it using that name.
    Variable(EcoString),
    /// The asserted value is an expression we need to inline in the error.
    Expression(&'a TypedExpr),
}

fn variable_name(name: &str) -> EcoString {
    let mut chars = name.chars();
    let first_char = chars.next();
    let first_uppercased = first_char.into_iter().flat_map(char::to_uppercase);
    first_uppercased.chain(chars).collect()
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
                result_type_var_ids(ids, arg_ok, arg_err);
            }
            _ => {
                for argument in arguments {
                    type_var_ids(argument, ids);
                }
            }
        },
        Type::Fn { arguments, return_ } => {
            for argument in arguments {
                type_var_ids(argument, ids);
            }
            type_var_ids(return_, ids);
        }
        Type::Tuple { elements } => {
            for element in elements {
                type_var_ids(element, ids);
            }
        }
    }
}

fn erl_safe_type_name(mut name: EcoString) -> EcoString {
    match name.as_str() {
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
        | "tuple" => {
            name.push('_');
            name
        }

        _ => name,
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

    pub fn type_<Output>(&self, builder: &mut impl ErlangBuilder<Output>, type_: &Type) {
        match type_ {
            Type::Var { type_ } => self.type_variable(builder, &type_.borrow()),
            Type::Named {
                name,
                module,
                arguments,
                ..
            } if is_prelude_module(module) => self.prelude_type(builder, name, arguments),
            Type::Named {
                name,
                module,
                arguments,
                ..
            } => self.named_type(builder, module.into(), name, arguments),
            Type::Fn { arguments, return_ } => {
                let function_type = builder.start_function_type();
                for argument in arguments {
                    self.type_(builder, argument);
                }
                let function_type = builder.end_function_type_arguments(function_type);
                self.type_(builder, return_);
                builder.end_function_type(function_type);
            }
            Type::Tuple { elements } => {
                let tuple = builder.start_tuple_type();
                for element in elements {
                    self.type_(builder, element);
                }
                builder.end_tuple_type(tuple);
            }
        }
    }

    fn type_variable<Output>(&self, builder: &mut impl ErlangBuilder<Output>, type_: &TypeVar) {
        match type_ {
            TypeVar::Link { type_ } => self.type_(builder, type_),
            TypeVar::Generic { id, .. } | TypeVar::Unbound { id, .. } => {
                if self.var_as_any || self.type_variable_is_used_exactly_once(*id) {
                    let any = builder.start_named_type("any");
                    builder.end_named_type(any);
                } else {
                    builder.type_variable(&id_to_type_var_str(*id));
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
        builder: &mut impl ErlangBuilder<Output>,
        name: &str,
        arguments: &[Arc<Type>],
    ) {
        match name {
            "Nil" => builder.literal_atom_type("nil"),
            "Int" | "UtfCodepoint" => {
                let integer = builder.start_named_type("integer");
                builder.end_named_type(integer);
            }
            "String" => {
                let string = builder.start_named_type("binary");
                builder.end_named_type(string);
            }
            "Bool" => {
                let boolean = builder.start_named_type("boolean");
                builder.end_named_type(boolean);
            }
            "Float" => {
                let float = builder.start_named_type("float");
                builder.end_named_type(float);
            }
            "BitArray" => {
                let bitstring = builder.start_named_type("bitstring");
                builder.end_named_type(bitstring);
            }
            "List" => {
                let list = builder.start_named_type("list");
                let list_item = arguments
                    .first()
                    .expect("prelude type list with no argument");
                self.type_(builder, list_item);
                builder.end_named_type(list);
            }
            "Result" => {
                let [ok_type, error_type] = arguments else {
                    panic!("result type with no ok and err types")
                };

                let result = builder.start_union_type();

                let ok = builder.start_tuple_type();
                builder.literal_atom_type("ok");
                self.type_(builder, ok_type);
                builder.end_tuple_type(ok);

                let error = builder.start_tuple_type();
                builder.literal_atom_type("error");
                self.type_(builder, error_type);
                builder.end_tuple_type(error);

                builder.end_union_type(result);
            }

            // Getting here should mean we either forgot a built-in type or there is a
            // compiler error
            name => panic!("{name} is not a prelude type."),
        }
    }

    fn named_type<Output>(
        &self,
        builder: &mut impl ErlangBuilder<Output>,
        module: EcoString,
        name: &str,
        arguments: &[Arc<Type>],
    ) {
        let name = erl_safe_type_name(to_snake_case(name));
        let type_ = if self.current_module == module {
            builder.start_named_type(&name)
        } else {
            builder.start_remote_named_type(ErlangModuleName::new(&module), &name)
        };

        for argument in arguments {
            self.type_(builder, argument);
        }

        builder.end_named_type(type_);
    }
}

fn find_private_functions_referenced_in_importable_constants(
    module: &TypedModule,
) -> im::HashSet<EcoString> {
    let mut overridden_publicity = im::HashSet::new();

    for constant in &module.definitions.constants {
        if constant.publicity.is_importable() {
            find_referenced_private_functions(&constant.value, &mut overridden_publicity);
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
