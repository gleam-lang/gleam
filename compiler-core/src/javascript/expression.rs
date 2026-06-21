// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

use num_bigint::BigInt;
use vec1::Vec1;

use super::{decision::ASSIGNMENT_VAR, *};
use crate::{
    ast::*,
    exhaustiveness::StringEncoding,
    line_numbers::LineNumbers,
    type_::{
        ModuleValueConstructor, Type, TypedCallArg, ValueConstructor, ValueConstructorVariant,
    },
};
use pretty_arena::*;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum Position {
    /// We are compiling the last expression in a function, meaning that it should
    /// use `return` to return the value it produces from the function.
    Tail,
    /// We are inside a function, but the value of this expression isn't being
    /// used, so we don't need to do anything with the returned value.
    Statement,
    /// The value of this expression needs to be used inside another expression,
    /// so we need to use the value that is returned by this expression.
    Expression(Ordering),
    /// We are compiling an expression inside a block, meaning we must assign
    /// to the `_block` variable at the end of the scope, because blocks are not
    /// expressions in JS.
    /// Since JS doesn't have variable shadowing, we must store the name of the
    /// variable being used, which will include the incrementing counter.
    /// For example, `block$2`
    Assign(EcoString),
}

impl Position {
    /// Returns `true` if the position is [`Tail`].
    ///
    /// [`Tail`]: Position::Tail
    #[must_use]
    pub fn is_tail(&self) -> bool {
        matches!(self, Self::Tail)
    }

    #[must_use]
    pub fn ordering(&self) -> Ordering {
        match self {
            Self::Expression(ordering) => *ordering,
            Self::Tail | Self::Assign(_) | Self::Statement => Ordering::Loose,
        }
    }
}

#[derive(Debug, Clone, Copy)]
/// Determines whether we can lift blocks into statement level instead of using
/// immediately invoked function expressions. Consider the following piece of code:
///
/// ```gleam
/// some_function(function_with_side_effect(), {
///   let a = 10
///   other_function_with_side_effects(a)
/// })
/// ```
/// Here, if we lift the block that is the second argument of the function, we
/// would end up running `other_function_with_side_effects` before
/// `function_with_side_effects`. This would be invalid, as code in Gleam should be
/// evaluated left-to-right, top-to-bottom. In this case, the ordering would be
/// `Strict`, indicating that we cannot lift the block.
///
/// However, in this example:
///
/// ```gleam
/// let value = !{
///   let value = False
///   some_function_with_side_effect()
///   value
/// }
/// ```
/// The only expression is the block, meaning it can be safely lifted without
/// changing the evaluation order of the program. So the ordering is `Loose`.
///
pub enum Ordering {
    Strict,
    Loose,
}

/// Tracking where the current function is a module function or an anonymous function.
#[derive(Debug)]
enum CurrentFunction {
    /// The current function is a module function
    ///
    /// ```gleam
    /// pub fn main() -> Nil {
    ///   // we are here
    /// }
    /// ```
    Module,

    /// The current function is a module function, but one of its arguments shadows
    /// the reference to itself so it cannot recurse.
    ///
    /// ```gleam
    /// pub fn main(main: fn() -> Nil) -> Nil {
    ///   // we are here
    /// }
    /// ```
    ModuleWithShadowingArgument,

    /// The current function is an anonymous function
    ///
    /// ```gleam
    /// pub fn main() -> Nil {
    ///   fn() {
    ///     // we are here
    ///   }
    /// }
    /// ```
    Anonymous,
}

impl CurrentFunction {
    #[inline]
    fn can_recurse(&self) -> bool {
        match self {
            CurrentFunction::Module => true,
            CurrentFunction::ModuleWithShadowingArgument => false,
            CurrentFunction::Anonymous => false,
        }
    }
}

/// The variables in scope while generating the code for a function.
///
/// User variables are tracked in a map keyed by name, so a shadowed name can be
/// given a unique JavaScript identifier. The compiler synthesised variables
/// (the `$` case subject, `_pipe`, `_block`, ...) are held in their own fields
/// instead. They can't be referenced by user code, and a branch that generates
/// directly into its enclosing scope needs to restore the user variables while
/// leaving the synthesised counters advanced, so that later code doesn't
/// redeclare one of them.
#[derive(Debug, Clone, Default)]
pub(crate) struct Scope {
    user_variables: im::HashMap<EcoString, usize>,
    /// The highest suffix handed out for each user variable still declared in
    /// the current JS scope, kept across a directly matching `case` branch so a
    /// variable that leaked out of it is not redeclared by a later `let`.
    high_water: im::HashMap<EcoString, usize>,
    assignment: Option<usize>,
    pipe: Option<usize>,
    block: Option<usize>,
    use_assignment: Option<usize>,
    record_update: Option<usize>,
    capture: Option<usize>,
    assert_subject: Option<usize>,
    assert_fail: Option<usize>,
}

impl Scope {
    fn new(user_variables: im::HashMap<EcoString, usize>) -> Self {
        Self {
            user_variables,
            ..Default::default()
        }
    }

    /// The counter for a variable name, whether user or synthesised.
    fn counter(&self, name: &str) -> Option<usize> {
        match name {
            ASSIGNMENT_VAR => self.assignment,
            PIPE_VARIABLE => self.pipe,
            BLOCK_VARIABLE => self.block,
            USE_ASSIGNMENT_VARIABLE => self.use_assignment,
            RECORD_UPDATE_VARIABLE => self.record_update,
            CAPTURE_VARIABLE => self.capture,
            ASSERT_SUBJECT_VARIABLE => self.assert_subject,
            ASSERT_FAIL_VARIABLE => self.assert_fail,
            _ => self.user_variables.get(name).copied(),
        }
    }

    /// Set the counter for a variable name, whether user or synthesised.
    pub(crate) fn set_counter(&mut self, name: &EcoString, value: usize) {
        match name.as_str() {
            ASSIGNMENT_VAR => self.assignment = Some(value),
            PIPE_VARIABLE => self.pipe = Some(value),
            BLOCK_VARIABLE => self.block = Some(value),
            USE_ASSIGNMENT_VARIABLE => self.use_assignment = Some(value),
            RECORD_UPDATE_VARIABLE => self.record_update = Some(value),
            CAPTURE_VARIABLE => self.capture = Some(value),
            ASSERT_SUBJECT_VARIABLE => self.assert_subject = Some(value),
            ASSERT_FAIL_VARIABLE => self.assert_fail = Some(value),
            _ => {
                let _ = self.user_variables.insert(name.clone(), value);
            }
        }
    }

    /// Advance the counter for a name to its next suffix, skipping any suffix
    /// already handed out for it in the current scope so a name that leaked out
    /// of a directly matching branch can't be redeclared.
    fn advance_counter(&mut self, name: &EcoString) {
        let in_scope = self.counter(name).map_or(0, |i| i + 1);
        let high_water = self.high_water.get(name).map_or(0, |i| i + 1);
        let next = in_scope.max(high_water);
        self.set_counter(name, next);
        // Only user variables leak out of a directly matching branch; the
        // synthesised counters survive the restore on their own.
        if self.user_variables.contains_key(name) {
            let _ = self.high_water.insert(name.clone(), next);
        }
    }

    /// The user variables currently in scope.
    pub(crate) fn user_variables(&self) -> &im::HashMap<EcoString, usize> {
        &self.user_variables
    }

    /// Restore previously saved user variables, reverting any counters advanced
    /// during the branch to their earlier values. Variables introduced in the
    /// branch with no earlier binding are kept, and the synthesised counters are
    /// left untouched.
    pub(crate) fn restore_user_variables(&mut self, previous: &im::HashMap<EcoString, usize>) {
        self.user_variables.extend(previous.clone());
    }
}

#[derive(Debug)]
pub(crate) struct Generator<'module, 'ast, 'doc> {
    module_name: EcoString,
    src_path: EcoString,
    line_numbers: &'module LineNumbers,
    function_name: EcoString,
    function_arguments: Vec<Option<&'module EcoString>>,
    current_function: CurrentFunction,
    pub current_scope: Scope,
    pub function_position: Position,
    pub scope_position: Position,
    // We register whether these features are used within an expression so that
    // the module generator can output a suitable function if it is needed.
    pub tracker: &'module mut UsageTracker,
    // We track whether tail call recursion is used so that we can render a loop
    // at the top level of the function to use in place of pushing new stack
    // frames.
    pub tail_recursion_used: bool,
    /// Statements to be compiled when lifting blocks into statement scope.
    /// For example, when compiling the following code:
    /// ```gleam
    /// let a = {
    ///   let b = 1
    ///   b + 1
    /// }
    /// ```
    /// There will be 2 items in `statement_level`: The first will be `let _block;`
    /// The second will be the generated code for the block being assigned to `a`.
    /// This lets use return `_block` as the value that the block evaluated to,
    /// while still including the necessary code in the output at the right place.
    ///
    /// Once the `let` statement has compiled its value, it will add anything accumulated
    /// in `statement_level` to the generated code, so it will result in:
    ///
    /// ```javascript
    /// let _block;
    /// {...}
    /// let a = _block;
    /// ```
    ///
    statement_level: Vec<Document<'ast, 'doc>>,

    /// This will be true if we've generated a `let assert` statement that we know
    /// is guaranteed to throw.
    /// This means we can stop code generation for all the following statements
    /// in the same block!
    pub let_assert_always_panics: bool,

    pub source_map_builder: Option<Rc<RefCell<DebugIgnore<sourcemap::SourceMapBuilder>>>>,
}

impl<'module, 'a, 'doc> Generator<'module, 'a, 'doc> {
    #[allow(clippy::too_many_arguments)] // TODO: FIXME
    pub fn new(
        module_name: EcoString,
        src_path: EcoString,
        line_numbers: &'module LineNumbers,
        function_name: EcoString,
        function_arguments: Vec<Option<&'module EcoString>>,
        tracker: &'module mut UsageTracker,
        initial_scope_vars: im::HashMap<EcoString, usize>,
        source_map_builder: Option<Rc<RefCell<DebugIgnore<sourcemap::SourceMapBuilder>>>>,
    ) -> Self {
        let mut current_scope = Scope::new(initial_scope_vars);
        let mut current_function = CurrentFunction::Module;
        for &name in function_arguments.iter().flatten() {
            // Initialise the function arguments
            current_scope.set_counter(name, 0);

            // If any of the function arguments shadow the current function then
            // recursion is no longer possible.
            if function_name.as_ref() == name {
                current_function = CurrentFunction::ModuleWithShadowingArgument;
            }
        }
        Self {
            tracker,
            module_name,
            src_path,
            line_numbers,
            function_name,
            function_arguments,
            tail_recursion_used: false,
            current_scope,
            current_function,
            function_position: Position::Tail,
            scope_position: Position::Tail,
            statement_level: Vec::new(),
            let_assert_always_panics: false,
            source_map_builder,
        }
    }

    pub fn local_var(&mut self, name: &EcoString) -> EcoString {
        match self.current_scope.counter(name) {
            None => {
                self.current_scope.set_counter(name, 0);
                maybe_escape_identifier(name)
            }
            Some(0) => maybe_escape_identifier(name),
            Some(n) if name == "$" => eco_format!("${n}"),
            Some(n) => eco_format!("{name}${n}"),
        }
    }

    pub fn next_local_var(&mut self, name: &EcoString) -> EcoString {
        self.current_scope.advance_counter(name);
        self.local_var(name)
    }

    pub fn function_body(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        body: &'a [TypedStatement],
        arguments: &'a [TypedArg],
    ) -> Document<'a, 'doc> {
        let body = self.statements(arena, body);
        if self.tail_recursion_used {
            self.tail_call_loop(arena, body, arguments)
        } else {
            body
        }
    }

    fn tail_call_loop(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        body: Document<'a, 'doc>,
        arguments: &'a [TypedArg],
    ) -> Document<'a, 'doc> {
        let loop_assignments = arena.concat(arguments.iter().flat_map(|arg| {
            arg.get_variable_name().map(|name| {
                let var = maybe_escape_identifier(name);
                docvec![
                    arena,
                    self.source_map_tracker(arena, arg.location.start),
                    "let ",
                    var,
                    " = loop$",
                    name,
                    ";",
                    LINE_DOCUMENT
                ]
            })
        }));
        docvec![
            arena,
            "while (true) {",
            docvec![arena, LINE_DOCUMENT, loop_assignments, body].nest(arena, INDENT),
            LINE_DOCUMENT,
            "}"
        ]
    }

    fn statement(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        statement: &'a TypedStatement,
    ) -> Document<'a, 'doc> {
        let expression_doc = match statement {
            Statement::Expression(expression) => self.expression(arena, expression),
            Statement::Assignment(assignment) => self.assignment(arena, assignment),
            Statement::Use(use_) => self.expression(arena, &use_.call),
            Statement::Assert(assert) => self.assert(arena, assert),
        };
        self.add_statement_level(arena, expression_doc)
    }

    fn add_statement_level(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: Document<'a, 'doc>,
    ) -> Document<'a, 'doc> {
        if self.statement_level.is_empty() {
            expression
        } else {
            let mut statements = std::mem::take(&mut self.statement_level);
            statements.push(expression);
            arena.join(statements, LINE_DOCUMENT)
        }
    }

    pub fn expression(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: &'a TypedExpr,
    ) -> Document<'a, 'doc> {
        let mut document = match expression {
            TypedExpr::String { value, .. } => string(arena, value),

            TypedExpr::Int { value, .. } => int(arena, value),
            TypedExpr::Float { float_value, .. } => float_from_value(arena, float_value.value()),

            TypedExpr::List { elements, tail, .. } => {
                self.not_in_tail_position(Some(Ordering::Strict), |this| match tail {
                    Some(tail) => {
                        this.tracker.prepend_used = true;
                        let tail = this.wrap_expression(arena, tail);
                        prepend(
                            arena,
                            elements
                                .iter()
                                .map(|element| this.wrap_expression(arena, element)),
                            tail,
                        )
                    }
                    None if elements.is_empty() => this.empty_list(arena),
                    None => {
                        this.tracker.list_used = true;
                        list(
                            arena,
                            elements
                                .iter()
                                .map(|element| this.wrap_expression(arena, element)),
                        )
                    }
                })
            }

            TypedExpr::Tuple { elements, .. } => self.tuple(arena, elements),
            TypedExpr::TupleIndex { tuple, index, .. } => self.tuple_index(arena, tuple, *index),

            TypedExpr::Case {
                subjects,
                clauses,
                compiled_case,
                ..
            } => decision::case(arena, compiled_case, clauses, subjects, self),

            TypedExpr::Call { fun, arguments, .. } => self.call(arena, fun, arguments),
            TypedExpr::Fn {
                arguments,
                body,
                kind,
                ..
            } => self.fn_(arena, arguments, body, kind),

            TypedExpr::RecordAccess { record, label, .. } => {
                self.record_access(arena, record, label)
            }

            TypedExpr::PositionalAccess { record, index, .. } => {
                self.positional_access(arena, record, *index)
            }

            TypedExpr::RecordUpdate {
                updated_record_assigned_name,
                updated_record,
                constructor,
                arguments,
                ..
            } => self.record_update(
                arena,
                updated_record_assigned_name,
                updated_record,
                constructor,
                arguments,
            ),

            TypedExpr::Var {
                name, constructor, ..
            } => self.variable(arena, name, constructor),

            TypedExpr::Pipeline {
                first_value,
                assignments,
                finally,
                ..
            } => self.pipeline(arena, first_value, assignments.as_slice(), finally),

            TypedExpr::Block { statements, .. } => self.block(arena, statements),

            TypedExpr::BinOp {
                operator,
                left,
                right,
                ..
            } => self.bin_op(arena, operator, left, right),

            TypedExpr::Todo {
                message, location, ..
            } => self.todo(arena, message.as_ref().map(|m| &**m), location),

            TypedExpr::Panic {
                location, message, ..
            } => self.panic(arena, location, message.as_ref().map(|m| &**m)),

            TypedExpr::BitArray { segments, .. } => self.bit_array(arena, segments),

            TypedExpr::ModuleSelect {
                module_alias,
                label,
                constructor,
                ..
            } => self.module_select(arena, module_alias, label, constructor),

            TypedExpr::NegateBool { value, .. } => self.negate_with(arena, "!", value),

            TypedExpr::NegateInt { value, .. } => self.negate_with(arena, "- ", value),

            TypedExpr::Echo {
                expression,
                message,
                location,
                ..
            } => {
                let expression = expression
                    .as_ref()
                    .expect("echo with no expression outside of pipe");
                let expresion_doc =
                    self.not_in_tail_position(None, |this| this.wrap_expression(arena, expression));
                self.echo(arena, expresion_doc, message.as_deref(), location)
            }

            TypedExpr::Invalid { .. } => {
                panic!("invalid expressions should not reach code generation")
            }
        };
        if let Position::Statement = self.scope_position
            && expression_requires_semicolon(expression)
        {
            document = document.append(arena, ";");
        }
        if expression.handles_own_return() {
            docvec![
                arena,
                self.source_map_tracker(arena, expression.location().start),
                document
            ]
        } else {
            docvec![
                arena,
                self.source_map_tracker(arena, expression.location().start),
                self.wrap_return(arena, document)
            ]
        }
    }

    /// Return the singleton empty list; all empty lists are the same underlying
    /// reference, which makes comparison faster.
    fn empty_list(&mut self, arena: &'doc DocumentArena<'a, 'doc>) -> Document<'a, 'doc> {
        self.tracker.list_empty_const_used = true;
        "$List$Empty$const".to_doc(arena)
    }

    fn negate_with(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        with: &'static str,
        value: &'a TypedExpr,
    ) -> Document<'a, 'doc> {
        self.not_in_tail_position(None, |this| {
            docvec![arena, with, this.wrap_expression(arena, value)]
        })
    }

    fn bit_array(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        segments: &'a [TypedExprBitArraySegment],
    ) -> Document<'a, 'doc> {
        self.tracker.bit_array_literal_used = true;

        // Collect all the values used in segments.
        let segments_array = array(
            arena,
            segments.iter().map(|segment| {
                let value = self.not_in_tail_position(Some(Ordering::Strict), |this| {
                    this.wrap_expression(arena, &segment.value)
                });

                let details = self.bit_array_segment_details(arena, segment);

                match details.type_ {
                    BitArraySegmentType::BitArray => {
                        if segment.size().is_some() {
                            self.tracker.bit_array_slice_used = true;
                            docvec![arena, "bitArraySlice(", value, ", 0, ", details.size, ")"]
                        } else {
                            value
                        }
                    }
                    BitArraySegmentType::Int => {
                        match (details.size_value, segment.value.as_ref()) {
                            (Some(size_value), TypedExpr::Int { int_value, .. })
                                if size_value <= SAFE_INT_SEGMENT_MAX_SIZE.into()
                                    && (&size_value % BigInt::from(8) == BigInt::ZERO) =>
                            {
                                let bytes = bit_array_segment_int_value_to_bytes(
                                    int_value.clone(),
                                    size_value,
                                    segment.endianness(),
                                );

                                u8_slice(arena, &bytes)
                            }

                            (Some(size_value), _) if size_value == 8.into() => value,

                            (Some(size_value), _) if size_value <= 0.into() => EMPTY_DOCUMENT,

                            _ => {
                                self.tracker.sized_integer_segment_used = true;
                                let size = details.size;
                                let is_big = bool(segment.endianness().is_big());
                                docvec![arena, "sizedInt(", value, ", ", size, ", ", is_big, ")"]
                            }
                        }
                    }
                    BitArraySegmentType::Float => {
                        self.tracker.float_bit_array_segment_used = true;
                        let size = details.size;
                        let is_big = bool(details.endianness.is_big());
                        docvec![arena, "sizedFloat(", value, ", ", size, ", ", is_big, ")"]
                    }
                    BitArraySegmentType::String(StringEncoding::Utf8) => {
                        self.tracker.string_bit_array_segment_used = true;
                        docvec![arena, "stringBits(", value, ")"]
                    }
                    BitArraySegmentType::String(StringEncoding::Utf16) => {
                        self.tracker.string_utf16_bit_array_segment_used = true;
                        let is_big = bool(details.endianness.is_big());
                        docvec![arena, "stringToUtf16(", value, ", ", is_big, ")"]
                    }
                    BitArraySegmentType::String(StringEncoding::Utf32) => {
                        self.tracker.string_utf32_bit_array_segment_used = true;
                        let is_big = bool(details.endianness.is_big());
                        docvec![arena, "stringToUtf32(", value, ", ", is_big, ")"]
                    }
                    BitArraySegmentType::UtfCodepoint(StringEncoding::Utf8) => {
                        self.tracker.codepoint_bit_array_segment_used = true;
                        docvec![arena, "codepointBits(", value, ")"]
                    }
                    BitArraySegmentType::UtfCodepoint(StringEncoding::Utf16) => {
                        self.tracker.codepoint_utf16_bit_array_segment_used = true;
                        let is_big = bool(details.endianness.is_big());
                        docvec![arena, "codepointToUtf16(", value, ", ", is_big, ")"]
                    }
                    BitArraySegmentType::UtfCodepoint(StringEncoding::Utf32) => {
                        self.tracker.codepoint_utf32_bit_array_segment_used = true;
                        let is_big = bool(details.endianness.is_big());
                        docvec![arena, "codepointToUtf32(", value, ", ", is_big, ")"]
                    }
                }
            }),
        );

        docvec![arena, "toBitArray(", segments_array, ")"]
    }

    fn bit_array_segment_details(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        segment: &'a TypedExprBitArraySegment,
    ) -> BitArraySegmentDetails<'a, 'doc> {
        let size = segment.size();
        let unit = segment.unit();
        let (size_value, size) = match size {
            Some(TypedExpr::Int { int_value, .. }) => {
                let size_value = int_value * unit;
                let size = eco_format!("{}", size_value).to_doc(arena);
                (Some(size_value), size)
            }
            Some(size) => {
                let mut size = self.not_in_tail_position(Some(Ordering::Strict), |this| {
                    this.wrap_expression(arena, size)
                });

                if unit != 1 {
                    size = size
                        .group(arena)
                        .append(arena, " * ".to_doc(arena).append(arena, unit.to_doc(arena)));
                }

                (None, size)
            }

            None => {
                let size_value: usize = if segment.type_.is_int() { 8 } else { 64 };
                (Some(BigInt::from(size_value)), size_value.to_doc(arena))
            }
        };

        let type_ = BitArraySegmentType::from_segment(segment);

        BitArraySegmentDetails {
            type_,
            size,
            size_value,
            endianness: segment.endianness(),
        }
    }

    pub fn wrap_return(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        document: Document<'a, 'doc>,
    ) -> Document<'a, 'doc> {
        match &self.scope_position {
            Position::Tail => docvec![arena, "return ", document, ";"],
            Position::Expression(_) | Position::Statement => document,
            Position::Assign(name) => docvec![arena, name.clone(), " = ", document, ";"],
        }
    }

    pub fn not_in_tail_position<CompileFn, Output>(
        &mut self,
        // If ordering is None, it is inherited from the parent scope.
        // It will be None in cases like `!x`, where `x` can be lifted
        // only if the ordering is already loose.
        ordering: Option<Ordering>,
        compile: CompileFn,
    ) -> Output
    where
        CompileFn: Fn(&mut Self) -> Output,
    {
        let new_ordering = ordering.unwrap_or(self.scope_position.ordering());

        let function_position = std::mem::replace(
            &mut self.function_position,
            Position::Expression(new_ordering),
        );
        let scope_position =
            std::mem::replace(&mut self.scope_position, Position::Expression(new_ordering));

        let result = compile(self);

        self.function_position = function_position;
        self.scope_position = scope_position;
        result
    }

    /// Use the `_block` variable if the expression is JS statement.
    pub fn wrap_expression(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: &'a TypedExpr,
    ) -> Document<'a, 'doc> {
        match (expression, &self.scope_position) {
            (_, Position::Tail | Position::Assign(_)) => self.expression(arena, expression),
            (
                TypedExpr::Panic { .. }
                | TypedExpr::Todo { .. }
                | TypedExpr::Case { .. }
                | TypedExpr::Pipeline { .. }
                | TypedExpr::RecordUpdate {
                    // Record updates that assign a variable generate multiple statements
                    updated_record_assigned_name: Some(_),
                    ..
                },
                Position::Expression(Ordering::Loose),
            ) => self.wrap_block(arena, |this| this.expression(arena, expression)),
            (
                TypedExpr::Panic { .. }
                | TypedExpr::Todo { .. }
                | TypedExpr::Case { .. }
                | TypedExpr::Pipeline { .. }
                | TypedExpr::RecordUpdate {
                    // Record updates that assign a variable generate multiple statements
                    updated_record_assigned_name: Some(_),
                    ..
                },
                Position::Expression(Ordering::Strict),
            ) => self.immediately_invoked_function_expression(
                arena,
                expression,
                |this, expression| this.expression(arena, expression),
            ),
            _ => self.expression(arena, expression),
        }
    }

    /// Wrap an expression using the `_block` variable if required due to being
    /// a JS statement, or in parens if required due to being an operator or
    /// a function literal.
    pub fn child_expression(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: &'a TypedExpr,
    ) -> Document<'a, 'doc> {
        match expression {
            TypedExpr::BinOp { operator, .. } if operator.is_operator_to_wrap() => {}
            TypedExpr::Fn { .. } => {}

            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Var { .. }
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
            | TypedExpr::Invalid { .. } => return self.wrap_expression(arena, expression),
        }

        let document = self.expression(arena, expression);
        match &self.scope_position {
            // Here the document is a return statement: `return <expr>;`
            // or an assignment: `_block = <expr>;`
            Position::Tail | Position::Assign(_) | Position::Statement => document,
            Position::Expression(_) => docvec![arena, "(", document, ")"],
        }
    }

    /// Wrap an expression in an immediately invoked function expression
    fn immediately_invoked_function_expression<T, ToDoc>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        statements: &'a T,
        to_doc: ToDoc,
    ) -> Document<'a, 'doc>
    where
        ToDoc: FnOnce(&mut Self, &'a T) -> Document<'a, 'doc>,
    {
        // Save initial state
        let scope_position = std::mem::replace(&mut self.scope_position, Position::Tail);
        let statement_level = std::mem::take(&mut self.statement_level);

        // Set state for in this iife
        let current_scope = self.current_scope.clone();

        // Generate the expression
        let result = to_doc(self, statements);
        let doc = self.add_statement_level(arena, result);
        let doc = immediately_invoked_function_expression_document(arena, doc);

        // Reset
        self.current_scope = current_scope;
        self.scope_position = scope_position;
        self.statement_level = statement_level;

        self.wrap_return(arena, doc)
    }

    fn wrap_block<CompileFn>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        compile: CompileFn,
    ) -> Document<'a, 'doc>
    where
        CompileFn: Fn(&mut Self) -> Document<'a, 'doc>,
    {
        let block_variable = self.next_local_var(&BLOCK_VARIABLE.into());

        // Save initial state
        let scope_position = std::mem::replace(
            &mut self.scope_position,
            Position::Assign(block_variable.clone()),
        );
        let function_position = std::mem::replace(
            &mut self.function_position,
            Position::Expression(Ordering::Strict),
        );

        // Generate the expression
        let statement_doc = compile(self);

        // Reset
        self.scope_position = scope_position;
        self.function_position = function_position;

        self.statement_level
            .push(docvec![arena, "let ", block_variable.clone(), ";"]);
        self.statement_level.push(statement_doc);

        self.wrap_return(arena, block_variable.to_doc(arena))
    }

    fn variable(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: &'a EcoString,
        constructor: &'a ValueConstructor,
    ) -> Document<'a, 'doc> {
        match &constructor.variant {
            ValueConstructorVariant::Record {
                arity,
                name: variant_name,
                ..
            } => {
                let type_ = constructor.type_.clone();
                self.record_constructor(arena, type_, None, variant_name, name, *arity)
            }
            ValueConstructorVariant::ModuleFn { .. }
            | ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::LocalVariable { .. } => self.local_var(name).to_doc(arena),
        }
    }

    fn pipeline(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        first_value: &'a TypedPipelineAssignment,
        assignments: &'a [(TypedPipelineAssignment, PipelineAssignmentKind)],
        finally: &'a TypedExpr,
    ) -> Document<'a, 'doc> {
        let count = assignments.len();
        let mut documents = Vec::with_capacity((count + 2) * 2);

        let all_assignments = std::iter::once(first_value)
            .chain(assignments.iter().map(|(assignment, _kind)| assignment));

        let mut latest_local_var: Option<EcoString> = None;
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
                documents.push(self.not_in_tail_position(Some(Ordering::Strict), |this| {
                    let var = latest_local_var
                        .as_ref()
                        .expect("echo with no previous step in a pipe");
                    this.echo(arena, var.to_doc(arena), message.as_deref(), location)
                }));
                documents.push(";".to_doc(arena));
            } else {
                // Otherwise we assign the intermediate pipe value to a variable.
                let assignment_document =
                    self.not_in_tail_position(Some(Ordering::Strict), |this| {
                        this.simple_variable_assignment(
                            arena,
                            &assignment.name,
                            &assignment.value,
                            assignment.location,
                        )
                    });
                documents.push(self.add_statement_level(arena, assignment_document));
                latest_local_var = Some(self.local_var(&assignment.name));
            }

            documents.push(LINE_DOCUMENT);
        }

        if let TypedExpr::Echo {
            expression: None,
            message,
            location,
            ..
        } = finally
        {
            let var = latest_local_var.expect("echo with no previous step in a pipe");
            documents.push(self.echo(arena, var.to_doc(arena), message.as_deref(), location));
            match &self.scope_position {
                Position::Statement => documents.push(";".to_doc(arena)),
                Position::Expression(_) | Position::Tail | Position::Assign(_) => {}
            }
        } else {
            let finally_doc = self.expression(arena, finally);
            documents.push(self.add_statement_level(arena, finally_doc));
        }

        arena.concat(documents).force_break(arena)
    }

    pub(crate) fn expression_flattening_blocks(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: &'a TypedExpr,
    ) -> Document<'a, 'doc> {
        if let TypedExpr::Block { statements, .. } = expression {
            self.statements(arena, statements)
        } else {
            self.expression(arena, expression)
        }
    }

    fn block(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        statements: &'a Vec1<TypedStatement>,
    ) -> Document<'a, 'doc> {
        if statements.len() == 1 {
            match statements.first() {
                Statement::Expression(expression) => {
                    return self.child_expression(arena, expression);
                }

                Statement::Assignment(assignment) => match &assignment.kind {
                    AssignmentKind::Let | AssignmentKind::Generated => {
                        return self.child_expression(arena, &assignment.value);
                    }
                    // We can't just return the right-hand side of a `let assert`
                    // assignment; we still need to check that the pattern matches.
                    AssignmentKind::Assert { .. } => {}
                },

                Statement::Use(use_) => return self.child_expression(arena, &use_.call),

                // Similar to `let assert`, we can't immediately return the value
                // that is asserted; we have to actually perform the assertion.
                Statement::Assert(_) => {}
            }
        }
        match &self.scope_position {
            Position::Tail | Position::Assign(_) | Position::Statement => {
                self.block_document(arena, statements)
            }
            Position::Expression(Ordering::Strict) => self.immediately_invoked_function_expression(
                arena,
                statements,
                |this, statements| this.statements(arena, statements),
            ),
            Position::Expression(Ordering::Loose) => self.wrap_block(arena, |this| {
                // Save previous scope
                let current_scope = this.current_scope.clone();

                let document = this.block_document(arena, statements);

                // Restore previous state
                this.current_scope = current_scope;

                document
            }),
        }
    }

    fn block_document(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        statements: &'a Vec1<TypedStatement>,
    ) -> Document<'a, 'doc> {
        let statements = self.statements(arena, statements);
        docvec![
            arena,
            "{",
            docvec![arena, LINE_DOCUMENT, statements].nest(arena, INDENT),
            LINE_DOCUMENT,
            "}"
        ]
    }

    fn statements(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        statements: &'a [TypedStatement],
    ) -> Document<'a, 'doc> {
        // If there are any statements that need to be printed at statement level, that's
        // for an outer scope so we don't want to print them inside this one.
        let statement_level = std::mem::take(&mut self.statement_level);
        let count = statements.len();
        let mut documents = Vec::with_capacity(count * 3);
        for (i, statement) in statements.iter().enumerate() {
            if i + 1 < count {
                let function_position =
                    std::mem::replace(&mut self.function_position, Position::Statement);
                let scope_position =
                    std::mem::replace(&mut self.scope_position, Position::Statement);

                documents.push(self.statement(arena, statement));

                self.function_position = function_position;
                self.scope_position = scope_position;

                documents.push(LINE_DOCUMENT);
            } else {
                documents.push(self.statement(arena, statement));
            }

            // If we've generated code for a statement that always throws, we
            // can skip code generation for all the following ones.
            if self.let_assert_always_panics {
                self.let_assert_always_panics = false;
                break;
            }
        }
        self.statement_level = statement_level;
        if count == 1 {
            arena.concat(documents)
        } else {
            arena.concat(documents).force_break(arena)
        }
    }

    fn simple_variable_assignment(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: &'a EcoString,
        value: &'a TypedExpr,
        location: SrcSpan,
    ) -> Document<'a, 'doc> {
        // Subject must be rendered before the variable for variable numbering
        let subject = self.not_in_tail_position(Some(Ordering::Loose), |this| {
            this.wrap_expression(arena, value)
        });
        let js_name = self.next_local_var(name);
        let assignment = docvec![
            arena,
            self.source_map_tracker(arena, location.start),
            "let ",
            js_name.clone(),
            " = ",
            subject,
            ";"
        ];
        let assignment = match &self.scope_position {
            Position::Expression(_) | Position::Statement => assignment,
            Position::Tail => docvec![arena, assignment, LINE_DOCUMENT, "return ", js_name, ";"],
            Position::Assign(block_variable) => docvec![
                arena,
                assignment,
                LINE_DOCUMENT,
                block_variable.clone(),
                " = ",
                js_name,
                ";"
            ],
        };

        assignment.force_break(arena)
    }

    fn assignment(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        assignment: &'a TypedAssignment,
    ) -> Document<'a, 'doc> {
        let TypedAssignment {
            pattern,
            kind,
            value,
            compiled_case,
            location,
            annotation: _,
        } = assignment;

        // In case the pattern is just a variable, we special case it to
        // generate just a simple assignment instead of using the decision tree
        // for the code generation step.
        if let TypedPattern::Variable { name, .. } = pattern {
            return self.simple_variable_assignment(arena, name, value, *location);
        }

        docvec![
            arena,
            self.source_map_tracker(arena, location.start),
            decision::let_(arena, compiled_case, value, kind, self, pattern)
        ]
    }

    fn assert(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        assert: &'a TypedAssert,
    ) -> Document<'a, 'doc> {
        let TypedAssert {
            location,
            value,
            message,
        } = assert;

        let message = match message {
            Some(message) => self.not_in_tail_position(Some(Ordering::Strict), |this| {
                this.expression(arena, message)
            }),
            None => string(arena, "Assertion failed."),
        };

        let check = self.not_in_tail_position(Some(Ordering::Loose), |this| {
            this.assert_check(arena, value, &message, *location)
        });

        match &self.scope_position {
            Position::Expression(_) | Position::Statement => check,
            Position::Tail | Position::Assign(_) => {
                docvec![
                    arena,
                    check,
                    LINE_DOCUMENT,
                    self.wrap_return(arena, "undefined".to_doc(arena))
                ]
            }
        }
    }

    fn assert_check(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        subject: &'a TypedExpr,
        message: &Document<'a, 'doc>,
        location: SrcSpan,
    ) -> Document<'a, 'doc> {
        let (subject_document, mut fields) = match subject {
            TypedExpr::Call { fun, arguments, .. } => {
                let argument_variables = arguments
                    .iter()
                    .map(|element| {
                        self.not_in_tail_position(Some(Ordering::Strict), |this| {
                            this.assign_to_variable(arena, &element.value)
                        })
                    })
                    .collect_vec();
                (
                    self.call_with_doc_arguments(arena, fun, argument_variables.clone()),
                    vec![
                        ("kind", string(arena, "function_call")),
                        (
                            "arguments",
                            array(
                                arena,
                                argument_variables.into_iter().zip(arguments).map(
                                    |(variable, argument)| {
                                        self.asserted_expression(
                                            arena,
                                            AssertExpression::from_expression(&argument.value),
                                            Some(variable),
                                            argument.location(),
                                        )
                                    },
                                ),
                            ),
                        ),
                    ],
                )
            }

            TypedExpr::BinOp {
                operator,
                left,
                right,
                ..
            } => {
                match operator {
                    BinOp::And => return self.assert_and(arena, left, right, message, location),
                    BinOp::Or => return self.assert_or(arena, left, right, message, location),
                    BinOp::Eq
                    | BinOp::NotEq
                    | BinOp::LtInt
                    | BinOp::LtEqInt
                    | BinOp::LtFloat
                    | BinOp::LtEqFloat
                    | BinOp::GtEqInt
                    | BinOp::GtInt
                    | BinOp::GtEqFloat
                    | BinOp::GtFloat
                    | BinOp::AddInt
                    | BinOp::AddFloat
                    | BinOp::SubInt
                    | BinOp::SubFloat
                    | BinOp::MultInt
                    | BinOp::MultFloat
                    | BinOp::DivInt
                    | BinOp::DivFloat
                    | BinOp::RemainderInt
                    | BinOp::Concatenate => {}
                }

                let left_document = self.not_in_tail_position(Some(Ordering::Loose), |this| {
                    this.assign_to_variable(arena, left)
                });
                let right_document = self.not_in_tail_position(Some(Ordering::Loose), |this| {
                    this.assign_to_variable(arena, right)
                });

                (
                    self.bin_op_with_doc_operands(
                        arena,
                        *operator,
                        left_document.clone(),
                        right_document.clone(),
                        &left.type_(),
                    )
                    .surround(arena, "(", ")"),
                    vec![
                        ("kind", string(arena, "binary_operator")),
                        ("operator", string(arena, operator.name())),
                        (
                            "left",
                            self.asserted_expression(
                                arena,
                                AssertExpression::from_expression(left),
                                Some(left_document),
                                left.location(),
                            ),
                        ),
                        (
                            "right",
                            self.asserted_expression(
                                arena,
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
                self.wrap_expression(arena, subject),
                vec![
                    ("kind", string(arena, "expression")),
                    (
                        "expression",
                        self.asserted_expression(
                            arena,
                            AssertExpression::from_expression(subject),
                            Some("false".to_doc(arena)),
                            subject.location(),
                        ),
                    ),
                ],
            ),
        };

        fields.push(("start", location.start.to_doc(arena)));
        fields.push(("end", subject.location().end.to_doc(arena)));
        fields.push(("expression_start", subject.location().start.to_doc(arena)));

        docvec![
            arena,
            self.source_map_tracker(arena, location.start),
            "if (",
            docvec![arena, "!", subject_document].nest(arena, INDENT),
            arena.break_("", ""),
            ") {",
            docvec![
                arena,
                LINE_DOCUMENT,
                self.throw_error(arena, "assert", message, location, fields),
            ]
            .nest(arena, INDENT),
            LINE_DOCUMENT,
            "}",
        ]
        .group(arena)
    }

    fn negate_bool_expression(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        value: &'a TypedExpr,
    ) -> Document<'a, 'doc> {
        match value {
            TypedExpr::BinOp {
                operator,
                left,
                right,
                ..
            } => match operator {
                BinOp::And => self.print_bin_op(arena, left, right, "||"),
                BinOp::Or => self.print_bin_op(arena, left, right, "&&"),
                BinOp::Eq => self.equal(arena, left, right, false),
                BinOp::NotEq => self.equal(arena, left, right, true),
                BinOp::LtInt | BinOp::LtFloat => self.print_bin_op(arena, left, right, ">="),
                BinOp::LtEqInt | BinOp::LtEqFloat => self.print_bin_op(arena, left, right, ">"),
                BinOp::GtInt | BinOp::GtFloat => self.print_bin_op(arena, left, right, "<="),
                BinOp::GtEqInt | BinOp::GtEqFloat => self.print_bin_op(arena, left, right, "<"),
                BinOp::AddInt
                | BinOp::AddFloat
                | BinOp::SubInt
                | BinOp::SubFloat
                | BinOp::MultInt
                | BinOp::MultFloat
                | BinOp::DivInt
                | BinOp::DivFloat
                | BinOp::RemainderInt
                | BinOp::Concatenate => unreachable!("type checking should make this impossible"),
            },
            TypedExpr::NegateBool { value, .. } => self.wrap_expression(arena, value),
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Call { .. }
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
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => docvec![arena, "!", self.wrap_expression(arena, value)],
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
    /// ```javascript
    /// if (expr1) {
    ///   if (!expr2) {
    ///     <throw exception>
    ///   }
    /// } else {
    ///   <throw exception>
    /// }
    /// ```
    ///
    fn assert_and(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        message: &Document<'a, 'doc>,
        location: SrcSpan,
    ) -> Document<'a, 'doc> {
        let left_kind = AssertExpression::from_expression(left);
        let right_kind = AssertExpression::from_expression(right);

        let fields_if_short_circuiting = vec![
            ("kind", string(arena, "binary_operator")),
            ("operator", string(arena, "&&")),
            (
                "left",
                self.asserted_expression(
                    arena,
                    left_kind,
                    Some("false".to_doc(arena)),
                    left.location(),
                ),
            ),
            (
                "right",
                self.asserted_expression(
                    arena,
                    AssertExpression::Unevaluated,
                    None,
                    right.location(),
                ),
            ),
            ("start", location.start.to_doc(arena)),
            ("end", right.location().end.to_doc(arena)),
            ("expression_start", left.location().start.to_doc(arena)),
        ];

        let fields = vec![
            ("kind", string(arena, "binary_operator")),
            ("operator", string(arena, "&&")),
            (
                "left",
                self.asserted_expression(
                    arena,
                    left_kind,
                    Some("true".to_doc(arena)),
                    left.location(),
                ),
            ),
            (
                "right",
                self.asserted_expression(
                    arena,
                    right_kind,
                    Some("false".to_doc(arena)),
                    right.location(),
                ),
            ),
            ("start", location.start.to_doc(arena)),
            ("end", right.location().end.to_doc(arena)),
            ("expression_start", left.location().start.to_doc(arena)),
        ];

        let left_value = self.not_in_tail_position(Some(Ordering::Loose), |this| {
            this.wrap_expression(arena, left)
        });

        let right_value = self.not_in_tail_position(Some(Ordering::Strict), |this| {
            this.negate_bool_expression(arena, right)
        });

        let right_check = docvec![
            arena,
            LINE_DOCUMENT,
            "if (",
            right_value.nest(arena, INDENT),
            ") {",
            docvec![
                arena,
                LINE_DOCUMENT,
                self.throw_error(arena, "assert", message, location, fields)
            ]
            .nest(arena, INDENT),
            LINE_DOCUMENT,
            "}",
        ];

        docvec![
            arena,
            self.source_map_tracker(arena, location.start),
            "if (",
            left_value.nest(arena, INDENT),
            ") {",
            right_check.nest(arena, INDENT),
            LINE_DOCUMENT,
            "} else {",
            docvec![
                arena,
                LINE_DOCUMENT,
                self.throw_error(
                    arena,
                    "assert",
                    message,
                    location,
                    fields_if_short_circuiting
                )
            ]
            .nest(arena, INDENT),
            LINE_DOCUMENT,
            "}"
        ]
    }

    /// Similar to `&&`, `||` is also short-circuiting in Gleam. However, if `||`
    /// short-circuits, that's because the first expression evaluated to `true`,
    /// meaning the whole assertion succeeds. This allows us to directly use the
    /// `||` operator in JavaScript.
    ///
    /// The only difference is that due to the nature of `||`, if the assertion fails,
    /// we know that both sides must have evaluated to `false`, so we don't
    /// need to store the values of them in variables beforehand.
    fn assert_or(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        message: &Document<'a, 'doc>,
        location: SrcSpan,
    ) -> Document<'a, 'doc> {
        let fields = vec![
            ("kind", string(arena, "binary_operator")),
            ("operator", string(arena, "||")),
            (
                "left",
                self.asserted_expression(
                    arena,
                    AssertExpression::from_expression(left),
                    Some("false".to_doc(arena)),
                    left.location(),
                ),
            ),
            (
                "right",
                self.asserted_expression(
                    arena,
                    AssertExpression::from_expression(right),
                    Some("false".to_doc(arena)),
                    right.location(),
                ),
            ),
            ("start", location.start.to_doc(arena)),
            ("end", right.location().end.to_doc(arena)),
            ("expression_start", left.location().start.to_doc(arena)),
        ];

        let left_value = self.not_in_tail_position(Some(Ordering::Loose), |this| {
            this.child_expression(arena, left)
        });

        let right_value = self.not_in_tail_position(Some(Ordering::Strict), |this| {
            this.child_expression(arena, right)
        });

        docvec![
            arena,
            LINE_DOCUMENT,
            self.source_map_tracker(arena, location.start),
            "if (",
            docvec![arena, "!(", left_value, " || ", right_value, ")"].nest(arena, INDENT),
            ") {",
            docvec![
                arena,
                LINE_DOCUMENT,
                self.throw_error(arena, "assert", message, location, fields)
            ]
            .nest(arena, INDENT),
            LINE_DOCUMENT,
            "}",
        ]
    }

    fn assign_to_variable(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        value: &'a TypedExpr,
    ) -> Document<'a, 'doc> {
        if let TypedExpr::Var { .. } = value {
            self.expression(arena, value)
        } else {
            let value = self.wrap_expression(arena, value);
            let variable = self.next_local_var(&ASSIGNMENT_VAR.into());
            let assignment = docvec![arena, "let ", variable.clone(), " = ", value, ";"];
            self.statement_level.push(assignment);
            variable.to_doc(arena)
        }
    }

    fn asserted_expression(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        kind: AssertExpression,
        value: Option<Document<'a, 'doc>>,
        location: SrcSpan,
    ) -> Document<'a, 'doc> {
        let kind = match kind {
            AssertExpression::Literal => string(arena, "literal"),
            AssertExpression::Expression => string(arena, "expression"),
            AssertExpression::Unevaluated => string(arena, "unevaluated"),
        };

        let start = location.start.to_doc(arena);
        let end = location.end.to_doc(arena);
        let items = if let Some(value) = value {
            vec![
                ("kind", kind),
                ("value", value),
                ("start", start),
                ("end", end),
            ]
        } else {
            vec![("kind", kind), ("start", start), ("end", end)]
        };

        wrap_object(
            arena,
            items
                .into_iter()
                .map(|(key, value)| (key.to_doc(arena), Some(value))),
        )
    }

    fn tuple(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        elements: &'a [TypedExpr],
    ) -> Document<'a, 'doc> {
        self.not_in_tail_position(Some(Ordering::Strict), |this| {
            array(
                arena,
                elements
                    .iter()
                    .map(|element| this.wrap_expression(arena, element)),
            )
        })
    }

    fn call(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        fun: &'a TypedExpr,
        arguments: &'a [TypedCallArg],
    ) -> Document<'a, 'doc> {
        let arguments = arguments
            .iter()
            .map(|element| {
                self.not_in_tail_position(Some(Ordering::Strict), |this| {
                    this.wrap_expression(arena, &element.value)
                })
            })
            .collect_vec();

        self.call_with_doc_arguments(arena, fun, arguments)
    }

    fn call_with_doc_arguments(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        fun: &'a TypedExpr,
        arguments: Vec<Document<'a, 'doc>>,
    ) -> Document<'a, 'doc> {
        match fun {
            // Qualified record construction
            TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Record { name, .. },
                module_alias,
                ..
            } => self.wrap_return(
                arena,
                construct_record(arena, Some(module_alias), name, arguments),
            ),

            // Record construction
            TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        variant: ValueConstructorVariant::Record { .. },
                        type_,
                        ..
                    },
                name,
                ..
            } => {
                if type_.is_result_constructor() {
                    if name == "Ok" {
                        self.tracker.ok_used = true;
                    } else if name == "Error" {
                        self.tracker.error_used = true;
                    }
                }
                self.wrap_return(arena, construct_record(arena, None, name, arguments))
            }

            // Tail call optimisation. If we are calling the current function
            // and we are in tail position we can avoid creating a new stack
            // frame, enabling recursion with constant memory usage.
            TypedExpr::Var { name, .. }
                if self.function_name == *name
                    && self.current_function.can_recurse()
                    && self.function_position.is_tail()
                    && self.current_scope.counter(name) == Some(0) =>
            {
                let mut docs = Vec::with_capacity(arguments.len() * 4);
                // Record that tail recursion is happening so that we know to
                // render the loop at the top level of the function.
                self.tail_recursion_used = true;

                for (i, (element, argument)) in arguments
                    .into_iter()
                    .zip(&self.function_arguments)
                    .enumerate()
                {
                    if i != 0 {
                        docs.push(LINE_DOCUMENT);
                    }
                    // Create an assignment for each variable created by the function arguments
                    if let Some(name) = argument {
                        docs.push("loop$".to_doc(arena));
                        docs.push(name.to_doc(arena));
                        docs.push(" = ".to_doc(arena));
                    }
                    // Render the value given to the function. Even if it is not
                    // assigned we still render it because the expression may
                    // have some side effects.
                    docs.push(element);
                    docs.push(";".to_doc(arena));
                }
                arena.concat(docs)
            }

            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
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
                let fun = self.not_in_tail_position(None, |this| -> Document<'_, '_> {
                    let is_fn_literal = matches!(fun, TypedExpr::Fn { .. });
                    let fun = this.wrap_expression(arena, fun);
                    if is_fn_literal {
                        docvec![arena, "(", fun, ")"]
                    } else {
                        fun
                    }
                });
                let arguments = call_arguments(arena, arguments);
                self.wrap_return(arena, docvec![arena, fun, arguments])
            }
        }
    }

    fn fn_(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        arguments: &'a [TypedArg],
        body: &'a [TypedStatement],
        kind: &FunctionLiteralKind,
    ) -> Document<'a, 'doc> {
        // New function, this is now the tail position
        let function_position = std::mem::replace(&mut self.function_position, Position::Tail);
        let scope_position = std::mem::replace(&mut self.scope_position, Position::Tail);

        // And there's a new scope
        let scope = self.current_scope.clone();
        for name in arguments.iter().flat_map(Arg::get_variable_name) {
            self.current_scope.set_counter(name, 0);
        }

        // This is a new function so track that so that we don't
        // mistakenly trigger tail call optimisation
        let mut current_function = CurrentFunction::Anonymous;
        std::mem::swap(&mut self.current_function, &mut current_function);

        // Generate the function body
        let result = self.statements(arena, body);

        // Reset function name, scope, and tail position tracking
        self.function_position = function_position;
        self.scope_position = scope_position;
        self.current_scope = scope;
        std::mem::swap(&mut self.current_function, &mut current_function);

        let mut docs = EMPTY_DOCUMENT;

        // If the function is a use then we need to add a source map tracker
        // before the result to denote that the function is created by the use
        if let FunctionLiteralKind::Use { location } = kind {
            docs = docs.append(arena, self.source_map_tracker(arena, location.start));
        }
        docs = docs.append(arena, fun_arguments(arena, arguments, false));
        docs = docs.append(arena, " => {".to_doc(arena));
        docs = docs.append(arena, arena.break_("", " "));
        docs = docs.append(arena, result);

        docvec![
            arena,
            docs.nest(arena, INDENT)
                .append(arena, arena.break_("", " "))
                .group(arena),
            "}",
        ]
    }

    fn record_access(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        record: &'a TypedExpr,
        label: &'a str,
    ) -> Document<'a, 'doc> {
        self.not_in_tail_position(None, |this| {
            let record = this.wrap_expression(arena, record);
            docvec![arena, record, ".", maybe_escape_property(label)]
        })
    }

    fn positional_access(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        record: &'a TypedExpr,
        index: u64,
    ) -> Document<'a, 'doc> {
        self.not_in_tail_position(None, |this| {
            let record = this.wrap_expression(arena, record);
            docvec![arena, record, "[", index, "]"]
        })
    }

    fn record_update(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        updated_record_assigned_name: &'a Option<EcoString>,
        updated_record: &'a TypedExpr,
        constructor: &'a TypedExpr,
        arguments: &'a [TypedCallArg],
    ) -> Document<'a, 'doc> {
        match updated_record_assigned_name.as_ref() {
            Some(name) => {
                docvec![
                    arena,
                    self.not_in_tail_position(None, |this| this.simple_variable_assignment(
                        arena,
                        name,
                        updated_record,
                        updated_record.location(),
                    )),
                    LINE_DOCUMENT,
                    self.call(arena, constructor, arguments),
                ]
            }
            None => self.call(arena, constructor, arguments),
        }
    }

    fn tuple_index(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        tuple: &'a TypedExpr,
        index: u64,
    ) -> Document<'a, 'doc> {
        self.not_in_tail_position(None, |this| {
            let tuple = this.wrap_expression(arena, tuple);
            docvec![arena, tuple, eco_format!("[{index}]")]
        })
    }

    fn bin_op(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: &'a BinOp,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
    ) -> Document<'a, 'doc> {
        match name {
            BinOp::And => self.print_bin_op(arena, left, right, "&&"),
            BinOp::Or => self.print_bin_op(arena, left, right, "||"),
            BinOp::LtInt | BinOp::LtFloat => self.print_bin_op(arena, left, right, "<"),
            BinOp::LtEqInt | BinOp::LtEqFloat => self.print_bin_op(arena, left, right, "<="),
            BinOp::Eq => self.equal(arena, left, right, true),
            BinOp::NotEq => self.equal(arena, left, right, false),
            BinOp::GtInt | BinOp::GtFloat => self.print_bin_op(arena, left, right, ">"),
            BinOp::GtEqInt | BinOp::GtEqFloat => self.print_bin_op(arena, left, right, ">="),
            BinOp::Concatenate | BinOp::AddInt | BinOp::AddFloat => {
                self.print_bin_op(arena, left, right, "+")
            }
            BinOp::SubInt | BinOp::SubFloat => self.print_bin_op(arena, left, right, "-"),
            BinOp::MultInt | BinOp::MultFloat => self.print_bin_op(arena, left, right, "*"),
            BinOp::RemainderInt => self.remainder_int(arena, left, right),
            BinOp::DivInt => self.div_int(arena, left, right),
            BinOp::DivFloat => self.div_float(arena, left, right),
        }
    }

    fn div_int(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
    ) -> Document<'a, 'doc> {
        let left_doc = self.not_in_tail_position(Some(Ordering::Strict), |this| {
            this.child_expression(arena, left)
        });
        let right_doc = self.not_in_tail_position(Some(Ordering::Strict), |this| {
            this.child_expression(arena, right)
        });

        // If we have a constant value divided by zero then it's safe to replace
        // it directly with 0.
        if left.is_literal() && right.is_zero_compile_time_number() {
            "0".to_doc(arena)
        } else if right.is_non_zero_compile_time_number() {
            let division = if let TypedExpr::BinOp { .. } = left {
                docvec![arena, left_doc.surround(arena, "(", ")"), " / ", right_doc]
            } else {
                docvec![arena, left_doc, " / ", right_doc]
            };
            docvec![
                arena,
                "globalThis.Math.trunc",
                wrap_arguments(arena, [division])
            ]
        } else {
            self.tracker.int_division_used = true;
            docvec![
                arena,
                "divideInt",
                wrap_arguments(arena, [left_doc, right_doc])
            ]
        }
    }

    fn remainder_int(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
    ) -> Document<'a, 'doc> {
        let left_doc = self.not_in_tail_position(Some(Ordering::Strict), |this| {
            this.child_expression(arena, left)
        });
        let right_doc = self.not_in_tail_position(Some(Ordering::Strict), |this| {
            this.child_expression(arena, right)
        });

        // If we have a constant value divided by zero then it's safe to replace
        // it directly with 0.
        if left.is_literal() && right.is_zero_compile_time_number() {
            "0".to_doc(arena)
        } else if right.is_non_zero_compile_time_number() {
            if let TypedExpr::BinOp { .. } = left {
                docvec![arena, left_doc.surround(arena, "(", ")"), " % ", right_doc]
            } else {
                docvec![arena, left_doc, " % ", right_doc]
            }
        } else {
            self.tracker.int_remainder_used = true;
            docvec![
                arena,
                "remainderInt",
                wrap_arguments(arena, [left_doc, right_doc])
            ]
        }
    }

    fn div_float(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
    ) -> Document<'a, 'doc> {
        let left_doc = self.not_in_tail_position(Some(Ordering::Strict), |this| {
            this.child_expression(arena, left)
        });
        let right_doc = self.not_in_tail_position(Some(Ordering::Strict), |this| {
            this.child_expression(arena, right)
        });

        // If we have a constant value divided by zero then it's safe to replace
        // it directly with 0.
        if left.is_literal() && right.is_zero_compile_time_number() {
            "0.0".to_doc(arena)
        } else if right.is_non_zero_compile_time_number() {
            if let TypedExpr::BinOp { .. } = left {
                docvec![arena, left_doc.surround(arena, "(", ")"), " / ", right_doc]
            } else {
                docvec![arena, left_doc, " / ", right_doc]
            }
        } else {
            self.tracker.float_division_used = true;
            docvec![
                arena,
                "divideFloat",
                wrap_arguments(arena, [left_doc, right_doc])
            ]
        }
    }

    fn equal(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        should_be_equal: bool,
    ) -> Document<'a, 'doc> {
        // If it is a simple scalar type then we can use JS' reference identity
        if is_js_scalar(left.type_()) {
            let left_doc = self.not_in_tail_position(Some(Ordering::Strict), |this| {
                this.child_expression(arena, left)
            });
            let right_doc = self.not_in_tail_position(Some(Ordering::Strict), |this| {
                this.child_expression(arena, right)
            });
            let operator = if should_be_equal { " === " } else { " !== " };
            return docvec![arena, left_doc, operator, right_doc];
        }

        // For comparison with singleton custom types, ie, one with no fields.
        // If you have some code like this
        // ```gleam
        //  pub type Wibble {
        //    Wibble
        //    Wobble
        //  }
        //
        //  pub fn is_wibble(w: Wibble) -> Bool {
        //    w == Wibble
        //  }
        // ```
        // Instead of `isEqual(w, new Wibble())`, generate `w instanceof Wibble`
        // because the first approach needs to construct a new Wibble, and then call the isEqual function,
        // which supports any shape of data, and so does a lot of extra logic which isn't necessary.

        if let Some(doc) = self.singleton_variant_equality(arena, left, right, should_be_equal) {
            return doc;
        }

        if let Some(doc) = self.singleton_variant_equality(arena, right, left, should_be_equal) {
            return doc;
        }

        // Other types must be compared using structural equality
        let left = self.not_in_tail_position(Some(Ordering::Strict), |this| {
            this.wrap_expression(arena, left)
        });
        let right = self.not_in_tail_position(Some(Ordering::Strict), |this| {
            this.wrap_expression(arena, right)
        });

        self.prelude_equal_call(arena, should_be_equal, left, right)
    }

    fn singleton_variant_equality(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        should_be_equal: bool,
    ) -> Option<Document<'a, 'doc>> {
        match right {
            TypedExpr::Var {
                name,
                constructor:
                    ValueConstructor {
                        variant:
                            ValueConstructorVariant::Record {
                                arity: 0,
                                name: variant_name,
                                ..
                            },
                        ..
                    },
                ..
            } => {
                let left_doc = self.not_in_tail_position(Some(Ordering::Strict), |this| {
                    this.wrap_expression(arena, left)
                });
                Some(self.singleton_equal(
                    arena,
                    left_doc,
                    None,
                    name.clone(),
                    should_be_equal,
                    variant_name.clone(),
                    right.type_(),
                ))
            }
            TypedExpr::ModuleSelect {
                module_alias,
                constructor: ModuleValueConstructor::Record { arity: 0, name, .. },
                ..
            } => {
                let left_doc = self.not_in_tail_position(Some(Ordering::Strict), |this| {
                    this.wrap_expression(arena, left)
                });
                Some(self.singleton_equal(
                    arena,
                    left_doc,
                    Some(module_alias),
                    name.clone(),
                    should_be_equal,
                    name.clone(),
                    right.type_(),
                ))
            }
            // Empty lists are implemented as a variant with no fields, so we can
            // use `instanceof` for a faster check.
            TypedExpr::List { elements, .. } if elements.is_empty() => {
                let left_doc = self.not_in_tail_position(Some(Ordering::Strict), |this| {
                    this.wrap_expression(arena, left)
                });
                self.tracker.list_empty_class_used = true;
                Some(self.singleton_equal(
                    arena,
                    left_doc,
                    None,
                    "$Empty".into(),
                    should_be_equal,
                    "Empty".into(),
                    right.type_(),
                ))
            }
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
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
            | TypedExpr::Invalid { .. } => None,
        }
    }

    fn singleton_equal(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        value: Document<'a, 'doc>,
        module: Option<&'a str>,
        name: EcoString,
        should_be_equal: bool,
        variant_name: EcoString,
        type_: Arc<Type>,
    ) -> Document<'a, 'doc> {
        // If we're using this variant unqualified, register it as used so that
        // we know to import it. This `instanceof` check only happens if the
        // variant has no fields, so we don't need to check that here.
        if module.is_none()
            && let Some((package, module, type_name)) = type_.named_type_name_and_package()
        {
            _ = self
                .tracker
                .variants_used_in_instanceof
                .insert(TypeVariant {
                    package,
                    module,
                    type_name,
                    name: variant_name,
                });
        }
        let record = if let Some(module) = module {
            docvec![arena, "$", module, ".", name]
        } else {
            name.to_doc(arena)
        };

        if should_be_equal {
            docvec![arena, value, " instanceof ", record]
        } else {
            docvec![arena, "!(", value, " instanceof ", record, ")"]
        }
    }

    fn equal_with_doc_operands(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        left: Document<'a, 'doc>,
        right: Document<'a, 'doc>,
        type_: Arc<Type>,
        should_be_equal: bool,
    ) -> Document<'a, 'doc> {
        // If it is a simple scalar type then we can use JS' reference identity
        if is_js_scalar(type_) {
            let operator = if should_be_equal { " === " } else { " !== " };
            return docvec![arena, left, operator, right];
        }

        // Other types must be compared using structural equality
        self.prelude_equal_call(arena, should_be_equal, left, right)
    }

    pub(super) fn prelude_equal_call(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        should_be_equal: bool,
        left: Document<'a, 'doc>,
        right: Document<'a, 'doc>,
    ) -> Document<'a, 'doc> {
        // Record that we need to import the prelude's isEqual function into the module
        self.tracker.object_equality_used = true;
        // Construct the call
        let arguments = wrap_arguments(arena, [left, right]);
        let operator = if should_be_equal {
            "isEqual"
        } else {
            "!isEqual"
        };
        docvec![arena, operator, arguments]
    }

    fn print_bin_op(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        op: &'a str,
    ) -> Document<'a, 'doc> {
        let left = self.not_in_tail_position(Some(Ordering::Strict), |this| {
            this.child_expression(arena, left)
        });
        let right = self.not_in_tail_position(Some(Ordering::Strict), |this| {
            this.child_expression(arena, right)
        });
        docvec![arena, left, " ", op, " ", right]
    }

    pub(super) fn bin_op_with_doc_operands(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        name: BinOp,
        left: Document<'a, 'doc>,
        right: Document<'a, 'doc>,
        type_: &Arc<Type>,
    ) -> Document<'a, 'doc> {
        match name {
            BinOp::And => docvec![arena, left, " && ", right],
            BinOp::Or => docvec![arena, left, " || ", right],
            BinOp::LtInt | BinOp::LtFloat => docvec![arena, left, " < ", right],
            BinOp::LtEqInt | BinOp::LtEqFloat => docvec![arena, left, " <= ", right],
            BinOp::Eq => self.equal_with_doc_operands(arena, left, right, type_.clone(), true),
            BinOp::NotEq => self.equal_with_doc_operands(arena, left, right, type_.clone(), false),
            BinOp::GtInt | BinOp::GtFloat => docvec![arena, left, " > ", right],
            BinOp::GtEqInt | BinOp::GtEqFloat => docvec![arena, left, " >= ", right],
            BinOp::Concatenate | BinOp::AddInt | BinOp::AddFloat => {
                docvec![arena, left, " + ", right]
            }
            BinOp::SubInt | BinOp::SubFloat => docvec![arena, left, " - ", right],
            BinOp::MultInt | BinOp::MultFloat => docvec![arena, left, " * ", right],
            BinOp::RemainderInt => {
                self.tracker.int_remainder_used = true;
                docvec![arena, "remainderInt", wrap_arguments(arena, [left, right])]
            }
            BinOp::DivInt => {
                self.tracker.int_division_used = true;
                docvec![arena, "divideInt", wrap_arguments(arena, [left, right])]
            }
            BinOp::DivFloat => {
                self.tracker.float_division_used = true;
                docvec![arena, "divideFloat", wrap_arguments(arena, [left, right])]
            }
        }
    }

    fn todo(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        message: Option<&'a TypedExpr>,
        location: &'a SrcSpan,
    ) -> Document<'a, 'doc> {
        let message = match message {
            Some(m) => self.not_in_tail_position(None, |this| this.wrap_expression(arena, m)),
            None => string(
                arena,
                "`todo` expression evaluated. This code has not yet been implemented.",
            ),
        };
        self.throw_error(arena, "todo", &message, *location, vec![])
    }

    fn panic(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        location: &'a SrcSpan,
        message: Option<&'a TypedExpr>,
    ) -> Document<'a, 'doc> {
        let message = match message {
            Some(m) => self.not_in_tail_position(None, |this| this.wrap_expression(arena, m)),
            None => string(arena, "`panic` expression evaluated."),
        };
        self.throw_error(arena, "panic", &message, *location, vec![])
    }

    pub(crate) fn throw_error<Fields>(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        error_name: &'a str,
        message: &Document<'a, 'doc>,
        location: SrcSpan,
        fields: Fields,
    ) -> Document<'a, 'doc>
    where
        Fields: IntoIterator<Item = (&'a str, Document<'a, 'doc>)>,
    {
        self.tracker.make_error_used = true;
        let module = self
            .module_name
            .clone()
            .to_doc(arena)
            .surround(arena, '"', '"');
        let function = self
            .function_name
            .clone()
            .to_doc(arena)
            .surround(arena, "\"", "\"");
        let line = self.line_numbers.line_number(location.start).to_doc(arena);
        let fields = wrap_object(
            arena,
            fields.into_iter().map(|(k, v)| (k.to_doc(arena), Some(v))),
        );

        docvec![
            arena,
            self.source_map_tracker(arena, location.start),
            "throw makeError",
            wrap_arguments(
                arena,
                [
                    string(arena, error_name),
                    "FILEPATH".to_doc(arena),
                    module,
                    line,
                    function,
                    message.clone(),
                    fields
                ]
            ),
        ]
    }

    fn module_select(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        module: &'a str,
        label: &'a EcoString,
        constructor: &'a ModuleValueConstructor,
    ) -> Document<'a, 'doc> {
        match constructor {
            ModuleValueConstructor::Fn { .. } | ModuleValueConstructor::Constant { .. } => {
                docvec![arena, "$", module, ".", maybe_escape_identifier(label)]
            }

            ModuleValueConstructor::Record {
                name, arity, type_, ..
            } => self.record_constructor(arena, type_.clone(), Some(module), name, name, *arity),
        }
    }

    fn echo(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: Document<'a, 'doc>,
        message: Option<&'a TypedExpr>,
        location: &'a SrcSpan,
    ) -> Document<'a, 'doc> {
        self.tracker.echo_used = true;

        let message = match message {
            Some(message) => self.not_in_tail_position(Some(Ordering::Strict), |this| {
                this.wrap_expression(arena, message)
            }),
            None => "undefined".to_doc(arena),
        };

        let echo_arguments = call_arguments(
            arena,
            vec![
                expression,
                message,
                self.src_path.clone().to_doc(arena),
                self.line_numbers.line_number(location.start).to_doc(arena),
            ],
        );
        self.wrap_return(arena, docvec![arena, "echo", echo_arguments])
    }

    pub(crate) fn constant_expression(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        context: Context,
        expression: &'a TypedConstant,
    ) -> Document<'a, 'doc> {
        match expression {
            Constant::Int { value, .. } => int(arena, value),
            Constant::Float { value, .. } => float(arena, value),
            Constant::String { value, .. } => string(arena, value),
            Constant::Tuple { elements, .. } => array(
                arena,
                elements
                    .iter()
                    .map(|element| self.constant_expression(arena, context, element)),
            ),

            Constant::List { elements, tail, .. } => {
                if tail.is_none() && elements.is_empty() {
                    return self.empty_list(arena);
                }

                self.tracker.list_used = true;
                let list = match tail {
                    // There's no tail in the list, we join all the elements and
                    // call it a day.
                    None => list(
                        arena,
                        elements
                            .iter()
                            .map(|element| self.constant_expression(arena, context, element)),
                    ),

                    Some(tail) => match tail.list_elements() {
                        // There's a tail in the list whose elements are all
                        // known at compile time. In this case we replace the
                        // tail with those elements and create a single flat
                        // list.
                        Some(tail_elements) => list(
                            arena,
                            elements
                                .iter()
                                .chain(tail_elements)
                                .map(|element| self.constant_expression(arena, context, element)),
                        ),
                        // There's a tail in the list but we can't really tell
                        // what its elements are at compile time. This means we
                        // have to prepend to this list.
                        None => {
                            self.tracker.prepend_used = true;
                            let tail = self.constant_expression(arena, context, tail);
                            prepend(
                                arena,
                                elements.iter().map(|element| {
                                    self.constant_expression(arena, context, element)
                                }),
                                tail,
                            )
                        }
                    },
                };
                match context {
                    Context::Constant => docvec![arena, "/* @__PURE__ */ ", list],
                    Context::Guard => list,
                }
            }

            Constant::Record { type_, name, .. } if type_.is_bool() && name == "True" => {
                "true".to_doc(arena)
            }
            Constant::Record { type_, name, .. } if type_.is_bool() && name == "False" => {
                "false".to_doc(arena)
            }
            Constant::Record { type_, .. } if type_.is_nil() => "undefined".to_doc(arena),

            Constant::Record {
                arguments,
                module,
                name,
                type_,
                ..
            } => {
                let tag = expression
                    .constant_record_tag()
                    .expect("record without inferred constructor made it to code generation");

                if module.is_none() && type_.is_result() {
                    if tag == "Ok" {
                        self.tracker.ok_used = true;
                    } else {
                        self.tracker.error_used = true;
                    }
                }

                // If there's no arguments, then this is either a constructor
                // which takes arguments being referenced rather than called,
                // or a variant with no fields at all.
                if arguments.is_none() {
                    // If the constructor is not a function that takes arguments,
                    // it effectively has zero arity.
                    let arity = type_.fn_arity().unwrap_or(0) as u16;
                    return self.record_constructor(
                        arena,
                        type_.clone(),
                        module.as_ref().map(|(name, _)| name.as_str()),
                        &tag,
                        name,
                        arity,
                    );
                }

                // Otherwise we're always constructing a record! Record updates
                // are fully expanded during type checking, so we just need to
                // handle the arguments here.
                let field_values = arguments
                    .iter()
                    .flatten()
                    .map(|argument| self.constant_expression(arena, context, &argument.value))
                    .collect_vec();

                let constructor = construct_record(
                    arena,
                    module.as_ref().map(|(module, _)| module.as_str()),
                    name,
                    field_values,
                );
                match context {
                    Context::Constant => docvec![arena, "/* @__PURE__ */ ", constructor],
                    Context::Guard => constructor,
                }
            }
            Constant::BitArray { segments, .. } => {
                let bit_array = self.constant_bit_array(arena, segments, context);
                match context {
                    Context::Constant => docvec![arena, "/* @__PURE__ */ ", bit_array],
                    Context::Guard => bit_array,
                }
            }

            Constant::Var { name, module, .. } => {
                match (module, context) {
                    (None, Context::Guard) => self.local_var(name).to_doc(arena),
                    (None, Context::Constant) => maybe_escape_identifier(name).to_doc(arena),
                    (Some((module, _)), _) => {
                        // JS keywords can be accessed here, but we must escape anyway
                        // as we escape when exporting such names in the first place,
                        // and the imported name has to match the exported name.
                        docvec![arena, "$", module, ".", maybe_escape_identifier(name)]
                    }
                }
            }

            Constant::StringConcatenation { left, right, .. } => {
                let left = self.constant_expression(arena, context, left);
                let right = self.constant_expression(arena, context, right);
                docvec![arena, left, " + ", right]
            }

            Constant::RecordUpdate { .. } => {
                panic!("record updates should not reach code generation")
            }
            Constant::Todo { .. } => {
                panic!("todo constants should not reach code generation")
            }
            Constant::Invalid { .. } => {
                panic!("invalid constants should not reach code generation")
            }
        }
    }

    fn constant_bit_array(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        segments: &'a [TypedConstantBitArraySegment],
        context: Context,
    ) -> Document<'a, 'doc> {
        self.tracker.bit_array_literal_used = true;
        let segments_array = array(
            arena,
            segments.iter().map(|segment| {
                let value = match context {
                    Context::Constant => self.constant_expression(arena, context, &segment.value),
                    Context::Guard => self.guard_constant_expression(arena, &segment.value),
                };

                let details = self.constant_bit_array_segment_details(arena, segment, context);

                match details.type_ {
                    BitArraySegmentType::BitArray => {
                        if segment.size().is_some() {
                            self.tracker.bit_array_slice_used = true;
                            docvec![arena, "bitArraySlice(", value, ", 0, ", details.size, ")"]
                        } else {
                            value
                        }
                    }
                    BitArraySegmentType::Int => {
                        match (details.size_value, segment.value.as_ref()) {
                            (Some(size_value), Constant::Int { int_value, .. })
                                if size_value <= SAFE_INT_SEGMENT_MAX_SIZE.into()
                                    && (&size_value % BigInt::from(8) == BigInt::ZERO) =>
                            {
                                let bytes = bit_array_segment_int_value_to_bytes(
                                    int_value.clone(),
                                    size_value,
                                    segment.endianness(),
                                );

                                u8_slice(arena, &bytes)
                            }

                            (Some(size_value), _) if size_value == 8.into() => value,

                            (Some(size_value), _) if size_value <= 0.into() => EMPTY_DOCUMENT,

                            _ => {
                                self.tracker.sized_integer_segment_used = true;
                                let size = details.size;
                                let is_big = bool(segment.endianness().is_big());
                                docvec![arena, "sizedInt(", value, ", ", size, ", ", is_big, ")"]
                            }
                        }
                    }
                    BitArraySegmentType::Float => {
                        self.tracker.float_bit_array_segment_used = true;
                        let size = details.size;
                        let is_big = bool(details.endianness.is_big());
                        docvec![arena, "sizedFloat(", value, ", ", size, ", ", is_big, ")"]
                    }
                    BitArraySegmentType::String(StringEncoding::Utf8) => {
                        self.tracker.string_bit_array_segment_used = true;
                        docvec![arena, "stringBits(", value, ")"]
                    }
                    BitArraySegmentType::String(StringEncoding::Utf16) => {
                        self.tracker.string_utf16_bit_array_segment_used = true;
                        let is_big = bool(details.endianness.is_big());
                        docvec![arena, "stringToUtf16(", value, ", ", is_big, ")"]
                    }
                    BitArraySegmentType::String(StringEncoding::Utf32) => {
                        self.tracker.string_utf32_bit_array_segment_used = true;
                        let is_big = bool(details.endianness.is_big());
                        docvec![arena, "stringToUtf32(", value, ", ", is_big, ")"]
                    }
                    BitArraySegmentType::UtfCodepoint(StringEncoding::Utf8) => {
                        self.tracker.codepoint_bit_array_segment_used = true;
                        docvec![arena, "codepointBits(", value, ")"]
                    }
                    BitArraySegmentType::UtfCodepoint(StringEncoding::Utf16) => {
                        self.tracker.codepoint_utf16_bit_array_segment_used = true;
                        let is_big = bool(details.endianness.is_big());
                        docvec![arena, "codepointToUtf16(", value, ", ", is_big, ")"]
                    }
                    BitArraySegmentType::UtfCodepoint(StringEncoding::Utf32) => {
                        self.tracker.codepoint_utf32_bit_array_segment_used = true;
                        let is_big = bool(details.endianness.is_big());
                        docvec![arena, "codepointToUtf32(", value, ", ", is_big, ")"]
                    }
                }
            }),
        );

        docvec![arena, "toBitArray(", segments_array, ")"]
    }

    fn constant_bit_array_segment_details(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        segment: &'a TypedConstantBitArraySegment,
        context: Context,
    ) -> BitArraySegmentDetails<'a, 'doc> {
        let size = segment.size();
        let unit = segment.unit();
        let (size_value, size) = match size {
            Some(Constant::Int { int_value, .. }) => {
                let size_value = int_value * unit;
                let size = eco_format!("{}", size_value).to_doc(arena);
                (Some(size_value), size)
            }

            Some(size) => {
                let mut size = match context {
                    Context::Constant => self.constant_expression(arena, context, size),
                    Context::Guard => self.guard_constant_expression(arena, size),
                };
                if unit != 1 {
                    size = size
                        .group(arena)
                        .append(arena, " * ".to_doc(arena).append(arena, unit.to_doc(arena)));
                }

                (None, size)
            }

            None => {
                let size_value: usize = if segment.type_.is_int() { 8 } else { 64 };
                (Some(BigInt::from(size_value)), size_value.to_doc(arena))
            }
        };

        let type_ = BitArraySegmentType::from_segment(segment);

        BitArraySegmentDetails {
            type_,
            size,
            size_value,
            endianness: segment.endianness(),
        }
    }

    pub(crate) fn guard(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        guard: &'a TypedClauseGuard,
    ) -> Document<'a, 'doc> {
        match guard {
            ClauseGuard::Invalid { .. } => unreachable!("invalid guard made it to code generation"),

            ClauseGuard::Block { value, .. } => self.guard(arena, value).surround(arena, "(", ")"),

            ClauseGuard::BinaryOperator {
                left,
                right,
                operator,
                ..
            } => {
                let operator = match operator {
                    BinOp::Eq if is_js_scalar(left.type_()) => "===",
                    BinOp::NotEq if is_js_scalar(left.type_()) => "!==",
                    BinOp::Eq | BinOp::NotEq => {
                        let should_be_equal = *operator == BinOp::Eq;

                        // Handle singleton equality optimization for guards
                        if let Some(doc) = self.singleton_variant_guard_equality(
                            arena,
                            left,
                            right,
                            should_be_equal,
                        ) {
                            return doc;
                        }

                        if let Some(doc) = self.singleton_variant_guard_equality(
                            arena,
                            right,
                            left,
                            should_be_equal,
                        ) {
                            return doc;
                        }

                        let left_doc = self.guard(arena, left);
                        let right_doc = self.guard(arena, right);
                        return self.prelude_equal_call(
                            arena,
                            should_be_equal,
                            left_doc,
                            right_doc,
                        );
                    }

                    BinOp::GtFloat | BinOp::GtInt => ">",
                    BinOp::GtEqFloat | BinOp::GtEqInt => ">=",
                    BinOp::LtFloat | BinOp::LtInt => "<",
                    BinOp::LtEqFloat | BinOp::LtEqInt => "<=",

                    BinOp::AddFloat | BinOp::AddInt | BinOp::Concatenate => "+",
                    BinOp::SubFloat | BinOp::SubInt => "-",
                    BinOp::MultFloat | BinOp::MultInt => "*",

                    BinOp::DivFloat => {
                        self.tracker.float_division_used = true;

                        return docvec![
                            arena,
                            "divideFloat",
                            wrap_arguments(
                                arena,
                                [self.guard(arena, left), self.guard(arena, right)]
                            )
                        ];
                    }

                    BinOp::DivInt => {
                        self.tracker.int_division_used = true;
                        return docvec![
                            arena,
                            "divideInt",
                            wrap_arguments(
                                arena,
                                [self.guard(arena, left), self.guard(arena, right)]
                            )
                        ];
                    }

                    BinOp::RemainderInt => {
                        self.tracker.int_remainder_used = true;
                        return docvec![
                            arena,
                            "remainderInt",
                            wrap_arguments(
                                arena,
                                [self.guard(arena, left), self.guard(arena, right)]
                            )
                        ];
                    }

                    BinOp::And => "&&",
                    BinOp::Or => "||",
                };

                let left_document = self.wrapped_guard(arena, left);
                let right_document = self.wrapped_guard(arena, right);

                docvec![arena, left_document, " ", operator, " ", right_document]
            }

            ClauseGuard::Var { name, .. } => self.local_var(name).to_doc(arena),

            ClauseGuard::TupleIndex { tuple, index, .. } => {
                docvec![arena, self.guard(arena, tuple,), "[", *index, "]"]
            }

            ClauseGuard::FieldAccess {
                label, container, ..
            } => docvec![
                arena,
                self.guard(arena, container),
                ".",
                maybe_escape_property(label)
            ],

            ClauseGuard::ModuleSelect {
                module_alias,
                label,
                ..
            } => docvec![arena, "$", module_alias, ".", label],

            ClauseGuard::Not { expression, .. } => {
                docvec![arena, "!", self.guard(arena, expression,)]
            }

            ClauseGuard::Constant(constant) => self.guard_constant_expression(arena, constant),
        }
    }

    fn singleton_variant_guard_equality(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        left: &'a TypedClauseGuard,
        right: &'a TypedClauseGuard,
        should_be_equal: bool,
    ) -> Option<Document<'a, 'doc>> {
        match right {
            ClauseGuard::Constant(Constant::Record {
                record_constructor: Some(constructor),
                module,
                name,
                ..
            }) if let ValueConstructorVariant::Record {
                arity: 0,
                name: variant_name,
                ..
            } = &constructor.variant =>
            {
                let left_doc = self.guard(arena, left);
                Some(self.singleton_equal(
                    arena,
                    left_doc,
                    module.as_ref().map(|(module, _)| module.as_str()),
                    name.clone(),
                    should_be_equal,
                    variant_name.clone(),
                    right.type_(),
                ))
            }
            ClauseGuard::Constant(Constant::List {
                elements,
                tail: None,
                ..
            }) if elements.is_empty() => {
                let left_doc = self.guard(arena, left);
                self.tracker.list_empty_class_used = true;
                Some(self.singleton_equal(
                    arena,
                    left_doc,
                    None,
                    "$Empty".into(),
                    should_be_equal,
                    "Empty".into(),
                    right.type_(),
                ))
            }
            ClauseGuard::Block { .. }
            | ClauseGuard::BinaryOperator { .. }
            | ClauseGuard::Not { .. }
            | ClauseGuard::Var { .. }
            | ClauseGuard::TupleIndex { .. }
            | ClauseGuard::FieldAccess { .. }
            | ClauseGuard::ModuleSelect { .. }
            | ClauseGuard::Constant(_)
            | ClauseGuard::Invalid { .. } => None,
        }
    }

    fn wrapped_guard(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        guard: &'a TypedClauseGuard,
    ) -> Document<'a, 'doc> {
        match guard {
            ClauseGuard::Invalid { .. } => unreachable!("invalid guard made it to code generation"),
            ClauseGuard::Var { .. }
            | ClauseGuard::TupleIndex { .. }
            | ClauseGuard::Constant(_)
            | ClauseGuard::Not { .. }
            | ClauseGuard::FieldAccess { .. }
            | ClauseGuard::Block { .. } => self.guard(arena, guard),

            ClauseGuard::BinaryOperator { .. } | ClauseGuard::ModuleSelect { .. } => {
                docvec![arena, "(", self.guard(arena, guard), ")"]
            }
        }
    }

    fn guard_constant_expression(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        expression: &'a TypedConstant,
    ) -> Document<'a, 'doc> {
        match expression {
            Constant::Tuple { elements, .. } => array(
                arena,
                elements
                    .iter()
                    .map(|element| self.guard_constant_expression(arena, element)),
            ),

            Constant::Record { type_, name, .. } if type_.is_bool() && name == "True" => {
                "true".to_doc(arena)
            }
            Constant::Record { type_, name, .. } if type_.is_bool() && name == "False" => {
                "false".to_doc(arena)
            }
            Constant::Record { type_, .. } if type_.is_nil() => "undefined".to_doc(arena),

            Constant::BitArray { segments, .. } => {
                self.constant_bit_array(arena, segments, Context::Guard)
            }

            Constant::Var { name, .. } => self.local_var(name).to_doc(arena),

            Constant::Record { .. }
            | Constant::Int { .. }
            | Constant::Float { .. }
            | Constant::String { .. }
            | Constant::List { .. }
            | Constant::RecordUpdate { .. }
            | Constant::StringConcatenation { .. }
            | Constant::Todo { .. }
            | Constant::Invalid { .. } => {
                self.constant_expression(arena, Context::Guard, expression)
            }
        }
    }

    pub fn source_map_tracker(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        start_index: u32,
    ) -> Document<'a, 'doc> {
        create_cursor_position_observer(
            arena,
            &self.source_map_builder,
            self.line_numbers,
            start_index,
        )
    }

    pub(crate) fn record_constructor(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        type_: Arc<Type>,
        qualifier: Option<&'a str>,
        variant_name: &EcoString,
        name: &'a EcoString,
        arity: u16,
    ) -> Document<'a, 'doc> {
        if qualifier.is_none() && type_.is_result_constructor() {
            if name == "Ok" {
                self.tracker.ok_used = true;
            } else if name == "Error" {
                self.tracker.error_used = true;
            }
        }
        if type_.is_bool() && name == "True" {
            "true".to_doc(arena)
        } else if type_.is_bool() {
            "false".to_doc(arena)
        } else if type_.is_nil() {
            "undefined".to_doc(arena)
        } else if arity == 0
            && let Some((package, module, type_name)) = type_.named_type_name_and_package()
        {
            // If the variant has no fields, return the singleton constant so
            // that all values of the variant are the same underlying reference,
            // and are faster to compare.
            match qualifier {
                Some(module) => docvec![arena, "$", module, ".", type_name, "$", name, "$const"],
                None => {
                    if module != self.module_name {
                        let alias = if name == variant_name {
                            None
                        } else {
                            Some(eco_format!("{}${}$const", type_name, name))
                        };
                        // Since this constant is an implementation detail and not
                        // present in Gleam code, we need to track it so that we
                        // import it, as it doesn't appear directly in the `import`s
                        // in the source code.
                        _ = self.tracker.variant_constants_used.insert(
                            TypeVariant {
                                package,
                                module,
                                type_name: type_name.clone(),
                                name: variant_name.clone(),
                            },
                            alias,
                        );
                    }
                    docvec![arena, type_name, "$", name, "$const"]
                }
            }
        } else {
            let vars = (0..arity).map(|i| eco_format!("var{i}").to_doc(arena));
            let body = docvec![
                arena,
                "return ",
                construct_record(arena, qualifier, name, vars.clone()),
                ";"
            ];
            docvec![
                arena,
                docvec![
                    arena,
                    wrap_arguments(arena, vars),
                    " => {",
                    arena.break_("", " "),
                    body
                ]
                .nest(arena, INDENT)
                .append(arena, arena.break_("", " "))
                .group(arena),
                "}",
            ]
        }
    }
}

#[derive(Clone, Copy)]
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

pub fn int<'a, 'doc>(arena: &'doc DocumentArena<'a, 'doc>, value: &'a str) -> Document<'a, 'doc> {
    eco_string_int(arena, value.into())
}

pub fn eco_string_int<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    value: EcoString,
) -> Document<'a, 'doc> {
    let mut out = EcoString::with_capacity(value.len());

    if value.starts_with('-') {
        out.push('-');
    } else if value.starts_with('+') {
        out.push('+');
    };
    let value = value.trim_start_matches(['+', '-'].as_ref());

    let value = if value.starts_with("0x") {
        out.push_str("0x");
        value.trim_start_matches("0x")
    } else if value.starts_with("0o") {
        out.push_str("0o");
        value.trim_start_matches("0o")
    } else if value.starts_with("0b") {
        out.push_str("0b");
        value.trim_start_matches("0b")
    } else {
        value
    };

    let value = value.trim_start_matches(['0', '_']);
    if value.is_empty() {
        out.push('0');
    }

    out.push_str(value);

    out.to_doc(arena)
}

pub fn float<'a, 'doc>(arena: &'doc DocumentArena<'a, 'doc>, value: &'a str) -> Document<'a, 'doc> {
    let mut out = EcoString::with_capacity(value.len());

    if value.starts_with('-') {
        out.push('-');
    } else if value.starts_with('+') {
        out.push('+');
    };
    let value = value.trim_start_matches(['+', '-'].as_ref());

    let value = value.trim_start_matches(['0', '_']);
    if value.starts_with(['.', 'e', 'E']) {
        out.push('0');
    }
    out.push_str(value);

    out.to_doc(arena)
}

pub fn float_from_value<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    value: f64,
) -> Document<'a, 'doc> {
    if value.is_infinite() {
        if value.is_sign_positive() {
            "Infinity".to_doc(arena)
        } else {
            "-Infinity".to_doc(arena)
        }
    } else if value.is_nan() {
        // NOTE: this case is probably unnecessary, as this function is only
        // invoked with `LiteralFloatValue` values, which cannot be nan.
        "NaN".to_doc(arena)
    } else {
        value.to_doc(arena)
    }
}

/// The context where the constant expression is used, it might be inside a
/// function call, or in the definition of another constant.
///
/// Based on the context we might want to annotate pure function calls as
/// "@__PURE__".
///
#[derive(Debug, Clone, Copy)]
pub enum Context {
    Constant,
    Guard,
}

#[derive(Debug)]
struct BitArraySegmentDetails<'a, 'doc> {
    type_: BitArraySegmentType,
    size: Document<'a, 'doc>,
    /// The size of the bit array segment stored as a BigInt.
    /// This has a value when the segment's size is known at compile time.
    size_value: Option<BigInt>,
    endianness: Endianness,
}

#[derive(Debug, Clone, Copy)]
enum BitArraySegmentType {
    BitArray,
    Int,
    Float,
    String(StringEncoding),
    UtfCodepoint(StringEncoding),
}

impl BitArraySegmentType {
    fn from_segment<Value>(segment: &BitArraySegment<Value, Arc<Type>>) -> Self {
        if segment.type_.is_int() {
            BitArraySegmentType::Int
        } else if segment.type_.is_float() {
            BitArraySegmentType::Float
        } else if segment.type_.is_bit_array() {
            BitArraySegmentType::BitArray
        } else if segment.type_.is_string() {
            let encoding = if segment.has_utf16_option() {
                StringEncoding::Utf16
            } else if segment.has_utf32_option() {
                StringEncoding::Utf32
            } else {
                StringEncoding::Utf8
            };
            BitArraySegmentType::String(encoding)
        } else if segment.type_.is_utf_codepoint() {
            let encoding = if segment.has_utf16_codepoint_option() {
                StringEncoding::Utf16
            } else if segment.has_utf32_codepoint_option() {
                StringEncoding::Utf32
            } else {
                StringEncoding::Utf8
            };
            BitArraySegmentType::UtfCodepoint(encoding)
        } else {
            panic!(
                "Invalid bit array segment type reached code generation: {:?}",
                segment.type_
            );
        }
    }
}

pub fn string<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    value: &'a str,
) -> Document<'a, 'doc> {
    if value.contains('\n') {
        EcoString::from(value.replace('\n', r"\n"))
            .to_doc(arena)
            .surround(arena, "\"", "\"")
    } else {
        value.to_doc(arena).surround(arena, "\"", "\"")
    }
}

pub(crate) fn array<'a, 'doc, Elements: IntoIterator<Item = Document<'a, 'doc>>>(
    arena: &'doc DocumentArena<'a, 'doc>,
    elements: Elements,
) -> Document<'a, 'doc> {
    let elements = arena.join(elements, arena.break_(",", ", "));
    if elements.is_empty() {
        // Do not add a trailing comma since that adds an 'undefined' element
        "[]".to_doc(arena)
    } else {
        docvec![
            arena,
            "[",
            docvec![arena, arena.break_("", ""), elements].nest(arena, INDENT),
            arena.break_(",", ""),
            "]"
        ]
        .group(arena)
    }
}

pub(crate) fn list<'a, 'doc, I: IntoIterator<Item = Document<'a, 'doc>>>(
    arena: &'doc DocumentArena<'a, 'doc>,
    elements: I,
) -> Document<'a, 'doc>
where
    I::IntoIter: DoubleEndedIterator,
{
    let array = array(arena, elements);
    docvec![arena, "toList(", array, ")"]
}

fn prepend<'a, 'doc, I: IntoIterator<Item = Document<'a, 'doc>>>(
    arena: &'doc DocumentArena<'a, 'doc>,
    elements: I,
    tail: Document<'a, 'doc>,
) -> Document<'a, 'doc>
where
    I::IntoIter: DoubleEndedIterator + ExactSizeIterator,
{
    elements.into_iter().rev().fold(tail, |tail, element| {
        let arguments = call_arguments(arena, [element, tail]);
        docvec![arena, "listPrepend", arguments]
    })
}

fn call_arguments<'a, 'doc, Elements: IntoIterator<Item = Document<'a, 'doc>>>(
    arena: &'doc DocumentArena<'a, 'doc>,
    elements: Elements,
) -> Document<'a, 'doc> {
    let elements = arena.join(elements, arena.break_(",", ", "));
    if elements.is_empty() {
        return "()".to_doc(arena);
    }
    docvec![
        arena,
        "(",
        docvec![arena, arena.break_("", ""), elements].nest(arena, INDENT),
        arena.break_(",", ""),
        ")"
    ]
    .group(arena)
}

pub(crate) fn construct_record<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    module: Option<&'a str>,
    name: &'a str,
    arguments: impl IntoIterator<Item = Document<'a, 'doc>>,
) -> Document<'a, 'doc> {
    let mut any_arguments = false;
    let arguments = arena.join(
        arguments.into_iter().inspect(|_| {
            any_arguments = true;
        }),
        arena.break_(",", ", "),
    );
    let arguments = docvec![arena, arena.break_("", ""), arguments].nest(arena, INDENT);
    let name = if let Some(module) = module {
        docvec![arena, "$", module, ".", name]
    } else {
        name.to_doc(arena)
    };
    if any_arguments {
        docvec![
            arena,
            "new ",
            name,
            "(",
            arguments,
            arena.break_(",", ""),
            ")"
        ]
        .group(arena)
    } else {
        docvec![arena, "new ", name, "()"]
    }
}

impl TypedExpr {
    fn handles_own_return(&self) -> bool {
        match self {
            TypedExpr::Todo { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Block { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::RecordUpdate { .. } => true,

            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => false,
        }
    }
}

impl BinOp {
    fn is_operator_to_wrap(&self) -> bool {
        match self {
            BinOp::And
            | BinOp::Or
            | BinOp::Eq
            | BinOp::NotEq
            | BinOp::LtInt
            | BinOp::LtEqInt
            | BinOp::LtFloat
            | BinOp::LtEqFloat
            | BinOp::GtEqInt
            | BinOp::GtInt
            | BinOp::GtEqFloat
            | BinOp::GtFloat
            | BinOp::AddInt
            | BinOp::AddFloat
            | BinOp::SubInt
            | BinOp::SubFloat
            | BinOp::MultFloat
            | BinOp::DivInt
            | BinOp::DivFloat
            | BinOp::RemainderInt
            | BinOp::Concatenate => true,
            BinOp::MultInt => false,
        }
    }
}

pub fn is_js_scalar(t: Arc<Type>) -> bool {
    t.is_int() || t.is_float() || t.is_bool() || t.is_nil() || t.is_string()
}

fn expression_requires_semicolon(expression: &TypedExpr) -> bool {
    match expression {
        TypedExpr::Int { .. }
        | TypedExpr::Fn { .. }
        | TypedExpr::Var { .. }
        | TypedExpr::List { .. }
        | TypedExpr::Call { .. }
        | TypedExpr::Echo { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::String { .. }
        | TypedExpr::BinOp { .. }
        | TypedExpr::Tuple { .. }
        | TypedExpr::NegateInt { .. }
        | TypedExpr::BitArray { .. }
        | TypedExpr::TupleIndex { .. }
        | TypedExpr::NegateBool { .. }
        | TypedExpr::RecordAccess { .. }
        | TypedExpr::PositionalAccess { .. }
        | TypedExpr::ModuleSelect { .. } => true,

        TypedExpr::Todo { .. }
        | TypedExpr::Case { .. }
        | TypedExpr::Panic { .. }
        | TypedExpr::Pipeline { .. }
        | TypedExpr::RecordUpdate { .. }
        | TypedExpr::Invalid { .. }
        | TypedExpr::Block { .. } => false,
    }
}

/// Wrap a document in an immediately invoked function expression
fn immediately_invoked_function_expression_document<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    document: Document<'a, 'doc>,
) -> Document<'a, 'doc> {
    docvec![
        arena,
        docvec![arena, "(() => {", arena.break_("", " "), document].nest(arena, INDENT),
        arena.break_("", " "),
        "})()",
    ]
    .group(arena)
}

fn u8_slice<'a, 'doc>(arena: &'doc DocumentArena<'a, 'doc>, bytes: &[u8]) -> Document<'a, 'doc> {
    let s: EcoString = bytes
        .iter()
        .map(u8::to_string)
        .collect::<Vec<_>>()
        .join(", ")
        .into();

    s.to_doc(arena)
}
