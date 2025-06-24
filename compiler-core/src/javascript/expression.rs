use num_bigint::BigInt;
use vec1::Vec1;

use super::{decision::ASSIGNMENT_VAR, *};
use crate::{
    ast::*,
    line_numbers::LineNumbers,
    pretty::*,
    type_::{
        ModuleValueConstructor, Type, TypedCallArg, ValueConstructor, ValueConstructorVariant,
    },
};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum Position {
    Tail,
    NotTail(Ordering),
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
            Self::NotTail(ordering) => *ordering,
            Self::Tail | Self::Assign(_) => Ordering::Loose,
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

#[derive(Debug)]
pub(crate) struct Generator<'module, 'ast> {
    module_name: EcoString,
    src_path: EcoString,
    line_numbers: &'module LineNumbers,
    function_name: EcoString,
    function_arguments: Vec<Option<&'module EcoString>>,
    current_function: CurrentFunction,
    pub current_scope_vars: im::HashMap<EcoString, usize>,
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
    statement_level: Vec<Document<'ast>>,

    /// This will be true if we've generated a `let assert` statement that we know
    /// is guaranteed to throw.
    /// This means we can stop code generation for all the following statements
    /// in the same block!
    pub let_assert_always_panics: bool,
}

impl<'module, 'a> Generator<'module, 'a> {
    #[allow(clippy::too_many_arguments)] // TODO: FIXME
    pub fn new(
        module_name: EcoString,
        src_path: EcoString,
        line_numbers: &'module LineNumbers,
        function_name: EcoString,
        function_arguments: Vec<Option<&'module EcoString>>,
        tracker: &'module mut UsageTracker,
        mut current_scope_vars: im::HashMap<EcoString, usize>,
    ) -> Self {
        let mut current_function = CurrentFunction::Module;
        for &name in function_arguments.iter().flatten() {
            // Initialise the function arguments
            let _ = current_scope_vars.insert(name.clone(), 0);

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
            current_scope_vars,
            current_function,
            function_position: Position::Tail,
            scope_position: Position::Tail,
            statement_level: Vec::new(),
            let_assert_always_panics: false,
        }
    }

    pub fn local_var(&mut self, name: &EcoString) -> EcoString {
        match self.current_scope_vars.get(name) {
            None => {
                let _ = self.current_scope_vars.insert(name.clone(), 0);
                maybe_escape_identifier(name)
            }
            Some(0) => maybe_escape_identifier(name),
            Some(n) if name == "$" => eco_format!("${n}"),
            Some(n) => eco_format!("{name}${n}"),
        }
    }

    pub fn next_local_var(&mut self, name: &EcoString) -> EcoString {
        let next = self.current_scope_vars.get(name).map_or(0, |i| i + 1);
        let _ = self.current_scope_vars.insert(name.clone(), next);
        self.local_var(name)
    }

    pub fn function_body(
        &mut self,
        body: &'a [TypedStatement],
        args: &'a [TypedArg],
    ) -> Output<'a> {
        let body = self.statements(body)?;
        if self.tail_recursion_used {
            self.tail_call_loop(body, args)
        } else {
            Ok(body)
        }
    }

    fn tail_call_loop(&mut self, body: Document<'a>, args: &'a [TypedArg]) -> Output<'a> {
        let loop_assignments = concat(args.iter().flat_map(Arg::get_variable_name).map(|name| {
            let var = maybe_escape_identifier(name);
            docvec!["let ", var, " = loop$", name, ";", line()]
        }));
        Ok(docvec![
            "while (true) {",
            docvec![line(), loop_assignments, body].nest(INDENT),
            line(),
            "}"
        ])
    }

    fn statement(&mut self, statement: &'a TypedStatement) -> Output<'a> {
        let expression_doc = match statement {
            Statement::Expression(expression) => self.expression(expression),
            Statement::Assignment(assignment) => self.assignment(assignment),
            Statement::Use(use_) => self.expression(&use_.call),
            Statement::Assert(assert) => self.assert(assert),
        }?;
        Ok(self.add_statement_level(expression_doc))
    }

    fn add_statement_level(&mut self, expression: Document<'a>) -> Document<'a> {
        if self.statement_level.is_empty() {
            expression
        } else {
            let mut statements = std::mem::take(&mut self.statement_level);
            statements.push(expression);
            join(statements, line())
        }
    }

    pub fn expression(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        let document = match expression {
            TypedExpr::String { value, .. } => Ok(string(value)),

            TypedExpr::Int { value, .. } => Ok(int(value)),
            TypedExpr::Float { value, .. } => Ok(float(value)),

            TypedExpr::List { elements, tail, .. } => {
                self.not_in_tail_position(Some(Ordering::Strict), |this| match tail {
                    Some(tail) => {
                        this.tracker.prepend_used = true;
                        let tail = this.wrap_expression(tail)?;
                        prepend(
                            elements.iter().map(|element| this.wrap_expression(element)),
                            tail,
                        )
                    }
                    None => {
                        this.tracker.list_used = true;
                        list(elements.iter().map(|element| this.wrap_expression(element)))
                    }
                })
            }

            TypedExpr::Tuple { elements, .. } => self.tuple(elements),
            TypedExpr::TupleIndex { tuple, index, .. } => self.tuple_index(tuple, *index),

            TypedExpr::Case {
                subjects,
                clauses,
                compiled_case,
                ..
            } => decision::case(compiled_case, clauses, subjects, self),

            TypedExpr::Call { fun, args, .. } => self.call(fun, args),
            TypedExpr::Fn { args, body, .. } => self.fn_(args, body),

            TypedExpr::RecordAccess { record, label, .. } => self.record_access(record, label),
            TypedExpr::RecordUpdate {
                record_assignment,
                constructor,
                args,
                ..
            } => self.record_update(record_assignment, constructor, args),

            TypedExpr::Var {
                name, constructor, ..
            } => self.variable(name, constructor),

            TypedExpr::Pipeline {
                first_value,
                assignments,
                finally,
                ..
            } => self.pipeline(first_value, assignments.as_slice(), finally),

            TypedExpr::Block { statements, .. } => self.block(statements),

            TypedExpr::BinOp {
                name, left, right, ..
            } => self.bin_op(name, left, right),

            TypedExpr::Todo {
                message, location, ..
            } => self.todo(message.as_ref().map(|m| &**m), location),

            TypedExpr::Panic {
                location, message, ..
            } => self.panic(location, message.as_ref().map(|m| &**m)),

            TypedExpr::BitArray { segments, .. } => self.bit_array(segments),

            TypedExpr::ModuleSelect {
                module_alias,
                label,
                constructor,
                ..
            } => Ok(self.module_select(module_alias, label, constructor)),

            TypedExpr::NegateBool { value, .. } => self.negate_with("!", value),

            TypedExpr::NegateInt { value, .. } => self.negate_with("- ", value),

            TypedExpr::Echo {
                expression,
                location,
                ..
            } => {
                let expression = expression
                    .as_ref()
                    .expect("echo with no expression outside of pipe");
                let expresion_doc =
                    self.not_in_tail_position(None, |this| this.wrap_expression(expression))?;
                self.echo(expresion_doc, location)
            }

            TypedExpr::Invalid { .. } => {
                panic!("invalid expressions should not reach code generation")
            }
        }?;
        Ok(if expression.handles_own_return() {
            document
        } else {
            self.wrap_return(document)
        })
    }

    fn negate_with(&mut self, with: &'static str, value: &'a TypedExpr) -> Output<'a> {
        self.not_in_tail_position(None, |this| Ok(docvec![with, this.wrap_expression(value)?]))
    }

    fn bit_array(&mut self, segments: &'a [TypedExprBitArraySegment]) -> Output<'a> {
        use BitArrayOption as Opt;

        self.tracker.bit_array_literal_used = true;

        // Collect all the values used in segments.
        let segments_array = array(segments.iter().map(|segment| {
            let value = self.not_in_tail_position(Some(Ordering::Strict), |this| {
                this.wrap_expression(&segment.value)
            })?;

            match segment.options.as_slice() {
                // Int segment
                _ if segment.type_.is_int() => {
                    let details = self.sized_bit_array_segment_details(segment)?;
                    match (details.size_value, segment.value.as_ref()) {
                        (Some(size_value), TypedExpr::Int { int_value, .. })
                            if size_value <= SAFE_INT_SEGMENT_MAX_SIZE.into()
                                && (&size_value % BigInt::from(8) == BigInt::ZERO) =>
                        {
                            let bytes = bit_array_segment_int_value_to_bytes(
                                int_value.clone(),
                                size_value,
                                segment.endianness(),
                            )?;

                            Ok(u8_slice(&bytes))
                        }

                        (Some(size_value), _) if size_value == 8.into() => Ok(value),

                        (Some(size_value), _) if size_value <= 0.into() => Ok(nil()),

                        _ => {
                            self.tracker.sized_integer_segment_used = true;
                            let size = details.size;
                            let is_big = bool(segment.endianness().is_big());
                            Ok(docvec!["sizedInt(", value, ", ", size, ", ", is_big, ")"])
                        }
                    }
                }

                // Float segment
                _ if segment.type_.is_float() => {
                    self.tracker.float_bit_array_segment_used = true;
                    let details = self.sized_bit_array_segment_details(segment)?;
                    let size = details.size;
                    let is_big = bool(segment.endianness().is_big());
                    Ok(docvec!["sizedFloat(", value, ", ", size, ", ", is_big, ")"])
                }

                // UTF8 strings
                [Opt::Utf8 { .. }] => {
                    self.tracker.string_bit_array_segment_used = true;
                    Ok(docvec!["stringBits(", value, ")"])
                }

                // UTF8 codepoints
                [Opt::Utf8Codepoint { .. }] => {
                    self.tracker.codepoint_bit_array_segment_used = true;
                    Ok(docvec!["codepointBits(", value, ")"])
                }

                // UTF16 strings
                [Opt::Utf16 { .. }]
                | [Opt::Utf16 { .. }, Opt::Big { .. }]
                | [Opt::Big { .. }, Opt::Utf16 { .. }] => {
                    self.tracker.string_utf16_bit_array_segment_used = true;
                    let is_big = "true".to_doc();
                    Ok(docvec!["stringToUtf16(", value, ", ", is_big, ")"])
                }

                [Opt::Utf16 { .. }, Opt::Little { .. }]
                | [Opt::Little { .. }, Opt::Utf16 { .. }] => {
                    self.tracker.string_utf16_bit_array_segment_used = true;
                    let is_big = "false".to_doc();
                    Ok(docvec!["stringToUtf16(", value, ", ", is_big, ")"])
                }

                // UTF16 codepoints
                [Opt::Utf16Codepoint { .. }]
                | [Opt::Utf16Codepoint { .. }, Opt::Big { .. }]
                | [Opt::Big { .. }, Opt::Utf16Codepoint { .. }] => {
                    self.tracker.codepoint_utf16_bit_array_segment_used = true;
                    let is_big = "true".to_doc();
                    Ok(docvec!["codepointToUtf16(", value, ", ", is_big, ")"])
                }

                [Opt::Utf16Codepoint { .. }, Opt::Little { .. }]
                | [Opt::Little { .. }, Opt::Utf16Codepoint { .. }] => {
                    self.tracker.codepoint_utf16_bit_array_segment_used = true;
                    let is_big = "false".to_doc();
                    Ok(docvec!["codepointToUtf16(", value, ", ", is_big, ")"])
                }

                // UTF32 strings
                [Opt::Utf32 { .. }]
                | [Opt::Utf32 { .. }, Opt::Big { .. }]
                | [Opt::Big { .. }, Opt::Utf32 { .. }] => {
                    self.tracker.string_utf32_bit_array_segment_used = true;
                    let is_big = "true".to_doc();
                    Ok(docvec!["stringToUtf32(", value, ", ", is_big, ")"])
                }

                [Opt::Utf32 { .. }, Opt::Little { .. }]
                | [Opt::Little { .. }, Opt::Utf32 { .. }] => {
                    self.tracker.string_utf32_bit_array_segment_used = true;
                    let is_big = "false".to_doc();
                    Ok(docvec!["stringToUtf32(", value, ", ", is_big, ")"])
                }

                // UTF32 codepoints
                [Opt::Utf32Codepoint { .. }]
                | [Opt::Utf32Codepoint { .. }, Opt::Big { .. }]
                | [Opt::Big { .. }, Opt::Utf32Codepoint { .. }] => {
                    self.tracker.codepoint_utf32_bit_array_segment_used = true;
                    let is_big = "true".to_doc();
                    Ok(docvec!["codepointToUtf32(", value, ", ", is_big, ")"])
                }

                [Opt::Utf32Codepoint { .. }, Opt::Little { .. }]
                | [Opt::Little { .. }, Opt::Utf32Codepoint { .. }] => {
                    self.tracker.codepoint_utf32_bit_array_segment_used = true;
                    let is_big = "false".to_doc();
                    Ok(docvec!["codepointToUtf32(", value, ", ", is_big, ")"])
                }

                // Bit arrays
                [Opt::Bits { .. }] => Ok(value),

                // Bit arrays with explicit size. The explicit size slices the bit array to the
                // specified size. A runtime exception is thrown if the size exceeds the number
                // of bits in the bit array.
                [Opt::Bits { .. }, Opt::Size { value: size, .. }]
                | [Opt::Size { value: size, .. }, Opt::Bits { .. }] => match &**size {
                    TypedExpr::Int { value: size, .. } => {
                        self.tracker.bit_array_slice_used = true;
                        Ok(docvec!["bitArraySlice(", value, ", 0, ", size, ")"])
                    }

                    TypedExpr::Var { name, .. } => {
                        self.tracker.bit_array_slice_used = true;
                        Ok(docvec!["bitArraySlice(", value, ", 0, ", name, ")"])
                    }

                    _ => Err(Error::Unsupported {
                        feature: "This bit array segment option".into(),
                        location: segment.location,
                    }),
                },

                // Anything else
                _ => Err(Error::Unsupported {
                    feature: "This bit array segment option".into(),
                    location: segment.location,
                }),
            }
        }))?;

        Ok(docvec!["toBitArray(", segments_array, ")"])
    }

    fn sized_bit_array_segment_details(
        &mut self,
        segment: &'a TypedExprBitArraySegment,
    ) -> Result<SizedBitArraySegmentDetails<'a>, Error> {
        let size = segment.size();
        let unit = segment.unit();
        let (size_value, size) = match size {
            Some(TypedExpr::Int { int_value, .. }) => {
                let size_value = int_value * unit;
                let size = eco_format!("{}", size_value).to_doc();
                (Some(size_value), size)
            }
            Some(size) => {
                let mut size = self.not_in_tail_position(Some(Ordering::Strict), |this| {
                    this.wrap_expression(size)
                })?;

                if unit != 1 {
                    size = size.group().append(" * ".to_doc().append(unit.to_doc()));
                }

                (None, size)
            }

            None => {
                let size_value: usize = if segment.type_.is_int() { 8 } else { 64 };
                (Some(BigInt::from(size_value)), docvec![size_value])
            }
        };

        Ok(SizedBitArraySegmentDetails { size, size_value })
    }

    pub fn wrap_return(&mut self, document: Document<'a>) -> Document<'a> {
        match &self.scope_position {
            Position::Tail => docvec!["return ", document, ";"],
            Position::NotTail(_) => document,
            Position::Assign(name) => docvec![name.clone(), " = ", document, ";"],
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

        let function_position =
            std::mem::replace(&mut self.function_position, Position::NotTail(new_ordering));
        let scope_position =
            std::mem::replace(&mut self.scope_position, Position::NotTail(new_ordering));

        let result = compile(self);

        self.function_position = function_position;
        self.scope_position = scope_position;
        result
    }

    /// Use the `_block` variable if the expression is JS statement.
    pub fn wrap_expression(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        match (expression, &self.scope_position) {
            (_, Position::Tail | Position::Assign(_)) => self.expression(expression),
            (
                TypedExpr::Panic { .. }
                | TypedExpr::Todo { .. }
                | TypedExpr::Case { .. }
                | TypedExpr::Pipeline { .. }
                | TypedExpr::RecordUpdate {
                    // Record updates that assign a variable generate multiple statements
                    record_assignment: Some(_),
                    ..
                },
                Position::NotTail(Ordering::Loose),
            ) => self.wrap_block(|this| this.expression(expression)),
            (
                TypedExpr::Panic { .. }
                | TypedExpr::Todo { .. }
                | TypedExpr::Case { .. }
                | TypedExpr::Pipeline { .. }
                | TypedExpr::RecordUpdate {
                    // Record updates that assign a variable generate multiple statements
                    record_assignment: Some(_),
                    ..
                },
                Position::NotTail(Ordering::Strict),
            ) => self.immediately_invoked_function_expression(expression, |this, expr| {
                this.expression(expr)
            }),
            _ => self.expression(expression),
        }
    }

    /// Wrap an expression using the `_block` variable if required due to being
    /// a JS statement, or in parens if required due to being an operator or
    /// a function literal.
    pub fn child_expression(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        match expression {
            TypedExpr::BinOp { name, .. } if name.is_operator_to_wrap() => {}
            TypedExpr::Fn { .. } => {}

            _ => return self.wrap_expression(expression),
        }

        let document = self.expression(expression)?;
        Ok(match &self.scope_position {
            // Here the document is a return statement: `return <expr>;`
            // or an assignment: `_block = <expr>;`
            Position::Tail | Position::Assign(_) => document,
            Position::NotTail(_) => docvec!["(", document, ")"],
        })
    }

    /// Wrap an expression in an immediately invoked function expression
    fn immediately_invoked_function_expression<T, ToDoc>(
        &mut self,
        statements: &'a T,
        to_doc: ToDoc,
    ) -> Output<'a>
    where
        ToDoc: FnOnce(&mut Self, &'a T) -> Output<'a>,
    {
        // Save initial state
        let scope_position = std::mem::replace(&mut self.scope_position, Position::Tail);

        // Set state for in this iife
        let current_scope_vars = self.current_scope_vars.clone();

        // Generate the expression
        let result = to_doc(self, statements);

        // Reset
        self.current_scope_vars = current_scope_vars;
        self.scope_position = scope_position;

        // Wrap in iife document
        let doc =
            immediately_invoked_function_expression_document(self.add_statement_level(result?));
        Ok(self.wrap_return(doc))
    }

    fn wrap_block<CompileFn>(&mut self, compile: CompileFn) -> Output<'a>
    where
        CompileFn: Fn(&mut Self) -> Output<'a>,
    {
        let block_variable = self.next_local_var(&BLOCK_VARIABLE.into());

        // Save initial state
        let scope_position = std::mem::replace(
            &mut self.scope_position,
            Position::Assign(block_variable.clone()),
        );
        let function_position = std::mem::replace(
            &mut self.function_position,
            Position::NotTail(Ordering::Strict),
        );

        // Generate the expression
        let statement_doc = compile(self);

        // Reset
        self.scope_position = scope_position;
        self.function_position = function_position;

        self.statement_level
            .push(docvec!["let ", block_variable.clone(), ";"]);
        self.statement_level.push(statement_doc?);

        Ok(self.wrap_return(block_variable.to_doc()))
    }

    fn variable(&mut self, name: &'a EcoString, constructor: &'a ValueConstructor) -> Output<'a> {
        match &constructor.variant {
            ValueConstructorVariant::LocalConstant { literal } => {
                self.constant_expression(Context::Function, literal)
            }
            ValueConstructorVariant::Record { arity, .. } => {
                let type_ = constructor.type_.clone();
                let tracker = &mut self.tracker;
                Ok(record_constructor(type_, None, name, *arity, tracker))
            }
            ValueConstructorVariant::ModuleFn { .. }
            | ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::LocalVariable { .. } => Ok(self.local_var(name).to_doc()),
        }
    }

    fn pipeline(
        &mut self,
        first_value: &'a TypedPipelineAssignment,
        assignments: &'a [(TypedPipelineAssignment, PipelineAssignmentKind)],
        finally: &'a TypedExpr,
    ) -> Output<'a> {
        let count = assignments.len();
        let mut documents = Vec::with_capacity((count + 2) * 2);

        let all_assignments = std::iter::once(first_value)
            .chain(assignments.iter().map(|(assignment, _kind)| assignment));

        let mut latest_local_var: Option<EcoString> = None;
        for assignment in all_assignments {
            match assignment.value.as_ref() {
                // An echo in a pipeline won't result in an assignment, instead it
                // just prints the previous variable assigned in the pipeline.
                TypedExpr::Echo {
                    expression: None,
                    location,
                    ..
                } => documents.push(self.not_in_tail_position(Some(Ordering::Strict), |this| {
                    let var = latest_local_var
                        .as_ref()
                        .expect("echo with no previous step in a pipe");
                    this.echo(var.to_doc(), location)
                })?),

                // Otherwise we assign the intermediate pipe value to a variable.
                _ => {
                    let assignment_document = self
                        .not_in_tail_position(Some(Ordering::Strict), |this| {
                            this.simple_variable_assignment(&assignment.name, &assignment.value)
                        })?;
                    documents.push(self.add_statement_level(assignment_document));
                    latest_local_var = Some(self.local_var(&assignment.name));
                }
            }

            documents.push(line());
        }

        match finally {
            TypedExpr::Echo {
                expression: None,
                location,
                ..
            } => {
                let var = latest_local_var.expect("echo with no previous step in a pipe");
                documents.push(self.echo(var.to_doc(), location)?);
            }
            _ => {
                let finally = self.expression(finally)?;
                documents.push(self.add_statement_level(finally))
            }
        }

        Ok(documents.to_doc().force_break())
    }

    pub(crate) fn expression_flattening_blocks(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        match expression {
            TypedExpr::Block { statements, .. } => self.statements(statements),
            _ => {
                let expression_document = self.expression(expression)?;
                Ok(self.add_statement_level(expression_document))
            }
        }
    }

    fn block(&mut self, statements: &'a Vec1<TypedStatement>) -> Output<'a> {
        if statements.len() == 1 {
            match statements.first() {
                Statement::Expression(expression) => return self.child_expression(expression),

                Statement::Assignment(assignment) => match &assignment.kind {
                    AssignmentKind::Let | AssignmentKind::Generated => {
                        return self.child_expression(&assignment.value);
                    }
                    // We can't just return the right-hand side of a `let assert`
                    // assignment; we still need to check that the pattern matches.
                    AssignmentKind::Assert { .. } => {}
                },

                Statement::Use(use_) => return self.child_expression(&use_.call),

                // Similar to `let assert`, we can't immediately return the value
                // that is asserted; we have to actually perform the assertion.
                Statement::Assert(_) => {}
            }
        }
        match &self.scope_position {
            Position::Tail | Position::Assign(_) => self.block_document(statements),
            Position::NotTail(Ordering::Strict) => self
                .immediately_invoked_function_expression(statements, |this, statements| {
                    this.statements(statements)
                }),
            Position::NotTail(Ordering::Loose) => self.wrap_block(|this| {
                // Save previous scope
                let current_scope_vars = this.current_scope_vars.clone();

                let document = this.block_document(statements)?;

                // Restore previous state
                this.current_scope_vars = current_scope_vars;

                Ok(document)
            }),
        }
    }

    fn block_document(&mut self, statements: &'a Vec1<TypedStatement>) -> Output<'a> {
        let statements = self.statements(statements)?;
        Ok(docvec![
            "{",
            docvec![line(), statements].nest(INDENT),
            line(),
            "}"
        ])
    }

    fn statements(&mut self, statements: &'a [TypedStatement]) -> Output<'a> {
        // If there are any statements that need to be printed at statement level, that's
        // for an outer scope so we don't want to print them inside this one.
        let statement_level = std::mem::take(&mut self.statement_level);
        let count = statements.len();
        let mut documents = Vec::with_capacity(count * 3);
        for (i, statement) in statements.iter().enumerate() {
            if i + 1 < count {
                documents.push(self.not_in_tail_position(Some(Ordering::Loose), |this| {
                    this.statement(statement)
                })?);
                if requires_semicolon(statement) {
                    documents.push(";".to_doc());
                }
                documents.push(line());
            } else {
                documents.push(self.statement(statement)?);
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
            Ok(documents.to_doc())
        } else {
            Ok(documents.to_doc().force_break())
        }
    }

    fn simple_variable_assignment(
        &mut self,
        name: &'a EcoString,
        value: &'a TypedExpr,
    ) -> Output<'a> {
        // Subject must be rendered before the variable for variable numbering
        let subject =
            self.not_in_tail_position(Some(Ordering::Loose), |this| this.wrap_expression(value))?;
        let js_name = self.next_local_var(name);
        let assignment = docvec!["let ", js_name.clone(), " = ", subject, ";"];
        let assignment = match &self.scope_position {
            Position::NotTail(_) => assignment,
            Position::Tail => docvec![assignment, line(), "return ", js_name, ";"],
            Position::Assign(block_variable) => docvec![
                assignment,
                line(),
                block_variable.clone(),
                " = ",
                js_name,
                ";"
            ],
        };

        Ok(assignment.force_break())
    }

    fn assignment(&mut self, assignment: &'a TypedAssignment) -> Output<'a> {
        let TypedAssignment {
            pattern,
            kind,
            value,
            compiled_case,
            annotation: _,
            location: _,
        } = assignment;

        // In case the pattern is just a variable, we special case it to
        // generate just a simple assignment instead of using the decision tree
        // for the code generation step.
        if let TypedPattern::Variable { name, .. } = pattern {
            return self.simple_variable_assignment(name, value);
        }

        decision::let_(compiled_case, value, kind, self, pattern.location())
    }

    fn assert(&mut self, assert: &'a TypedAssert) -> Output<'a> {
        let TypedAssert {
            location,
            value,
            message,
        } = assert;

        let message = match message {
            Some(m) => self.not_in_tail_position(
                Some(Ordering::Strict),
                |this: &mut Generator<'module, 'a>| this.expression(m),
            )?,
            None => string("Assertion failed."),
        };

        let check = self.not_in_tail_position(Some(Ordering::Loose), |this| {
            this.assert_check(value, &message, *location)
        })?;

        Ok(match &self.scope_position {
            Position::NotTail(_) => check,
            Position::Tail | Position::Assign(_) => {
                docvec![check, line(), self.wrap_return("undefined".to_doc())]
            }
        })
    }

    fn assert_check(
        &mut self,
        subject: &'a TypedExpr,
        message: &Document<'a>,
        location: SrcSpan,
    ) -> Output<'a> {
        let (subject_document, mut fields) = match subject {
            TypedExpr::Call { fun, args, .. } => {
                let argument_variables: Vec<_> = args
                    .iter()
                    .map(|element| {
                        self.not_in_tail_position(Some(Ordering::Strict), |this| {
                            this.assign_to_variable(&element.value)
                        })
                    })
                    .try_collect()?;
                (
                    self.call_with_doc_args(fun, argument_variables.clone())?,
                    vec![
                        ("kind", string("function_call")),
                        (
                            "arguments",
                            array(argument_variables.into_iter().zip(args).map(
                                |(variable, argument)| {
                                    Ok(self.asserted_expression(
                                        AssertExpression::from_expression(&argument.value),
                                        Some(variable),
                                        argument.location(),
                                    ))
                                },
                            ))?,
                        ),
                    ],
                )
            }

            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                match name {
                    BinOp::And => return self.assert_and(left, right, message, location),
                    BinOp::Or => return self.assert_or(left, right, message, location),
                    _ => {}
                }

                let left_document = self.not_in_tail_position(Some(Ordering::Loose), |this| {
                    this.assign_to_variable(left)
                })?;
                let right_document = self.not_in_tail_position(Some(Ordering::Loose), |this| {
                    this.assign_to_variable(right)
                })?;

                (
                    self.bin_op_with_doc_operands(
                        *name,
                        left_document.clone(),
                        right_document.clone(),
                        &left.type_(),
                    )
                    .surround("(", ")"),
                    vec![
                        ("kind", string("binary_operator")),
                        ("operator", string(name.name())),
                        (
                            "left",
                            self.asserted_expression(
                                AssertExpression::from_expression(left),
                                Some(left_document),
                                left.location(),
                            ),
                        ),
                        (
                            "right",
                            self.asserted_expression(
                                AssertExpression::from_expression(right),
                                Some(right_document),
                                right.location(),
                            ),
                        ),
                    ],
                )
            }

            _ => (
                self.wrap_expression(subject)?,
                vec![
                    ("kind", string("expression")),
                    (
                        "expression",
                        self.asserted_expression(
                            AssertExpression::from_expression(subject),
                            Some("false".to_doc()),
                            subject.location(),
                        ),
                    ),
                ],
            ),
        };

        fields.push(("start", location.start.to_doc()));
        fields.push(("end", subject.location().end.to_doc()));
        fields.push(("expression_start", subject.location().start.to_doc()));

        Ok(docvec![
            "if (",
            docvec!["!", subject_document].nest(INDENT),
            break_("", ""),
            ") {",
            docvec![
                line(),
                self.throw_error("assert", message, location, fields),
            ]
            .nest(INDENT),
            line(),
            "}",
        ]
        .group())
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
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        message: &Document<'a>,
        location: SrcSpan,
    ) -> Output<'a> {
        let left_kind = AssertExpression::from_expression(left);
        let right_kind = AssertExpression::from_expression(right);

        let fields_if_short_circuiting = vec![
            ("kind", string("binary_operator")),
            ("operator", string("&&")),
            (
                "left",
                self.asserted_expression(left_kind, Some("false".to_doc()), left.location()),
            ),
            (
                "right",
                self.asserted_expression(AssertExpression::Unevaluated, None, right.location()),
            ),
            ("start", location.start.to_doc()),
            ("end", right.location().end.to_doc()),
            ("expression_start", left.location().start.to_doc()),
        ];

        let fields = vec![
            ("kind", string("binary_operator")),
            ("operator", string("&&")),
            (
                "left",
                self.asserted_expression(left_kind, Some("true".to_doc()), left.location()),
            ),
            (
                "right",
                self.asserted_expression(right_kind, Some("false".to_doc()), right.location()),
            ),
            ("start", location.start.to_doc()),
            ("end", right.location().end.to_doc()),
            ("expression_start", left.location().start.to_doc()),
        ];

        let left_value =
            self.not_in_tail_position(Some(Ordering::Loose), |this| this.wrap_expression(left))?;

        let right_value =
            self.not_in_tail_position(Some(Ordering::Strict), |this| this.wrap_expression(right))?;

        let right_check = docvec![
            line(),
            "if (",
            docvec!["!", right_value].nest(INDENT),
            ") {",
            docvec![
                line(),
                self.throw_error("assert", message, location, fields)
            ]
            .nest(INDENT),
            line(),
            "}",
        ];

        Ok(docvec![
            "if (",
            left_value.nest(INDENT),
            ") {",
            right_check.nest(INDENT),
            line(),
            "} else {",
            docvec![
                line(),
                self.throw_error("assert", message, location, fields_if_short_circuiting)
            ]
            .nest(INDENT),
            line(),
            "}"
        ])
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
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        message: &Document<'a>,
        location: SrcSpan,
    ) -> Output<'a> {
        let fields = vec![
            ("kind", string("binary_operator")),
            ("operator", string("||")),
            (
                "left",
                self.asserted_expression(
                    AssertExpression::from_expression(left),
                    Some("false".to_doc()),
                    left.location(),
                ),
            ),
            (
                "right",
                self.asserted_expression(
                    AssertExpression::from_expression(right),
                    Some("false".to_doc()),
                    right.location(),
                ),
            ),
            ("start", location.start.to_doc()),
            ("end", right.location().end.to_doc()),
            ("expression_start", left.location().start.to_doc()),
        ];

        let left_value =
            self.not_in_tail_position(Some(Ordering::Loose), |this| this.child_expression(left))?;

        let right_value =
            self.not_in_tail_position(Some(Ordering::Strict), |this| this.child_expression(right))?;

        Ok(docvec![
            line(),
            "if (",
            docvec!["!(", left_value, " || ", right_value, ")"].nest(INDENT),
            ") {",
            docvec![
                line(),
                self.throw_error("assert", message, location, fields)
            ]
            .nest(INDENT),
            line(),
            "}",
        ])
    }

    fn assign_to_variable(&mut self, value: &'a TypedExpr) -> Output<'a> {
        match value {
            TypedExpr::Var { .. } => self.expression(value),
            _ => {
                let value = self.wrap_expression(value)?;
                let variable = self.next_local_var(&ASSIGNMENT_VAR.into());
                let assignment = docvec!["let ", variable.clone(), " = ", value, ";"];
                self.statement_level.push(assignment);
                Ok(variable.to_doc())
            }
        }
    }

    fn asserted_expression(
        &mut self,
        kind: AssertExpression,
        value: Option<Document<'a>>,
        location: SrcSpan,
    ) -> Document<'a> {
        let kind = match kind {
            AssertExpression::Literal => string("literal"),
            AssertExpression::Expression => string("expression"),
            AssertExpression::Unevaluated => string("unevaluated"),
        };

        let start = location.start.to_doc();
        let end = location.end.to_doc();
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
            items
                .into_iter()
                .map(|(key, value)| (key.to_doc(), Some(value))),
        )
    }

    fn tuple(&mut self, elements: &'a [TypedExpr]) -> Output<'a> {
        self.not_in_tail_position(Some(Ordering::Strict), |this| {
            array(elements.iter().map(|element| this.wrap_expression(element)))
        })
    }

    fn call(&mut self, fun: &'a TypedExpr, arguments: &'a [TypedCallArg]) -> Output<'a> {
        let arguments = arguments
            .iter()
            .map(|element| {
                self.not_in_tail_position(Some(Ordering::Strict), |this| {
                    this.wrap_expression(&element.value)
                })
            })
            .try_collect()?;

        self.call_with_doc_args(fun, arguments)
    }

    fn call_with_doc_args(
        &mut self,
        fun: &'a TypedExpr,
        arguments: Vec<Document<'a>>,
    ) -> Output<'a> {
        match fun {
            // Qualified record construction
            TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Record { name, .. },
                module_alias,
                ..
            } => Ok(self.wrap_return(construct_record(Some(module_alias), name, arguments))),

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
                Ok(self.wrap_return(construct_record(None, name, arguments)))
            }

            // Tail call optimisation. If we are calling the current function
            // and we are in tail position we can avoid creating a new stack
            // frame, enabling recursion with constant memory usage.
            TypedExpr::Var { name, .. }
                if self.function_name == *name
                    && self.current_function.can_recurse()
                    && self.function_position.is_tail()
                    && self.current_scope_vars.get(name) == Some(&0) =>
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
                        docs.push(line());
                    }
                    // Create an assignment for each variable created by the function arguments
                    if let Some(name) = argument {
                        docs.push("loop$".to_doc());
                        docs.push(name.to_doc());
                        docs.push(" = ".to_doc());
                    }
                    // Render the value given to the function. Even if it is not
                    // assigned we still render it because the expression may
                    // have some side effects.
                    docs.push(element);
                    docs.push(";".to_doc());
                }
                Ok(docs.to_doc())
            }

            _ => {
                let fun = self.not_in_tail_position(None, |this| {
                    let is_fn_literal = matches!(fun, TypedExpr::Fn { .. });
                    let fun = this.wrap_expression(fun)?;
                    if is_fn_literal {
                        Ok(docvec!["(", fun, ")"])
                    } else {
                        Ok(fun)
                    }
                })?;
                let arguments = call_arguments(arguments.into_iter().map(Ok))?;
                Ok(self.wrap_return(docvec![fun, arguments]))
            }
        }
    }

    fn fn_(&mut self, arguments: &'a [TypedArg], body: &'a [TypedStatement]) -> Output<'a> {
        // New function, this is now the tail position
        let function_position = std::mem::replace(&mut self.function_position, Position::Tail);
        let scope_position = std::mem::replace(&mut self.scope_position, Position::Tail);

        // And there's a new scope
        let scope = self.current_scope_vars.clone();
        for name in arguments.iter().flat_map(Arg::get_variable_name) {
            let _ = self.current_scope_vars.insert(name.clone(), 0);
        }

        // This is a new function so track that so that we don't
        // mistakenly trigger tail call optimisation
        let mut current_function = CurrentFunction::Anonymous;
        std::mem::swap(&mut self.current_function, &mut current_function);

        // Generate the function body
        let result = self.statements(body);

        // Reset function name, scope, and tail position tracking
        self.function_position = function_position;
        self.scope_position = scope_position;
        self.current_scope_vars = scope;
        std::mem::swap(&mut self.current_function, &mut current_function);

        Ok(docvec![
            docvec![
                fun_args(arguments, false),
                " => {",
                break_("", " "),
                result?
            ]
            .nest(INDENT)
            .append(break_("", " "))
            .group(),
            "}",
        ])
    }

    fn record_access(&mut self, record: &'a TypedExpr, label: &'a str) -> Output<'a> {
        self.not_in_tail_position(None, |this| {
            let record = this.wrap_expression(record)?;
            Ok(docvec![record, ".", maybe_escape_property(label)])
        })
    }

    fn record_update(
        &mut self,
        record: &'a Option<Box<TypedAssignment>>,
        constructor: &'a TypedExpr,
        args: &'a [TypedCallArg],
    ) -> Output<'a> {
        match record.as_ref() {
            Some(record) => Ok(docvec![
                self.not_in_tail_position(None, |this| this.assignment(record))?,
                line(),
                self.call(constructor, args)?,
            ]),
            None => self.call(constructor, args),
        }
    }

    fn tuple_index(&mut self, tuple: &'a TypedExpr, index: u64) -> Output<'a> {
        self.not_in_tail_position(None, |this| {
            let tuple = this.wrap_expression(tuple)?;
            Ok(docvec![tuple, eco_format!("[{index}]")])
        })
    }

    fn bin_op(&mut self, name: &'a BinOp, left: &'a TypedExpr, right: &'a TypedExpr) -> Output<'a> {
        match name {
            BinOp::And => self.print_bin_op(left, right, "&&"),
            BinOp::Or => self.print_bin_op(left, right, "||"),
            BinOp::LtInt | BinOp::LtFloat => self.print_bin_op(left, right, "<"),
            BinOp::LtEqInt | BinOp::LtEqFloat => self.print_bin_op(left, right, "<="),
            BinOp::Eq => self.equal(left, right, true),
            BinOp::NotEq => self.equal(left, right, false),
            BinOp::GtInt | BinOp::GtFloat => self.print_bin_op(left, right, ">"),
            BinOp::GtEqInt | BinOp::GtEqFloat => self.print_bin_op(left, right, ">="),
            BinOp::Concatenate | BinOp::AddInt | BinOp::AddFloat => {
                self.print_bin_op(left, right, "+")
            }
            BinOp::SubInt | BinOp::SubFloat => self.print_bin_op(left, right, "-"),
            BinOp::MultInt | BinOp::MultFloat => self.print_bin_op(left, right, "*"),
            BinOp::RemainderInt => self.remainder_int(left, right),
            BinOp::DivInt => self.div_int(left, right),
            BinOp::DivFloat => self.div_float(left, right),
        }
    }

    fn div_int(&mut self, left: &'a TypedExpr, right: &'a TypedExpr) -> Output<'a> {
        let left =
            self.not_in_tail_position(Some(Ordering::Strict), |this| this.child_expression(left))?;
        let right =
            self.not_in_tail_position(Some(Ordering::Strict), |this| this.child_expression(right))?;
        self.tracker.int_division_used = true;
        Ok(docvec!["divideInt", wrap_args([left, right])])
    }

    fn remainder_int(&mut self, left: &'a TypedExpr, right: &'a TypedExpr) -> Output<'a> {
        let left =
            self.not_in_tail_position(Some(Ordering::Strict), |this| this.child_expression(left))?;
        let right =
            self.not_in_tail_position(Some(Ordering::Strict), |this| this.child_expression(right))?;
        self.tracker.int_remainder_used = true;
        Ok(docvec!["remainderInt", wrap_args([left, right])])
    }

    fn div_float(&mut self, left: &'a TypedExpr, right: &'a TypedExpr) -> Output<'a> {
        let left =
            self.not_in_tail_position(Some(Ordering::Strict), |this| this.child_expression(left))?;
        let right =
            self.not_in_tail_position(Some(Ordering::Strict), |this| this.child_expression(right))?;
        self.tracker.float_division_used = true;
        Ok(docvec!["divideFloat", wrap_args([left, right])])
    }

    fn equal(
        &mut self,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        should_be_equal: bool,
    ) -> Output<'a> {
        // If it is a simple scalar type then we can use JS' reference identity
        if is_js_scalar(left.type_()) {
            let left_doc = self
                .not_in_tail_position(Some(Ordering::Strict), |this| this.child_expression(left))?;
            let right_doc = self.not_in_tail_position(Some(Ordering::Strict), |this| {
                this.child_expression(right)
            })?;
            let operator = if should_be_equal { " === " } else { " !== " };
            return Ok(docvec![left_doc, operator, right_doc]);
        }

        // Other types must be compared using structural equality
        let left =
            self.not_in_tail_position(Some(Ordering::Strict), |this| this.wrap_expression(left))?;
        let right =
            self.not_in_tail_position(Some(Ordering::Strict), |this| this.wrap_expression(right))?;
        Ok(self.prelude_equal_call(should_be_equal, left, right))
    }

    fn equal_with_doc_operands(
        &mut self,
        left: Document<'a>,
        right: Document<'a>,
        type_: Arc<Type>,
        should_be_equal: bool,
    ) -> Document<'a> {
        // If it is a simple scalar type then we can use JS' reference identity
        if is_js_scalar(type_) {
            let operator = if should_be_equal { " === " } else { " !== " };
            return docvec![left, operator, right];
        }

        // Other types must be compared using structural equality
        self.prelude_equal_call(should_be_equal, left, right)
    }

    pub(super) fn prelude_equal_call(
        &mut self,
        should_be_equal: bool,
        left: Document<'a>,
        right: Document<'a>,
    ) -> Document<'a> {
        // Record that we need to import the prelude's isEqual function into the module
        self.tracker.object_equality_used = true;
        // Construct the call
        let args = wrap_args([left, right]);
        let operator = if should_be_equal {
            "isEqual"
        } else {
            "!isEqual"
        };
        docvec![operator, args]
    }

    fn print_bin_op(
        &mut self,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        op: &'a str,
    ) -> Output<'a> {
        let left =
            self.not_in_tail_position(Some(Ordering::Strict), |this| this.child_expression(left))?;
        let right =
            self.not_in_tail_position(Some(Ordering::Strict), |this| this.child_expression(right))?;
        Ok(docvec![left, " ", op, " ", right])
    }

    fn bin_op_with_doc_operands(
        &mut self,
        name: BinOp,
        left: Document<'a>,
        right: Document<'a>,
        type_: &Arc<Type>,
    ) -> Document<'a> {
        match name {
            BinOp::And => docvec![left, " && ", right],
            BinOp::Or => docvec![left, " || ", right],
            BinOp::LtInt | BinOp::LtFloat => docvec![left, " < ", right],
            BinOp::LtEqInt | BinOp::LtEqFloat => docvec![left, " <= ", right],
            BinOp::Eq => self.equal_with_doc_operands(left, right, type_.clone(), true),
            BinOp::NotEq => self.equal_with_doc_operands(left, right, type_.clone(), false),
            BinOp::GtInt | BinOp::GtFloat => docvec![left, " > ", right],
            BinOp::GtEqInt | BinOp::GtEqFloat => docvec![left, " >= ", right],
            BinOp::Concatenate | BinOp::AddInt | BinOp::AddFloat => {
                docvec![left, " + ", right]
            }
            BinOp::SubInt | BinOp::SubFloat => docvec![left, " - ", right],
            BinOp::MultInt | BinOp::MultFloat => docvec![left, " * ", right],
            BinOp::RemainderInt => {
                self.tracker.int_remainder_used = true;
                docvec!["remainderInt", wrap_args([left, right])]
            }
            BinOp::DivInt => {
                self.tracker.int_remainder_used = true;
                docvec!["divideInt", wrap_args([left, right])]
            }
            BinOp::DivFloat => {
                self.tracker.int_remainder_used = true;
                docvec!["divideFloat", wrap_args([left, right])]
            }
        }
    }

    fn todo(&mut self, message: Option<&'a TypedExpr>, location: &'a SrcSpan) -> Output<'a> {
        let message = match message {
            Some(m) => self.not_in_tail_position(None, |this| this.wrap_expression(m))?,
            None => string("`todo` expression evaluated. This code has not yet been implemented."),
        };
        let doc = self.throw_error("todo", &message, *location, vec![]);

        Ok(doc)
    }

    fn panic(&mut self, location: &'a SrcSpan, message: Option<&'a TypedExpr>) -> Output<'a> {
        let message = match message {
            Some(m) => self.not_in_tail_position(None, |this| this.wrap_expression(m))?,
            None => string("`panic` expression evaluated."),
        };
        let doc = self.throw_error("panic", &message, *location, vec![]);

        Ok(doc)
    }

    pub(crate) fn throw_error<Fields>(
        &mut self,
        error_name: &'a str,
        message: &Document<'a>,
        location: SrcSpan,
        fields: Fields,
    ) -> Document<'a>
    where
        Fields: IntoIterator<Item = (&'a str, Document<'a>)>,
    {
        self.tracker.make_error_used = true;
        let module = self.module_name.clone().to_doc().surround('"', '"');
        let function = self.function_name.clone().to_doc().surround("\"", "\"");
        let line = self.line_numbers.line_number(location.start).to_doc();
        let fields = wrap_object(fields.into_iter().map(|(k, v)| (k.to_doc(), Some(v))));

        docvec![
            "throw makeError",
            wrap_args([
                string(error_name),
                "FILEPATH".to_doc(),
                module,
                line,
                function,
                message.clone(),
                fields
            ]),
        ]
    }

    fn module_select(
        &mut self,
        module: &'a str,
        label: &'a EcoString,
        constructor: &'a ModuleValueConstructor,
    ) -> Document<'a> {
        match constructor {
            ModuleValueConstructor::Fn { .. } | ModuleValueConstructor::Constant { .. } => {
                docvec!["$", module, ".", maybe_escape_identifier(label)]
            }

            ModuleValueConstructor::Record {
                name, arity, type_, ..
            } => record_constructor(type_.clone(), Some(module), name, *arity, self.tracker),
        }
    }

    fn echo(&mut self, expression: Document<'a>, location: &'a SrcSpan) -> Output<'a> {
        self.tracker.echo_used = true;

        let echo_argument = call_arguments(vec![
            Ok(expression),
            Ok(self.src_path.clone().to_doc()),
            Ok(self.line_numbers.line_number(location.start).to_doc()),
        ])?;
        Ok(self.wrap_return(docvec!["echo", echo_argument]))
    }

    pub(crate) fn constant_expression(
        &mut self,
        context: Context,
        expression: &'a TypedConstant,
    ) -> Output<'a> {
        match expression {
            Constant::Int { value, .. } => Ok(int(value)),
            Constant::Float { value, .. } => Ok(float(value)),
            Constant::String { value, .. } => Ok(string(value)),
            Constant::Tuple { elements, .. } => array(
                elements
                    .iter()
                    .map(|element| self.constant_expression(context, element)),
            ),

            Constant::List { elements, .. } => {
                self.tracker.list_used = true;
                let list = list(
                    elements
                        .iter()
                        .map(|element| self.constant_expression(context, element)),
                )?;

                match context {
                    Context::Constant => Ok(docvec!["/* @__PURE__ */ ", list]),
                    Context::Function => Ok(list),
                }
            }

            Constant::Record { type_, name, .. } if type_.is_bool() && name == "True" => {
                Ok("true".to_doc())
            }
            Constant::Record { type_, name, .. } if type_.is_bool() && name == "False" => {
                Ok("false".to_doc())
            }
            Constant::Record { type_, .. } if type_.is_nil() => Ok("undefined".to_doc()),

            Constant::Record {
                args,
                module,
                name,
                tag,
                type_,
                ..
            } => {
                if type_.is_result() {
                    if tag == "Ok" {
                        self.tracker.ok_used = true;
                    } else {
                        self.tracker.error_used = true;
                    }
                }

                // If there's no arguments and the type is a function that takes
                // arguments then this is the constructor being referenced, not the
                // function being called.
                if let Some(arity) = type_.fn_arity() {
                    if args.is_empty() && arity != 0 {
                        let arity = arity as u16;
                        return Ok(record_constructor(
                            type_.clone(),
                            None,
                            name,
                            arity,
                            self.tracker,
                        ));
                    }
                }

                let field_values: Vec<_> = args
                    .iter()
                    .map(|arg| self.constant_expression(context, &arg.value))
                    .try_collect()?;

                let constructor = construct_record(
                    module.as_ref().map(|(module, _)| module.as_str()),
                    name,
                    field_values,
                );
                match context {
                    Context::Constant => Ok(docvec!["/* @__PURE__ */ ", constructor]),
                    Context::Function => Ok(constructor),
                }
            }

            Constant::BitArray { segments, .. } => {
                let bit_array = self.constant_bit_array(segments)?;
                match context {
                    Context::Constant => Ok(docvec!["/* @__PURE__ */ ", bit_array]),
                    Context::Function => Ok(bit_array),
                }
            }

            Constant::Var { name, module, .. } => Ok({
                match module {
                    None => maybe_escape_identifier(name).to_doc(),
                    Some((module, _)) => {
                        // JS keywords can be accessed here, but we must escape anyway
                        // as we escape when exporting such names in the first place,
                        // and the imported name has to match the exported name.
                        docvec!["$", module, ".", maybe_escape_identifier(name)]
                    }
                }
            }),

            Constant::StringConcatenation { left, right, .. } => {
                let left = self.constant_expression(context, left)?;
                let right = self.constant_expression(context, right)?;
                Ok(docvec![left, " + ", right])
            }

            Constant::Invalid { .. } => {
                panic!("invalid constants should not reach code generation")
            }
        }
    }

    fn constant_bit_array(&mut self, segments: &'a [TypedConstantBitArraySegment]) -> Output<'a> {
        use BitArrayOption as Opt;

        self.tracker.bit_array_literal_used = true;
        let segments_array = array(segments.iter().map(|segment| {
            let value = self.constant_expression(Context::Constant, &segment.value)?;

            match segment.options.as_slice() {
                // Int segment
                _ if segment.type_.is_int() => {
                    let details = self.constant_sized_bit_array_segment_details(segment)?;
                    match (details.size_value, segment.value.as_ref()) {
                        (Some(size_value), Constant::Int { int_value, .. })
                            if size_value <= SAFE_INT_SEGMENT_MAX_SIZE.into()
                                && (&size_value % BigInt::from(8) == BigInt::ZERO) =>
                        {
                            let bytes = bit_array_segment_int_value_to_bytes(
                                int_value.clone(),
                                size_value,
                                segment.endianness(),
                            )?;

                            Ok(u8_slice(&bytes))
                        }

                        (Some(size_value), _) if size_value == 8.into() => Ok(value),

                        (Some(size_value), _) if size_value <= 0.into() => Ok(nil()),

                        _ => {
                            self.tracker.sized_integer_segment_used = true;
                            let size = details.size;
                            let is_big = bool(segment.endianness().is_big());
                            Ok(docvec!["sizedInt(", value, ", ", size, ", ", is_big, ")"])
                        }
                    }
                }

                // Float segments
                _ if segment.type_.is_float() => {
                    self.tracker.float_bit_array_segment_used = true;
                    let details = self.constant_sized_bit_array_segment_details(segment)?;
                    let size = details.size;
                    let is_big = bool(segment.endianness().is_big());
                    Ok(docvec!["sizedFloat(", value, ", ", size, ", ", is_big, ")"])
                }

                // UTF8 strings
                [Opt::Utf8 { .. }] => {
                    self.tracker.string_bit_array_segment_used = true;
                    Ok(docvec!["stringBits(", value, ")"])
                }

                // UTF8 codepoints
                [Opt::Utf8Codepoint { .. }] => {
                    self.tracker.codepoint_bit_array_segment_used = true;
                    Ok(docvec!["codepointBits(", value, ")"])
                }

                // UTF16 strings
                [Opt::Utf16 { .. }]
                | [Opt::Utf16 { .. }, Opt::Big { .. }]
                | [Opt::Big { .. }, Opt::Utf16 { .. }] => {
                    self.tracker.string_utf16_bit_array_segment_used = true;
                    let is_big = "true".to_doc();
                    Ok(docvec!["stringToUtf16(", value, ", ", is_big, ")"])
                }

                [Opt::Utf16 { .. }, Opt::Little { .. }]
                | [Opt::Little { .. }, Opt::Utf16 { .. }] => {
                    self.tracker.string_utf16_bit_array_segment_used = true;
                    let is_big = "false".to_doc();
                    Ok(docvec!["stringToUtf16(", value, ", ", is_big, ")"])
                }

                // UTF16 codepoints
                [Opt::Utf16Codepoint { .. }]
                | [Opt::Utf16Codepoint { .. }, Opt::Big { .. }]
                | [Opt::Big { .. }, Opt::Utf16Codepoint { .. }] => {
                    self.tracker.codepoint_utf16_bit_array_segment_used = true;
                    let is_big = "true".to_doc();
                    Ok(docvec!["codepointToUtf16(", value, ", ", is_big, ")"])
                }

                [Opt::Utf16Codepoint { .. }, Opt::Little { .. }]
                | [Opt::Little { .. }, Opt::Utf16Codepoint { .. }] => {
                    self.tracker.codepoint_utf16_bit_array_segment_used = true;
                    let is_big = "false".to_doc();
                    Ok(docvec!["codepointToUtf16(", value, ", ", is_big, ")"])
                }

                // UTF32 strings
                [Opt::Utf32 { .. }]
                | [Opt::Utf32 { .. }, Opt::Big { .. }]
                | [Opt::Big { .. }, Opt::Utf32 { .. }] => {
                    self.tracker.string_utf32_bit_array_segment_used = true;
                    let is_big = "true".to_doc();
                    Ok(docvec!["stringToUtf32(", value, ", ", is_big, ")"])
                }

                [Opt::Utf32 { .. }, Opt::Little { .. }]
                | [Opt::Little { .. }, Opt::Utf32 { .. }] => {
                    self.tracker.string_utf32_bit_array_segment_used = true;
                    let is_big = "false".to_doc();
                    Ok(docvec!["stringToUtf32(", value, ", ", is_big, ")"])
                }

                // UTF32 codepoints
                [Opt::Utf32Codepoint { .. }]
                | [Opt::Utf32Codepoint { .. }, Opt::Big { .. }]
                | [Opt::Big { .. }, Opt::Utf32Codepoint { .. }] => {
                    self.tracker.codepoint_utf32_bit_array_segment_used = true;
                    let is_big = "true".to_doc();
                    Ok(docvec!["codepointToUtf32(", value, ", ", is_big, ")"])
                }

                [Opt::Utf32Codepoint { .. }, Opt::Little { .. }]
                | [Opt::Little { .. }, Opt::Utf32Codepoint { .. }] => {
                    self.tracker.codepoint_utf32_bit_array_segment_used = true;
                    let is_big = "false".to_doc();
                    Ok(docvec!["codepointToUtf32(", value, ", ", is_big, ")"])
                }

                // Bit arrays
                [Opt::Bits { .. }] => Ok(value),

                // Bit arrays with explicit size. The explicit size slices the bit array to the
                // specified size. A runtime exception is thrown if the size exceeds the number
                // of bits in the bit array.
                [Opt::Bits { .. }, Opt::Size { value: size, .. }]
                | [Opt::Size { value: size, .. }, Opt::Bits { .. }] => match &**size {
                    Constant::Int { value: size, .. } => {
                        self.tracker.bit_array_slice_used = true;
                        Ok(docvec!["bitArraySlice(", value, ", 0, ", size, ")"])
                    }

                    _ => Err(Error::Unsupported {
                        feature: "This bit array segment option".into(),
                        location: segment.location,
                    }),
                },

                // Anything else
                _ => Err(Error::Unsupported {
                    feature: "This bit array segment option".into(),
                    location: segment.location,
                }),
            }
        }))?;

        Ok(docvec!["toBitArray(", segments_array, ")"])
    }

    fn constant_sized_bit_array_segment_details(
        &mut self,
        segment: &'a TypedConstantBitArraySegment,
    ) -> Result<SizedBitArraySegmentDetails<'a>, Error> {
        let size = segment.size();
        let unit = segment.unit();
        let (size_value, size) = match size {
            Some(Constant::Int { int_value, .. }) => {
                let size_value = int_value * unit;
                let size = eco_format!("{}", size_value).to_doc();
                (Some(size_value), size)
            }

            Some(size) => {
                let mut size = self.constant_expression(Context::Constant, size)?;
                if unit != 1 {
                    size = size.group().append(" * ".to_doc().append(unit.to_doc()));
                }

                (None, size)
            }

            None => {
                let size_value: usize = if segment.type_.is_int() { 8 } else { 64 };
                (Some(BigInt::from(size_value)), docvec![size_value])
            }
        };

        Ok(SizedBitArraySegmentDetails { size, size_value })
    }

    pub(crate) fn guard(&mut self, guard: &'a TypedClauseGuard) -> Output<'a> {
        match guard {
            ClauseGuard::Equals { left, right, .. } if is_js_scalar(left.type_()) => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                Ok(docvec![left, " === ", right])
            }

            ClauseGuard::NotEquals { left, right, .. } if is_js_scalar(left.type_()) => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                Ok(docvec![left, " !== ", right])
            }

            ClauseGuard::Equals { left, right, .. } => {
                let left = self.guard(left)?;
                let right = self.guard(right)?;
                Ok(self.prelude_equal_call(true, left, right))
            }

            ClauseGuard::NotEquals { left, right, .. } => {
                let left = self.guard(left)?;
                let right = self.guard(right)?;
                Ok(self.prelude_equal_call(false, left, right))
            }

            ClauseGuard::GtFloat { left, right, .. } | ClauseGuard::GtInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                Ok(docvec![left, " > ", right])
            }

            ClauseGuard::GtEqFloat { left, right, .. }
            | ClauseGuard::GtEqInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                Ok(docvec![left, " >= ", right])
            }

            ClauseGuard::LtFloat { left, right, .. } | ClauseGuard::LtInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                Ok(docvec![left, " < ", right])
            }

            ClauseGuard::LtEqFloat { left, right, .. }
            | ClauseGuard::LtEqInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                Ok(docvec![left, " <= ", right])
            }

            ClauseGuard::AddFloat { left, right, .. } | ClauseGuard::AddInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                Ok(docvec![left, " + ", right])
            }

            ClauseGuard::SubFloat { left, right, .. } | ClauseGuard::SubInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                Ok(docvec![left, " - ", right])
            }

            ClauseGuard::MultFloat { left, right, .. }
            | ClauseGuard::MultInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                Ok(docvec![left, " * ", right])
            }

            ClauseGuard::DivFloat { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                self.tracker.float_division_used = true;
                Ok(docvec!["divideFloat", wrap_args([left, right])])
            }

            ClauseGuard::DivInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                self.tracker.int_division_used = true;
                Ok(docvec!["divideInt", wrap_args([left, right])])
            }

            ClauseGuard::RemainderInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                self.tracker.int_remainder_used = true;
                Ok(docvec!["remainderInt", wrap_args([left, right])])
            }

            ClauseGuard::Or { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                Ok(docvec![left, " || ", right])
            }

            ClauseGuard::And { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                Ok(docvec![left, " && ", right])
            }

            ClauseGuard::Var { name, .. } => Ok(self.local_var(name).to_doc()),

            ClauseGuard::TupleIndex { tuple, index, .. } => {
                Ok(docvec![self.guard(tuple,)?, "[", index, "]"])
            }

            ClauseGuard::FieldAccess {
                label, container, ..
            } => Ok(docvec![
                self.guard(container,)?,
                ".",
                maybe_escape_property(label)
            ]),

            ClauseGuard::ModuleSelect {
                module_alias,
                label,
                ..
            } => Ok(docvec!["$", module_alias, ".", label]),

            ClauseGuard::Not { expression, .. } => Ok(docvec!["!", self.guard(expression,)?]),

            ClauseGuard::Constant(constant) => self.guard_constant_expression(constant),
        }
    }

    fn wrapped_guard(&mut self, guard: &'a TypedClauseGuard) -> Result<Document<'a>, Error> {
        match guard {
            ClauseGuard::Var { .. }
            | ClauseGuard::TupleIndex { .. }
            | ClauseGuard::Constant(_)
            | ClauseGuard::Not { .. }
            | ClauseGuard::FieldAccess { .. } => self.guard(guard),

            ClauseGuard::Equals { .. }
            | ClauseGuard::NotEquals { .. }
            | ClauseGuard::GtInt { .. }
            | ClauseGuard::GtEqInt { .. }
            | ClauseGuard::LtInt { .. }
            | ClauseGuard::LtEqInt { .. }
            | ClauseGuard::GtFloat { .. }
            | ClauseGuard::GtEqFloat { .. }
            | ClauseGuard::LtFloat { .. }
            | ClauseGuard::LtEqFloat { .. }
            | ClauseGuard::AddInt { .. }
            | ClauseGuard::AddFloat { .. }
            | ClauseGuard::SubInt { .. }
            | ClauseGuard::SubFloat { .. }
            | ClauseGuard::MultInt { .. }
            | ClauseGuard::MultFloat { .. }
            | ClauseGuard::DivInt { .. }
            | ClauseGuard::DivFloat { .. }
            | ClauseGuard::RemainderInt { .. }
            | ClauseGuard::Or { .. }
            | ClauseGuard::And { .. }
            | ClauseGuard::ModuleSelect { .. } => Ok(docvec!["(", self.guard(guard,)?, ")"]),
        }
    }

    fn guard_constant_expression(&mut self, expression: &'a TypedConstant) -> Output<'a> {
        match expression {
            Constant::Tuple { elements, .. } => array(
                elements
                    .iter()
                    .map(|element| self.guard_constant_expression(element)),
            ),

            Constant::List { elements, .. } => {
                self.tracker.list_used = true;
                list(
                    elements
                        .iter()
                        .map(|element| self.guard_constant_expression(element)),
                )
            }
            Constant::Record { type_, name, .. } if type_.is_bool() && name == "True" => {
                Ok("true".to_doc())
            }
            Constant::Record { type_, name, .. } if type_.is_bool() && name == "False" => {
                Ok("false".to_doc())
            }
            Constant::Record { type_, .. } if type_.is_nil() => Ok("undefined".to_doc()),

            Constant::Record {
                args,
                module,
                name,
                tag,
                type_,
                ..
            } => {
                if type_.is_result() {
                    if tag == "Ok" {
                        self.tracker.ok_used = true;
                    } else {
                        self.tracker.error_used = true;
                    }
                }

                // If there's no arguments and the type is a function that takes
                // arguments then this is the constructor being referenced, not the
                // function being called.
                if let Some(arity) = type_.fn_arity() {
                    if args.is_empty() && arity != 0 {
                        let arity = arity as u16;
                        return Ok(record_constructor(
                            type_.clone(),
                            None,
                            name,
                            arity,
                            self.tracker,
                        ));
                    }
                }

                let field_values: Vec<_> = args
                    .iter()
                    .map(|arg| self.guard_constant_expression(&arg.value))
                    .try_collect()?;
                Ok(construct_record(
                    module.as_ref().map(|(module, _)| module.as_str()),
                    name,
                    field_values,
                ))
            }

            Constant::BitArray { segments, .. } => self.constant_bit_array(segments),

            Constant::Var { name, .. } => Ok(self.local_var(name).to_doc()),

            expression => self.constant_expression(Context::Function, expression),
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

pub fn int(value: &str) -> Document<'_> {
    eco_string_int(value.into())
}

pub fn eco_string_int<'a>(value: EcoString) -> Document<'a> {
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

    let value = value.trim_start_matches('0');
    if value.is_empty() {
        out.push('0');
    }

    // If the number starts with a `0` then an underscore, the `0` will be stripped,
    // leaving the number to look something like `_1_2_3`, which is not valid syntax.
    // Therefore, we strip the `_` to avoid this case.
    let value = value.trim_start_matches('_');

    out.push_str(value);

    out.to_doc()
}

pub fn float(value: &str) -> Document<'_> {
    let mut out = EcoString::with_capacity(value.len());

    if value.starts_with('-') {
        out.push('-');
    } else if value.starts_with('+') {
        out.push('+');
    };
    let value = value.trim_start_matches(['+', '-'].as_ref());

    let value = value.trim_start_matches('0');
    if value.starts_with(['.', 'e', 'E']) {
        out.push('0');
    }
    out.push_str(value);

    out.to_doc()
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
    Function,
}

#[derive(Debug)]
struct SizedBitArraySegmentDetails<'a> {
    size: Document<'a>,
    /// The size of the bit array segment stored as a BigInt.
    /// This has a value when the segment's size is known at compile time.
    size_value: Option<BigInt>,
}

pub fn string(value: &str) -> Document<'_> {
    if value.contains('\n') {
        EcoString::from(value.replace('\n', r"\n"))
            .to_doc()
            .surround("\"", "\"")
    } else {
        value.to_doc().surround("\"", "\"")
    }
}

pub(crate) fn array<'a, Elements: IntoIterator<Item = Output<'a>>>(
    elements: Elements,
) -> Output<'a> {
    let elements = Itertools::intersperse(elements.into_iter(), Ok(break_(",", ", ")))
        .collect::<Result<Vec<_>, _>>()?;
    if elements.is_empty() {
        // Do not add a trailing comma since that adds an 'undefined' element
        Ok("[]".to_doc())
    } else {
        Ok(docvec![
            "[",
            docvec![break_("", ""), elements].nest(INDENT),
            break_(",", ""),
            "]"
        ]
        .group())
    }
}

pub(crate) fn list<'a, I: IntoIterator<Item = Output<'a>>>(elements: I) -> Output<'a>
where
    I::IntoIter: DoubleEndedIterator + ExactSizeIterator,
{
    let array = array(elements);
    Ok(docvec!["toList(", array?, ")"])
}

fn prepend<'a, I: IntoIterator<Item = Output<'a>>>(elements: I, tail: Document<'a>) -> Output<'a>
where
    I::IntoIter: DoubleEndedIterator + ExactSizeIterator,
{
    elements.into_iter().rev().try_fold(tail, |tail, element| {
        let args = call_arguments([element, Ok(tail)])?;
        Ok(docvec!["listPrepend", args])
    })
}

fn call_arguments<'a, Elements: IntoIterator<Item = Output<'a>>>(elements: Elements) -> Output<'a> {
    let elements = Itertools::intersperse(elements.into_iter(), Ok(break_(",", ", ")))
        .collect::<Result<Vec<_>, _>>()?
        .to_doc();
    if elements.is_empty() {
        return Ok("()".to_doc());
    }
    Ok(docvec![
        "(",
        docvec![break_("", ""), elements].nest(INDENT),
        break_(",", ""),
        ")"
    ]
    .group())
}

pub(crate) fn construct_record<'a>(
    module: Option<&'a str>,
    name: &'a str,
    arguments: impl IntoIterator<Item = Document<'a>>,
) -> Document<'a> {
    let mut any_arguments = false;
    let arguments = join(
        arguments.into_iter().inspect(|_| {
            any_arguments = true;
        }),
        break_(",", ", "),
    );
    let arguments = docvec![break_("", ""), arguments].nest(INDENT);
    let name = if let Some(module) = module {
        docvec!["$", module, ".", name]
    } else {
        name.to_doc()
    };
    if any_arguments {
        docvec!["new ", name, "(", arguments, break_(",", ""), ")"].group()
    } else {
        docvec!["new ", name, "()"]
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

fn requires_semicolon(statement: &TypedStatement) -> bool {
    match statement {
        Statement::Expression(
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
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Block { .. },
        ) => true,

        Statement::Expression(
            TypedExpr::Todo { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::Invalid { .. },
        ) => false,

        Statement::Assignment(_) => false,
        Statement::Use(_) => false,
        Statement::Assert(_) => false,
    }
}

/// Wrap a document in an immediately invoked function expression
fn immediately_invoked_function_expression_document(document: Document<'_>) -> Document<'_> {
    docvec![
        docvec!["(() => {", break_("", " "), document].nest(INDENT),
        break_("", " "),
        "})()",
    ]
    .group()
}

pub(crate) fn record_constructor<'a>(
    type_: Arc<Type>,
    qualifier: Option<&'a str>,
    name: &'a str,
    arity: u16,
    tracker: &mut UsageTracker,
) -> Document<'a> {
    if qualifier.is_none() && type_.is_result_constructor() {
        if name == "Ok" {
            tracker.ok_used = true;
        } else if name == "Error" {
            tracker.error_used = true;
        }
    }
    if type_.is_bool() && name == "True" {
        "true".to_doc()
    } else if type_.is_bool() {
        "false".to_doc()
    } else if type_.is_nil() {
        "undefined".to_doc()
    } else if arity == 0 {
        match qualifier {
            Some(module) => docvec!["new $", module, ".", name, "()"],
            None => docvec!["new ", name, "()"],
        }
    } else {
        let vars = (0..arity).map(|i| eco_format!("var{i}").to_doc());
        let body = docvec![
            "return ",
            construct_record(qualifier, name, vars.clone()),
            ";"
        ];
        docvec![
            docvec![wrap_args(vars), " => {", break_("", " "), body]
                .nest(INDENT)
                .append(break_("", " "))
                .group(),
            "}",
        ]
    }
}

fn u8_slice<'a>(bytes: &[u8]) -> Document<'a> {
    let s: EcoString = bytes
        .iter()
        .map(u8::to_string)
        .collect::<Vec<_>>()
        .join(", ")
        .into();

    docvec![s]
}
