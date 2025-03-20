use num_bigint::BigInt;
use vec1::Vec1;

use super::{
    pattern::{Assignment, CompiledPattern},
    *,
};
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

#[derive(Debug)]
pub(crate) struct Generator<'module, 'ast> {
    module_name: EcoString,
    src_path: &'module Utf8Path,
    project_root: &'module Utf8Path,
    line_numbers: &'module LineNumbers,
    function_name: Option<EcoString>,
    function_arguments: Vec<Option<&'module EcoString>>,
    current_scope_vars: im::HashMap<EcoString, usize>,
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
}

impl<'module, 'a> Generator<'module, 'a> {
    #[allow(clippy::too_many_arguments)] // TODO: FIXME
    pub fn new(
        module_name: EcoString,
        src_path: &'module Utf8Path,
        project_root: &'module Utf8Path,
        line_numbers: &'module LineNumbers,
        function_name: EcoString,
        function_arguments: Vec<Option<&'module EcoString>>,
        tracker: &'module mut UsageTracker,
        mut current_scope_vars: im::HashMap<EcoString, usize>,
    ) -> Self {
        let mut function_name = Some(function_name);
        for &name in function_arguments.iter().flatten() {
            // Initialise the function arguments
            let _ = current_scope_vars.insert(name.clone(), 0);

            // If any of the function arguments shadow the current function then
            // recursion is no longer possible.
            if function_name.as_ref() == Some(name) {
                function_name = None;
            }
        }
        Self {
            tracker,
            module_name,
            src_path,
            project_root,
            line_numbers,
            function_name,
            function_arguments,
            tail_recursion_used: false,
            current_scope_vars,
            function_position: Position::Tail,
            scope_position: Position::Tail,
            statement_level: Vec::new(),
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
            Statement::Use(_use) => self.expression(&_use.call),
        }?;
        if self.statement_level.is_empty() {
            Ok(expression_doc)
        } else {
            let mut statements = std::mem::take(&mut self.statement_level);
            statements.push(expression_doc);
            Ok(Itertools::intersperse(statements.into_iter(), line())
                .collect_vec()
                .to_doc())
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
                subjects, clauses, ..
            } => self.case(subjects, clauses),

            TypedExpr::Call { fun, args, .. } => self.call(fun, args),
            TypedExpr::Fn { args, body, .. } => self.fn_(args, body),

            TypedExpr::RecordAccess { record, label, .. } => self.record_access(record, label),
            TypedExpr::RecordUpdate {
                record,
                constructor,
                args,
                ..
            } => self.record_update(record, constructor, args),

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
        self.tracker.bit_array_literal_used = true;

        use BitArrayOption as Opt;

        // Collect all the values used in segments.
        let segments_array = array(segments.iter().map(|segment| {
            let value = self.not_in_tail_position(Some(Ordering::Strict), |this| {
                this.wrap_expression(&segment.value)
            })?;

            if segment.type_.is_int() || segment.type_.is_float() {
                let details = self.sized_bit_array_segment_details(segment)?;

                if segment.type_.is_int() {
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
                            Ok(docvec![
                                "sizedInt(",
                                value,
                                ", ",
                                details.size,
                                ", ",
                                bool(segment.endianness().is_big()),
                                ")"
                            ])
                        }
                    }
                } else {
                    self.tracker.float_bit_array_segment_used = true;
                    Ok(docvec![
                        "sizedFloat(",
                        value,
                        ", ",
                        details.size,
                        ", ",
                        bool(segment.endianness().is_big()),
                        ")"
                    ])
                }
            } else {
                match segment.options.as_slice() {
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
            }
        }))?;

        Ok(docvec!["toBitArray(", segments_array, ")"])
    }

    fn sized_bit_array_segment_details(
        &mut self,
        segment: &'a TypedExprBitArraySegment,
    ) -> Result<SizedBitArraySegmentDetails<'a>, Error> {
        if segment.has_native_option() {
            return Err(Error::Unsupported {
                feature: "This bit array segment option".into(),
                location: segment.location,
            });
        }

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

    pub fn not_in_tail_position<CompileFn>(
        &mut self,
        // If ordering is None, it is inherited from the parent scope.
        // It will be None in cases like `!x`, where `x` can be lifted
        // only if the ordering is already loose.
        ordering: Option<Ordering>,
        compile: CompileFn,
    ) -> Output<'a>
    where
        CompileFn: Fn(&mut Self) -> Output<'a>,
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
                | TypedExpr::RecordUpdate { .. },
                Position::NotTail(Ordering::Loose),
            ) => self.wrap_block(|this| this.expression(expression)),
            (
                TypedExpr::Panic { .. }
                | TypedExpr::Todo { .. }
                | TypedExpr::Case { .. }
                | TypedExpr::Pipeline { .. }
                | TypedExpr::RecordUpdate { .. },
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
        let doc = immediately_invoked_function_expression_document(result?);
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
                constant_expression(Context::Function, self.tracker, literal)
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
                    documents.push(self.not_in_tail_position(Some(Ordering::Strict), |this| {
                        this.simple_variable_assignment(&assignment.name, &assignment.value)
                    })?);
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
            _ => documents.push(self.expression(finally)?),
        }

        Ok(documents.to_doc().force_break())
    }

    fn expression_flattening_blocks(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        match expression {
            TypedExpr::Block { statements, .. } => self.statements(statements),
            _ => self.expression(expression),
        }
    }

    fn block(&mut self, statements: &'a Vec1<TypedStatement>) -> Output<'a> {
        if statements.len() == 1 {
            match statements.first() {
                Statement::Expression(expression) => self.child_expression(expression),

                Statement::Assignment(assignment) => {
                    self.child_expression(assignment.value.as_ref())
                }

                Statement::Use(use_) => self.child_expression(&use_.call),
            }
        } else {
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
        let assignment = if self.scope_position.is_tail() {
            docvec![assignment, line(), "return ", js_name, ";"]
        } else {
            assignment
        };

        Ok(assignment.force_break())
    }

    fn assignment(&mut self, assignment: &'a TypedAssignment) -> Output<'a> {
        let TypedAssignment {
            pattern,
            kind,
            value,
            annotation: _,
            location: _,
        } = assignment;

        // If it is a simple assignment to a variable we can generate a normal
        // JS assignment
        if let TypedPattern::Variable { name, .. } = pattern {
            return self.simple_variable_assignment(name, value);
        }

        // Otherwise we need to compile the patterns
        let (subject, subject_assignment) = pattern::assign_subject(self, value);
        // Value needs to be rendered before traversing pattern to have correctly incremented variables.
        let value =
            self.not_in_tail_position(Some(Ordering::Loose), |this| this.wrap_expression(value))?;
        let mut pattern_generator = pattern::Generator::new(self);
        pattern_generator.traverse_pattern(&subject, pattern)?;
        let compiled = pattern_generator.take_compiled();

        // If we are in tail position we can return value being assigned
        let afterwards = if self.scope_position.is_tail() {
            docvec![
                line(),
                "return ",
                subject_assignment.clone().unwrap_or_else(|| value.clone()),
                ";"
            ]
        } else {
            nil()
        };

        let compiled =
            self.pattern_into_assignment_doc(compiled, subject, pattern.location(), kind)?;
        // If there is a subject name given create a variable to hold it for
        // use in patterns
        let doc = match subject_assignment {
            Some(name) => docvec!["let ", name, " = ", value, ";", line(), compiled],
            None => compiled,
        };

        Ok(doc.append(afterwards).force_break())
    }

    fn case(&mut self, subject_values: &'a [TypedExpr], clauses: &'a [TypedClause]) -> Output<'a> {
        let (subjects, subject_assignments): (Vec<_>, Vec<_>) =
            pattern::assign_subjects(self, subject_values)
                .into_iter()
                .unzip();
        let mut generator = pattern::Generator::new(self);

        let mut doc = nil();

        // We wish to be able to know whether this is the first or clause being
        // processed, so record the index number. We use this instead of
        // `Iterator.enumerate` because we are using a nested for loop.
        let mut clause_number = 0;
        let total_patterns: usize = clauses
            .iter()
            .map(|c| c.alternative_patterns.len())
            .sum::<usize>()
            + clauses.len();

        // A case has many clauses `pattern -> consequence`
        for clause in clauses {
            let multipattern = std::iter::once(&clause.pattern);
            let multipatterns = multipattern.chain(&clause.alternative_patterns);

            // A clause can have many patterns `pattern, pattern ->...`
            for multipatterns in multipatterns {
                let scope = generator.expression_generator.current_scope_vars.clone();
                let mut compiled =
                    generator.generate(&subjects, multipatterns, clause.guard.as_ref())?;
                let consequence = generator
                    .expression_generator
                    .expression_flattening_blocks(&clause.then)?;

                // We've seen one more clause
                clause_number += 1;

                // Reset the scope now that this clause has finished, causing the
                // variables to go out of scope.
                generator.expression_generator.current_scope_vars = scope;

                // If the pattern assigns any variables we need to render assignments
                let body = if compiled.has_assignments() {
                    let assignments = generator
                        .expression_generator
                        .pattern_take_assignments_doc(&mut compiled);
                    docvec![assignments, line(), consequence]
                } else {
                    consequence
                };

                let is_final_clause = clause_number == total_patterns;
                let is_first_clause = clause_number == 1;
                let is_only_clause = is_final_clause && is_first_clause;

                doc = if is_only_clause {
                    // If this is the only clause and there are no checks then we can
                    // render just the body as the case does nothing
                    // A block is used as it could declare variables still.
                    doc.append("{")
                        .append(docvec![line(), body].nest(INDENT))
                        .append(line())
                        .append("}")
                } else if is_final_clause {
                    // If this is the final clause and there are no checks then we can
                    // render `else` instead of `else if (...)`
                    doc.append(" else {")
                        .append(docvec![line(), body].nest(INDENT))
                        .append(line())
                        .append("}")
                } else {
                    doc.append(if is_first_clause {
                        "if ("
                    } else {
                        " else if ("
                    })
                    .append(
                        generator
                            .expression_generator
                            .pattern_take_checks_doc(&mut compiled, true),
                    )
                    .append(") {")
                    .append(docvec![line(), body].nest(INDENT))
                    .append(line())
                    .append("}")
                };
            }
        }

        // If there is a subject name given create a variable to hold it for
        // use in patterns
        let subject_assignments: Vec<_> = subject_assignments
            .into_iter()
            .zip(subject_values)
            .flat_map(|(assignment_name, value)| assignment_name.map(|name| (name, value)))
            .map(|(name, value)| {
                let value = self.not_in_tail_position(Some(Ordering::Strict), |this| {
                    this.wrap_expression(value)
                })?;
                Ok(docvec!["let ", name, " = ", value, ";", line()])
            })
            .try_collect()?;

        Ok(docvec![subject_assignments, doc].force_break())
    }

    fn assignment_no_match(
        &mut self,
        location: SrcSpan,
        subject: Document<'a>,
        message: Option<&'a TypedExpr>,
    ) -> Output<'a> {
        let message = match message {
            Some(m) => {
                self.not_in_tail_position(Some(Ordering::Loose), |this| this.expression(m))?
            }
            None => string("Pattern match failed, no pattern matched the value."),
        };

        Ok(self.throw_error("let_assert", &message, location, [("value", subject)]))
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
                if self.function_name.as_ref() == Some(name)
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

        // This is a new function so unset the recorded name so that we don't
        // mistakenly trigger tail call optimisation
        let mut name = None;
        std::mem::swap(&mut self.function_name, &mut name);

        // Generate the function body
        let result = self.statements(body);

        // Reset function name, scope, and tail position tracking
        self.function_position = function_position;
        self.scope_position = scope_position;
        self.current_scope_vars = scope;
        std::mem::swap(&mut self.function_name, &mut name);

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
            Ok(docvec![record, ".", maybe_escape_property_doc(label)])
        })
    }

    fn record_update(
        &mut self,
        record: &'a TypedAssignment,
        constructor: &'a TypedExpr,
        args: &'a [TypedCallArg],
    ) -> Output<'a> {
        Ok(docvec![
            self.not_in_tail_position(None, |this| this.assignment(record))?,
            line(),
            self.call(constructor, args)?,
        ])
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

    fn todo(&mut self, message: Option<&'a TypedExpr>, location: &'a SrcSpan) -> Output<'a> {
        let message = match message {
            Some(m) => self.not_in_tail_position(None, |this| this.expression(m))?,
            None => string("`todo` expression evaluated. This code has not yet been implemented."),
        };
        let doc = self.throw_error("todo", &message, *location, vec![]);

        Ok(doc)
    }

    fn panic(&mut self, location: &'a SrcSpan, message: Option<&'a TypedExpr>) -> Output<'a> {
        let message = match message {
            Some(m) => self.not_in_tail_position(None, |this| this.expression(m))?,
            None => string("`panic` expression evaluated."),
        };
        let doc = self.throw_error("panic", &message, *location, vec![]);

        Ok(doc)
    }

    fn throw_error<Fields>(
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
        let function = self
            .function_name
            .clone()
            .unwrap_or_default()
            .to_doc()
            .surround("\"", "\"");
        let line = self.line_numbers.line_number(location.start).to_doc();
        let fields = wrap_object(fields.into_iter().map(|(k, v)| (k.to_doc(), Some(v))));

        docvec![
            "throw makeError",
            wrap_args([
                string(error_name),
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

    fn pattern_into_assignment_doc(
        &mut self,
        compiled_pattern: CompiledPattern<'a>,
        subject: Document<'a>,
        location: SrcSpan,
        kind: &'a AssignmentKind<TypedExpr>,
    ) -> Output<'a> {
        let any_assignments = !compiled_pattern.assignments.is_empty();
        let assignments = Self::pattern_assignments_doc(compiled_pattern.assignments);

        // If it's an assert then it is likely that the pattern is inexhaustive. When a value is
        // provided that does not get matched the code needs to throw an exception, which is done
        // by the pattern_checks_or_throw_doc method.
        match kind {
            AssignmentKind::Assert { message, .. } if !compiled_pattern.checks.is_empty() => {
                let checks = self.pattern_checks_or_throw_doc(
                    compiled_pattern.checks,
                    subject,
                    location,
                    message.as_deref(),
                )?;

                if !any_assignments {
                    Ok(checks)
                } else {
                    Ok(docvec![checks, line(), assignments])
                }
            }
            _ => Ok(assignments),
        }
    }

    fn pattern_checks_or_throw_doc(
        &mut self,
        checks: Vec<pattern::Check<'a>>,
        subject: Document<'a>,
        location: SrcSpan,
        message: Option<&'a TypedExpr>,
    ) -> Output<'a> {
        let checks = self.pattern_checks_doc(checks, false);
        Ok(docvec![
            "if (",
            docvec![break_("", ""), checks].nest(INDENT),
            break_("", ""),
            ") {",
            docvec![
                line(),
                self.assignment_no_match(location, subject, message)?
            ]
            .nest(INDENT),
            line(),
            "}",
        ]
        .group())
    }

    fn pattern_assignments_doc(assignments: Vec<Assignment<'_>>) -> Document<'_> {
        let assignments = assignments.into_iter().map(Assignment::into_doc);
        join(assignments, line())
    }

    fn pattern_take_assignments_doc(
        &self,
        compiled_pattern: &mut CompiledPattern<'a>,
    ) -> Document<'a> {
        let assignments = std::mem::take(&mut compiled_pattern.assignments);
        Self::pattern_assignments_doc(assignments)
    }

    fn pattern_take_checks_doc(
        &self,
        compiled_pattern: &mut CompiledPattern<'a>,
        match_desired: bool,
    ) -> Document<'a> {
        let checks = std::mem::take(&mut compiled_pattern.checks);
        self.pattern_checks_doc(checks, match_desired)
    }

    fn pattern_checks_doc(
        &self,
        checks: Vec<pattern::Check<'a>>,
        match_desired: bool,
    ) -> Document<'a> {
        if checks.is_empty() {
            return "true".to_doc();
        };
        let operator = if match_desired {
            break_(" &&", " && ")
        } else {
            break_(" ||", " || ")
        };

        let checks_len = checks.len();
        join(
            checks.into_iter().map(|check| {
                if checks_len > 1 && check.may_require_wrapping() {
                    docvec!["(", check.into_doc(match_desired), ")"]
                } else {
                    check.into_doc(match_desired)
                }
            }),
            operator,
        )
        .group()
    }

    fn echo(&mut self, expression: Document<'a>, location: &'a SrcSpan) -> Output<'a> {
        self.tracker.echo_used = true;

        let relative_path = self
            .src_path
            .strip_prefix(self.project_root)
            .unwrap_or(self.src_path)
            .as_str()
            .replace("\\", "\\\\");

        let relative_path_doc = EcoString::from(relative_path).to_doc();

        let echo_argument = call_arguments(vec![
            Ok(expression),
            Ok(relative_path_doc.surround("\"", "\"")),
            Ok(self.line_numbers.line_number(location.start).to_doc()),
        ])?;
        Ok(self.wrap_return(docvec!["echo", echo_argument]))
    }
}

pub fn int(value: &str) -> Document<'_> {
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

pub(crate) fn guard_constant_expression<'a>(
    assignments: &mut Vec<Assignment<'a>>,
    tracker: &mut UsageTracker,
    expression: &'a TypedConstant,
) -> Output<'a> {
    match expression {
        Constant::Tuple { elements, .. } => array(
            elements
                .iter()
                .map(|element| guard_constant_expression(assignments, tracker, element)),
        ),

        Constant::List { elements, .. } => {
            tracker.list_used = true;
            list(
                elements
                    .iter()
                    .map(|element| guard_constant_expression(assignments, tracker, element)),
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
                    tracker.ok_used = true;
                } else {
                    tracker.error_used = true;
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
                        tracker,
                    ));
                }
            }

            let field_values: Vec<_> = args
                .iter()
                .map(|arg| guard_constant_expression(assignments, tracker, &arg.value))
                .try_collect()?;
            Ok(construct_record(
                module.as_ref().map(|(module, _)| module.as_str()),
                name,
                field_values,
            ))
        }

        Constant::BitArray { segments, .. } => bit_array(tracker, segments, |tracker, constant| {
            guard_constant_expression(assignments, tracker, constant)
        }),

        Constant::Var { name, .. } => Ok(assignments
            .iter()
            .find(|assignment| assignment.name == name)
            .map(|assignment| assignment.subject.clone())
            .unwrap_or_else(|| maybe_escape_identifier(name).to_doc())),

        expression => constant_expression(Context::Function, tracker, expression),
    }
}

#[derive(Debug, Clone, Copy)]
/// The context where the constant expression is used, it might be inside a
/// function call, or in the definition of another constant.
///
/// Based on the context we might want to annotate pure function calls as
/// "@__PURE__".
///
pub enum Context {
    Constant,
    Function,
}

pub(crate) fn constant_expression<'a>(
    context: Context,
    tracker: &mut UsageTracker,
    expression: &'a TypedConstant,
) -> Output<'a> {
    match expression {
        Constant::Int { value, .. } => Ok(int(value)),
        Constant::Float { value, .. } => Ok(float(value)),
        Constant::String { value, .. } => Ok(string(value)),
        Constant::Tuple { elements, .. } => array(
            elements
                .iter()
                .map(|element| constant_expression(context, tracker, element)),
        ),

        Constant::List { elements, .. } => {
            tracker.list_used = true;
            let list = list(
                elements
                    .iter()
                    .map(|element| constant_expression(context, tracker, element)),
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
                    tracker.ok_used = true;
                } else {
                    tracker.error_used = true;
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
                        tracker,
                    ));
                }
            }

            let field_values: Vec<_> = args
                .iter()
                .map(|arg| constant_expression(context, tracker, &arg.value))
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
            let bit_array = bit_array(tracker, segments, |tracker, expr| {
                constant_expression(context, tracker, expr)
            })?;
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
            let left = constant_expression(context, tracker, left)?;
            let right = constant_expression(context, tracker, right)?;
            Ok(docvec![left, " + ", right])
        }

        Constant::Invalid { .. } => panic!("invalid constants should not reach code generation"),
    }
}

fn bit_array<'a>(
    tracker: &mut UsageTracker,
    segments: &'a [TypedConstantBitArraySegment],
    mut constant_expr_fun: impl FnMut(&mut UsageTracker, &'a TypedConstant) -> Output<'a>,
) -> Output<'a> {
    tracker.bit_array_literal_used = true;

    use BitArrayOption as Opt;

    let segments_array = array(segments.iter().map(|segment| {
        let value = constant_expr_fun(tracker, &segment.value)?;

        if segment.type_ == crate::type_::int() || segment.type_ == crate::type_::float() {
            let details =
                sized_bit_array_segment_details(segment, tracker, &mut constant_expr_fun)?;

            if segment.type_ == crate::type_::int() {
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
                        tracker.sized_integer_segment_used = true;
                        Ok(docvec![
                            "sizedInt(",
                            value,
                            ", ",
                            details.size,
                            ", ",
                            bool(segment.endianness().is_big()),
                            ")"
                        ])
                    }
                }
            } else {
                tracker.float_bit_array_segment_used = true;
                Ok(docvec![
                    "sizedFloat(",
                    value,
                    ", ",
                    details.size,
                    ", ",
                    bool(segment.endianness().is_big()),
                    ")"
                ])
            }
        } else {
            match segment.options.as_slice() {
                // UTF8 strings
                [Opt::Utf8 { .. }] => {
                    tracker.string_bit_array_segment_used = true;
                    Ok(docvec!["stringBits(", value, ")"])
                }

                // UTF8 codepoints
                [Opt::Utf8Codepoint { .. }] => {
                    tracker.codepoint_bit_array_segment_used = true;
                    Ok(docvec!["codepointBits(", value, ")"])
                }

                // Bit arrays
                [Opt::Bits { .. }] => Ok(value),

                // Bit arrays with explicit size. The explicit size slices the bit array to the
                // specified size. A runtime exception is thrown if the size exceeds the number
                // of bits in the bit array.
                [Opt::Bits { .. }, Opt::Size { value: size, .. }]
                | [Opt::Size { value: size, .. }, Opt::Bits { .. }] => match &**size {
                    Constant::Int { value: size, .. } => {
                        tracker.bit_array_slice_used = true;
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
        }
    }))?;

    Ok(docvec!["toBitArray(", segments_array, ")"])
}

#[derive(Debug)]
struct SizedBitArraySegmentDetails<'a> {
    size: Document<'a>,
    /// The size of the bit array segment stored as a BigInt. This has a value when the segment's
    /// size is known at compile time.
    size_value: Option<BigInt>,
}

fn sized_bit_array_segment_details<'a>(
    segment: &'a TypedConstantBitArraySegment,
    tracker: &mut UsageTracker,
    constant_expr_fun: &mut impl FnMut(&mut UsageTracker, &'a TypedConstant) -> Output<'a>,
) -> Result<SizedBitArraySegmentDetails<'a>, Error> {
    use BitArrayOption as Opt;

    if segment
        .options
        .iter()
        .any(|x| matches!(x, Opt::Native { .. }))
    {
        return Err(Error::Unsupported {
            feature: "This bit array segment option".into(),
            location: segment.location,
        });
    }

    let unit = segment
        .options
        .iter()
        .find_map(|option| match option {
            Opt::Unit { value, .. } => Some(*value),
            _ => None,
        })
        .unwrap_or(1);

    let size = segment
        .options
        .iter()
        .find(|x| matches!(x, Opt::Size { .. }));

    let (size_value, size) = match size {
        Some(Opt::Size { value: size, .. }) => match *size.clone() {
            Constant::Int { int_value, .. } => {
                let size_value = int_value * unit as usize;
                let size = eco_format!("{}", size_value).to_doc();

                (Some(size_value), size)
            }
            _ => {
                let mut size = constant_expr_fun(tracker, size)?;

                if unit != 1 {
                    size = size.group().append(" * ".to_doc().append(unit.to_doc()));
                }

                (None, size)
            }
        },
        _ => {
            let size_value = if segment.type_ == crate::type_::int() {
                8usize
            } else {
                64usize
            };

            (Some(BigInt::from(size_value)), docvec![size_value])
        }
    };

    Ok(SizedBitArraySegmentDetails { size, size_value })
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

pub fn array<'a, Elements: IntoIterator<Item = Output<'a>>>(elements: Elements) -> Output<'a> {
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

fn list<'a, I: IntoIterator<Item = Output<'a>>>(elements: I) -> Output<'a>
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

fn construct_record<'a>(
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

fn record_constructor<'a>(
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
