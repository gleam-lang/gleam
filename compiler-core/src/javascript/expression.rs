use vec1::Vec1;

use super::{
    pattern::{Assignment, CompiledPattern},
    *,
};
use crate::{
    ast::*,
    line_numbers::LineNumbers,
    pretty::*,
    type_::{ModuleValueConstructor, Type, ValueConstructor, ValueConstructorVariant},
};
use std::sync::Arc;

#[derive(Debug, Clone, Copy)]
pub enum Position {
    Tail,
    NotTail,
}

impl Position {
    /// Returns `true` if the position is [`Tail`].
    ///
    /// [`Tail`]: Position::Tail
    #[must_use]
    pub fn is_tail(&self) -> bool {
        matches!(self, Self::Tail)
    }
}

#[derive(Debug)]
pub(crate) struct Generator<'module> {
    module_name: EcoString,
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
}

impl<'module> Generator<'module> {
    #[allow(clippy::too_many_arguments)] // TODO: FIXME
    pub fn new(
        module_name: EcoString,
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
            line_numbers,
            function_name,
            function_arguments,
            tail_recursion_used: false,
            current_scope_vars,
            function_position: Position::Tail,
            scope_position: Position::Tail,
        }
    }

    pub fn local_var<'a>(&mut self, name: &'a EcoString) -> Document<'a> {
        match self.current_scope_vars.get(name) {
            None => {
                let _ = self.current_scope_vars.insert(name.clone(), 0);
                maybe_escape_identifier_doc(name)
            }
            Some(0) => maybe_escape_identifier_doc(name),
            Some(n) if name == "$" => Document::String(format!("${n}")),
            Some(n) => Document::String(format!("{name}${n}")),
        }
    }

    pub fn next_local_var<'a>(&mut self, name: &'a EcoString) -> Document<'a> {
        let next = self.current_scope_vars.get(name).map_or(0, |i| i + 1);
        let _ = self.current_scope_vars.insert(name.clone(), next);
        self.local_var(name)
    }

    pub fn function_body<'a>(
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

    fn tail_call_loop<'a>(&mut self, body: Document<'a>, args: &'a [TypedArg]) -> Output<'a> {
        let loop_assignments = concat(args.iter().flat_map(Arg::get_variable_name).map(|name| {
            let var = maybe_escape_identifier_doc(name);
            docvec!["let ", var, " = loop$", name, ";", line()]
        }));
        Ok(docvec!(
            "while (true) {",
            docvec!(line(), loop_assignments, body).nest(INDENT),
            line(),
            "}"
        ))
    }

    fn statement<'a>(&mut self, statement: &'a TypedStatement) -> Output<'a> {
        match statement {
            Statement::Expression(expression) => self.expression(expression),
            Statement::Assignment(assignment) => self.assignment(assignment),
            Statement::Use(_use) => {
                unreachable!("Use must not be present for JavaScript generation")
            }
        }
    }

    pub fn expression<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        let document = match expression {
            TypedExpr::String { value, .. } => Ok(string(value)),

            TypedExpr::Int { value, .. } => Ok(int(value)),
            TypedExpr::Float { value, .. } => Ok(float(value)),

            TypedExpr::List { elements, tail, .. } => self.not_in_tail_position(|gen| match tail {
                Some(tail) => {
                    gen.tracker.prepend_used = true;
                    let tail = gen.wrap_expression(tail)?;
                    prepend(elements.iter().map(|e| gen.wrap_expression(e)), tail)
                }
                None => {
                    gen.tracker.list_used = true;
                    list(elements.iter().map(|e| gen.wrap_expression(e)))
                }
            }),

            TypedExpr::Tuple { elems, .. } => self.tuple(elems),
            TypedExpr::TupleIndex { tuple, index, .. } => self.tuple_index(tuple, *index),

            TypedExpr::Case {
                subjects, clauses, ..
            } => self.case(subjects, clauses),

            TypedExpr::Call { fun, args, .. } => self.call(fun, args),
            TypedExpr::Fn { args, body, .. } => self.fn_(args, body),

            TypedExpr::RecordAccess { record, label, .. } => self.record_access(record, label),
            TypedExpr::RecordUpdate { spread, args, .. } => self.record_update(spread, args),

            TypedExpr::Var {
                name, constructor, ..
            } => self.variable(name, constructor),

            TypedExpr::Pipeline {
                assignments,
                finally,
                ..
            } => self.pipeline(assignments.as_slice(), finally),

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
        }?;
        Ok(if expression.handles_own_return() {
            document
        } else {
            self.wrap_return(document)
        })
    }

    fn negate_with<'a>(&mut self, with: &'static str, value: &'a TypedExpr) -> Output<'a> {
        self.not_in_tail_position(|gen| Ok(docvec!(with, gen.wrap_expression(value)?)))
    }

    fn bit_array<'a>(&mut self, segments: &'a [TypedExprBitArraySegment]) -> Output<'a> {
        self.tracker.bit_array_literal_used = true;

        use BitArrayOption as Opt;

        // Collect all the values used in segments.
        let segments_array = array(segments.iter().map(|segment| {
            let value = self.not_in_tail_position(|gen| gen.wrap_expression(&segment.value))?;
            match segment.options.as_slice() {
                // Ints
                [] | [Opt::Int { .. }] => Ok(value),

                // Sized ints
                [Opt::Size { value: size, .. }] => {
                    self.tracker.sized_integer_segment_used = true;
                    let size = self.not_in_tail_position(|gen| gen.wrap_expression(size))?;
                    Ok(docvec!["sizedInt(", value, ", ", size, ")"])
                }

                // Floats
                [Opt::Float { .. }] => {
                    self.tracker.float_bit_array_segment_used = true;
                    Ok(docvec!["float64Bits(", value, ")"])
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

                // Bit arrays
                [Opt::Bytes { .. } | Opt::Bits { .. }] => Ok(docvec![value, ".buffer"]),

                // Anything else
                _ => Err(Error::Unsupported {
                    feature: "This bit array segment option".into(),
                    location: segment.location,
                }),
            }
        }))?;

        Ok(docvec!["toBitArray(", segments_array, ")"])
    }

    pub fn wrap_return<'a>(&mut self, document: Document<'a>) -> Document<'a> {
        if self.scope_position.is_tail() {
            docvec!["return ", document, ";"]
        } else {
            document
        }
    }

    pub fn not_in_tail_position<'a, CompileFn>(&mut self, compile: CompileFn) -> Output<'a>
    where
        CompileFn: Fn(&mut Self) -> Output<'a>,
    {
        let function_position = self.function_position;
        let scope_position = self.scope_position;

        self.function_position = Position::NotTail;
        self.scope_position = Position::NotTail;
        let result = compile(self);

        self.function_position = function_position;
        self.scope_position = scope_position;
        result
    }

    /// Wrap an expression in an immediately involked function expression if
    /// required due to being a JS statement
    pub fn wrap_expression<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        match expression {
            TypedExpr::Panic { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::Pipeline { .. } => self
                .immediately_involked_function_expression(expression, |gen, expr| {
                    gen.expression(expr)
                }),
            _ => self.expression(expression),
        }
    }

    /// Wrap an expression in an immediately involked function expression if
    /// required due to being a JS statement, or in parens if required due to
    /// being an operator or a function literal.
    pub fn child_expression<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        match expression {
            TypedExpr::BinOp { name, .. } if name.is_operator_to_wrap() => {}
            TypedExpr::Fn { .. } => {}

            _ => return self.wrap_expression(expression),
        }

        let document = self.expression(expression)?;
        Ok(if self.scope_position.is_tail() {
            // Here the document is a return statement: `return <expr>;`
            document
        } else {
            docvec!("(", document, ")")
        })
    }

    /// Wrap an expression in an immediately involked function expression
    fn immediately_involked_function_expression<'a, T, ToDoc>(
        &mut self,
        statements: &'a T,
        to_doc: ToDoc,
    ) -> Output<'a>
    where
        ToDoc: FnOnce(&mut Self, &'a T) -> Output<'a>,
    {
        // Save initial state
        let scope_position = self.scope_position;

        // Set state for in this iife
        self.scope_position = Position::Tail;
        let current_scope_vars = self.current_scope_vars.clone();

        // Generate the expression
        let result = to_doc(self, statements);

        // Reset
        self.current_scope_vars = current_scope_vars;
        self.scope_position = scope_position;

        // Wrap in iife document
        let doc = self.immediately_involked_function_expression_document(result?);
        Ok(self.wrap_return(doc))
    }

    /// Wrap a document in an immediately involked function expression
    fn immediately_involked_function_expression_document<'a>(
        &mut self,
        document: Document<'a>,
    ) -> Document<'a> {
        docvec!(
            docvec!("(() => {", break_("", " "), document).nest(INDENT),
            break_("", " "),
            "})()",
        )
        .group()
    }

    fn variable<'a>(
        &mut self,
        name: &'a EcoString,
        constructor: &'a ValueConstructor,
    ) -> Output<'a> {
        match &constructor.variant {
            ValueConstructorVariant::LocalConstant { literal } => {
                constant_expression(self.tracker, literal)
            }
            ValueConstructorVariant::Record { arity, .. } => {
                Ok(self.record_constructor(constructor.type_.clone(), None, name, *arity))
            }
            ValueConstructorVariant::ModuleFn { .. }
            | ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::LocalVariable { .. } => Ok(self.local_var(name)),
        }
    }

    fn record_constructor<'a>(
        &mut self,
        type_: Arc<Type>,
        qualifier: Option<&'a str>,
        name: &'a str,
        arity: u16,
    ) -> Document<'a> {
        if qualifier.is_none() && type_.is_result_constructor() {
            if name == "Ok" {
                self.tracker.ok_used = true;
            } else if name == "Error" {
                self.tracker.error_used = true;
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
            let vars = (0..arity).map(|i| Document::String(format!("var{i}")));
            let body = docvec![
                "return ",
                construct_record(qualifier, name, vars.clone()),
                ";"
            ];
            docvec!(
                docvec!(wrap_args(vars), " => {", break_("", " "), body)
                    .nest(INDENT)
                    .append(break_("", " "))
                    .group(),
                "}",
            )
        }
    }

    fn pipeline<'a>(
        &mut self,
        assignments: &'a [TypedAssignment],
        finally: &'a TypedExpr,
    ) -> Output<'a> {
        let count = assignments.len();
        let mut documents = Vec::with_capacity((count + 1) * 2);
        for assignment in assignments.iter() {
            documents.push(self.not_in_tail_position(|gen| gen.assignment(assignment))?);
            documents.push(line());
        }
        documents.push(self.expression(finally)?);
        Ok(documents.to_doc().force_break())
    }

    fn expression_flattening_blocks<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        match expression {
            TypedExpr::Block { statements, .. } => self.statements(statements),
            _ => self.expression(expression),
        }
    }

    fn block<'a>(&mut self, statements: &'a Vec1<TypedStatement>) -> Output<'a> {
        if statements.len() == 1 {
            match statements.first() {
                Statement::Expression(expression) => self.child_expression(expression),

                Statement::Assignment(assignment) => {
                    self.child_expression(assignment.value.as_ref())
                }

                Statement::Use(_) => {
                    unreachable!("use statements must not be present for JavaScript generation")
                }
            }
        } else {
            self.immediately_involked_function_expression(statements, |gen, statements| {
                gen.statements(statements)
            })
        }
    }

    fn statements<'a>(&mut self, statements: &'a [TypedStatement]) -> Output<'a> {
        let count = statements.len();
        let mut documents = Vec::with_capacity(count * 3);
        for (i, statement) in statements.iter().enumerate() {
            if i + 1 < count {
                documents.push(self.not_in_tail_position(|gen| gen.statement(statement))?);
                if requires_semicolon(statement) {
                    documents.push(";".to_doc());
                }
                documents.push(line());
            } else {
                documents.push(self.statement(statement)?);
            }
        }
        if count == 1 {
            Ok(documents.to_doc())
        } else {
            Ok(documents.to_doc().force_break())
        }
    }

    fn assignment<'a>(&mut self, assignment: &'a TypedAssignment) -> Output<'a> {
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
            // Subject must be rendered before the variable for variable numbering
            let subject = self.not_in_tail_position(|gen| gen.wrap_expression(value))?;
            let js_name = self.next_local_var(name);
            return Ok(if self.scope_position.is_tail() {
                docvec![
                    "let ",
                    js_name.clone(),
                    " = ",
                    subject,
                    ";",
                    line(),
                    "return ",
                    js_name,
                    ";"
                ]
            } else {
                docvec!["let ", js_name, " = ", subject, ";"]
            }
            .force_break());
        }

        // Otherwise we need to compile the patterns
        let (subject, subject_assignment) = pattern::assign_subject(self, value);
        // Value needs to be rendered before traversing pattern to have correctly incremented variables.
        let value = self.not_in_tail_position(|gen| gen.wrap_expression(value))?;
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
            self.pattern_into_assignment_doc(compiled, subject, pattern.location(), *kind)?;
        // If there is a subject name given create a variable to hold it for
        // use in patterns
        let doc = match subject_assignment {
            Some(name) => docvec!("let ", name, " = ", value, ";", line(), compiled),
            None => compiled,
        };

        Ok(doc.append(afterwards).force_break())
    }

    fn case<'a>(
        &mut self,
        subject_values: &'a [TypedExpr],
        clauses: &'a [TypedClause],
    ) -> Output<'a> {
        let (subjects, subject_assignments): (Vec<_>, Vec<_>) =
            pattern::assign_subjects(self, subject_values)
                .into_iter()
                .unzip();
        let mut gen = pattern::Generator::new(self);

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
                let scope = gen.expression_generator.current_scope_vars.clone();
                let mut compiled = gen.generate(&subjects, multipatterns, clause.guard.as_ref())?;
                let consequence = gen
                    .expression_generator
                    .expression_flattening_blocks(&clause.then)?;

                // We've seen one more clause
                clause_number += 1;

                // Reset the scope now that this clause has finished, causing the
                // variables to go out of scope.
                gen.expression_generator.current_scope_vars = scope;

                // If the pattern assigns any variables we need to render assignments
                let body = if compiled.has_assignments() {
                    let assignments = gen
                        .expression_generator
                        .pattern_take_assignments_doc(&mut compiled);
                    docvec!(assignments, line(), consequence)
                } else {
                    consequence
                };

                let is_final_clause = clause_number == total_patterns;
                let is_first_clause = clause_number == 1;
                let is_only_clause = is_final_clause && is_first_clause;

                doc = if is_only_clause {
                    // If this is the only clause and there are no checks then we can
                    // render just the body as the case does nothing
                    doc.append(body)
                } else if is_final_clause {
                    // If this is the final clause and there are no checks then we can
                    // render `else` instead of `else if (...)`
                    doc.append(" else {")
                        .append(docvec!(line(), body).nest(INDENT))
                        .append(line())
                        .append("}")
                } else {
                    doc.append(if is_first_clause {
                        "if ("
                    } else {
                        " else if ("
                    })
                    .append(
                        gen.expression_generator
                            .pattern_take_checks_doc(&mut compiled, true),
                    )
                    .append(") {")
                    .append(docvec!(line(), body).nest(INDENT))
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
                let value = self.not_in_tail_position(|gen| gen.wrap_expression(value))?;
                Ok(docvec!("let ", name, " = ", value, ";", line()))
            })
            .try_collect()?;

        Ok(docvec![subject_assignments, doc].force_break())
    }

    fn assignment_no_match<'a>(&mut self, location: SrcSpan, subject: Document<'a>) -> Output<'a> {
        Ok(self.throw_error(
            "assignment_no_match",
            &string("Assignment pattern did not match"),
            location,
            [("value", subject)],
        ))
    }

    fn tuple<'a>(&mut self, elements: &'a [TypedExpr]) -> Output<'a> {
        self.not_in_tail_position(|gen| {
            array(elements.iter().map(|element| gen.wrap_expression(element)))
        })
    }

    fn call<'a>(&mut self, fun: &'a TypedExpr, arguments: &'a [CallArg<TypedExpr>]) -> Output<'a> {
        let scope_position = self.scope_position;
        let function_position = self.function_position;

        self.scope_position = Position::NotTail;
        self.function_position = Position::NotTail;
        let arguments = arguments
            .iter()
            .map(|element| self.wrap_expression(&element.value))
            .try_collect()?;

        self.function_position = function_position;
        self.scope_position = scope_position;

        self.call_with_doc_args(fun, arguments)
    }

    fn call_with_doc_args<'a>(
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
                        docs.push(Document::String((*name).to_string()));
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
                let fun = self.not_in_tail_position(|gen| {
                    let is_fn_literal = matches!(fun, TypedExpr::Fn { .. });
                    let fun = gen.wrap_expression(fun)?;
                    if is_fn_literal {
                        Ok(docvec!("(", fun, ")"))
                    } else {
                        Ok(fun)
                    }
                })?;
                let arguments = call_arguments(arguments.into_iter().map(Ok))?;
                Ok(self.wrap_return(docvec![fun, arguments]))
            }
        }
    }

    fn fn_<'a>(&mut self, arguments: &'a [TypedArg], body: &'a [TypedStatement]) -> Output<'a> {
        // New function, this is now the tail position
        let function_position = self.function_position;
        let scope_position = self.scope_position;
        self.function_position = Position::Tail;
        self.scope_position = Position::Tail;

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

        Ok(docvec!(
            docvec!(
                fun_args(arguments, false),
                " => {",
                break_("", " "),
                result?
            )
            .nest(INDENT)
            .append(break_("", " "))
            .group(),
            "}",
        ))
    }

    fn record_access<'a>(&mut self, record: &'a TypedExpr, label: &'a str) -> Output<'a> {
        self.not_in_tail_position(|gen| {
            let record = gen.wrap_expression(record)?;
            Ok(docvec![record, ".", label])
        })
    }

    fn record_update<'a>(
        &mut self,
        record: &'a TypedExpr,
        updates: &'a [TypedRecordUpdateArg],
    ) -> Output<'a> {
        self.not_in_tail_position(|gen| {
            let record = gen.wrap_expression(record)?;
            let fields = updates
                .iter()
                .map(|TypedRecordUpdateArg { label, value, .. }| {
                    (label.to_doc(), gen.wrap_expression(value))
                });
            let object = try_wrap_object(fields)?;
            Ok(docvec![record, ".withFields(", object, ")"])
        })
    }

    fn tuple_index<'a>(&mut self, tuple: &'a TypedExpr, index: u64) -> Output<'a> {
        self.not_in_tail_position(|gen| {
            let tuple = gen.wrap_expression(tuple)?;
            Ok(docvec![tuple, Document::String(format!("[{index}]"))])
        })
    }

    fn bin_op<'a>(
        &mut self,
        name: &'a BinOp,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
    ) -> Output<'a> {
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

    fn div_int<'a>(&mut self, left: &'a TypedExpr, right: &'a TypedExpr) -> Output<'a> {
        let left = self.not_in_tail_position(|gen| gen.expression(left))?;
        let right = self.not_in_tail_position(|gen| gen.expression(right))?;
        self.tracker.int_division_used = true;
        Ok(docvec!("divideInt", wrap_args([left, right])))
    }

    fn remainder_int<'a>(&mut self, left: &'a TypedExpr, right: &'a TypedExpr) -> Output<'a> {
        let left = self.not_in_tail_position(|gen| gen.expression(left))?;
        let right = self.not_in_tail_position(|gen| gen.expression(right))?;
        self.tracker.int_remainder_used = true;
        Ok(docvec!("remainderInt", wrap_args([left, right])))
    }

    fn div_float<'a>(&mut self, left: &'a TypedExpr, right: &'a TypedExpr) -> Output<'a> {
        let left = self.not_in_tail_position(|gen| gen.expression(left))?;
        let right = self.not_in_tail_position(|gen| gen.expression(right))?;
        self.tracker.float_division_used = true;
        Ok(docvec!("divideFloat", wrap_args([left, right])))
    }

    fn equal<'a>(
        &mut self,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        should_be_equal: bool,
    ) -> Output<'a> {
        // If it is a simple scalar type then we can use JS' reference identity
        if is_js_scalar(left.type_()) {
            let left_doc = self.not_in_tail_position(|gen| gen.child_expression(left))?;
            let right_doc = self.not_in_tail_position(|gen| gen.child_expression(right))?;
            let operator = if should_be_equal { " === " } else { " !== " };
            return Ok(docvec!(left_doc, operator, right_doc));
        }

        // Other types must be compared using structural equality
        let left = self.not_in_tail_position(|gen| gen.wrap_expression(left))?;
        let right = self.not_in_tail_position(|gen| gen.wrap_expression(right))?;
        Ok(self.prelude_equal_call(should_be_equal, left, right))
    }

    pub(super) fn prelude_equal_call<'a>(
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
        docvec!(operator, args)
    }

    fn print_bin_op<'a>(
        &mut self,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
        op: &'a str,
    ) -> Output<'a> {
        let left = self.not_in_tail_position(|gen| gen.child_expression(left))?;
        let right = self.not_in_tail_position(|gen| gen.child_expression(right))?;
        Ok(docvec!(left, " ", op, " ", right))
    }

    fn todo<'a>(&mut self, message: Option<&'a TypedExpr>, location: &'a SrcSpan) -> Output<'a> {
        let scope_position = self.scope_position;
        self.scope_position = Position::NotTail;

        let message = match message {
            Some(m) => self.expression(m)?,
            None => string("This has not yet been implemented"),
        };
        let doc = self.throw_error("todo", &message, *location, vec![]);

        // Reset tail position so later values are returned as needed. i.e.
        // following clauses in a case expression.
        self.scope_position = scope_position;

        Ok(doc)
    }

    fn panic<'a>(&mut self, location: &'a SrcSpan, message: Option<&'a TypedExpr>) -> Output<'a> {
        let scope_position = self.scope_position;
        self.scope_position = Position::NotTail;

        let message = match message {
            Some(m) => self.expression(m)?,
            None => string("panic expression evaluated"),
        };
        let doc = self.throw_error("panic", &message, *location, vec![]);

        // Reset tail position so later values are returned as needed. i.e.
        // following clauses in a case expression.
        self.scope_position = scope_position;

        Ok(doc)
    }

    fn throw_error<'a, Fields>(
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

    fn module_select<'a>(
        &mut self,
        module: &'a str,
        label: &'a str,
        constructor: &'a ModuleValueConstructor,
    ) -> Document<'a> {
        match constructor {
            ModuleValueConstructor::Fn { .. } | ModuleValueConstructor::Constant { .. } => {
                docvec!["$", module, ".", maybe_escape_identifier_doc(label)]
            }

            ModuleValueConstructor::Record {
                name, arity, type_, ..
            } => self.record_constructor(type_.clone(), Some(module), name, *arity),
        }
    }

    fn pattern_into_assignment_doc<'a>(
        &mut self,
        compiled_pattern: CompiledPattern<'a>,
        subject: Document<'a>,
        location: SrcSpan,
        kind: AssignmentKind,
    ) -> Output<'a> {
        let any_assignments = !compiled_pattern.assignments.is_empty();
        let assignments = Self::pattern_assignments_doc(compiled_pattern.assignments);

        // If it's an assert then it is likely that the pattern is inexhaustive. When a value is
        // provided that does not get matched the code needs to throw an exception, which is done
        // by the pattern_checks_or_throw_doc method.
        if kind.is_assert() && !compiled_pattern.checks.is_empty() {
            let checks =
                self.pattern_checks_or_throw_doc(compiled_pattern.checks, subject, location)?;

            if !any_assignments {
                Ok(checks)
            } else {
                Ok(docvec![checks, line(), assignments])
            }
        } else {
            Ok(assignments)
        }
    }

    fn pattern_checks_or_throw_doc<'a>(
        &mut self,
        checks: Vec<pattern::Check<'a>>,
        subject: Document<'a>,
        location: SrcSpan,
    ) -> Output<'a> {
        let checks = self.pattern_checks_doc(checks, false);
        Ok(docvec![
            "if (",
            docvec![break_("", ""), checks].nest(INDENT),
            break_("", ""),
            ") {",
            docvec![line(), self.assignment_no_match(location, subject)?].nest(INDENT),
            line(),
            "}",
        ]
        .group())
    }

    fn pattern_assignments_doc(assignments: Vec<Assignment<'_>>) -> Document<'_> {
        let assignments = assignments.into_iter().map(Assignment::into_doc);
        join(assignments, line())
    }

    fn pattern_take_assignments_doc<'a>(
        &self,
        compiled_pattern: &mut CompiledPattern<'a>,
    ) -> Document<'a> {
        let assignments = std::mem::take(&mut compiled_pattern.assignments);
        Self::pattern_assignments_doc(assignments)
    }

    fn pattern_take_checks_doc<'a>(
        &self,
        compiled_pattern: &mut CompiledPattern<'a>,
        match_desired: bool,
    ) -> Document<'a> {
        let checks = std::mem::take(&mut compiled_pattern.checks);
        self.pattern_checks_doc(checks, match_desired)
    }

    fn pattern_checks_doc<'a>(
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
                .map(|e| guard_constant_expression(assignments, tracker, e)),
        ),

        Constant::List { elements, .. } => {
            tracker.list_used = true;
            list(
                elements
                    .iter()
                    .map(|e| guard_constant_expression(assignments, tracker, e)),
            )
        }
        Constant::Record { typ, name, .. } if typ.is_bool() && name == "True" => {
            Ok("true".to_doc())
        }
        Constant::Record { typ, name, .. } if typ.is_bool() && name == "False" => {
            Ok("false".to_doc())
        }
        Constant::Record { typ, .. } if typ.is_nil() => Ok("undefined".to_doc()),

        Constant::Record {
            tag,
            typ,
            args,
            module,
            ..
        } => {
            if typ.is_result() {
                if tag == "Ok" {
                    tracker.ok_used = true;
                } else {
                    tracker.error_used = true;
                }
            }
            let field_values: Vec<_> = args
                .iter()
                .map(|arg| guard_constant_expression(assignments, tracker, &arg.value))
                .try_collect()?;
            Ok(construct_record(module.as_deref(), tag, field_values))
        }

        Constant::BitArray { segments, .. } => bit_array(tracker, segments, |tracker, constant| {
            guard_constant_expression(assignments, tracker, constant)
        }),

        Constant::Var { name, .. } => Ok(assignments
            .iter()
            .find(|assignment| assignment.name == name)
            .map(|assignment| assignment.subject.clone().append(assignment.path.clone()))
            .unwrap_or_else(|| maybe_escape_identifier_doc(name))),

        expression => constant_expression(tracker, expression),
    }
}

pub(crate) fn constant_expression<'a>(
    tracker: &mut UsageTracker,
    expression: &'a TypedConstant,
) -> Output<'a> {
    match expression {
        Constant::Int { value, .. } => Ok(int(value)),
        Constant::Float { value, .. } => Ok(float(value)),
        Constant::String { value, .. } => Ok(string(value)),
        Constant::Tuple { elements, .. } => {
            array(elements.iter().map(|e| constant_expression(tracker, e)))
        }

        Constant::List { elements, .. } => {
            tracker.list_used = true;
            list(elements.iter().map(|e| constant_expression(tracker, e)))
        }

        Constant::Record { typ, name, .. } if typ.is_bool() && name == "True" => {
            Ok("true".to_doc())
        }
        Constant::Record { typ, name, .. } if typ.is_bool() && name == "False" => {
            Ok("false".to_doc())
        }
        Constant::Record { typ, .. } if typ.is_nil() => Ok("undefined".to_doc()),

        Constant::Record {
            tag,
            typ,
            args,
            module,
            ..
        } => {
            if typ.is_result() {
                if tag == "Ok" {
                    tracker.ok_used = true;
                } else {
                    tracker.error_used = true;
                }
            }
            let field_values: Vec<_> = args
                .iter()
                .map(|arg| constant_expression(tracker, &arg.value))
                .try_collect()?;
            Ok(construct_record(module.as_deref(), tag, field_values))
        }

        Constant::BitArray { segments, .. } => bit_array(tracker, segments, constant_expression),

        Constant::Var { name, module, .. } => Ok({
            match module {
                None => maybe_escape_identifier_doc(name),
                Some(module) => {
                    // JS keywords can be accessed here, but we must escape anyway
                    // as we escape when exporting such names in the first place,
                    // and the imported name has to match the exported name.
                    docvec!["$", module, ".", maybe_escape_identifier_doc(name)]
                }
            }
        }),

        Constant::Invalid { .. } => panic!("invalid constants should not reach code generation"),
    }
}

fn bit_array<'a>(
    tracker: &mut UsageTracker,
    segments: &'a [BitArraySegment<TypedConstant, Arc<Type>>],
    mut constant_expr_fun: impl FnMut(&mut UsageTracker, &'a TypedConstant) -> Output<'a>,
) -> Output<'a> {
    tracker.bit_array_literal_used = true;

    use BitArrayOption as Opt;

    let segments_array = array(segments.iter().map(|segment| {
        let value = constant_expr_fun(tracker, &segment.value)?;
        match segment.options.as_slice() {
            // Ints
            [] | [Opt::Int { .. }] => Ok(value),

            // Sized ints
            [Opt::Size { value: size, .. }] => {
                tracker.sized_integer_segment_used = true;
                let size = constant_expr_fun(tracker, size)?;
                Ok(docvec!["sizedInt(", value, ", ", size, ")"])
            }

            // Floats
            [Opt::Float { .. }] => {
                tracker.float_bit_array_segment_used = true;
                Ok(docvec!["float64Bits(", value, ")"])
            }

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

            // Bit strings
            [Opt::Bits { .. }] => Ok(docvec![value, ".buffer"]),

            // Anything else
            _ => Err(Error::Unsupported {
                feature: "This bit array segment option".into(),
                location: segment.location,
            }),
        }
    }))?;

    Ok(docvec!["toBitArray(", segments_array, ")"])
}

pub fn string(value: &str) -> Document<'_> {
    if value.contains('\n') {
        Document::String(value.replace('\n', r"\n")).surround("\"", "\"")
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
        arguments.into_iter().map(|a| {
            any_arguments = true;
            a
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
            | TypedExpr::Pipeline { .. } => true,

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
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. } => false,
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
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Block { .. },
        ) => true,

        Statement::Expression(
            TypedExpr::Todo { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Pipeline { .. },
        ) => false,

        Statement::Assignment(_) => false,
        Statement::Use(_) => false,
    }
}
