use std::sync::Arc;

use super::{pattern::CompiledPattern, *};
use crate::{
    ast::*,
    line_numbers::LineNumbers,
    pretty::*,
    type_::{HasType, ModuleValueConstructor, Type, ValueConstructor, ValueConstructorVariant},
};

#[derive(Debug)]
pub(crate) struct Generator<'module> {
    module_name: &'module [String],
    line_numbers: &'module LineNumbers,
    function_name: Option<&'module str>,
    function_arguments: Vec<Option<&'module str>>,
    current_scope_vars: im::HashMap<String, usize>,
    pub tail_position: bool,
    // We register whether these features are used within an expression so that
    // the module generator can output a suitable function if it is needed.
    pub tracker: &'module mut UsageTracker,
    // We track whether tail call recusion is used so that we can render a loop
    // at the top level of the function to use in place of pushing new stack
    // frames.
    pub tail_recursion_used: bool,
}

impl<'module> Generator<'module> {
    #[allow(clippy::too_many_arguments)] // TODO: FIXME
    pub fn new(
        module_name: &'module [String],
        line_numbers: &'module LineNumbers,
        function_name: &'module str,
        function_arguments: Vec<Option<&'module str>>,
        tracker: &'module mut UsageTracker,
        mut current_scope_vars: im::HashMap<String, usize>,
    ) -> Self {
        for &name in function_arguments.iter().flatten() {
            let _ = current_scope_vars.insert(name.to_string(), 0);
        }
        Self {
            tracker,
            module_name,
            line_numbers,
            function_name: Some(function_name),
            function_arguments,
            tail_recursion_used: false,
            current_scope_vars,
            tail_position: true,
        }
    }

    pub fn local_var<'a>(&mut self, name: &'a str) -> Document<'a> {
        match self.current_scope_vars.get(name) {
            None => {
                let _ = self.current_scope_vars.insert(name.to_string(), 0);
                maybe_escape_identifier_doc(name)
            }
            Some(0) => maybe_escape_identifier_doc(name),
            Some(n) if name == "$" => Document::String(format!("${}", n)),
            Some(n) => Document::String(format!("{}${}", name, n)),
        }
    }

    pub fn next_local_var<'a>(&mut self, name: &'a str) -> Document<'a> {
        let next = self.current_scope_vars.get(name).map_or(0, |i| i + 1);
        let _ = self.current_scope_vars.insert(name.to_string(), next);
        self.local_var(name)
    }

    pub fn function_body<'a>(
        &mut self,
        expression: &'a TypedExpr,
        args: &'a [TypedArg],
    ) -> Output<'a> {
        let body = self.expression(expression)?;
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

    pub fn expression<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        let document = match expression {
            TypedExpr::String { value, .. } => Ok(string(value)),

            TypedExpr::Int { value, .. } => Ok(int(value)),
            TypedExpr::Float { value, .. } => Ok(float(value)),

            TypedExpr::List { elements, tail, .. } => {
                self.tracker.list_used = true;
                self.not_in_tail_position(|gen| {
                    let tail = match tail {
                        Some(tail) => Some(gen.wrap_expression(tail)?),
                        None => None,
                    };
                    list(elements.iter().map(|e| gen.wrap_expression(e)), tail)
                })
            }

            TypedExpr::Tuple { elems, .. } => self.tuple(elems),
            TypedExpr::TupleIndex { tuple, index, .. } => self.tuple_index(tuple, *index),

            TypedExpr::Case {
                location,
                subjects,
                clauses,
                ..
            } => self.case(*location, subjects, clauses),

            TypedExpr::Call { fun, args, .. } => self.call(fun, args),
            TypedExpr::Fn { args, body, .. } => self.fn_(args, body),

            TypedExpr::RecordAccess { record, label, .. } => self.record_access(record, label),
            TypedExpr::RecordUpdate { spread, args, .. } => self.record_update(spread, args),

            TypedExpr::Var {
                name, constructor, ..
            } => Ok(self.variable(name, constructor)),

            TypedExpr::Pipeline { expressions, .. } | TypedExpr::Sequence { expressions, .. } => {
                self.sequence(expressions)
            }

            TypedExpr::Assignment { value, pattern, .. } => self.assignment(value, pattern),

            TypedExpr::Try {
                value,
                then,
                pattern,
                ..
            } => self.try_(value, pattern, then),

            TypedExpr::BinOp {
                name, left, right, ..
            } => self.bin_op(name, left, right),

            TypedExpr::Todo {
                label, location, ..
            } => Ok(self.todo(label, location)),

            TypedExpr::BitString { segments, .. } => self.bit_string(segments),

            TypedExpr::ModuleSelect {
                module_alias,
                label,
                constructor,
                ..
            } => Ok(self.module_select(module_alias, label, constructor)),

            TypedExpr::Negate { value, .. } => self.negate(value),
        }?;
        Ok(if expression.handles_own_return() {
            document
        } else {
            self.wrap_return(document)
        })
    }

    fn negate<'a>(&mut self, value: &'a TypedExpr) -> Output<'a> {
        self.not_in_tail_position(|gen| Ok(docvec!("!", gen.wrap_expression(value)?)))
    }

    fn bit_string<'a>(&mut self, segments: &'a [TypedExprBitStringSegment]) -> Output<'a> {
        self.tracker.bit_string_literal_used = true;

        use BitStringSegmentOption as Opt;

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
                    Ok(docvec!["sizedInteger(", value, ", ", size, ")"])
                }

                // Floats
                [Opt::Float { .. }] => {
                    self.tracker.float_bit_string_segment_used = true;
                    Ok(docvec!["float64Bits(", value, ")"])
                }

                // UTF8 strings
                [Opt::Utf8 { .. }] => {
                    self.tracker.string_bit_string_segment_used = true;
                    Ok(docvec!["stringBits(", value, ")"])
                }

                // UTF8 codepoints
                [Opt::Utf8Codepoint { .. }] => {
                    self.tracker.codepoint_bit_string_segment_used = true;
                    Ok(docvec!["codepointBits(", value, ")"])
                }

                // Bit strings
                [Opt::BitString { .. }] => Ok(docvec![value, ".buffer"]),

                // Anything else
                _ => Err(Error::Unsupported {
                    feature: "This bit string segment option".to_string(),
                    location: segment.location,
                }),
            }
        }))?;

        Ok(docvec!["toBitString(", segments_array, ")"])
    }

    pub fn wrap_return<'a>(&self, document: Document<'a>) -> Document<'a> {
        if self.tail_position {
            docvec!["return ", document, ";"]
        } else {
            document
        }
    }

    pub fn not_in_tail_position<'a, CompileFn>(&mut self, compile: CompileFn) -> Output<'a>
    where
        CompileFn: Fn(&mut Self) -> Output<'a>,
    {
        let tail = self.tail_position;
        self.tail_position = false;
        let result = compile(self);
        self.tail_position = tail;
        result
    }

    /// Wrap an expression in an immediately involked function expression if
    /// required due to being a JS statement
    pub fn wrap_expression<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        match expression {
            TypedExpr::Todo { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::Sequence { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Assignment { .. }
            | TypedExpr::Try { .. } => self.immediately_involked_function_expression(expression),
            _ => self.expression(expression),
        }
    }

    /// Wrap an expression in an immediately involked function expression if
    /// required due to being a JS statement, or in parens if required due to
    /// being an operator
    pub fn binop_child_expression<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        match expression {
            TypedExpr::BinOp { name, .. } if name.is_operator_to_wrap() => {
                Ok(docvec!("(", self.expression(expression)?, ")"))
            }
            TypedExpr::Case { .. }
            | TypedExpr::Sequence { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Assignment { .. }
            | TypedExpr::Try { .. } => self.immediately_involked_function_expression(expression),
            _ => self.expression(expression),
        }
    }

    /// Wrap an expression in an immediately involked function expression
    fn immediately_involked_function_expression<'a>(
        &mut self,
        expression: &'a TypedExpr,
    ) -> Output<'a> {
        let tail = self.tail_position;
        self.tail_position = true;
        let current_scope_vars = self.current_scope_vars.clone();
        let result = self.expression(expression);
        self.tail_position = tail;
        self.current_scope_vars = current_scope_vars;
        Ok(self.immediately_involked_function_expression_document(result?))
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

    fn variable<'a>(&mut self, name: &'a str, constructor: &'a ValueConstructor) -> Document<'a> {
        match &constructor.variant {
            ValueConstructorVariant::Record { arity, .. } => {
                self.record_constructor(constructor.type_.clone(), None, name, *arity)
            }
            ValueConstructorVariant::ModuleFn { .. }
            | ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::LocalVariable { .. } => self.local_var(name),
        }
    }

    fn record_constructor<'a>(
        &mut self,
        type_: Arc<Type>,
        qualifier: Option<&'a str>,
        name: &'a str,
        arity: usize,
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
            let vars = (0..arity).map(|i| Document::String(format!("var{}", i)));
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

    fn sequence<'a>(&mut self, expressions: &'a [TypedExpr]) -> Output<'a> {
        let count = expressions.len();
        let mut documents = Vec::with_capacity(count * 3);
        documents.push(force_break());
        for (i, expression) in expressions.iter().enumerate() {
            if i + 1 < count {
                documents.push(self.not_in_tail_position(|gen| gen.expression(expression))?);
                if !matches!(
                    expression,
                    TypedExpr::Assignment { .. } | TypedExpr::Case { .. }
                ) {
                    documents.push(";".to_doc());
                }
                documents.push(line());
            } else {
                documents.push(self.expression(expression)?);
            }
        }
        Ok(documents.to_doc())
    }

    fn try_<'a>(
        &mut self,
        subject: &'a TypedExpr,
        pattern: &'a TypedPattern,
        then: &'a TypedExpr,
    ) -> Output<'a> {
        let mut docs = vec![force_break()];

        // If the subject is not a variable then we will need to save it to a
        // variable to prevent any side effects from rendering the same
        // expression twice.
        let subject_doc = if let TypedExpr::Var { name, .. } = subject {
            self.local_var(name)
        } else {
            let subject = self.not_in_tail_position(|gen| gen.wrap_expression(subject))?;
            let name = self.next_local_var(pattern::ASSIGNMENT_VAR);
            docs.push("let ".to_doc());
            docs.push(name.clone());
            docs.push(" = ".to_doc());
            docs.push(subject);
            docs.push(";".to_doc());
            docs.push(line());
            name
        };

        // We return early if the subject is an error
        docs.push("if (!".to_doc());
        docs.push(subject_doc.clone());
        docs.push(r#".isOk()) return "#.to_doc());
        docs.push(subject_doc.clone());
        docs.push(";".to_doc());

        match pattern {
            // Assign the inner value to a variable if it used
            TypedPattern::Var { name, .. } => {
                docs.push(line());
                docs.push("let ".to_doc());
                docs.push(self.next_local_var(name));
                docs.push(" = ".to_doc());
                docs.push(subject_doc);
                docs.push("[0];".to_doc());
                docs.push(lines(2));
            }

            TypedPattern::Discard { .. } => {
                docs.push(lines(1));
            }

            // TODO: At time of writing to support patterns in trys we will need
            // to adapt the `assignment` method to take a Document as a value
            // and pass the subject document into that also rather than letting
            // the pattern generator determine what it should be.
            pattern => {
                let subject = subject_doc.append("[0]");
                let mut pattern_generator = pattern::Generator::new(self);
                pattern_generator.traverse_pattern(&subject, pattern)?;
                let compiled = pattern_generator.take_compiled();
                docs.push(line());
                docs.push(self.pattern_into_assignment_doc(
                    compiled,
                    subject,
                    pattern.location(),
                )?);
                docs.push(lines(2));
            }
        }

        // Lastly, whatever comes next
        docs.push(self.expression(then)?);

        Ok(docs.to_doc())
    }

    fn assignment<'a>(&mut self, value: &'a TypedExpr, pattern: &'a TypedPattern) -> Output<'a> {
        // If it is a simple assignment to a variable we can generate a normal
        // JS assignment
        if let TypedPattern::Var { name, .. } = pattern {
            // Subject must be rendered before the variable for variable numbering
            let subject = self.not_in_tail_position(|gen| gen.wrap_expression(value))?;
            let name = self.next_local_var(name);
            return Ok(if self.tail_position {
                docvec![
                    force_break(),
                    "let ",
                    name.clone(),
                    " = ",
                    subject,
                    ";",
                    line(),
                    "return ",
                    name,
                    ";"
                ]
            } else {
                docvec![force_break(), "let ", name, " = ", subject, ";"]
            });
        }

        // Otherwise we need to compile the patterns
        let (subject, subject_assignment) = pattern::assign_subject(self, value);
        // Value needs to be rendered before traversing pattern to have correctly incremented variables.
        let value = self.not_in_tail_position(|gen| gen.wrap_expression(value))?;
        let mut pattern_generator = pattern::Generator::new(self);
        pattern_generator.traverse_pattern(&subject, pattern)?;
        let compiled = pattern_generator.take_compiled();

        // If we are in tail position we can return value being assigned
        let afterwards = if self.tail_position {
            line()
                .append("return ")
                .append(subject_assignment.clone().unwrap_or_else(|| value.clone()))
                .append(";")
        } else {
            nil()
        };

        // If there is a subject name given create a variable to hold it for
        // use in patterns
        let doc = match subject_assignment {
            Some(name) => {
                let compiled =
                    self.pattern_into_assignment_doc(compiled, subject, pattern.location())?;
                docvec!("let ", name, " = ", value, ";", line(), compiled)
            }
            None => self.pattern_into_assignment_doc(compiled, subject, pattern.location())?,
        };

        Ok(docvec!(force_break(), doc.append(afterwards)))
    }

    fn case<'a>(
        &mut self,
        location: SrcSpan,
        subject_values: &'a [TypedExpr],
        clauses: &'a [TypedClause],
    ) -> Output<'a> {
        let mut possibility_of_no_match = true;

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
                let consequence = gen.expression_generator.expression(&clause.then)?;

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
                let is_catch_all = !compiled.has_checks() && clause.guard.is_none();

                if is_catch_all {
                    possibility_of_no_match = false;
                }

                doc = if is_only_clause && is_catch_all {
                    // If this is the only clause and there are no checks then we can
                    // render just the body as the case does nothing
                    doc.append(body)
                } else if is_final_clause && is_catch_all {
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

        if possibility_of_no_match {
            // Lastly append an error if no clause matches.
            // We can remove this when we get exhaustiveness checking.
            doc = doc
                .append(" else {")
                .append(
                    docvec!(line(), self.case_no_match(location, subjects.into_iter())?)
                        .nest(INDENT),
                )
                .append(line())
                .append("}")
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

        Ok(docvec![force_break(), subject_assignments, doc])
    }

    fn case_no_match<'a, Subjects>(&mut self, location: SrcSpan, subjects: Subjects) -> Output<'a>
    where
        Subjects: IntoIterator<Item = Document<'a>>,
    {
        Ok(self.throw_error(
            "case_no_match",
            "No case clause matched",
            location,
            [("values", array(subjects.into_iter().map(Ok))?)],
        ))
    }

    fn assignment_no_match<'a>(&mut self, location: SrcSpan, subject: Document<'a>) -> Output<'a> {
        Ok(self.throw_error(
            "assignment_no_match",
            "Assignment pattern did not match",
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
        let tail = self.tail_position;
        self.tail_position = false;
        let arguments = arguments
            .iter()
            .map(|element| self.wrap_expression(&element.value))
            .try_collect()?;
        self.tail_position = tail;
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
                if self.function_name == Some(name)
                    && self.tail_position
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

    fn fn_<'a>(&mut self, arguments: &'a [TypedArg], body: &'a TypedExpr) -> Output<'a> {
        // New function, this is now the tail position
        let tail = self.tail_position;
        self.tail_position = true;
        // And there's a new scope
        let scope = self.current_scope_vars.clone();
        for name in arguments.iter().flat_map(Arg::get_variable_name) {
            let _ = self.current_scope_vars.remove(name);
        }

        // This is a new function so unset the recorded name so that we don't
        // mistakenly trigger tail call optimisation
        let mut name = None;
        std::mem::swap(&mut self.function_name, &mut name);

        // Generate the function body
        let result = self.expression(body);

        // Reset function name, scope, and tail position tracking
        self.tail_position = tail;
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
            Ok(docvec![tuple, Document::String(format!("[{}]", index))])
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
            BinOp::AddInt | BinOp::AddFloat => self.print_bin_op(left, right, "+"),
            BinOp::SubInt | BinOp::SubFloat => self.print_bin_op(left, right, "-"),
            BinOp::MultInt => self.mult_int(left, right),
            BinOp::MultFloat => self.print_bin_op(left, right, "*"),
            BinOp::ModuloInt => self.print_bin_op(left, right, "%"),
            BinOp::DivInt => self.div_int(left, right),
            BinOp::DivFloat => self.div_float(left, right),
        }
    }

    fn mult_int<'a>(&mut self, left: &'a TypedExpr, right: &'a TypedExpr) -> Output<'a> {
        let left = self.not_in_tail_position(|gen| gen.expression(left))?;
        let right = self.not_in_tail_position(|gen| gen.expression(right))?;
        Ok(docvec!("Math.imul", wrap_args([left, right])))
    }

    fn div_int<'a>(&mut self, left: &'a TypedExpr, right: &'a TypedExpr) -> Output<'a> {
        let left = self.not_in_tail_position(|gen| gen.expression(left))?;
        let right = self.not_in_tail_position(|gen| gen.expression(right))?;
        self.tracker.int_division_used = true;
        Ok(docvec!("divideInt", wrap_args([left, right])))
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
            let left_doc = self.not_in_tail_position(|gen| gen.binop_child_expression(left))?;
            let right_doc = self.not_in_tail_position(|gen| gen.binop_child_expression(right))?;
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
        let left = self.not_in_tail_position(|gen| gen.binop_child_expression(left))?;
        let right = self.not_in_tail_position(|gen| gen.binop_child_expression(right))?;
        Ok(docvec!(left, " ", op, " ", right))
    }

    fn todo<'a>(&mut self, message: &'a Option<String>, location: &'a SrcSpan) -> Document<'a> {
        let tail_position = self.tail_position;
        self.tail_position = false;

        let message = message
            .as_deref()
            .unwrap_or("This has not yet been implemented");
        let doc = self.throw_error("todo", message, *location, vec![]);

        // Reset tail position so later values are returned as needed. i.e.
        // following clauses in a case expression.
        self.tail_position = tail_position;

        doc
    }

    fn throw_error<'a, Fields>(
        &mut self,
        error_name: &'a str,
        message: &'a str,
        location: SrcSpan,
        fields: Fields,
    ) -> Document<'a>
    where
        Fields: IntoIterator<Item = (&'a str, Document<'a>)>,
    {
        self.tracker.throw_error_used = true;
        let module = Document::String(self.module_name.join("/")).surround('"', '"');
        // TODO switch to use `string(self.function_name)`
        // This will require resolving the
        // difference in lifetimes 'module and 'a.
        let function = Document::String(self.function_name.unwrap_or_default().to_string())
            .surround("\"", "\"");
        let line = self.line_numbers.line_number(location.start).to_doc();
        let fields = wrap_object(fields.into_iter().map(|(k, v)| (k.to_doc(), Some(v))));
        docvec![
            "throwError",
            wrap_args([
                string(error_name),
                module,
                line,
                function,
                string(message),
                fields
            ]),
            ";"
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
    ) -> Output<'a> {
        if compiled_pattern.checks.is_empty() {
            return Ok(Self::pattern_assignments_doc(compiled_pattern.assignments));
        }
        if compiled_pattern.assignments.is_empty() {
            return self.pattern_checks_or_throw_doc(compiled_pattern.checks, subject, location);
        }

        Ok(docvec![
            self.pattern_checks_or_throw_doc(compiled_pattern.checks, subject, location)?,
            line(),
            Self::pattern_assignments_doc(compiled_pattern.assignments)
        ])
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

    fn pattern_assignments_doc(assignments: Vec<pattern::Assignment<'_>>) -> Document<'_> {
        let assignments = assignments.into_iter().map(pattern::Assignment::into_doc);
        concat(Itertools::intersperse(assignments, line()))
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

        concat(Itertools::intersperse(
            checks
                .into_iter()
                .map(|check| check.into_doc(match_desired)),
            operator,
        ))
    }
}

pub fn int(value: &str) -> Document<'_> {
    value.to_doc()
}

pub fn float(value: &str) -> Document<'_> {
    value.to_doc()
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
            list(
                elements.iter().map(|e| constant_expression(tracker, e)),
                None,
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
                .map(|arg| constant_expression(tracker, &arg.value))
                .try_collect()?;
            Ok(construct_record(module.as_deref(), tag, field_values))
        }

        Constant::BitString { location, .. } => Err(Error::Unsupported {
            feature: "Bit string syntax".to_string(),
            location: *location,
        }),
    }
}

pub fn string(value: &str) -> Document<'_> {
    if value.contains('\n') {
        Document::String(value.replace('\n', r#"\n"#)).surround("\"", "\"")
    } else {
        value.to_doc().surround("\"", "\"")
    }
}

pub fn array<'a, Elements: IntoIterator<Item = Output<'a>>>(elements: Elements) -> Output<'a> {
    let elements = Itertools::intersperse(elements.into_iter(), Ok(break_(",", ", ")))
        .collect::<Result<Vec<_>, _>>()?
        .to_doc();
    Ok(docvec![
        "[",
        docvec![break_("", ""), elements].nest(INDENT),
        break_(",", ""),
        "]"
    ]
    .group())
}

fn list<'a, I: IntoIterator<Item = Output<'a>>>(
    elements: I,
    tail: Option<Document<'a>>,
) -> Output<'a>
where
    I::IntoIter: DoubleEndedIterator + ExactSizeIterator,
{
    let array = array(elements);
    if let Some(tail) = tail {
        let args = [array, Ok(tail)];
        Ok(docvec!["toList", call_arguments(args)?])
    } else {
        Ok(docvec!["toList(", array?, ")"])
    }
}

fn call_arguments<'a, Elements: IntoIterator<Item = Output<'a>>>(elements: Elements) -> Output<'a> {
    let elements = Itertools::intersperse(elements.into_iter(), Ok(break_(",", ", ")))
        .collect::<Result<Vec<_>, _>>()?
        .to_doc();
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
    let arguments = concat(Itertools::intersperse(
        arguments.into_iter().map(|a| {
            any_arguments = true;
            a
        }),
        break_(",", ", "),
    ));
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
        matches!(
            self,
            TypedExpr::Try { .. }
                | TypedExpr::Todo { .. }
                | TypedExpr::Call { .. }
                | TypedExpr::Case { .. }
                | TypedExpr::Sequence { .. }
                | TypedExpr::Pipeline { .. }
                | TypedExpr::Assignment { .. }
        )
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
            | BinOp::ModuloInt => true,
            BinOp::MultInt => false,
        }
    }
}

pub fn is_js_scalar(t: Arc<Type>) -> bool {
    t.is_int() || t.is_float() || t.is_bool() || t.is_nil() || t.is_string()
}
