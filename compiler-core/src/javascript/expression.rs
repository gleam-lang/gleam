use std::sync::Arc;

use super::*;
use crate::{
    ast::*,
    line_numbers::LineNumbers,
    pretty::*,
    type_::{
        FieldMap, HasType, ModuleValueConstructor, Type, ValueConstructor, ValueConstructorVariant,
    },
};

static RECORD_KEY: &str = "type";

#[derive(Debug)]
pub struct Generator<'module> {
    module_name: &'module [String],
    line_numbers: &'module LineNumbers,
    function_name: Option<&'module str>,
    function_arguments: Vec<Option<&'module str>>,
    current_scope_vars: im::HashMap<String, usize>,
    pub tail_position: bool,
    // We register whether float division or object equality are used within an
    // expression so that the module generator can output a suitable function if
    // it is needed.
    float_division_used: &'module mut bool,
    object_equality_used: &'module mut bool,
    // We track whether tail call recusion is used so that we can render a loop
    // at the top level of the function to use in place of pushing new stack
    // frames.
    tail_recursion_used: bool,
}

impl<'module> Generator<'module> {
    pub fn new(
        module_name: &'module [String],
        line_numbers: &'module LineNumbers,
        function_name: &'module str,
        function_arguments: Vec<Option<&'module str>>,
        float_division_used: &'module mut bool,
        object_equality_used: &'module mut bool,
        mut current_scope_vars: im::HashMap<String, usize>,
    ) -> Self {
        for &name in function_arguments.iter().flatten() {
            let _ = current_scope_vars.insert(name.to_string(), 0);
        }
        Self {
            module_name,
            line_numbers,
            function_name: Some(function_name),
            function_arguments,
            tail_recursion_used: false,
            current_scope_vars,
            tail_position: true,
            float_division_used,
            object_equality_used,
        }
    }

    pub fn local_var<'a>(&mut self, name: &'a str) -> Document<'a> {
        match self.current_scope_vars.get(name) {
            None => {
                let _ = self.current_scope_vars.insert(name.to_string(), 0);
                maybe_escape_identifier(name)
            }
            Some(0) => maybe_escape_identifier(name),
            Some(n) if name == "$" => Document::String(format!("${}", n)),
            Some(n) => Document::String(format!("{}${}", name, n)),
        }
    }

    pub fn next_local_var<'a>(&mut self, name: &'a str) -> Document<'a> {
        let next = self.current_scope_vars.get(name).map_or(0, |i| i + 1);
        let _ = self.current_scope_vars.insert(name.to_string(), next);
        self.local_var(name)
    }

    pub fn function_body<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        let body = self.expression(expression)?;
        if self.tail_recursion_used {
            Ok(docvec!(
                "while (true) {",
                docvec!(line(), body).nest(INDENT),
                line(),
                "}"
            ))
        } else {
            Ok(body)
        }
    }

    pub fn expression<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        let document = match expression {
            TypedExpr::String { value, .. } => Ok(string(value)),

            TypedExpr::Int { value, .. } => Ok(int(value)),
            TypedExpr::Float { value, .. } => Ok(float(value)),

            TypedExpr::List { elements, tail, .. } => self.not_in_tail_position(|gen| {
                let tail = match tail.as_ref() {
                    Some(tail) => Some(gen.wrap_expression(tail)?),
                    None => None,
                };
                list(elements.iter().map(|e| gen.wrap_expression(e)), tail)
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
            } => Ok(self.variable(name, constructor)),

            TypedExpr::Sequence { expressions, .. } => self.sequence(expressions),

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

            TypedExpr::BitString { .. } => unsupported("Bitstring"),

            TypedExpr::ModuleSelect {
                module_alias,
                label,
                constructor,
                ..
            } => Ok(self.module_select(module_alias, label, constructor)),
        }?;
        Ok(if expression.handles_own_return() {
            document
        } else {
            self.wrap_return(document)
        })
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
            TypedExpr::Case { .. }
            | TypedExpr::Sequence { .. }
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
        Ok(docvec!(
            docvec!("(() => {", break_("", " "), result?).nest(INDENT),
            break_("", " "),
            "})()",
        )
        .group())
    }

    fn variable<'a>(&mut self, name: &'a str, constructor: &'a ValueConstructor) -> Document<'a> {
        match &constructor.variant {
            ValueConstructorVariant::Record {
                name,
                arity,
                field_map,
                ..
            } => self.record_constructor(constructor.type_.clone(), name, *arity, field_map),
            ValueConstructorVariant::ModuleFn { .. }
            | ValueConstructorVariant::ModuleConstant { .. }
            | ValueConstructorVariant::LocalVariable => self.local_var(name),
        }
    }

    fn record_constructor<'a>(
        &mut self,
        type_: Arc<Type>,
        name: &'a str,
        arity: usize,
        field_map: &'a Option<FieldMap>,
    ) -> Document<'a> {
        if type_.is_bool() && name == "True" {
            "true".to_doc()
        } else if type_.is_bool() {
            "false".to_doc()
        } else if type_.is_nil() {
            "undefined".to_doc()
        } else if arity == 0 {
            let record_type = name.to_doc().surround("\"", "\"");
            let record_head = (RECORD_KEY.to_doc(), Some(record_type));
            wrap_object(std::iter::once(record_head))
        } else {
            let vars = (0..arity)
                .into_iter()
                .map(|i| Document::String(format!("var{}", i)));

            let body = docvec![
                "return ",
                construct_record(name, arity, field_map, vars.clone()),
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
                match expression {
                    TypedExpr::Assignment { .. } | TypedExpr::Case { .. } => (),
                    _ => documents.push(";".to_doc()),
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
        docs.push("if (".to_doc());
        docs.push(subject_doc.clone());
        docs.push(r#".type === "Error") return "#.to_doc());
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

            // TODO: to support this we will need to adapt the `assignment`
            // method to take a Document as a value and pass the subject
            // document into that also rather than letting the pattern generator
            // determine what it should be.
            // adapting the `assignment` function would probably use `new_with_document`
            // at which point it could be the only "new" function for a pattern generator and the name of `new_with_document` could be shortened
            pattern => {
                let value = subject_doc.append("[0]");
                let mut pattern_generator =
                    pattern::Generator::new_with_document(self, value.clone());
                let compiled = pattern_generator.generate(pattern, None)?;
                docs.push(line());
                docs.push(compiled.into_assignment_doc(&value));
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
            return Ok(docvec!(force_break(), "let ", name, " = ", subject, ";"));
        }

        // Otherwise we need to compile the patterns
        let (mut patten_generator, subject) = pattern::Generator::new(self, value)?;
        let compiled = patten_generator.generate(pattern, None)?;

        let value = self.not_in_tail_position(|gen| gen.wrap_expression(value))?;

        // If we are in tail position we can return value being assigned
        let afterwards = if self.tail_position {
            line()
                .append("return ")
                .append(subject.clone().unwrap_or_else(|| value.clone()))
                .append(";")
        } else {
            nil()
        };

        // If there is a subject name given create a variable to hold it for
        // use in patterns
        let doc = match subject {
            Some(name) => {
                let compiled = compiled.into_assignment_doc(&name);
                docvec!("let ", name, " = ", value, ";", line(), compiled)
            }
            None => compiled.into_assignment_doc(&value),
        };

        Ok(docvec!(force_break(), doc.append(afterwards)))
    }

    fn case<'a>(&mut self, subjects: &'a [TypedExpr], clauses: &'a [TypedClause]) -> Output<'a> {
        let mut possibility_of_no_match = true;

        let value = match subjects {
            [subject] => subject,
            _ => return unsupported("Cases with multiple subjects"),
        };

        let (mut gen, subject) = pattern::Generator::new(self, value)?;
        let value = gen
            .expression_generator
            .not_in_tail_position(|gen| gen.expression(value))?;

        let mut doc = force_break();

        // We wish to be able to know whether this is the first or clause being
        // processed, so record the index number. We use this instead of
        // `Iterator.enumerate` because we are using a nested for loop.
        let mut clause_number = 0;
        let total_patterns: usize = clauses
            .iter()
            .map(|c| c.alternative_patterns.len())
            .sum::<usize>()
            + clauses.len();

        // TODO: handle multiple subjects gracefully
        for clause in clauses {
            let mut patterns = vec![clause.pattern.get(0).expect("JS clause pattern indexing")];

            patterns.extend(clause.alternative_patterns.iter().flatten());

            for pattern in patterns {
                let scope = gen.expression_generator.current_scope_vars.clone();
                let mut compiled = gen.generate(pattern, clause.guard.as_ref())?;
                let consequence = gen.expression_generator.expression(&clause.then)?;

                // We've seen one more clause
                clause_number += 1;

                // Reset the scope now that this clause has finished, causing the
                // variables to go out of scope.
                gen.expression_generator.current_scope_vars = scope;

                // If the pattern assigns any variables we need to render assignments
                let body = if compiled.has_assignments() {
                    let subject = match subject {
                        None => &value,
                        Some(ref name) => name,
                    };
                    let assignments = compiled.take_assignments_doc(subject);

                    docvec!(line(), assignments, line(), consequence).nest(INDENT)
                } else {
                    docvec!(line(), consequence).nest(INDENT)
                };

                let is_final_clause = clause_number == total_patterns;
                doc = if is_final_clause && !compiled.has_checks() && clause.guard.is_none() {
                    // If this is the final clause and there are no checks then we can
                    // render `else` instead of `else if (...)`
                    possibility_of_no_match = false;
                    doc.append(" else {")
                } else {
                    doc.append(if clause_number == 1 {
                        "if ("
                    } else {
                        " else if ("
                    })
                    .append(compiled.take_checks_doc(true))
                    .append(") {")
                };

                doc = doc.append(body).append(line()).append("}");
            }
        }

        if possibility_of_no_match {
            // Lastly append an error if no clause matches.
            // We can remove this when we get exhaustiveness checking.
            doc = doc
                .append(" else {")
                .append(docvec!(line(), r#"throw new Error("Bad match");"#).nest(INDENT))
                .append(line())
                .append("}")
        }

        // If there is a subject name given create a variable to hold it for
        // use in patterns
        Ok(match subject {
            Some(name) => docvec!("let ", name, " = ", value, ";", line(), doc),
            None => doc,
        })
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
            .collect::<Result<Vec<_>, _>>()?;
        self.tail_position = tail;
        self.call_with_doc_args(fun, arguments)
    }

    fn call_with_doc_args<'a>(
        &mut self,
        fun: &'a TypedExpr,
        arguments: Vec<Document<'a>>,
    ) -> Output<'a> {
        match fun {
            // Record construction
            TypedExpr::ModuleSelect {
                constructor:
                    ModuleValueConstructor::Record {
                        name,
                        arity,
                        field_map,
                        ..
                    },
                ..
            }
            | TypedExpr::Var {
                constructor:
                    ValueConstructor {
                        variant:
                            ValueConstructorVariant::Record {
                                name,
                                arity,
                                field_map,
                                ..
                            },
                        ..
                    },
                ..
            } => Ok(self.wrap_return(construct_record(
                name,
                *arity,
                field_map,
                arguments.into_iter(),
            ))),

            // Tail call optimisation. If we are calling the current function
            // and we are in tail position we can avoid creating a new stack
            // frame, enabling recursion with constant memory usage.
            TypedExpr::Var { name, .. }
                if self.function_name == Some(name.as_str())
                    && self.tail_position
                    && self.current_scope_vars.get(name) == Some(&0) =>
            {
                let mut docs = Vec::with_capacity(arguments.len() * 4);
                // Record that tail recursion is happening so that we know to
                // render the loop at the top level of the function.
                self.tail_recursion_used = true;

                for (i, (element, argument)) in arguments
                    .into_iter()
                    .zip(self.function_arguments.clone())
                    .enumerate()
                {
                    if i != 0 {
                        docs.push(line());
                    }
                    // Create an assignment for each variable created by the function arguments
                    if let Some(name) = argument {
                        docs.push(Document::String(format!("{} = ", name)));
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
                    let fun = gen.expression(fun)?;
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

        // This is a new function so unset the recorded name so that we don't
        // mistakenly trigger tail call optimisation
        let mut name = None;
        std::mem::swap(&mut self.function_name, &mut name);

        // Generate the function body
        let result = self.expression(body);

        // Reset function name and tail position tracking
        self.tail_position = tail;
        std::mem::swap(&mut self.function_name, &mut name);

        Ok(docvec!(
            docvec!(fun_args(arguments), " => {", break_("", " "), result?)
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
        spread: &'a TypedExpr,
        updates: &'a [TypedRecordUpdateArg],
    ) -> Output<'a> {
        self.not_in_tail_position(|gen| {
            let spread = gen.wrap_expression(spread)?;
            let updates: Vec<(Document<'a>, Option<Document<'a>>)> = updates
                .iter()
                .map(|TypedRecordUpdateArg { label, value, .. }| {
                    Ok((label.to_doc(), Some(gen.wrap_expression(value)?)))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let assign_args = vec!["{}".to_doc(), spread, wrap_object(updates.into_iter())];
            Ok(docvec!["Object.assign", wrap_args(assign_args.into_iter())])
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
            BinOp::DivInt => Ok(self.print_bin_op(left, right, "/")?.append(" | 0")),
            BinOp::ModuloInt => self.print_bin_op(left, right, "%"),
            BinOp::DivFloat => self.div_float(left, right),
        }
    }

    fn mult_int<'a>(&mut self, left: &'a TypedExpr, right: &'a TypedExpr) -> Output<'a> {
        let left = self.not_in_tail_position(|gen| gen.expression(left))?;
        let right = self.not_in_tail_position(|gen| gen.expression(right))?;
        use std::iter::once;
        Ok(docvec!(
            "Math.imul",
            wrap_args(once(left).chain(once(right)))
        ))
    }

    fn div_float<'a>(&mut self, left: &'a TypedExpr, right: &'a TypedExpr) -> Output<'a> {
        let left = self.not_in_tail_position(|gen| gen.expression(left))?;
        let right = self.not_in_tail_position(|gen| gen.expression(right))?;
        use std::iter::once;
        *self.float_division_used = true;
        Ok(docvec!("$divide", wrap_args(once(left).chain(once(right)))))
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
        Ok(self.dollar_equal_call(should_be_equal, left, right))
    }

    pub(super) fn dollar_equal_call<'a>(
        &mut self,
        should_be_equal: bool,
        left: Document<'a>,
        right: Document<'a>,
    ) -> Document<'a> {
        // Record that we need to render the $equal function into the module
        *self.object_equality_used = true;
        // Construct the call
        use std::iter::once;
        let args = wrap_args(once(left).chain(once(right)));
        let operator = if should_be_equal { "$equal" } else { "!$equal" };
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
        self.tail_position = false;
        let gleam_error = "todo";
        let message = message
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or_else(|| "This has not yet been implemented");
        let module_name = Document::String(self.module_name.join("/"));
        let line = self.line_numbers.line_number(location.start);

        docvec![
            "throw Object.assign",
            wrap_args(
                vec![
                    docvec!["new Error", wrap_args(std::iter::once(string(message)))],
                    wrap_object(
                        vec![
                            ("gleam_error".to_doc(), Some(string(gleam_error))),
                            ("module".to_doc(), Some(module_name.surround("\"", "\""))),
                            (
                                "function".to_doc(),
                                Some(
                                    // TODO switch to use `string(self.function_name)`
                                    // This will require resolving the
                                    // difference in lifetimes 'module and 'a.
                                    Document::String(
                                        self.function_name.unwrap_or_default().to_string()
                                    )
                                    .surround("\"", "\"")
                                )
                            ),
                            ("line".to_doc(), Some(line.to_doc())),
                        ]
                        .into_iter()
                    )
                ]
                .into_iter()
            )
        ]
    }

    fn module_select<'a>(
        &mut self,
        module: &'a str,
        label: &'a str,
        constructor: &'a ModuleValueConstructor,
    ) -> Document<'a> {
        match constructor {
            ModuleValueConstructor::Fn | ModuleValueConstructor::Constant { .. } => {
                docvec![
                    Document::String(module.to_camel_case()),
                    ".",
                    maybe_escape_identifier(label)
                ]
            }

            ModuleValueConstructor::Record {
                name,
                arity,
                field_map,
                type_,
                ..
            } => self.record_constructor(type_.clone(), name, *arity, field_map),
        }
    }
}

pub fn int(value: &str) -> Document<'_> {
    value.to_doc()
}

pub fn float(value: &str) -> Document<'_> {
    value.to_doc()
}

pub fn constant_expression(expression: &'_ TypedConstant) -> Output<'_> {
    match expression {
        Constant::Int { value, .. } => Ok(int(value)),
        Constant::Float { value, .. } => Ok(float(value)),
        Constant::String { value, .. } => Ok(string(value.as_str())),
        Constant::Tuple { elements, .. } => array(elements.iter().map(|e| constant_expression(e))),
        Constant::List { elements, .. } => list(elements.iter().map(constant_expression), None),
        Constant::Record { typ, name, .. } if typ.is_bool() && name == "True" => {
            Ok("true".to_doc())
        }
        Constant::Record { typ, name, .. } if typ.is_bool() && name == "False" => {
            Ok("false".to_doc())
        }
        Constant::Record { typ, .. } if typ.is_nil() => Ok("undefined".to_doc()),
        Constant::Record {
            tag,
            args,
            field_map,
            ..
        } => {
            let field_values: Result<Vec<_>, _> = args
                .iter()
                .map(|arg| constant_expression(&arg.value))
                .collect();
            Ok(construct_record(
                tag,
                args.len(),
                field_map,
                field_values?.into_iter(),
            ))
        }
        Constant::BitString { .. } => unsupported("BitString as constant"),
    }
}

pub fn string(value: &str) -> Document<'_> {
    if value.contains('\n') {
        Document::String(value.replace('\n', r#"\n"#)).surround("\"", "\"")
    } else {
        value.to_doc().surround("\"", "\"")
    }
}

pub fn array<'a, Elements: Iterator<Item = Output<'a>>>(elements: Elements) -> Output<'a> {
    let elements = Itertools::intersperse(elements, Ok(break_(",", ", ")))
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

fn list<'a>(
    elements: impl Iterator<Item = Output<'a>> + DoubleEndedIterator + ExactSizeIterator,
    mut tail: Option<Document<'a>>,
) -> Output<'a> {
    for (i, element) in elements.enumerate().rev() {
        let mut doc = match tail {
            Some(tail) => docvec![element?.group(), break_(",", ", "), tail],
            None => docvec![element?.group(), break_(",", ", "), "[]"],
        };
        if i != 0 {
            doc = docvec!("[", doc, "]");
        }
        tail = Some(doc);
    }
    Ok(docvec![
        "[",
        docvec!(docvec!(break_("", ""), tail).nest(INDENT), break_(",", ""),),
        "]"
    ]
    .group())
}

fn call_arguments<'a, Elements: Iterator<Item = Output<'a>>>(elements: Elements) -> Output<'a> {
    let elements = Itertools::intersperse(elements, Ok(break_(",", ", ")))
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
    name: &'a str,
    arity: usize,
    field_map: &'a Option<FieldMap>,
    values: impl Iterator<Item = Document<'a>>,
) -> Document<'a> {
    let field_names: Vec<Document<'_>> = match field_map {
        Some(FieldMap { fields, .. }) => fields
            .iter()
            .sorted_by_key(|(_, &v)| v)
            .map(|x| x.0.as_str().to_doc())
            .collect(),
        None => (0..arity)
            .into_iter()
            .map(|i| Document::String(format!("{}", i)))
            .collect(),
    };

    let record_head = (
        RECORD_KEY.to_doc(),
        Some(name.to_doc().surround("\"", "\"")),
    );

    let record_values = field_names
        .into_iter()
        .zip(values)
        .map(|(name, value)| (name, Some(value)));

    wrap_object(std::iter::once(record_head).chain(record_values))
}

impl TypedExpr {
    fn handles_own_return(&self) -> bool {
        matches!(
            self,
            TypedExpr::Try { .. }
                | TypedExpr::Call { .. }
                | TypedExpr::Case { .. }
                | TypedExpr::Sequence { .. }
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
