use super::{
    expression::{constant_expression, is_js_scalar},
    *,
};
use crate::type_::{FieldMap, PatternConstructor};

pub static ASSIGNMENT_VAR: &str = "$";

#[derive(Debug)]
enum Index<'a> {
    Int(usize),
    String(&'a str),
}

#[derive(Debug)]
pub struct Generator<'module, 'expression, 'a> {
    pub expression_generator: &'expression mut expression::Generator<'module>,
    path: Vec<Index<'a>>,
    subject: Document<'a>,
    checks: Vec<Check<'a>>,
    assignments: Vec<Assignment<'a>>,
}

impl<'module, 'expression, 'a> Generator<'module, 'expression, 'a> {
    pub fn new(
        expression_generator: &'expression mut expression::Generator<'module>,
        subject: &'a TypedExpr,
    ) -> Result<(Self, Option<Document<'a>>), Error> {
        // If the value is a variable we don't need to assign it to a new
        // variable, we can the value expression safely without worrying about
        // performing computation or side effects multiple times.
        let value_variable_name = match subject {
            TypedExpr::Var { name, .. } => Some(name),
            _ => None,
        };
        let (subject, assignment) = match &value_variable_name {
            Some(name) => (expression_generator.local_var(name), None),
            None => {
                let subject = expression_generator.next_local_var(ASSIGNMENT_VAR);
                let assignment = Some(subject.clone());
                (subject, assignment)
            }
        };
        let me = Self {
            path: vec![],
            checks: vec![],
            assignments: vec![],
            expression_generator,
            subject: subject.clone(),
        };
        Ok((me, assignment))
    }

    fn next_local_var(&mut self, name: &'a str) -> Document<'a> {
        self.expression_generator.next_local_var(name)
    }

    fn local_var(&mut self, name: &'a str) -> Document<'a> {
        self.expression_generator.local_var(name)
    }

    fn push_string(&mut self, s: &'a str) {
        self.path.push(Index::String(s));
    }

    fn push_int(&mut self, i: usize) {
        self.path.push(Index::Int(i));
    }

    fn push_int_times(&mut self, index: usize, times: usize) {
        for _ in 0..times {
            self.push_int(index);
        }
    }

    fn pop(&mut self) {
        let _ = self.path.pop();
    }

    fn pop_times(&mut self, times: usize) {
        for _ in 0..times {
            self.pop();
        }
    }

    fn path_document(&self) -> Document<'a> {
        concat(self.path.iter().map(|segment| match segment {
            Index::Int(i) => Document::String(format!("[{}]", i)),
            Index::String(s) => docvec!(".", s),
        }))
    }

    pub fn generate(
        &mut self,
        pattern: &'a TypedPattern,
        guard: Option<&'a TypedClauseGuard>,
    ) -> Result<CompiledPattern<'a>, Error> {
        let _ = self.traverse_pattern(pattern)?;
        if let Some(guard) = guard {
            let _ = self.push_guard_check(guard)?;
        }

        Ok(CompiledPattern {
            checks: std::mem::take(&mut self.checks),
            assignments: std::mem::take(&mut self.assignments),
        })
    }

    fn push_guard_check(&mut self, guard: &'a TypedClauseGuard) -> Result<(), Error> {
        let expression = self.guard(guard)?;
        self.checks.push(Check::Guard { expression });
        Ok(())
    }

    fn wrapped_guard(&mut self, guard: &'a TypedClauseGuard) -> Result<Document<'a>, Error> {
        match guard {
            ClauseGuard::Var { .. } | ClauseGuard::TupleIndex { .. } | ClauseGuard::Constant(_) => {
                self.guard(guard)
            }
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
            | ClauseGuard::Or { .. }
            | ClauseGuard::And { .. } => Ok(docvec!("(", self.guard(guard)?, ")")),
        }
    }

    fn guard(&mut self, guard: &'a TypedClauseGuard) -> Output<'a> {
        Ok(match guard {
            ClauseGuard::Equals { left, right, .. } if is_js_scalar(left.type_()) => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec!(left, " === ", right)
            }

            ClauseGuard::NotEquals { left, right, .. } if is_js_scalar(left.type_()) => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec!(left, " !== ", right)
            }

            ClauseGuard::Equals { left, right, .. } => {
                let left = self.guard(left)?;
                let right = self.guard(right)?;
                self.expression_generator
                    .dollar_equal_call(true, left, right)
            }

            ClauseGuard::NotEquals { left, right, .. } => {
                let left = self.guard(left)?;
                let right = self.guard(right)?;
                self.expression_generator
                    .dollar_equal_call(false, left, right)
            }

            ClauseGuard::GtFloat { left, right, .. } | ClauseGuard::GtInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec!(left, " > ", right)
            }

            ClauseGuard::GtEqFloat { left, right, .. }
            | ClauseGuard::GtEqInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec!(left, " >= ", right)
            }

            ClauseGuard::LtFloat { left, right, .. } | ClauseGuard::LtInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec!(left, " < ", right)
            }

            ClauseGuard::LtEqFloat { left, right, .. }
            | ClauseGuard::LtEqInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec!(left, " <= ", right)
            }

            ClauseGuard::Or { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec!(left, " || ", right)
            }

            ClauseGuard::And { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec!(left, " && ", right)
            }

            ClauseGuard::Var { name, .. } => self
                .path_doc_from_assignments(name)
                .unwrap_or_else(|| self.local_var(name)),

            ClauseGuard::TupleIndex { tuple, index, .. } => {
                docvec!(self.guard(tuple)?, "[", index, "]")
            }

            ClauseGuard::Constant(constant) => return constant_expression(constant),
        })
    }

    /// Get the path that would assign a variable, if there is one for the given name.
    /// This is in used in clause guards where may use variables defined in
    /// patterns can be referenced, but in the compiled JavaScript they have not
    /// yet been defined.
    fn path_doc_from_assignments(&self, name: &str) -> Option<Document<'a>> {
        self.assignments
            .iter()
            .find(|assignment| assignment.name == name)
            .map(|assignment| self.subject.clone().append(assignment.path.clone()))
    }

    fn traverse_pattern(&mut self, pattern: &'a TypedPattern) -> Result<(), Error> {
        match pattern {
            Pattern::String { value, .. } => {
                self.push_equality_check(expression::string(value));
                Ok(())
            }
            Pattern::Int { value, .. } => {
                self.push_equality_check(expression::int(value));
                Ok(())
            }
            Pattern::Float { value, .. } => {
                self.push_equality_check(expression::float(value));
                Ok(())
            }

            Pattern::Discard { .. } => Ok(()),

            Pattern::Var { name, .. } => {
                self.push_assignment(name);
                Ok(())
            }

            Pattern::Assign { name, pattern, .. } => {
                self.push_assignment(name);
                self.traverse_pattern(pattern)
            }

            Pattern::List { elements, tail, .. } => {
                self.push_list_length_check(elements.len(), tail.is_some());
                for pattern in elements.iter() {
                    self.push_int(0);
                    self.traverse_pattern(pattern)?;
                    self.pop();
                    self.push_int(1);
                }
                self.pop_times(elements.len());
                if let Some(pattern) = tail {
                    self.push_int_times(1, elements.len());
                    self.traverse_pattern(pattern)?;
                    self.pop_times(elements.len());
                }
                Ok(())
            }

            Pattern::Tuple { elems, .. } => {
                // We don't check the length because type system ensures it's a tuple
                for (index, pattern) in elems.iter().enumerate() {
                    self.push_int(index);
                    self.traverse_pattern(pattern)?;
                    self.pop();
                }
                Ok(())
            }

            Pattern::Constructor {
                type_,
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if type_.is_bool() && name == "True" => {
                self.push_booly_check(true);
                Ok(())
            }

            Pattern::Constructor {
                type_,
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if type_.is_bool() && name == "False" => {
                self.push_booly_check(false);
                Ok(())
            }

            Pattern::Constructor {
                type_,
                constructor: PatternConstructor::Record { .. },
                ..
            } if type_.is_nil() => {
                self.push_booly_check(false);
                Ok(())
            }

            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, field_map },
                arguments,
                ..
            } => {
                self.push_string("type");
                self.push_equality_check(expression::string(name));
                self.pop();

                for (index, arg) in arguments.iter().enumerate() {
                    match field_map {
                        None => self.push_int(index),
                        Some(FieldMap { fields, .. }) => {
                            let find = |(key, &val)| {
                                if val == index {
                                    Some(key)
                                } else {
                                    None
                                }
                            };
                            let label = fields.iter().find_map(find);
                            self.push_string(label.gleam_expect("argument present in field map"));
                        }
                    }
                    self.traverse_pattern(&arg.value)?;
                    self.pop();
                }
                Ok(())
            }

            Pattern::VarUsage { .. } | Pattern::BitString { .. } => {
                unsupported("Bit string matching")
            }
        }
    }

    fn push_assignment(&mut self, name: &'a str) {
        let var = self.next_local_var(name);
        let path = self.path_document();
        self.assignments.push(Assignment { path, var, name });
    }

    fn push_booly_check(&mut self, expected_to_be_truthy: bool) {
        self.checks.push(Check::Booly {
            expected_to_be_truthy,
            subject: self.subject.clone(),
            path: self.path_document(),
        })
    }

    fn push_equality_check(&mut self, to: Document<'a>) {
        self.checks.push(Check::Equal {
            to,
            subject: self.subject.clone(),
            path: self.path_document(),
        })
    }

    fn push_list_length_check(&mut self, expected_length: usize, has_tail_spread: bool) {
        self.checks.push(Check::ListLength {
            expected_length,
            has_tail_spread,
            subject: self.subject.clone(),
            path: self.path_document(),
        })
    }
}

#[derive(Debug)]
pub struct CompiledPattern<'a> {
    checks: Vec<Check<'a>>,
    assignments: Vec<Assignment<'a>>,
}

impl<'a> CompiledPattern<'a> {
    pub fn into_assignment_doc(self, subject: &Document<'a>) -> Document<'a> {
        if self.checks.is_empty() {
            return Self::assignments_doc(subject, self.assignments);
        }
        if self.assignments.is_empty() {
            return Self::checks_or_throw_doc(self.checks);
        }

        docvec![
            Self::checks_or_throw_doc(self.checks),
            line(),
            Self::assignments_doc(subject, self.assignments)
        ]
    }

    pub fn has_assignments(&self) -> bool {
        !self.assignments.is_empty()
    }

    pub fn has_checks(&self) -> bool {
        !self.checks.is_empty()
    }

    pub fn take_assignments_doc(&mut self, subject: &Document<'a>) -> Document<'a> {
        let assignments = std::mem::take(&mut self.assignments);
        Self::assignments_doc(subject, assignments)
    }

    pub fn assignments_doc(
        subject: &Document<'a>,
        assignments: Vec<Assignment<'a>>,
    ) -> Document<'a> {
        let assignments = assignments.into_iter().map(|Assignment { var, path, .. }| {
            docvec!["let ", var, " = ", subject.clone(), path, ";"]
        });
        concat(Itertools::intersperse(assignments, line()))
    }

    pub fn take_checks_doc(&mut self, match_desired: bool) -> Document<'a> {
        let checks = std::mem::take(&mut self.checks);
        Self::checks_doc(checks, match_desired)
    }

    pub fn checks_doc(checks: Vec<Check<'a>>, match_desired: bool) -> Document<'a> {
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

    pub fn checks_or_throw_doc(checks: Vec<Check<'a>>) -> Document<'a> {
        let checks = Self::checks_doc(checks, false);
        docvec![
            "if (",
            docvec![break_("", ""), checks].nest(INDENT),
            break_("", ""),
            ") throw new Error(\"Bad match\");",
        ]
        .group()
    }
}

#[derive(Debug)]
pub struct Assignment<'a> {
    name: &'a str,
    var: Document<'a>,
    path: Document<'a>,
}

#[derive(Debug)]
pub enum Check<'a> {
    Equal {
        subject: Document<'a>,
        path: Document<'a>,
        to: Document<'a>,
    },
    ListLength {
        subject: Document<'a>,
        path: Document<'a>,
        expected_length: usize,
        has_tail_spread: bool,
    },
    Booly {
        subject: Document<'a>,
        path: Document<'a>,
        expected_to_be_truthy: bool,
    },
    Guard {
        expression: Document<'a>,
    },
}

impl<'a> Check<'a> {
    pub fn into_doc(self, match_desired: bool) -> Document<'a> {
        match self {
            Check::Guard { expression } => {
                if match_desired {
                    expression
                } else {
                    docvec!["!", expression]
                }
            }

            Check::Booly {
                expected_to_be_truthy,
                subject,
                path,
            } => {
                if expected_to_be_truthy == match_desired {
                    docvec![subject, path]
                } else {
                    docvec!["!", subject, path]
                }
            }

            Check::Equal { subject, path, to } => {
                let operator = if match_desired { " === " } else { " !== " };
                docvec![subject, path, operator, to]
            }

            Check::ListLength {
                subject,
                path,
                expected_length,
                has_tail_spread,
            } => {
                let length_check = if has_tail_spread && match_desired {
                    "?.length !== undefined".to_doc()
                } else if has_tail_spread {
                    "?.length === undefined".to_doc()
                } else if match_desired {
                    "?.length === 0".to_doc()
                } else {
                    "?.length !== 0".to_doc()
                };
                docvec![
                    subject,
                    path,
                    Document::String("?.[1]".repeat(expected_length)),
                    length_check,
                ]
            }
        }
    }
}
