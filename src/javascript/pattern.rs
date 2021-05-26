use super::*;
use crate::type_::{FieldMap, PatternConstructor};

static ASSIGNMENT_VAR: &str = "$";

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
    assignments: Vec<Document<'a>>,
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
            Some(name) => (expression_generator.local_var_name(name), None),
            None => {
                let subject = expression_generator.next_local_var_name(ASSIGNMENT_VAR);
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

    fn next_local_var_name(&mut self, name: &'a str) -> Document<'a> {
        self.expression_generator.next_local_var_name(name)
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

    pub fn generate(&mut self, pattern: &'a TypedPattern) -> Result<CompiledPattern<'a>, Error> {
        let _ = self.traverse_pattern(pattern)?;

        Ok(CompiledPattern {
            checks: std::mem::take(&mut self.checks),
            assignments: std::mem::take(&mut self.assignments),
        })
    }

    fn traverse_pattern(&mut self, pattern: &'a TypedPattern) -> Result<(), Error> {
        match pattern {
            Pattern::String { value, .. } => {
                self.equality_check(expression::string(value));
                Ok(())
            }
            Pattern::Int { value, .. } => {
                self.equality_check(expression::int(value));
                Ok(())
            }
            Pattern::Float { value, .. } => {
                self.equality_check(expression::float(value));
                Ok(())
            }

            Pattern::Var { name, .. } => {
                let assignment = docvec![
                    "let ",
                    self.next_local_var_name(name),
                    " = ",
                    self.subject.clone(),
                    self.path_document(),
                    ";",
                ];
                self.assignments.push(assignment);
                Ok(())
            }

            Pattern::Discard { .. } => Ok(()),
            Pattern::Assign { name, pattern, .. } => {
                let assignments = docvec![
                    "let ",
                    self.next_local_var_name(name),
                    " = ",
                    self.subject.clone(),
                    self.path_document(),
                    ";",
                ];
                self.assignments.push(assignments);
                self.traverse_pattern(pattern)
            }

            Pattern::List { elements, tail, .. } => {
                self.list_length_check(elements.len(), tail.is_some());
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
                // We don't check the length, because type system means it's a tuple
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
                self.booly_check(true);
                Ok(())
            }

            Pattern::Constructor {
                type_,
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if type_.is_bool() && name == "False" => {
                self.booly_check(false);
                Ok(())
            }

            Pattern::Constructor {
                type_,
                constructor: PatternConstructor::Record { .. },
                ..
            } if type_.is_nil() => {
                self.booly_check(false);
                Ok(())
            }

            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, field_map },
                arguments,
                ..
            } => {
                self.push_string("type");
                self.equality_check(expression::string(name));
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
                unsupported("BitString matching not supported in JS backend")
            }
        }
    }

    fn booly_check(&mut self, expected_to_be_truthy: bool) {
        self.push_check(CheckKind::Booly {
            expected_to_be_truthy,
        })
    }

    fn equality_check(&mut self, to: Document<'a>) {
        self.push_check(CheckKind::Equal { to })
    }

    fn list_length_check(&mut self, expected_length: usize, has_tail_spread: bool) {
        self.push_check(CheckKind::ListLength {
            expected_length,
            has_tail_spread,
        })
    }

    fn push_check(&mut self, kind: CheckKind<'a>) {
        self.checks.push(Check {
            subject: self.subject.clone(),
            path: self.path_document(),
            kind,
        })
    }
}

#[derive(Debug)]
pub struct CompiledPattern<'a> {
    checks: Vec<Check<'a>>,
    assignments: Vec<Document<'a>>,
}

impl<'a> CompiledPattern<'a> {
    pub fn into_assignment_doc(self) -> Document<'a> {
        if self.checks.is_empty() {
            return Self::assignments_doc(self.assignments);
        }
        if self.assignments.is_empty() {
            return Self::checks_or_throw_doc(self.checks);
        }

        docvec![
            Self::checks_or_throw_doc(self.checks),
            line(),
            Self::assignments_doc(self.assignments)
        ]
    }

    pub fn has_assignments(&self) -> bool {
        !self.assignments.is_empty()
    }

    pub fn has_checks(&self) -> bool {
        !self.checks.is_empty()
    }

    pub fn take_assignments_doc(&mut self) -> Document<'a> {
        let assignments = std::mem::take(&mut self.assignments);
        Self::assignments_doc(assignments)
    }

    pub fn assignments_doc(assignments: Vec<Document<'a>>) -> Document<'a> {
        concat(Itertools::intersperse(assignments.into_iter(), line()))
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
pub struct Check<'a> {
    subject: Document<'a>,
    path: Document<'a>,
    kind: CheckKind<'a>,
}

impl<'a> Check<'a> {
    pub fn into_doc(self, match_desired: bool) -> Document<'a> {
        match self.kind {
            CheckKind::Booly {
                expected_to_be_truthy,
            } => {
                if expected_to_be_truthy == match_desired {
                    docvec![self.subject, self.path]
                } else {
                    docvec!["!", self.subject, self.path]
                }
            }

            CheckKind::Equal { to } => {
                let operator = if match_desired { " === " } else { " !== " };
                docvec![self.subject, self.path, operator, to]
            }

            CheckKind::ListLength {
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
                    self.subject,
                    self.path,
                    Document::String("?.[1]".repeat(expected_length)),
                    length_check,
                ]
            }
        }
    }
}

#[derive(Debug)]
enum CheckKind<'a> {
    Equal {
        to: Document<'a>,
    },
    ListLength {
        expected_length: usize,
        has_tail_spread: bool,
    },
    Booly {
        expected_to_be_truthy: bool,
    },
}
