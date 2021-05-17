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
    expression_generator: &'expression mut expression::Generator<'module>,
    path: Vec<Index<'a>>,
    subject: Document<'a>,
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
            expression_generator,
            subject: subject.clone(),
            path: vec![],
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
        // match pattern {
        //     // We can do a regular assignment if it is an assignment to just a pattern
        //     Pattern::Var { name, .. } => Ok(docvec![
        //         "let ",
        //         self.next_local_var_name(name),
        //         " = ",
        //         self.subject,
        //         ";"
        //     ]),
        //     _ => {
        let mut checks = vec![];
        let mut assignments = vec![];

        let () =
            self.traverse_pattern(pattern, self.subject.clone(), &mut checks, &mut assignments)?;

        Ok(CompiledPattern {
            checks,
            assignments,
        })
        // }
        // }
    }

    fn traverse_pattern(
        &mut self,
        pattern: &'a TypedPattern,
        value_name: Document<'a>,
        checks: &mut Vec<Document<'a>>,
        assignments: &mut Vec<Document<'a>>,
    ) -> Result<(), Error> {
        match pattern {
            Pattern::String { value, .. } => {
                checks.push(self.equality(value_name, expression::string(value)));
                Ok(())
            }
            Pattern::Int { value, .. } => {
                checks.push(self.equality(value_name, expression::int(value)));
                Ok(())
            }
            Pattern::Float { value, .. } => {
                checks.push(self.equality(value_name, expression::float(value)));
                Ok(())
            }

            Pattern::Var { name, .. } => {
                assignments.push(docvec![
                    "let ",
                    self.next_local_var_name(name),
                    " = ",
                    value_name,
                    self.path_document(),
                    ";",
                ]);
                Ok(())
            }

            Pattern::Discard { .. } => Ok(()),
            Pattern::Assign { name, pattern, .. } => {
                assignments.push(docvec![
                    "let ",
                    self.next_local_var_name(name),
                    " = ",
                    value_name.clone(),
                    self.path_document(),
                    ";",
                ]);
                self.traverse_pattern(pattern, value_name, checks, assignments)
            }

            Pattern::List { elements, tail, .. } => {
                let path_string = self.path_document();
                let length_check = match tail {
                    Some(_) => "?.length === undefined".to_doc(),
                    None => "?.length !== 0".to_doc(),
                };
                checks.push(docvec![
                    value_name.clone(),
                    path_string,
                    Document::String("?.[1]".repeat(elements.len())),
                    length_check,
                ]);
                for pattern in elements.iter() {
                    self.push_int(0);
                    self.traverse_pattern(pattern, value_name.clone(), checks, assignments)?;
                    self.pop();
                    self.push_int(1);
                }
                self.pop_times(elements.len());
                if let Some(pattern) = tail {
                    self.push_int_times(1, elements.len());
                    self.traverse_pattern(pattern, value_name, checks, assignments)?;
                    self.pop_times(elements.len());
                }
                Ok(())
            }

            Pattern::Tuple { elems, .. } => {
                // We don't check the length, because type system means it's a tuple
                for (index, pattern) in elems.iter().enumerate() {
                    self.push_int(index);
                    self.traverse_pattern(pattern, value_name.clone(), checks, assignments)?;
                    self.pop();
                }
                Ok(())
            }

            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if name == "True" => {
                checks.push(self.equality(value_name, "true".to_doc()));
                Ok(())
            }

            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if name == "False" => {
                checks.push(self.equality(value_name, "false".to_doc()));
                Ok(())
            }

            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if name == "Nil" => {
                checks.push(self.equality(value_name, "undefined".to_doc()));
                Ok(())
            }

            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, field_map },
                arguments,
                ..
            } => {
                self.push_string("type");
                checks.push(self.equality(value_name.clone(), expression::string(name)));
                self.pop();

                for (index, arg) in arguments.iter().enumerate() {
                    match field_map {
                        None => self.push_int(index),
                        Some(FieldMap { fields, .. }) => {
                            let label =
                                fields.iter().find_map(
                                    |(key, &val)| {
                                        if val == index {
                                            Some(key)
                                        } else {
                                            None
                                        }
                                    },
                                );
                            self.push_string(label.gleam_expect("argument present in field map"));
                        }
                    }
                    self.traverse_pattern(&arg.value, value_name.clone(), checks, assignments)?;
                    self.pop_times(arguments.len());
                }
                Ok(())
            }

            Pattern::VarUsage { .. } | Pattern::BitString { .. } => {
                unsupported("BitString matching not supported in JS backend")
            }
        }
    }

    fn equality(&mut self, value_name: Document<'a>, to_match: Document<'a>) -> Document<'a> {
        docvec![value_name, self.path_document(), " !== ", to_match]
    }
}

#[derive(Debug)]
pub struct CompiledPattern<'a> {
    checks: Vec<Document<'a>>,
    assignments: Vec<Document<'a>>,
}

impl<'a> CompiledPattern<'a> {
    pub fn into_doc(self) -> Document<'a> {
        if self.checks.is_empty() {
            return Self::assignments_doc(self.assignments);
        }
        if self.assignments.is_empty() {
            return Self::checks_doc(self.checks);
        }

        docvec![
            Self::checks_doc(self.checks),
            line(),
            Self::assignments_doc(self.assignments)
        ]
    }

    pub fn assignments_doc(assignments: Vec<Document<'a>>) -> Document<'a> {
        concat(Itertools::intersperse(assignments.into_iter(), line()))
    }

    pub fn checks_doc(checks: Vec<Document<'a>>) -> Document<'a> {
        let checks = concat(Itertools::intersperse(
            checks.into_iter(),
            break_(" ||", " || "),
        ));
        docvec![
            "if (",
            docvec![break_("", ""), checks].nest(INDENT),
            break_("", ""),
            ") throw new Error(\"Bad match\");",
        ]
        .group()
    }
}
