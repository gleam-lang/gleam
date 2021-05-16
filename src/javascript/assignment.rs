use crate::type_::{FieldMap, PatternConstructor};

use super::*;

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
}

impl<'module, 'expression, 'a> Generator<'module, 'expression, 'a> {
    pub fn new(expression_generator: &'expression mut expression::Generator<'module>) -> Self {
        Self {
            expression_generator,
            path: vec![],
        }
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

    pub fn generate(&mut self, value: &'a TypedExpr, pattern: &'a TypedPattern) -> Output<'a> {
        // Render the value before printing patterns as patterns might update local variable state.
        let value = self
            .expression_generator
            .not_in_tail_position(|gen| gen.wrap_expression(value))?;
        match pattern {
            // Return early don't use gleam$temp if single assignment
            Pattern::Var { name, .. } => Ok(docvec![
                "let ",
                self.next_local_var_name(name),
                " = ",
                value,
                ";"
            ]),
            _ => {
                let mut checks = vec![];
                let mut assignments = vec![];

                let tmp_var = self.next_local_var_name(ASSIGNMENT_VAR);

                let () =
                    self.traverse_pattern(pattern, tmp_var.clone(), &mut checks, &mut assignments)?;

                let check_line = match checks.is_empty() {
                    true => "".to_doc(),
                    false => docvec![
                        "if (",
                        docvec![
                            break_("", ""),
                            Itertools::intersperse(checks.into_iter(), break_(" ||", " || "))
                                .collect::<Vec<_>>()
                                .to_doc(),
                        ]
                        .nest(INDENT),
                        break_("", ""),
                        ") throw new Error(\"Bad match\");",
                    ]
                    .group(),
                };

                let assign_line = docvec!["let ", tmp_var, " = ", value, ";"];
                use std::iter::once;
                let lines = once(assign_line).chain(once(check_line)).chain(assignments);
                // let expressions generate multiple lines of JS.
                // Add a clear line after the generated JS unless it is tail position.
                Ok(concat(Itertools::intersperse(lines, line())).append(
                    if self.expression_generator.tail_position {
                        "".to_doc()
                    } else {
                        line()
                    },
                ))
            }
        }
    }

    fn traverse_pattern(
        &mut self,
        pattern: &'a TypedPattern,
        tmp_var: Document<'a>,
        checks: &mut Vec<Document<'a>>,
        assignments: &mut Vec<Document<'a>>,
    ) -> Result<(), Error> {
        match pattern {
            Pattern::String { value, .. } => {
                checks.push(self.equality(tmp_var, expression::string(value)));
                Ok(())
            }
            Pattern::Int { value, .. } => {
                checks.push(self.equality(tmp_var, expression::int(value)));
                Ok(())
            }
            Pattern::Float { value, .. } => {
                checks.push(self.equality(tmp_var, expression::float(value)));
                Ok(())
            }

            Pattern::Var { name, .. } => {
                assignments.push(docvec![
                    "let ",
                    self.next_local_var_name(name),
                    " = ",
                    tmp_var,
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
                    tmp_var.clone(),
                    self.path_document(),
                    ";",
                ]);
                self.traverse_pattern(pattern, tmp_var, checks, assignments)
            }

            Pattern::List { elements, tail, .. } => {
                let path_string = self.path_document();
                let length_check = match tail {
                    Some(_) => "?.length === undefined".to_doc(),
                    None => "?.length !== 0".to_doc(),
                };
                checks.push(docvec![
                    tmp_var.clone(),
                    path_string,
                    Document::String("?.[1]".repeat(elements.len())),
                    length_check,
                ]);
                for pattern in elements.iter() {
                    self.push_int(0);
                    self.traverse_pattern(pattern, tmp_var.clone(), checks, assignments)?;
                    self.pop();
                    self.push_int(1);
                }
                self.pop_times(elements.len());
                if let Some(pattern) = tail {
                    self.push_int_times(1, elements.len());
                    self.traverse_pattern(pattern, tmp_var, checks, assignments)?;
                    self.pop_times(elements.len());
                }
                Ok(())
            }

            Pattern::Tuple { elems, .. } => {
                // We don't check the length, because type system means it's a tuple
                for (index, pattern) in elems.iter().enumerate() {
                    self.push_int(index);
                    self.traverse_pattern(pattern, tmp_var.clone(), checks, assignments)?;
                    self.pop();
                }
                Ok(())
            }

            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if name == "True" => {
                checks.push(self.equality(tmp_var, "true".to_doc()));
                Ok(())
            }

            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if name == "False" => {
                checks.push(self.equality(tmp_var, "false".to_doc()));
                Ok(())
            }

            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if name == "Nil" => {
                checks.push(self.equality(tmp_var, "undefined".to_doc()));
                Ok(())
            }

            Pattern::Constructor {
                constructor: PatternConstructor::Record { name, field_map },
                arguments,
                ..
            } => {
                self.push_string("type");
                checks.push(self.equality(tmp_var.clone(), expression::string(name)));
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
                    self.traverse_pattern(&arg.value, tmp_var.clone(), checks, assignments)?;
                    self.pop_times(arguments.len());
                }
                Ok(())
            }

            Pattern::VarUsage { .. } | Pattern::BitString { .. } => {
                unsupported("BitString matching not supported in JS backend")
            }
        }
    }

    fn equality(&mut self, tmp_var: Document<'a>, to_match: Document<'a>) -> Document<'a> {
        docvec![tmp_var, self.path_document(), " !== ", to_match]
    }
}
