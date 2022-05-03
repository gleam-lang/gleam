use super::{expression::is_js_scalar, *};
use crate::type_::{FieldMap, PatternConstructor};

pub static ASSIGNMENT_VAR: &str = "$";

#[derive(Debug)]
enum Index<'a> {
    Int(usize),
    String(&'a str),
    ByteAt(usize),
    IntFromSlice(usize, usize),
    FloatAt(usize),
    SliceAfter(usize),
}

#[derive(Debug)]
pub struct Subjects<'a> {
    pub values: Vec<Document<'a>>,
    pub assignments: Vec<(Document<'a>, Document<'a>)>,
}

#[derive(Debug)]
pub(crate) struct Generator<'module_ctx, 'expression_gen, 'a> {
    pub expression_generator: &'expression_gen mut expression::Generator<'module_ctx>,
    path: Vec<Index<'a>>,
    checks: Vec<Check<'a>>,
    assignments: Vec<Assignment<'a>>,
}

struct Offset {
    bytes: usize,
    open_ended: bool,
}

impl Offset {
    pub fn new() -> Self {
        Self {
            bytes: 0,
            open_ended: false,
        }
    }
    // This should never be called on an open ended offset
    // However previous checks ensure bit_string segements without a size are only allowed at the end of a pattern
    pub fn increment(&mut self, step: usize) {
        self.bytes += step
    }
    pub fn set_open_ended(&mut self) {
        self.open_ended = true
    }
}

impl<'module_ctx, 'expression_gen, 'a> Generator<'module_ctx, 'expression_gen, 'a> {
    pub fn new(
        expression_generator: &'expression_gen mut expression::Generator<'module_ctx>,
    ) -> Self {
        Self {
            path: vec![],
            checks: vec![],
            assignments: vec![],
            expression_generator,
        }
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

    fn push_byte_at(&mut self, i: usize) {
        self.path.push(Index::ByteAt(i));
    }

    fn push_int_from_slice(&mut self, start: usize, end: usize) {
        self.path.push(Index::IntFromSlice(start, end));
    }

    fn push_float_at(&mut self, i: usize) {
        self.path.push(Index::FloatAt(i));
    }

    fn push_rest_from(&mut self, i: usize) {
        self.path.push(Index::SliceAfter(i));
    }

    fn push_string_times(&mut self, s: &'a str, times: usize) {
        for _ in 0..times {
            self.push_string(s);
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
            // TODO: escape string if needed
            Index::String(s) => docvec!(".", s),
            Index::ByteAt(i) => docvec!(".byteAt(", i, ")"),
            Index::IntFromSlice(start, end) => docvec!(".intFromSlice(", start, ", ", end, ")"),
            Index::FloatAt(i) => docvec!(".floatAt(", i, ")"),
            Index::SliceAfter(i) => docvec!(".sliceAfter(", i, ")"),
        }))
    }

    pub fn generate(
        &mut self,
        subjects: &[Document<'a>],
        patterns: &'a [TypedPattern],
        guard: Option<&'a TypedClauseGuard>,
    ) -> Result<CompiledPattern<'a>, Error> {
        for (subject, pattern) in subjects.iter().zip_eq(patterns) {
            let _ = self.traverse_pattern(subject, pattern)?;
        }
        if let Some(guard) = guard {
            let _ = self.push_guard_check(guard)?;
        }

        Ok(self.take_compiled())
    }

    pub fn take_compiled(&mut self) -> CompiledPattern<'a> {
        CompiledPattern {
            checks: std::mem::take(&mut self.checks),
            assignments: std::mem::take(&mut self.assignments),
        }
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
                    .prelude_equal_call(true, left, right)
            }

            ClauseGuard::NotEquals { left, right, .. } => {
                let left = self.guard(left)?;
                let right = self.guard(right)?;
                self.expression_generator
                    .prelude_equal_call(false, left, right)
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

            ClauseGuard::Constant(constant) => {
                return expression::constant_expression(self.expression_generator.tracker, constant)
            }
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
            .map(|assignment| assignment.subject.clone().append(assignment.path.clone()))
    }

    pub fn traverse_pattern(
        &mut self,
        subject: &Document<'a>,
        pattern: &'a TypedPattern,
    ) -> Result<(), Error> {
        match pattern {
            Pattern::String { value, .. } => {
                self.push_equality_check(subject.clone(), expression::string(value));
                Ok(())
            }
            Pattern::Int { value, .. } => {
                self.push_equality_check(subject.clone(), expression::int(value));
                Ok(())
            }
            Pattern::Float { value, .. } => {
                self.push_equality_check(subject.clone(), expression::float(value));
                Ok(())
            }

            Pattern::Discard { .. } => Ok(()),

            Pattern::Var { name, .. } => {
                self.push_assignment(subject.clone(), name);
                Ok(())
            }

            Pattern::Assign { name, pattern, .. } => {
                self.push_assignment(subject.clone(), name);
                self.traverse_pattern(subject, pattern)
            }

            Pattern::List { elements, tail, .. } => {
                self.push_list_length_check(subject.clone(), elements.len(), tail.is_some());
                for pattern in elements {
                    self.push_string("head");
                    self.traverse_pattern(subject, pattern)?;
                    self.pop();
                    self.push_string("tail");
                }
                self.pop_times(elements.len());
                if let Some(pattern) = tail {
                    self.push_string_times("tail", elements.len());
                    self.traverse_pattern(subject, pattern)?;
                    self.pop_times(elements.len());
                }
                Ok(())
            }

            Pattern::Tuple { elems, .. } => {
                // We don't check the length because type system ensures it's a
                // tuple of the correct size
                for (index, pattern) in elems.iter().enumerate() {
                    self.push_int(index);
                    self.traverse_pattern(subject, pattern)?;
                    self.pop();
                }
                Ok(())
            }

            Pattern::Constructor {
                type_,
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if type_.is_bool() && name == "True" => {
                self.push_booly_check(subject.clone(), true);
                Ok(())
            }

            Pattern::Constructor {
                type_,
                constructor: PatternConstructor::Record { name, .. },
                ..
            } if type_.is_bool() && name == "False" => {
                self.push_booly_check(subject.clone(), false);
                Ok(())
            }

            Pattern::Constructor {
                type_,
                constructor: PatternConstructor::Record { .. },
                ..
            } if type_.is_nil() => {
                self.push_booly_check(subject.clone(), false);
                Ok(())
            }

            Pattern::Constructor {
                constructor:
                    PatternConstructor::Record {
                        field_map,
                        name: record_name,
                        ..
                    },
                arguments,
                name,
                type_,
                module,
                ..
            } => {
                match module {
                    _ if type_.is_result_constructor() => {
                        self.push_result_check(subject.clone(), record_name == "Ok")
                    }
                    Some(m) => self.push_variant_check(subject.clone(), docvec!["$", m, ".", name]),
                    None => self.push_variant_check(subject.clone(), name.to_doc()),
                }

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
                            self.push_string(label.expect("argument present in field map"));
                        }
                    }
                    self.traverse_pattern(subject, &arg.value)?;
                    self.pop();
                }
                Ok(())
            }

            Pattern::BitString { segments, .. } => {
                use BitStringSegmentOption as Opt;

                let mut offset = Offset::new();
                for segment in segments {
                    let _ = match segment.options.as_slice() {
                        [] | [Opt::Int { .. }] => {
                            self.push_byte_at(offset.bytes);
                            self.traverse_pattern(subject, &segment.value)?;
                            self.pop();
                            offset.increment(1);
                            Ok(())
                        }

                        [Opt::Size { value: size, .. }] => match &**size {
                            Pattern::Int { value, .. } => {
                                let start = offset.bytes;
                                let increment = value
                                    .parse::<usize>()
                                    .expect("part of an Int node should always parse as integer");
                                offset.increment(increment / 8);
                                let end = offset.bytes;

                                self.push_int_from_slice(start, end);
                                self.traverse_pattern(subject, &segment.value)?;
                                self.pop();
                                Ok(())
                            }
                            _ => Err(Error::Unsupported {
                                feature: "This bit string size option in patterns".to_string(),
                                location: segment.location,
                            }),
                        },

                        [Opt::Float { .. }] => {
                            self.push_float_at(offset.bytes);
                            self.traverse_pattern(subject, &segment.value)?;
                            self.pop();
                            offset.increment(8);
                            Ok(())
                        }

                        [Opt::Binary { .. }] => {
                            self.push_rest_from(offset.bytes);
                            self.traverse_pattern(subject, &segment.value)?;
                            self.pop();
                            offset.set_open_ended();
                            Ok(())
                        }

                        _ => Err(Error::Unsupported {
                            feature: "This bit string segment option in patterns".to_string(),
                            location: segment.location,
                        }),
                    }?;
                }

                self.push_bitstring_length_check(subject.clone(), offset.bytes, offset.open_ended);
                Ok(())
            }
            Pattern::VarUsage { location, .. } => Err(Error::Unsupported {
                feature: "Bit string matching".to_string(),
                location: *location,
            }),
        }
    }

    fn push_assignment(&mut self, subject: Document<'a>, name: &'a str) {
        let var = self.next_local_var(name);
        let path = self.path_document();
        self.assignments.push(Assignment {
            subject,
            path,
            var,
            name,
        });
    }

    fn push_booly_check(&mut self, subject: Document<'a>, expected_to_be_truthy: bool) {
        self.checks.push(Check::Booly {
            expected_to_be_truthy,
            subject,
            path: self.path_document(),
        })
    }

    fn push_equality_check(&mut self, subject: Document<'a>, to: Document<'a>) {
        self.checks.push(Check::Equal {
            to,
            subject,
            path: self.path_document(),
        })
    }

    fn push_variant_check(&mut self, subject: Document<'a>, kind: Document<'a>) {
        self.checks.push(Check::Variant {
            kind,
            subject,
            path: self.path_document(),
        })
    }

    fn push_result_check(&mut self, subject: Document<'a>, is_ok: bool) {
        self.checks.push(Check::Result {
            is_ok,
            subject,
            path: self.path_document(),
        })
    }

    fn push_list_length_check(
        &mut self,
        subject: Document<'a>,
        expected_length: usize,
        has_tail_spread: bool,
    ) {
        self.checks.push(Check::ListLength {
            expected_length,
            has_tail_spread,
            subject,
            path: self.path_document(),
        })
    }

    fn push_bitstring_length_check(
        &mut self,
        subject: Document<'a>,
        expected_bytes: usize,
        has_tail_spread: bool,
    ) {
        self.checks.push(Check::BitStringLength {
            expected_bytes,
            has_tail_spread,
            subject,
            path: self.path_document(),
        })
    }
}

#[derive(Debug)]
pub struct CompiledPattern<'a> {
    pub checks: Vec<Check<'a>>,
    pub assignments: Vec<Assignment<'a>>,
}

impl<'a> CompiledPattern<'a> {
    pub fn has_assignments(&self) -> bool {
        !self.assignments.is_empty()
    }

    pub fn has_checks(&self) -> bool {
        !self.checks.is_empty()
    }
}

#[derive(Debug)]
pub struct Assignment<'a> {
    name: &'a str,
    var: Document<'a>,
    subject: Document<'a>,
    path: Document<'a>,
}

impl<'a> Assignment<'a> {
    pub fn into_doc(self) -> Document<'a> {
        docvec!["let ", self.var, " = ", self.subject, self.path, ";"]
    }
}

#[derive(Debug)]
pub enum Check<'a> {
    Result {
        subject: Document<'a>,
        path: Document<'a>,
        is_ok: bool,
    },
    Variant {
        subject: Document<'a>,
        path: Document<'a>,
        kind: Document<'a>,
    },
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
    BitStringLength {
        subject: Document<'a>,
        path: Document<'a>,
        expected_bytes: usize,
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

            Check::Variant {
                subject,
                path,
                kind,
            } => {
                if match_desired {
                    docvec![subject, path, " instanceof ", kind]
                } else {
                    docvec!["!(", subject, path, " instanceof ", kind, ")"]
                }
            }

            Check::Result {
                subject,
                path,
                is_ok,
            } => {
                if match_desired == is_ok {
                    docvec![subject, path, ".isOk()"]
                } else {
                    docvec!["!", subject, path, ".isOk()"]
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
                let length_check = Document::String(if has_tail_spread {
                    format!(".atLeastLength({})", expected_length)
                } else {
                    format!(".hasLength({})", expected_length)
                });
                if match_desired {
                    docvec![subject, path, length_check,]
                } else {
                    docvec!["!", subject, path, length_check,]
                }
            }
            Check::BitStringLength {
                subject,
                path,
                expected_bytes,
                has_tail_spread,
            } => {
                let length_check = Document::String(if has_tail_spread {
                    format!(".length >= {}", expected_bytes)
                } else {
                    format!(".length == {}", expected_bytes)
                });
                if match_desired {
                    docvec![subject, path, length_check,]
                } else {
                    docvec!["!(", subject, path, length_check, ")",]
                }
            }
        }
    }
}

pub(crate) fn assign_subject<'a>(
    expression_generator: &mut expression::Generator<'_>,
    subject: &'a TypedExpr,
) -> (Document<'a>, Option<Document<'a>>) {
    match subject {
        // If the value is a variable we don't need to assign it to a new
        // variable, we can the value expression safely without worrying about
        // performing computation or side effects multiple times.
        TypedExpr::Var {
            name, constructor, ..
        } if constructor.is_local_variable() => (expression_generator.local_var(name), None),
        // If it's not a variable we need to assign it to a variable
        // to avoid rendering the subject expression multiple times
        _ => {
            let subject = expression_generator.next_local_var(ASSIGNMENT_VAR);
            (subject.clone(), Some(subject))
        }
    }
}

pub(crate) fn assign_subjects<'a>(
    expression_generator: &mut expression::Generator<'_>,
    subjects: &'a [TypedExpr],
) -> Vec<(Document<'a>, Option<Document<'a>>)> {
    let mut out = Vec::with_capacity(subjects.len());
    for subject in subjects {
        out.push(assign_subject(expression_generator, subject))
    }
    out
}
