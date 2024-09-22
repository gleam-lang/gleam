use std::sync::OnceLock;

use super::{expression::is_js_scalar, *};
use crate::{
    analyse::Inferred,
    javascript::endianness::Endianness,
    strings::convert_string_escape_chars,
    type_::{FieldMap, PatternConstructor},
};

pub static ASSIGNMENT_VAR: &str = "$";

#[derive(Debug)]
enum Index<'a> {
    Int(usize),
    String(&'a str),
    ByteAt(usize),
    IntFromSlice {
        start: usize,
        end: usize,
        endianness: Endianness,
        is_signed: bool,
    },
    FloatFromSlice {
        start: usize,
        end: usize,
        endianness: Endianness,
    },
    BinaryFromSlice(usize, usize),
    SliceAfter(usize),
    StringPrefixSlice(usize),
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
    // However previous checks ensure bit_array segments without a size are only
    // allowed at the end of a pattern
    pub fn increment(&mut self, step: usize) {
        self.bytes += step
    }
    pub fn set_open_ended(&mut self) {
        self.open_ended = true
    }
}

#[derive(Debug)]
struct SizedBitArraySegmentDetails {
    size: usize,
    endianness: Endianness,
    is_signed: bool,
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

    fn next_local_var(&mut self, name: &'a EcoString) -> Document<'a> {
        self.expression_generator.next_local_var(name)
    }

    fn local_var(&mut self, name: &'a EcoString) -> Document<'a> {
        self.expression_generator.local_var(name)
    }

    fn push_string(&mut self, s: &'a str) {
        self.path.push(Index::String(s));
    }

    fn push_int(&mut self, i: usize) {
        self.path.push(Index::Int(i));
    }

    fn push_string_prefix_slice(&mut self, i: usize) {
        self.path.push(Index::StringPrefixSlice(i));
    }

    fn push_byte_at(&mut self, i: usize) {
        self.path.push(Index::ByteAt(i));
    }

    fn push_int_from_slice(
        &mut self,
        start: usize,
        end: usize,
        endianness: Endianness,
        is_signed: bool,
    ) {
        self.path.push(Index::IntFromSlice {
            start,
            end,
            endianness,
            is_signed,
        });
    }

    fn push_float_from_slice(&mut self, start: usize, end: usize, endianness: Endianness) {
        self.path.push(Index::FloatFromSlice {
            start,
            end,
            endianness,
        });
    }

    fn push_binary_from_slice(&mut self, start: usize, end: usize) {
        self.path.push(Index::BinaryFromSlice(start, end));
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
            Index::Int(i) => eco_format!("[{i}]").to_doc(),
            // TODO: escape string if needed
            Index::String(s) => docvec!(".", s),
            Index::ByteAt(i) => docvec!(".byteAt(", i, ")"),
            Index::IntFromSlice {
                start,
                end,
                endianness,
                is_signed,
            } => docvec!(
                ".intFromSlice(",
                start,
                ", ",
                end,
                ", ",
                bool(endianness.is_big()),
                ", ",
                bool(*is_signed),
                ")"
            ),
            Index::FloatFromSlice {
                start,
                end,
                endianness,
            } => docvec!(
                ".floatFromSlice(",
                start,
                ", ",
                end,
                ", ",
                bool(endianness.is_big()),
                ")"
            ),
            Index::BinaryFromSlice(start, end) => {
                docvec!(".binaryFromSlice(", start, ", ", end, ")")
            }
            Index::SliceAfter(i) => docvec!(".sliceAfter(", i, ")"),
            Index::StringPrefixSlice(i) => docvec!(".slice(", i, ")"),
        }))
    }

    pub fn generate(
        &mut self,
        subjects: &[Document<'a>],
        patterns: &'a [TypedPattern],
        guard: Option<&'a TypedClauseGuard>,
    ) -> Result<CompiledPattern<'a>, Error> {
        for (subject, pattern) in subjects.iter().zip_eq(patterns) {
            self.traverse_pattern(subject, pattern)?;
        }
        if let Some(guard) = guard {
            self.push_guard_check(guard)?;
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
            | ClauseGuard::ModuleSelect { .. } => Ok(docvec!("(", self.guard(guard)?, ")")),
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

            ClauseGuard::AddFloat { left, right, .. } | ClauseGuard::AddInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec!(left, " + ", right)
            }

            ClauseGuard::SubFloat { left, right, .. } | ClauseGuard::SubInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec!(left, " - ", right)
            }

            ClauseGuard::MultFloat { left, right, .. }
            | ClauseGuard::MultInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec!(left, " * ", right)
            }

            ClauseGuard::DivFloat { left, right, .. } | ClauseGuard::DivInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec!(left, " / ", right)
            }

            ClauseGuard::RemainderInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec!(left, " % ", right)
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

            ClauseGuard::FieldAccess {
                label, container, ..
            } => {
                docvec!(self.guard(container)?, ".", label)
            }

            ClauseGuard::ModuleSelect {
                module_alias,
                label,
                ..
            } => docvec!("$", module_alias, ".", label),

            ClauseGuard::Not { expression, .. } => {
                docvec!["!", self.guard(expression)?]
            }

            ClauseGuard::Constant(constant) => {
                return expression::guard_constant_expression(
                    &mut self.assignments,
                    self.expression_generator.tracker,
                    constant,
                )
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

            Pattern::Variable { name, .. } => {
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
                constructor: Inferred::Known(PatternConstructor { name, .. }),
                ..
            } if type_.is_bool() && name == "True" => {
                self.push_booly_check(subject.clone(), true);
                Ok(())
            }

            Pattern::Constructor {
                type_,
                constructor: Inferred::Known(PatternConstructor { name, .. }),
                ..
            } if type_.is_bool() && name == "False" => {
                self.push_booly_check(subject.clone(), false);
                Ok(())
            }

            Pattern::Constructor {
                type_,
                constructor: Inferred::Known(PatternConstructor { .. }),
                ..
            } if type_.is_nil() => {
                self.push_booly_check(subject.clone(), false);
                Ok(())
            }

            Pattern::Constructor {
                constructor: Inferred::Unknown,
                ..
            } => {
                panic!("JavaScript generation performed with uninferred pattern constructor");
            }

            Pattern::StringPrefix {
                left_side_string,
                right_side_assignment,
                left_side_assignment,
                ..
            } => {
                self.push_string_prefix_check(subject.clone(), left_side_string);
                if let AssignName::Variable(right) = right_side_assignment {
                    self.push_string_prefix_slice(utf16_no_escape_len(left_side_string));
                    self.push_assignment(subject.clone(), right);
                    // After pushing the assignment we need to pop the prefix slicing we used to
                    // check the condition.
                    self.pop();
                }
                if let Some((left, _)) = left_side_assignment {
                    // "wibble" as prefix <> rest
                    //          ^^^^^^^^^ In case the left prefix of the pattern matching is given an
                    //                    alias we bind it to a local variable so that it can be
                    //                    correctly referenced inside the case branch.
                    // let prefix = "wibble";
                    // ^^^^^^^^^^^^^^^^^^^^^ we're adding this assignment inside the if clause
                    //                       the case branch gets translated into.
                    self.push_assignment(expression::string(left_side_string), left);
                }
                Ok(())
            }

            Pattern::Constructor {
                constructor:
                    Inferred::Known(PatternConstructor {
                        field_map,
                        name: record_name,
                        ..
                    }),
                arguments,
                name,
                type_,
                module,
                ..
            } => {
                match module {
                    _ if type_.is_result() => {
                        self.push_result_check(subject.clone(), record_name == "Ok")
                    }
                    Some((m, _)) => {
                        self.push_variant_check(subject.clone(), docvec!["$", m, ".", name])
                    }
                    None => self.push_variant_check(subject.clone(), name.to_doc()),
                }

                for (index, arg) in arguments.iter().enumerate() {
                    match field_map {
                        None => self.push_int(index),
                        Some(FieldMap { fields, .. }) => {
                            let find = |(key, &val)| {
                                if val as usize == index {
                                    Some(key)
                                } else {
                                    None
                                }
                            };
                            let label = fields.iter().find_map(find);
                            match label {
                                Some(label) => self.push_string(label),
                                None => self.push_int(index),
                            }
                        }
                    }
                    self.traverse_pattern(subject, &arg.value)?;
                    self.pop();
                }
                Ok(())
            }

            Pattern::BitArray { segments, .. } => {
                use BitArrayOption as Opt;

                let mut offset = Offset::new();
                for segment in segments {
                    if segment.type_ == crate::type_::int()
                        || segment.type_ == crate::type_::float()
                    {
                        let details = Self::sized_bit_array_segment_details(segment)?;

                        let start = offset.bytes;
                        let increment = details.size / 8;
                        let end = offset.bytes + increment;

                        if segment.type_ == crate::type_::int() {
                            if details.size == 8 && !details.is_signed {
                                self.push_byte_at(offset.bytes);
                            } else {
                                self.push_int_from_slice(
                                    start,
                                    end,
                                    details.endianness,
                                    details.is_signed,
                                );
                            }
                        } else {
                            self.push_float_from_slice(start, end, details.endianness);
                        }

                        self.traverse_pattern(subject, &segment.value)?;
                        self.pop();
                        offset.increment(increment);
                    } else {
                        match segment.options.as_slice() {
                            [Opt::Bytes { .. }] => {
                                self.push_rest_from(offset.bytes);
                                self.traverse_pattern(subject, &segment.value)?;
                                self.pop();
                                offset.set_open_ended();
                                Ok(())
                            }

                            [Opt::Bytes { .. }, Opt::Size { value: size, .. }]
                            | [Opt::Size { value: size, .. }, Opt::Bytes { .. }] => match &**size {
                                Pattern::Int { value, .. } => {
                                    let start = offset.bytes;
                                    let increment = value.parse::<usize>().expect(
                                        "part of an Int node should always parse as integer",
                                    );
                                    offset.increment(increment);
                                    let end = offset.bytes;

                                    self.push_binary_from_slice(start, end);
                                    self.traverse_pattern(subject, &segment.value)?;
                                    self.pop();
                                    Ok(())
                                }

                                _ => Err(Error::Unsupported {
                                    feature: "This bit array size option in patterns".into(),
                                    location: segment.location,
                                }),
                            },

                            [Opt::Utf8 { .. }] => match segment.value.as_ref() {
                                Pattern::String { value, .. } => {
                                    for byte in value.as_bytes() {
                                        self.push_byte_at(offset.bytes);
                                        self.push_equality_check(
                                            subject.clone(),
                                            EcoString::from(format!("0x{byte:X}")).to_doc(),
                                        );
                                        self.pop();
                                        offset.increment(1);
                                    }

                                    Ok(())
                                }

                                _ => Err(Error::Unsupported {
                                    feature: "This bit array segment option in patterns".into(),
                                    location: segment.location,
                                }),
                            },

                            _ => Err(Error::Unsupported {
                                feature: "This bit array segment option in patterns".into(),
                                location: segment.location,
                            }),
                        }?;
                    }
                }

                self.push_bit_array_length_check(subject.clone(), offset.bytes, offset.open_ended);
                Ok(())
            }
            Pattern::VarUsage { location, .. } => Err(Error::Unsupported {
                feature: "Bit array matching".into(),
                location: *location,
            }),
            Pattern::Invalid { .. } => panic!("invalid patterns should not reach code generation"),
        }
    }

    fn sized_bit_array_segment_details(
        segment: &TypedPatternBitArraySegment,
    ) -> Result<SizedBitArraySegmentDetails, Error> {
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

        let endianness = if segment
            .options
            .iter()
            .any(|x| matches!(x, Opt::Little { .. }))
        {
            Endianness::Little
        } else {
            Endianness::Big
        };

        let size = match segment
            .options
            .iter()
            .find(|x| matches!(x, Opt::Size { .. }))
        {
            Some(Opt::Size { value: size, .. }) => match &**size {
                Pattern::Int { value, .. } => Ok(value
                    .parse::<usize>()
                    .expect("part of an Int node should always parse as integer")),
                _ => Err(Error::Unsupported {
                    feature: "Non-constant size option in patterns".into(),
                    location: segment.location,
                }),
            },

            _ => {
                let default_size = if segment.type_ == crate::type_::int() {
                    8usize
                } else {
                    64usize
                };

                Ok(default_size)
            }
        }?;

        // 16-bit floats are not supported
        if segment.type_ == crate::type_::float() && size == 16 {
            return Err(Error::Unsupported {
                feature: "Float width of 16 bits in patterns".into(),
                location: segment.location,
            });
        }

        // Ints that aren't byte-aligned are not supported
        if segment.type_ == crate::type_::int() && size % 8 != 0 {
            return Err(Error::Unsupported {
                feature: "Non byte aligned integer in patterns".into(),
                location: segment.location,
            });
        }

        let is_signed = segment
            .options
            .iter()
            .any(|x| matches!(x, Opt::Signed { .. }));

        Ok(SizedBitArraySegmentDetails {
            size,
            endianness,
            is_signed,
        })
    }

    fn push_assignment(&mut self, subject: Document<'a>, name: &'a EcoString) {
        let var = self.next_local_var(name);
        let path = self.path_document();
        self.assignments.push(Assignment {
            subject,
            path,
            var,
            name,
        });
    }

    fn push_string_prefix_check(&mut self, subject: Document<'a>, prefix: &'a str) {
        self.checks.push(Check::StringPrefix {
            prefix,
            subject,
            path: self.path_document(),
        })
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

    fn push_bit_array_length_check(
        &mut self,
        subject: Document<'a>,
        expected_bytes: usize,
        has_tail_spread: bool,
    ) {
        self.checks.push(Check::BitArrayLength {
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
}

#[derive(Debug)]
pub struct Assignment<'a> {
    pub name: &'a str,
    var: Document<'a>,
    pub subject: Document<'a>,
    pub path: Document<'a>,
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
    BitArrayLength {
        subject: Document<'a>,
        path: Document<'a>,
        expected_bytes: usize,
        has_tail_spread: bool,
    },
    StringPrefix {
        subject: Document<'a>,
        path: Document<'a>,
        prefix: &'a str,
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
                let length_check = if has_tail_spread {
                    eco_format!(".atLeastLength({expected_length})").to_doc()
                } else {
                    eco_format!(".hasLength({expected_length})").to_doc()
                };
                if match_desired {
                    docvec![subject, path, length_check,]
                } else {
                    docvec!["!", subject, path, length_check,]
                }
            }
            Check::BitArrayLength {
                subject,
                path,
                expected_bytes,
                has_tail_spread,
            } => {
                let length_check = if has_tail_spread {
                    eco_format!(".length >= {expected_bytes}").to_doc()
                } else {
                    eco_format!(".length == {expected_bytes}").to_doc()
                };
                if match_desired {
                    docvec![subject, path, length_check,]
                } else {
                    docvec!["!(", subject, path, length_check, ")",]
                }
            }
            Check::StringPrefix {
                subject,
                path,
                prefix,
            } => {
                let prefix = expression::string(prefix);
                if match_desired {
                    docvec![subject, path, ".startsWith(", prefix, ")"]
                } else {
                    docvec!["!", subject, path, ".startsWith(", prefix, ")"]
                }
            }
        }
    }

    pub(crate) fn may_require_wrapping(&self) -> bool {
        match self {
            Check::Result { .. }
            | Check::Variant { .. }
            | Check::Equal { .. }
            | Check::ListLength { .. }
            | Check::BitArrayLength { .. }
            | Check::StringPrefix { .. }
            | Check::Booly { .. } => false,
            Check::Guard { .. } => true,
        }
    }
}

pub(crate) fn assign_subject<'a>(
    expression_generator: &mut expression::Generator<'_>,
    subject: &'a TypedExpr,
) -> (Document<'a>, Option<Document<'a>>) {
    static ASSIGNMENT_VAR_ECO_STR: OnceLock<EcoString> = OnceLock::new();

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
            let subject = expression_generator
                .next_local_var(ASSIGNMENT_VAR_ECO_STR.get_or_init(|| ASSIGNMENT_VAR.into()));
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

/// Calculates the length of str as utf16 without escape characters.
fn utf16_no_escape_len(str: &EcoString) -> usize {
    convert_string_escape_chars(str).encode_utf16().count()
}
