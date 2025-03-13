use num_bigint::BigInt;
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
    ByteAt(OffsetBits),
    BitArraySliceToInt {
        start: OffsetBits,
        end: OffsetBits,
        endianness: Endianness,
        is_signed: bool,
    },
    BitArraySliceToFloat {
        start: OffsetBits,
        end: OffsetBits,
        endianness: Endianness,
    },
    BitArraySlice(OffsetBits, Option<OffsetBits>),
    StringPrefixSlice(usize),
}

#[derive(Debug)]
pub(crate) struct Generator<'module_ctx, 'expression_gen, 'a> {
    pub expression_generator: &'expression_gen mut expression::Generator<'module_ctx, 'a>,
    path: Vec<Index<'a>>,
    checks: Vec<Check<'a>>,
    assignments: Vec<Assignment<'a>>,
}

#[derive(Debug)]
pub enum BitArrayTailSpreadType {
    /// The tail of the bit array pattern is for all remaining bits
    Bits,

    /// The tail of the bit array pattern is for all remaining whole bytes. This
    /// requires an additional runtime check that the number of remaining bits
    /// is a multiple of 8, as otherwise the pattern doesn't match.
    Bytes,
}

#[derive(Debug, Clone)]
/// Represents the offset into a bit array which is needed to extract the value
/// of a specific pattern. This can either be a constant value, if we know it at
/// compile-time, or a sum of some variables. For example:
/// ```gleam
/// case todo {
///   <<_:size(8), extract_this>> -> todo
///   //           ^ This has a known offset of 8
///   <<x:size(8), y:size(x), z:size(y)>>
///   //                      ^ This starts at offset x + 8, and ends at offset x + y + 8
/// }
/// ```
pub struct OffsetBits {
    /// The size of the known offset. The total offset will be at least this size.
    /// If this field is 0, the offset is entirely composed of variable sizes.
    constant: usize,
    /// Any variables which must be added to the known offset at runtime.
    /// Empty if we know the size at compile-time.
    variables: Vec<EcoString>,
    /// Sometimes we need to divide an offset by a certain number, such as 8, because
    /// sizes can be specified using bits or bytes in different circumstances.
    /// If the size is constant, we can just divide that. However, if the size needs
    /// to be computed at runtime, we must add that division as part of the calculation.
    /// If this field is 1, we ignore it.
    divide_by: usize,
}

impl OffsetBits {
    fn increment(&mut self, size: BitArraySize) {
        match size {
            BitArraySize::Literal(size) => self.constant += size,
            BitArraySize::Variable(name) => self.variables.push(name),
        }
    }

    fn is_whole_number_of_bytes(&self) -> bool {
        self.variables.is_empty() && self.constant % 8 == 0
    }

    fn divide(mut self, by: usize) -> Self {
        if self.variables.is_empty() {
            self.constant = self.constant / by;
        } else {
            self.divide_by = self.divide_by * by;
        }

        self
    }

    fn to_doc(&self) -> Document<'static> {
        let doc = if self.variables.is_empty() {
            self.constant.to_doc()
        } else if self.constant == 0 {
            join(
                self.variables
                    .iter()
                    .map(|variable| variable.clone().to_doc()),
                " + ".to_doc(),
            )
            .group()
        } else {
            docvec![
                join(
                    self.variables
                        .iter()
                        .map(|variable| variable.clone().to_doc()),
                    " + ".to_doc()
                ),
                " + ",
                self.constant,
            ]
            .group()
        };

        match self.divide_by {
            1 => doc,
            _ => doc.append(" / ".to_doc().append(self.divide_by)).group(),
        }
    }
}

struct Offset {
    bits: OffsetBits,
    tail_spread_type: Option<BitArrayTailSpreadType>,
}

impl Offset {
    pub fn new() -> Self {
        Self {
            bits: OffsetBits {
                constant: 0,
                variables: Vec::new(),
                divide_by: 1,
            },
            tail_spread_type: None,
        }
    }
    pub fn set_open_ended(&mut self, tail_spread_type: BitArrayTailSpreadType) {
        self.tail_spread_type = Some(tail_spread_type);
    }
}

#[derive(Debug, Clone)]
enum BitArraySize {
    Literal(usize),
    Variable(EcoString),
}

impl BitArraySize {
    fn is_constant_value(&self, value: usize) -> bool {
        match self {
            BitArraySize::Literal(size) => *size == value,
            BitArraySize::Variable(_) => false,
        }
    }
}

#[derive(Debug)]
struct SizedBitArraySegmentDetails {
    size: BitArraySize,
    endianness: Endianness,
    is_signed: bool,
}

impl<'module_ctx, 'expression_gen, 'a> Generator<'module_ctx, 'expression_gen, 'a> {
    pub fn new(
        expression_generator: &'expression_gen mut expression::Generator<'module_ctx, 'a>,
    ) -> Self {
        Self {
            path: vec![],
            checks: vec![],
            assignments: vec![],
            expression_generator,
        }
    }

    fn next_local_var(&mut self, name: &EcoString) -> EcoString {
        self.expression_generator.next_local_var(name)
    }

    fn local_var(&mut self, name: &EcoString) -> EcoString {
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

    fn push_byte_at(&mut self, i: OffsetBits) {
        self.path.push(Index::ByteAt(i));
    }

    fn push_bit_array_slice_to_int(
        &mut self,
        start: OffsetBits,
        end: OffsetBits,
        endianness: Endianness,
        is_signed: bool,
    ) {
        self.expression_generator
            .tracker
            .bit_array_slice_to_int_used = true;

        self.path.push(Index::BitArraySliceToInt {
            start,
            end,
            endianness,
            is_signed,
        });
    }

    fn push_bit_array_slice_to_float(
        &mut self,
        start: OffsetBits,
        end: OffsetBits,
        endianness: Endianness,
    ) {
        self.expression_generator
            .tracker
            .bit_array_slice_to_float_used = true;

        self.path.push(Index::BitArraySliceToFloat {
            start,
            end,
            endianness,
        });
    }

    fn push_bit_array_slice(&mut self, start: OffsetBits, end: Option<OffsetBits>) {
        self.expression_generator.tracker.bit_array_slice_used = true;
        self.path.push(Index::BitArraySlice(start, end));
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

    fn apply_path_to_subject(&self, subject: Document<'a>) -> Document<'a> {
        self.path
            .iter()
            .fold(subject, |acc, segment| match segment {
                Index::Int(i) => acc.append(eco_format!("[{i}]").to_doc()),
                // TODO: escape string if needed
                Index::String(s) => acc.append(docvec![".", maybe_escape_property_doc(s)]),
                Index::ByteAt(i) => acc.append(docvec![".byteAt(", i, ")"]),
                Index::BitArraySliceToInt {
                    start,
                    end,
                    endianness,
                    is_signed,
                } => docvec![
                    "bitArraySliceToInt(",
                    acc,
                    ", ",
                    start,
                    ", ",
                    end,
                    ", ",
                    bool(endianness.is_big()),
                    ", ",
                    bool(*is_signed),
                    ")"
                ],
                Index::BitArraySliceToFloat {
                    start,
                    end,
                    endianness,
                } => docvec![
                    "bitArraySliceToFloat(",
                    acc,
                    ", ",
                    start,
                    ", ",
                    end,
                    ", ",
                    bool(endianness.is_big()),
                    ")"
                ],
                Index::BitArraySlice(start, end) => match end {
                    Some(end) => {
                        docvec!["bitArraySlice(", acc, ", ", start, ", ", end, ")"]
                    }
                    None => docvec!["bitArraySlice(", acc, ", ", start, ")"],
                },
                Index::StringPrefixSlice(i) => docvec!(acc, ".slice(", i, ")"),
            })
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
            | ClauseGuard::ModuleSelect { .. } => Ok(docvec!["(", self.guard(guard)?, ")"]),
        }
    }

    fn guard(&mut self, guard: &'a TypedClauseGuard) -> Output<'a> {
        Ok(match guard {
            ClauseGuard::Equals { left, right, .. } if is_js_scalar(left.type_()) => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec![left, " === ", right]
            }

            ClauseGuard::NotEquals { left, right, .. } if is_js_scalar(left.type_()) => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec![left, " !== ", right]
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
                docvec![left, " > ", right]
            }

            ClauseGuard::GtEqFloat { left, right, .. }
            | ClauseGuard::GtEqInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec![left, " >= ", right]
            }

            ClauseGuard::LtFloat { left, right, .. } | ClauseGuard::LtInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec![left, " < ", right]
            }

            ClauseGuard::LtEqFloat { left, right, .. }
            | ClauseGuard::LtEqInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec![left, " <= ", right]
            }

            ClauseGuard::AddFloat { left, right, .. } | ClauseGuard::AddInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec![left, " + ", right]
            }

            ClauseGuard::SubFloat { left, right, .. } | ClauseGuard::SubInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec![left, " - ", right]
            }

            ClauseGuard::MultFloat { left, right, .. }
            | ClauseGuard::MultInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec![left, " * ", right]
            }

            ClauseGuard::DivFloat { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                self.expression_generator.tracker.float_division_used = true;
                docvec!["divideFloat", wrap_args([left, right])]
            }

            ClauseGuard::DivInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                self.expression_generator.tracker.int_division_used = true;
                docvec!["divideInt", wrap_args([left, right])]
            }

            ClauseGuard::RemainderInt { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                self.expression_generator.tracker.int_remainder_used = true;
                docvec!["remainderInt", wrap_args([left, right])]
            }

            ClauseGuard::Or { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec![left, " || ", right]
            }

            ClauseGuard::And { left, right, .. } => {
                let left = self.wrapped_guard(left)?;
                let right = self.wrapped_guard(right)?;
                docvec![left, " && ", right]
            }

            ClauseGuard::Var { name, .. } => self
                .path_doc_from_assignments(name)
                .unwrap_or_else(|| self.local_var(name).to_doc()),

            ClauseGuard::TupleIndex { tuple, index, .. } => {
                docvec![self.guard(tuple)?, "[", index, "]"]
            }

            ClauseGuard::FieldAccess {
                label, container, ..
            } => {
                docvec![
                    self.guard(container)?,
                    ".",
                    maybe_escape_property_doc(label)
                ]
            }

            ClauseGuard::ModuleSelect {
                module_alias,
                label,
                ..
            } => docvec!["$", module_alias, ".", label],

            ClauseGuard::Not { expression, .. } => {
                docvec!["!", self.guard(expression)?]
            }

            ClauseGuard::Constant(constant) => {
                return expression::guard_constant_expression(
                    &mut self.assignments,
                    self.expression_generator.tracker,
                    constant,
                );
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
            .map(|assignment| assignment.subject.clone())
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
                    //
                    // We also want to push this assignment without using push_assignment, since we
                    // do _not_ want to access the current path on the static string!
                    let var = self.next_local_var(left).to_doc();
                    self.assignments.push(Assignment {
                        subject: expression::string(left_side_string),
                        name: left,
                        var,
                    });
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
                        let details = self.sized_bit_array_segment_details(segment)?;

                        match (segment.value.as_ref(), details.size) {
                            (Pattern::Int { int_value, .. }, BitArraySize::Literal(size))
                                if size <= SAFE_INT_SEGMENT_MAX_SIZE
                                    && size % 8 == 0
                                    && offset.bits.is_whole_number_of_bytes() =>
                            {
                                let bytes = bit_array_segment_int_value_to_bytes(
                                    (*int_value).clone(),
                                    BigInt::from(size),
                                    details.endianness,
                                )?;

                                for byte in bytes {
                                    self.push_byte_at(offset.bits.clone().divide(8));
                                    self.push_equality_check(subject.clone(), docvec![byte]);
                                    self.pop();
                                    offset.bits.increment(BitArraySize::Literal(8));
                                }
                            }

                            (Pattern::Discard { .. }, size) => {
                                offset.bits.increment(size);
                            }

                            (_, size) => {
                                let start = offset.bits.clone();
                                let mut end = offset.bits.clone();
                                end.increment(size.clone());

                                if segment.type_ == crate::type_::int() {
                                    if size.is_constant_value(8)
                                        && !details.is_signed
                                        && offset.bits.is_whole_number_of_bytes()
                                    {
                                        self.push_byte_at(offset.bits.clone().divide(8));
                                    } else {
                                        self.push_bit_array_slice_to_int(
                                            start,
                                            end,
                                            details.endianness,
                                            details.is_signed,
                                        );
                                    }
                                } else {
                                    self.push_bit_array_slice_to_float(
                                        start,
                                        end,
                                        details.endianness,
                                    );
                                }

                                self.traverse_pattern(subject, &segment.value)?;
                                self.pop();
                                offset.bits.increment(size);
                            }
                        }
                    } else {
                        match segment.options.as_slice() {
                            [Opt::Bits { .. }] => {
                                self.push_bit_array_slice(offset.bits.clone(), None);
                                self.traverse_pattern(subject, &segment.value)?;
                                self.pop();
                                offset.set_open_ended(BitArrayTailSpreadType::Bits);
                                Ok(())
                            }

                            [Opt::Bits { .. }, Opt::Size { value: size, .. }]
                            | [Opt::Size { value: size, .. }, Opt::Bits { .. }] => match &**size {
                                Pattern::Int { value, .. } => {
                                    let start = offset.bits.clone();
                                    let increment = value.parse::<usize>().expect(
                                        "part of an Int node should always parse as integer",
                                    );
                                    offset.bits.increment(BitArraySize::Literal(increment));
                                    let end = offset.bits.clone();

                                    self.push_bit_array_slice(start, Some(end));
                                    self.traverse_pattern(subject, &segment.value)?;
                                    self.pop();
                                    Ok(())
                                }

                                Pattern::VarUsage { name, .. } => {
                                    let name = self.expression_generator.local_var(name);
                                    let start = offset.bits.clone();

                                    offset.bits.increment(BitArraySize::Variable(name));
                                    let end = offset.bits.clone();

                                    self.push_bit_array_slice(start, Some(end));
                                    self.traverse_pattern(subject, &segment.value)?;
                                    self.pop();
                                    Ok(())
                                }

                                _ => Err(Error::Unsupported {
                                    feature: "This bit array size option in patterns".into(),
                                    location: segment.location,
                                }),
                            },

                            [Opt::Bytes { .. }] => {
                                self.push_bit_array_slice(offset.bits.clone(), None);
                                self.traverse_pattern(subject, &segment.value)?;
                                self.pop();
                                offset.set_open_ended(BitArrayTailSpreadType::Bytes);
                                Ok(())
                            }

                            [Opt::Bytes { .. }, Opt::Size { value: size, .. }]
                            | [Opt::Size { value: size, .. }, Opt::Bytes { .. }] => match &**size {
                                Pattern::Int { value, .. } => {
                                    let start = offset.bits.clone();
                                    let increment = value.parse::<usize>().expect(
                                        "part of an Int node should always parse as integer",
                                    ) * 8;
                                    offset.bits.increment(BitArraySize::Literal(increment));
                                    let end = offset.bits.clone();

                                    self.push_bit_array_slice(start, Some(end));
                                    self.traverse_pattern(subject, &segment.value)?;
                                    self.pop();
                                    Ok(())
                                }

                                Pattern::VarUsage { name, .. } => {
                                    let start = offset.bits.clone();
                                    let mut name = self.expression_generator.local_var(name);
                                    name.push_str(" * 8");
                                    offset.bits.increment(BitArraySize::Variable(name));
                                    let end = offset.bits.clone();

                                    self.push_bit_array_slice(start, Some(end));
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
                                    for byte in convert_string_escape_chars(value).as_bytes() {
                                        if offset.bits.is_whole_number_of_bytes() {
                                            self.push_byte_at(offset.bits.clone().divide(8));
                                        } else {
                                            let mut end = offset.bits.clone();
                                            end.increment(BitArraySize::Literal(8));

                                            self.push_bit_array_slice_to_int(
                                                offset.bits.clone(),
                                                end,
                                                Endianness::Big,
                                                false,
                                            );
                                        }
                                        self.push_equality_check(subject.clone(), byte.to_doc());
                                        self.pop();
                                        offset.bits.increment(BitArraySize::Literal(8));
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

                self.push_bit_array_bit_size_check(
                    subject.clone(),
                    offset.bits.clone(),
                    offset.tail_spread_type,
                );
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
        &mut self,
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

        let unit = segment
            .options
            .iter()
            .find_map(|option| match option {
                Opt::Unit { value, .. } => Some(*value),
                _ => None,
            })
            .unwrap_or(1);

        let size = match segment
            .options
            .iter()
            .find(|x| matches!(x, Opt::Size { .. }))
        {
            Some(Opt::Size { value: size, .. }) => match &**size {
                Pattern::Int { value, .. } => Ok(BitArraySize::Literal(
                    value
                        .parse::<usize>()
                        .expect("part of an Int node should always parse as integer")
                        * unit as usize,
                )),
                Pattern::VarUsage { name, .. } => {
                    let mut variable = self.expression_generator.local_var(name);
                    if unit != 1 {
                        variable.push_str(&eco_format!(" * {unit}"));
                    }
                    Ok(BitArraySize::Variable(variable))
                }
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

                Ok(BitArraySize::Literal(default_size))
            }
        }?;

        // 16-bit floats are not supported
        if segment.type_ == crate::type_::float() && size.is_constant_value(16) {
            return Err(Error::Unsupported {
                feature: "Float width of 16 bits in patterns".into(),
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
        let var = self.next_local_var(name).to_doc();
        let subject = self.apply_path_to_subject(subject);
        self.assignments.push(Assignment { subject, var, name });
    }

    fn push_string_prefix_check(&mut self, subject: Document<'a>, prefix: &'a str) {
        self.checks.push(Check::StringPrefix {
            prefix,
            subject: self.apply_path_to_subject(subject),
        })
    }

    fn push_booly_check(&mut self, subject: Document<'a>, expected_to_be_truthy: bool) {
        self.checks.push(Check::Booly {
            expected_to_be_truthy,
            subject: self.apply_path_to_subject(subject),
        })
    }

    fn push_equality_check(&mut self, subject: Document<'a>, to: Document<'a>) {
        self.checks.push(Check::Equal {
            to,
            subject: self.apply_path_to_subject(subject),
        })
    }

    fn push_variant_check(&mut self, subject: Document<'a>, kind: Document<'a>) {
        self.checks.push(Check::Variant {
            kind,
            subject: self.apply_path_to_subject(subject),
        })
    }

    fn push_result_check(&mut self, subject: Document<'a>, is_ok: bool) {
        self.checks.push(Check::Result {
            is_ok,
            subject: self.apply_path_to_subject(subject),
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
            subject: self.apply_path_to_subject(subject),
        })
    }

    fn push_bit_array_bit_size_check(
        &mut self,
        subject: Document<'a>,
        expected_bit_size: OffsetBits,
        tail_spread_type: Option<BitArrayTailSpreadType>,
    ) {
        self.checks.push(Check::BitArrayBitSize {
            expected_bit_size,
            tail_spread_type,
            subject: self.apply_path_to_subject(subject),
        })
    }
}

#[derive(Debug)]
pub struct CompiledPattern<'a> {
    pub checks: Vec<Check<'a>>,
    pub assignments: Vec<Assignment<'a>>,
}

impl CompiledPattern<'_> {
    pub fn has_assignments(&self) -> bool {
        !self.assignments.is_empty()
    }
}

#[derive(Debug)]
pub struct Assignment<'a> {
    pub name: &'a str,
    var: Document<'a>,
    pub subject: Document<'a>,
}

impl<'a> Assignment<'a> {
    pub fn into_doc(self) -> Document<'a> {
        docvec!["let ", self.var, " = ", self.subject, ";"]
    }
}

#[derive(Debug)]
pub enum Check<'a> {
    Result {
        subject: Document<'a>,
        is_ok: bool,
    },
    Variant {
        subject: Document<'a>,
        kind: Document<'a>,
    },
    Equal {
        subject: Document<'a>,
        to: Document<'a>,
    },
    ListLength {
        subject: Document<'a>,
        expected_length: usize,
        has_tail_spread: bool,
    },
    BitArrayBitSize {
        subject: Document<'a>,
        expected_bit_size: OffsetBits,
        tail_spread_type: Option<BitArrayTailSpreadType>,
    },
    StringPrefix {
        subject: Document<'a>,
        prefix: &'a str,
    },
    Booly {
        subject: Document<'a>,
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
            } => {
                if expected_to_be_truthy == match_desired {
                    subject
                } else {
                    docvec!["!", subject]
                }
            }

            Check::Variant { subject, kind } => {
                if match_desired {
                    docvec![subject, " instanceof ", kind]
                } else {
                    docvec!["!(", subject, " instanceof ", kind, ")"]
                }
            }

            Check::Result { subject, is_ok } => {
                if match_desired == is_ok {
                    docvec![subject, ".isOk()"]
                } else {
                    docvec!["!", subject, ".isOk()"]
                }
            }

            Check::Equal { subject, to } => {
                let operator = if match_desired { " === " } else { " !== " };
                docvec![subject, operator, to]
            }

            Check::ListLength {
                subject,
                expected_length,
                has_tail_spread,
            } => {
                let length_check = if has_tail_spread {
                    eco_format!(".atLeastLength({expected_length})").to_doc()
                } else {
                    eco_format!(".hasLength({expected_length})").to_doc()
                };
                if match_desired {
                    docvec![subject, length_check,]
                } else {
                    docvec!["!", subject, length_check,]
                }
            }
            Check::BitArrayBitSize {
                subject,
                expected_bit_size,
                tail_spread_type,
            } => {
                let bit_size = docvec![subject.clone(), ".bitSize"];

                let bit_size_check = match tail_spread_type {
                    Some(BitArrayTailSpreadType::Bits) => {
                        docvec![bit_size, " >= ", expected_bit_size]
                    }
                    Some(BitArrayTailSpreadType::Bytes) => {
                        // When the tail spread is for bytes rather than bits,
                        // check that there is a whole number of bytes left in
                        // the bit array
                        docvec![
                            "(",
                            bit_size.clone(),
                            " >= ",
                            expected_bit_size,
                            " && (",
                            bit_size,
                            " - ",
                            expected_bit_size,
                            ") % 8 === 0)"
                        ]
                    }
                    None => docvec![bit_size, " == ", expected_bit_size],
                };

                if match_desired {
                    bit_size_check
                } else {
                    docvec!["!(", bit_size_check, ")"]
                }
            }
            Check::StringPrefix { subject, prefix } => {
                let prefix = expression::string(prefix);
                if match_desired {
                    docvec![subject, ".startsWith(", prefix, ")"]
                } else {
                    docvec!["!", subject, ".startsWith(", prefix, ")"]
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
            | Check::BitArrayBitSize { .. }
            | Check::StringPrefix { .. }
            | Check::Booly { .. } => false,
            Check::Guard { .. } => true,
        }
    }
}

pub(crate) fn assign_subject<'a>(
    expression_generator: &mut expression::Generator<'_, 'a>,
    subject: &'a TypedExpr,
) -> (Document<'a>, Option<Document<'a>>) {
    static ASSIGNMENT_VAR_ECO_STR: OnceLock<EcoString> = OnceLock::new();

    match subject {
        // If the value is a variable we don't need to assign it to a new
        // variable, we can the value expression safely without worrying about
        // performing computation or side effects multiple times.
        TypedExpr::Var {
            name, constructor, ..
        } if constructor.is_local_variable() => {
            (expression_generator.local_var(name).to_doc(), None)
        }
        // If it's not a variable we need to assign it to a variable
        // to avoid rendering the subject expression multiple times
        _ => {
            let subject = expression_generator
                .next_local_var(ASSIGNMENT_VAR_ECO_STR.get_or_init(|| ASSIGNMENT_VAR.into()))
                .to_doc();
            (subject.clone(), Some(subject))
        }
    }
}

pub(crate) fn assign_subjects<'a>(
    expression_generator: &mut expression::Generator<'_, 'a>,
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
