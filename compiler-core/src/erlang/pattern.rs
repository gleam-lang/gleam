// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

use erlang_abstract_format::BitArraySegmentSpecifier;

use crate::{analyse::Inferred, parse::LiteralFloatValue};

use super::*;

/// This is used to generate the code for a pattern.
/// Most Gleam patterns can be translated to Erlang in a pretty straightforward
/// way but there's notable exceptions that require some extra bookeping, this
/// helps with that.
pub(super) struct PatternGenerator<'a, 'generator, 'module> {
    pub generator: &'generator mut FunctionGenerator<'a, 'module>,

    /// Not all Gleam patterns can be cleanly (or efficiently!) translated to
    /// Erlang ones. In particular, we allow aliasing almost all patterns like:
    ///
    /// ```gleam
    /// "a" as letter <> _ -> todo
    /// //  ^^^^^^^^^ This...
    /// <<1 as number, _:bits>> -> todo
    /// //  ^^^^^^^^^ ...or this!
    /// ```
    ///
    /// In those cases we generate a pattern matching on the literal value and
    /// keep track of the fact we'll have to define such variable in the
    /// following case branch.
    ///
    /// This map maps the name of those Gleam variables that have been
    /// introduced with an alias to their constant value and position.
    /// You can check the docs of `AliasedValue` for some more examples and a
    /// more in depth explanation.
    pub variables_to_add_later: HashMap<EcoString, AliasedLiteral>,
}

/// This is used to hold data about string prefix pattern with an alias like:
/// `"a" as letter <> _`.
///
/// This pattern cannot be easily translated to Erlang since it doesn't allow to
/// write something like this in a bitstring: `<<"a" = Letter, _:bits>>`.
/// So what the generator will do is it will generate the following simpler
/// pattern:
///
/// ```erl
/// <<"a", _:bits>>
/// % ^^^ Notice how this isn't bound to a `Letter` variable
/// ```
///
/// And it will return this data structure so we can then generate the needed
/// variable assignment later in the case body. So, overall, this:
///
/// ```gleam
/// "a" as letter <> _ -> ...
/// ```
///
/// Will become:
///
/// ```erl
/// <<"a", _:bits>> ->
///   Letter = "a",
///   ...
/// ```
///
/// > Note: We could have also generated slightly different code, where we use
/// > a guard `<<Letter, _:bits>> when Letter =:= "a"`. That would mean we don't
/// > have to add that additional variable binding; the problem is that the
/// > Erlang compiler doesn't seem to be able to optimise that as well as the
/// > one with the literal value in the pattern!
///
#[derive(Debug)]
pub enum AliasedLiteral {
    String {
        /// The location of the name given to the alias:
        ///
        /// ```gleam
        /// "a" as letter <> _
        /// //     ^^^^^^ This span here
        ///
        /// <<"a" as letter>>
        /// //       ^^^^^^ or, if we're dealing with bit arrays this span here
        /// ```
        ///
        location: SrcSpan,

        /// This is the content of the literal string.
        ///
        /// ```gleam
        ///   "książka" as word <> _
        /// // ^^^^^^^  This right here
        /// ```
        ///
        value: EcoString,
    },
    Int {
        /// The location of the name given to the alias:
        ///
        /// ```gleam
        /// <<1 as digit>>
        /// //     ^^^^^ This span here
        /// ```
        location: SrcSpan,

        /// The value of the literal int being aliased.
        value: BigInt,
    },
    Float {
        /// The location of the name given to the alias:
        ///
        /// ```gleam
        /// <<1.1 as number>>
        /// //       ^^^^^^ This span here
        /// ```
        location: SrcSpan,

        /// The value of the literal float being aliased.
        value: LiteralFloatValue,
    },
}

impl<'a, 'generator, 'module> PatternGenerator<'a, 'generator, 'module> {
    pub(super) fn new(generator: &'generator mut FunctionGenerator<'a, 'module>) -> Self {
        Self {
            generator,
            variables_to_add_later: HashMap::new(),
        }
    }

    pub(super) fn pattern<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        pattern: &'a TypedPattern,
    ) {
        match pattern {
            Pattern::Discard { .. } => eaf.discard_pattern(),
            Pattern::Float { float_value, .. } => eaf.float_pattern(float_value.value()),
            Pattern::Int { int_value, .. } => eaf.int_pattern(int_value.clone()),
            Pattern::String { value, .. } => eaf.string_pattern(value),
            Pattern::Variable { name, location, .. } => {
                eaf.variable_pattern(&self.generator.new_erlang_variable(name, *location))
            }

            Pattern::Assign {
                name,
                pattern,
                location,
            } => {
                eaf.match_pattern();
                self.pattern(eaf, pattern);
                eaf.variable_pattern(&self.generator.new_erlang_variable(name, *location));
            }

            Pattern::Tuple { elements, .. } => {
                let tuple = eaf.start_tuple_pattern();
                for element in elements {
                    self.pattern(eaf, element);
                }
                eaf.end_tuple_pattern(tuple);
            }

            Pattern::List { elements, tail, .. } => {
                for element in elements {
                    eaf.cons_list_pattern();
                    self.pattern(eaf, element);
                }
                if let Some(tail) = tail {
                    self.pattern(eaf, &tail.pattern);
                } else {
                    eaf.empty_list_pattern();
                }
            }

            Pattern::Constructor {
                arguments,
                constructor,
                ..
            } => {
                let Inferred::Known(PatternConstructor { name, .. }) = constructor else {
                    panic!("uninferred constructor made it to codegen ")
                };

                if arguments.is_empty() {
                    eaf.atom_pattern(&to_snake_case(name));
                } else {
                    let tuple = eaf.start_tuple_pattern();
                    eaf.atom_pattern(&to_snake_case(name));
                    for argument in arguments {
                        self.pattern(eaf, &argument.value);
                    }
                    eaf.end_tuple_pattern(tuple);
                }
            }

            Pattern::StringPrefix {
                left_side_string,
                left_side_assignment,
                right_side_assignment,
                right_location,
                ..
            } => {
                // If the constant string prefix is being aliased we need to add
                // that value to the variables that are going to be generated
                // later:
                if let Some((prefix_name, prefix_location)) = left_side_assignment {
                    let _ = self.variables_to_add_later.insert(
                        prefix_name.clone(),
                        AliasedLiteral::String {
                            location: *prefix_location,
                            value: left_side_string.clone(),
                        },
                    );
                }

                let bit_array = eaf.start_bit_array_pattern();

                // We first generate a segment matching on the literal prefix.
                eaf.bit_array_segment();
                eaf.string_pattern(left_side_string);
                eaf.atom("deafult");
                eaf.bit_array_segment_specifiers([BitArraySegmentSpecifier::Utf8]);

                // We then add a segment matching on the rest of the string.
                eaf.bit_array_segment();
                match right_side_assignment {
                    AssignName::Variable(name) => eaf.variable_pattern(
                        &self.generator.new_erlang_variable(name, *right_location),
                    ),
                    AssignName::Discard(_) => eaf.discard_pattern(),
                }
                eaf.atom("default");
                eaf.bit_array_segment_specifiers([BitArraySegmentSpecifier::Binary]);

                eaf.end_bit_array_pattern(bit_array);
            }

            Pattern::BitArray { segments, .. } => {
                let bit_array = eaf.start_bit_array_pattern();
                for segment in segments {
                    eaf.bit_array_segment();
                    self.bit_array_pattern_segment_value(eaf, segment);
                    self.bit_array_pattern_segment_size(eaf, segment);
                    self.generator.bit_array_segment_specifiers(eaf, segment);
                }
                eaf.end_bit_array_pattern(bit_array);
            }

            Pattern::BitArraySize(size) => self.bit_array_size(eaf, size),

            Pattern::Invalid { .. } => {
                panic!("invalid patterns should not reach code generation")
            }
        }
    }

    fn bit_array_size<Output>(&mut self, eaf: &mut impl Eaf<Output>, size: &'a TypedBitArraySize) {
        match size {
            BitArraySize::Int { int_value, .. } => eaf.int(int_value.clone()),
            BitArraySize::Block { inner, .. } => self.bit_array_size(eaf, inner),

            BitArraySize::Variable {
                constructor, name, ..
            } => match self.variables_to_add_later.get(name) {
                Some(AliasedLiteral::Int { value, .. }) => eaf.int(value.clone()),
                Some(_) => panic!("segment size that is not int made it through type checking"),
                None => {
                    let constructor = constructor.as_ref().expect("variable with no constructor");
                    match &constructor.variant {
                        ValueConstructorVariant::ModuleConstant { literal, .. } => {
                            self.generator.inlined_constant(eaf, literal)
                        }
                        ValueConstructorVariant::LocalVariable { location, .. } => {
                            eaf.variable(&self.generator.local_var_name(location))
                        }
                        ValueConstructorVariant::ModuleFn { .. }
                        | ValueConstructorVariant::Record { .. } => panic!("invalid segment"),
                    }
                }
            },

            BitArraySize::BinaryOperator {
                operator,
                left,
                right,
                ..
            } => {
                let operator = match operator {
                    IntOperator::Add => "+",
                    IntOperator::Subtract => "-",
                    IntOperator::Multiply => "*",
                    IntOperator::Divide => {
                        return self.bit_array_size_divide(eaf, left, right, "div");
                    }
                    IntOperator::Remainder => {
                        return self.bit_array_size_divide(eaf, left, right, "rem");
                    }
                };
                eaf.binary_operator(operator);
                self.bit_array_size(eaf, left);
                self.bit_array_size(eaf, right);
            }
        }
    }

    fn bit_array_pattern_segment_value<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        segment: &'a TypedPatternBitArraySegment,
    ) {
        let Pattern::Assign {
            name,
            location,
            pattern,
        } = segment.value.as_ref()
        else {
            // If the pattern is not an assign, it needs no extra care, we can
            // just produce the code for such pattern!
            self.pattern(eaf, &segment.value);
            return;
        };

        // But if we're dealing with an assign pattern inside a bit array
        // segment we have to give it the same treatment we reserve for string
        // prefixes (after all those are aliased bit array patterns too, since
        // strings are just bitstrings!).
        //
        // After reading the docs for those you might already be familiar with
        // the problem. But it's still worth going over that too one more time.
        // In Gleam we can write `<<1 as a, _:bits>>` but in Erlang we can't
        // produce the following pattern: `<<1 = A, _:bits>>`.
        // So what we will do is match on the literal value and keep track of
        // the constant value we'll have to add into scope later.
        let aliased_value = match pattern.as_ref() {
            Pattern::Int { int_value, .. } => AliasedLiteral::Int {
                location: *location,
                value: int_value.clone(),
            },

            Pattern::Float { float_value, .. } => AliasedLiteral::Float {
                location: *location,
                value: *float_value,
            },
            Pattern::String { value, .. } => AliasedLiteral::String {
                location: *location,
                value: value.clone(),
            },

            // Aliasing a discard is the same as just producing a variable
            // pattern, that makes things even simpler, we can just produce the
            // code for a variable pattern with the wanted name and call it a day
            Pattern::Discard { .. } => {
                eaf.variable_pattern(&self.generator.new_erlang_variable(name, *location));
                return;
            }

            Pattern::Variable { .. }
            | Pattern::BitArraySize(_)
            | Pattern::Assign { .. }
            | Pattern::List { .. }
            | Pattern::Constructor { .. }
            | Pattern::Tuple { .. }
            | Pattern::BitArray { .. }
            | Pattern::StringPrefix { .. }
            | Pattern::Invalid { .. } => {
                panic!("invalid pattern inside aliased bit array pattern segment")
            }
        };

        let _ = self
            .variables_to_add_later
            .insert(name.clone(), aliased_value);
        self.pattern(eaf, pattern);
    }

    fn bit_array_pattern_segment_size<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        segment: &'a TypedPatternBitArraySegment,
    ) {
        let Some(size) = segment.size() else {
            eaf.atom("default");
            return;
        };
        let TypedPattern::BitArraySize(size) = size else {
            panic!("invalid size in pattern size segment")
        };
        self.bit_array_size(eaf, size);
    }

    fn bit_array_size_divide<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        left: &'a TypedBitArraySize,
        right: &'a TypedBitArraySize,
        operator: &'static str,
    ) {
        if right.non_zero_compile_time_number() {
            eaf.binary_operator(operator);
            self.bit_array_size(eaf, left);
            self.bit_array_size(eaf, right);
        } else {
            let case = eaf.start_case();
            self.bit_array_size(eaf, right);

            let clause = eaf.start_case_clause();
            eaf.int_pattern(BigInt::ZERO);
            let clause = eaf.end_clause_pattern(clause);
            let clause = eaf.end_clause_guards(clause);
            eaf.int(BigInt::ZERO);
            eaf.end_clause_body(clause);

            let clause = eaf.start_case_clause();
            let denominator = self.generator.new_throwaway_variable();
            eaf.variable_pattern(&denominator);
            let clause = eaf.end_clause_pattern(clause);
            let clause = eaf.end_clause_guards(clause);
            eaf.binary_operator(operator);
            self.bit_array_size(eaf, left);
            eaf.variable(&denominator);
            eaf.end_clause_body(clause);
            eaf.end_case(case);
        }
    }
}
