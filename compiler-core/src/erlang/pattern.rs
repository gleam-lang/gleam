// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

use ecow::eco_format;

use crate::analyse::Inferred;

use super::*;

pub(super) struct PatternPrinter<'a, 'generator, 'module> {
    pub generator: &'generator mut FunctionGenerator<'a, 'module>,
    pub variables: Vec<&'a str>,
    pub guards: Vec<Document<'a>>,
    /// In case we're dealing with string patterns, we might have something like
    /// this: `"a" as letter <> rest`. In this case we want to compile it to
    /// `<<"a"/utf8, rest/binary>>` and then bind a variable to `"a"`.
    /// This way it's easier for the erlang compiler to optimise the pattern
    /// matching.
    ///
    /// Here we store a list of gleam variable name to its name used in the
    /// Erlang code and its literal value.
    pub assignments: Vec<StringPatternAssignment<'a>>,
}

/// This is used to hold data about string patterns with an alias like:
/// `"a" as letter <> _`
pub struct StringPatternAssignment<'a> {
    /// The name assigned to the pattern in the Gleam code:
    ///
    /// ```gleam
    /// "a" as letter <> _
    /// //     ^^^^^^ This one
    /// ```
    ///
    pub gleam_name: EcoString,
    /// The name we're using for that same variable in the generated Erlang
    /// code, could have numbers added to it to make sure it's unique, like
    /// `Letter@1`.
    ///
    pub erlang_name: Document<'a>,
    /// The document representing the literal value of that variable. For
    /// example, if we had this pattern `"a" <> letter` it's literal value in
    /// Erlang is going to be a document with the following string
    /// `<<"a"/utf8>>`.
    ///
    pub literal_value: Document<'a>,
}

impl<'a> StringPatternAssignment<'a> {
    pub fn to_assignment_doc(&self) -> Document<'a> {
        docvec![self.erlang_name.clone(), " = ", self.literal_value.clone()]
    }
}

impl<'a, 'generator, 'module> PatternPrinter<'a, 'generator, 'module> {
    pub(super) fn new(generator: &'generator mut FunctionGenerator<'a, 'module>) -> Self {
        Self {
            generator,
            variables: vec![],
            guards: vec![],
            assignments: vec![],
        }
    }

    pub(super) fn reset_variables(&mut self) {
        self.variables = vec![];
    }

    pub(super) fn print<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        pattern: &'a TypedPattern,
    ) -> Document<'a> {
        match pattern {
            Pattern::Variable { name, location, .. } => {
                eaf.variable_pattern(&self.generator.new_erlang_variable(name, *location));
                return nil();
            }
            Pattern::Discard { .. } => {
                eaf.discard_pattern();
                return nil();
            }
            _ => eaf.variable_pattern("TODO"),
        }

        match pattern {
            Pattern::Discard { .. } | Pattern::Variable { .. } => panic!("already ported"),

            Pattern::Assign {
                name,
                pattern,
                location,
                ..
            } => {
                self.variables.push(name);
                self.print(eaf, pattern)
                    .append(" = ")
                    .append(self.generator.new_erlang_variable(name, *location))
            }

            Pattern::List { elements, tail, .. } => {
                self.pattern_list(eaf, elements, tail.as_deref())
            }

            Pattern::BitArraySize(size) => match size {
                BitArraySize::Int { .. }
                | BitArraySize::Variable { .. }
                | BitArraySize::Block { .. } => self.bit_array_size(eaf, size),
                BitArraySize::BinaryOperator { .. } => {
                    self.bit_array_size(eaf, size).surround("(", ")")
                }
            },

            Pattern::Int { int_value, .. } => nil(),
            Pattern::Float { float_value, .. } => nil(),
            Pattern::String { value, .. } => nil(),

            Pattern::Constructor {
                arguments,
                constructor: Inferred::Known(PatternConstructor { name, .. }),
                ..
            } => self.tag_tuple_pattern(eaf, name, arguments),

            Pattern::Constructor {
                constructor: Inferred::Unknown,
                ..
            } => {
                panic!("Erlang generation performed with uninferred pattern constructor")
            }

            Pattern::Tuple { elements, .. } => {
                tuple(elements.iter().map(|pattern| self.print(eaf, pattern)))
            }

            Pattern::BitArray { segments, .. } => bit_array(
                segments
                    .iter()
                    .map(|s| self.pattern_segment(eaf, &s.value, &s.options)),
            ),

            Pattern::StringPrefix {
                left_side_string,
                right_side_assignment,
                left_side_assignment,
                right_location,
                ..
            } => {
                let right = match right_side_assignment {
                    AssignName::Variable(right) => {
                        let name = self.generator.new_erlang_variable(right, *right_location);
                        // TODO: Boh!
                        //self.variables.push(name);
                        name.to_doc()
                    }
                    AssignName::Discard(_) => "_".to_doc(),
                };

                if let Some((left_name, left_location)) = left_side_assignment {
                    // "wibble" as prefix <> rest
                    //             ^^^^^^^^^ In case the left prefix of the pattern matching is given an alias
                    //                       we bind it to a local variable so that it can be correctly
                    //                       referenced inside the case branch.
                    //
                    // So we will end up with something that looks like this:
                    //
                    // <<"wibble"/binary, Rest/binary>> ->
                    //     Prefix = "wibble",
                    //     ...
                    //
                    self.variables.push(left_name);

                    let erlang_name = self
                        .generator
                        .new_erlang_variable(left_name, *left_location);

                    self.assignments.push(StringPatternAssignment {
                        gleam_name: left_name.clone(),
                        erlang_name: erlang_name.to_doc(),
                        literal_value: string(left_side_string),
                    });
                }

                docvec![
                    "<<\"",
                    string_inner(left_side_string),
                    "\"/utf8",
                    ", ",
                    right,
                    "/binary>>"
                ]
            }

            Pattern::Invalid { .. } => panic!("invalid patterns should not reach code generation"),
        }
    }

    fn bit_array_size<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        size: &'a TypedBitArraySize,
    ) -> Document<'a> {
        match size {
            BitArraySize::Int { value, .. } => int(value),
            BitArraySize::Block { inner, .. } => self.bit_array_size(eaf, inner).surround("(", ")"),
            BitArraySize::Variable {
                name, constructor, ..
            } => {
                let variant = &constructor
                    .as_ref()
                    .expect("Constructor not found for variable usage")
                    .variant;
                match variant {
                    ValueConstructorVariant::ModuleConstant { literal, .. } => {
                        self.generator.const_inline(eaf, literal)
                    }
                    ValueConstructorVariant::LocalVariable { location, .. } => {
                        self.generator.local_var_name(location).to_doc()
                    }

                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::Record { .. } => panic!("invalid segment"),
                }
            }
            BitArraySize::BinaryOperator {
                operator,
                left,
                right,
                ..
            } => {
                let operator = match operator {
                    IntOperator::Add => " + ",
                    IntOperator::Subtract => " - ",
                    IntOperator::Multiply => " * ",
                    IntOperator::Divide => {
                        return self.bit_array_size_divide(eaf, left, right, "div");
                    }
                    IntOperator::Remainder => {
                        return self.bit_array_size_divide(eaf, left, right, "rem");
                    }
                };

                docvec![
                    self.bit_array_size(eaf, left),
                    operator,
                    self.bit_array_size(eaf, right)
                ]
            }
        }
    }

    fn bit_array_size_divide<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        left: &'a TypedBitArraySize,
        right: &'a TypedBitArraySize,
        operator: &'static str,
    ) -> Document<'a> {
        if right.non_zero_compile_time_number() {
            return self.bit_array_size_operator(eaf, left, operator, right);
        }

        let left = self.bit_array_size(eaf, left);
        let right = self.bit_array_size(eaf, right);
        let denominator = self.generator.new_throwaway_variable();
        let clauses = docvec![
            line(),
            "0 -> 0;",
            line(),
            denominator.clone(),
            " -> ",
            binop_documents(left, operator, denominator.to_doc())
        ];
        docvec!["case ", right, " of", clauses.nest(INDENT), line(), "end"]
    }

    fn bit_array_size_operator<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        left: &'a TypedBitArraySize,
        operator: &'static str,
        right: &'a TypedBitArraySize,
    ) -> Document<'a> {
        let left = if let BitArraySize::BinaryOperator { .. } = left {
            self.bit_array_size(eaf, left).surround("(", ")")
        } else {
            self.bit_array_size(eaf, left)
        };
        let right = if let BitArraySize::BinaryOperator { .. } = right {
            self.bit_array_size(eaf, right).surround("(", ")")
        } else {
            self.bit_array_size(eaf, right)
        };
        binop_documents(left, operator, right)
    }

    fn tag_tuple_pattern<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        name: &'a str,
        arguments: &'a [CallArg<TypedPattern>],
    ) -> Document<'a> {
        if arguments.is_empty() {
            atom_string(to_snake_case(name))
        } else {
            tuple(
                [atom_string(to_snake_case(name))].into_iter().chain(
                    arguments
                        .iter()
                        .map(|argument| self.print(eaf, &argument.value)),
                ),
            )
        }
    }

    fn pattern_list<Output>(
        &mut self,
        eaf: &mut impl Eaf<Output>,
        elements: &'a [TypedPattern],
        tail: Option<&'a TypedTailPattern>,
    ) -> Document<'a> {
        let elements = join(
            elements.iter().map(|element| self.print(eaf, element)),
            break_(",", ", "),
        );
        let tail = tail.map(|tail| self.print(eaf, &tail.pattern));
        nil()
    }

    fn pattern_segment<Output, Gen: Eaf<Output>>(
        &mut self,
        eaf: &mut Gen,
        value: &'a TypedPattern,
        options: &'a [BitArrayOption<TypedPattern>],
    ) -> Document<'a> {
        let pattern_is_a_string_literal = matches!(value, Pattern::String { .. });
        let pattern_is_a_discard = matches!(value, Pattern::Discard { .. });

        let create_document =
            |eaf: &mut Gen, this: &mut PatternPrinter<'a, 'generator, 'module>| match value {
                Pattern::String { value, .. } => string_inner(value).surround("\"", "\""),
                Pattern::Discard { .. }
                | Pattern::Variable { .. }
                | Pattern::Int { .. }
                | Pattern::Float { .. } => this.print(eaf, value),

                Pattern::Assign {
                    name,
                    pattern,
                    location,
                    ..
                } => {
                    this.variables.push(name);
                    let variable_name =
                        this.generator.new_erlang_variable(name, *location).to_doc();

                    match pattern.as_ref() {
                        // In Erlang, assignment patterns inside bit arrays are not allowed. So instead of
                        // generating `<<1 = A>>`, we  use guards, and generate `<<A>> when A =:= 1`.
                        Pattern::Int { value, .. } => {
                            this.guards
                                .push(docvec![variable_name.clone(), " =:= ", int(value)]);
                            variable_name
                        }
                        Pattern::Float { value, .. } => {
                            this.guards
                                .push(docvec![variable_name.clone(), " =:= ", float(value)]);
                            variable_name
                        }

                        // Here we do the same as for floats and ints, but we must calculate the size of
                        // the string first, so we can correctly match the bit array segment then compare
                        // it afterwards.
                        Pattern::String { value, .. } => {
                            this.guards.push(docvec![
                                variable_name.clone(),
                                " =:= ",
                                string(value)
                            ]);
                            docvec![variable_name, ":", string_length_utf8_bytes(value)]
                        }

                        // Doing a pattern such as `<<_ as a>>` is the same as just `<<a>>`, so we treat it
                        // as such.
                        Pattern::Discard { .. } => variable_name,

                        // Any other pattern is invalid as a bit array segment. We already handle the case
                        // of `<<a as b>>` in the type-checker, and assignment patterns cannot be nested.
                        Pattern::Variable { .. }
                        | Pattern::BitArraySize(_)
                        | Pattern::Assign { .. }
                        | Pattern::List { .. }
                        | Pattern::Constructor { .. }
                        | Pattern::Tuple { .. }
                        | Pattern::BitArray { .. }
                        | Pattern::StringPrefix { .. }
                        | Pattern::Invalid { .. } => panic!("Pattern segment match not recognised"),
                    }
                }

                Pattern::BitArraySize(_)
                | Pattern::List { .. }
                | Pattern::Constructor { .. }
                | Pattern::Tuple { .. }
                | Pattern::BitArray { .. }
                | Pattern::StringPrefix { .. }
                | Pattern::Invalid { .. } => panic!("Pattern segment match not recognised"),
            };

        let size = |eaf: &mut Gen,
                    value: &'a TypedPattern,
                    this: &mut PatternPrinter<'a, 'generator, 'module>| {
            Some(":".to_doc().append(this.print(eaf, value)))
        };

        let unit = |_eaf: &mut Gen, value: &'a u8| Some(eco_format!("unit:{value}").to_doc());

        bit_array_segment(
            eaf,
            create_document,
            options,
            size,
            unit,
            pattern_is_a_string_literal,
            pattern_is_a_discard,
            self,
        )
    }
}
