use ecow::eco_format;

use crate::analyse::Inferred;

use super::*;

pub(super) struct PatternPrinter<'a, 'env> {
    pub environment: &'env mut Env<'a>,
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

impl<'a, 'env> PatternPrinter<'a, 'env> {
    pub(super) fn new(environment: &'env mut Env<'a>) -> Self {
        Self {
            environment,
            variables: vec![],
            guards: vec![],
            assignments: vec![],
        }
    }

    pub(super) fn reset_variables(&mut self) {
        self.variables = vec![];
    }

    pub(super) fn print(&mut self, pattern: &'a TypedPattern) -> Document<'a> {
        match pattern {
            Pattern::Assign { name, pattern, .. } => {
                self.variables.push(name);
                self.print(pattern)
                    .append(" = ")
                    .append(self.environment.next_local_var_name(name))
            }

            Pattern::List { elements, tail, .. } => self.pattern_list(elements, tail.as_deref()),

            Pattern::Discard { .. } => "_".to_doc(),

            Pattern::BitArraySize(size) => match size {
                BitArraySize::Int { .. }
                | BitArraySize::Variable { .. }
                | BitArraySize::Block { .. } => self.bit_array_size(size),
                BitArraySize::BinaryOperator { .. } => self.bit_array_size(size).surround("(", ")"),
            },

            Pattern::Variable { name, .. } => {
                self.variables.push(name);
                self.environment.next_local_var_name(name)
            }

            Pattern::Int { value, .. } => int(value),
            Pattern::Float { value, .. } => float(value),
            Pattern::String { value, .. } => string(value),

            Pattern::Constructor {
                arguments,
                constructor: Inferred::Known(PatternConstructor { name, .. }),
                ..
            } => self.tag_tuple_pattern(name, arguments),

            Pattern::Constructor {
                constructor: Inferred::Unknown,
                ..
            } => {
                panic!("Erlang generation performed with uninferred pattern constructor")
            }

            Pattern::Tuple { elements, .. } => {
                tuple(elements.iter().map(|pattern| self.print(pattern)))
            }

            Pattern::BitArray { segments, .. } => bit_array(
                segments
                    .iter()
                    .map(|s| self.pattern_segment(&s.value, &s.options)),
            ),

            Pattern::StringPrefix {
                left_side_string,
                right_side_assignment,
                left_side_assignment,
                ..
            } => {
                let right = match right_side_assignment {
                    AssignName::Variable(right) => {
                        self.variables.push(right);
                        self.environment.next_local_var_name(right)
                    }
                    AssignName::Discard(_) => "_".to_doc(),
                };

                if let Some((left_name, _)) = left_side_assignment {
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

                    self.assignments.push(StringPatternAssignment {
                        gleam_name: left_name.clone(),
                        erlang_name: self.environment.next_local_var_name(left_name),
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

    fn bit_array_size(&mut self, size: &'a TypedBitArraySize) -> Document<'a> {
        match size {
            BitArraySize::Int { value, .. } => int(value),
            BitArraySize::Block { inner, .. } => self.bit_array_size(inner).surround("(", ")"),
            BitArraySize::Variable {
                name, constructor, ..
            } => {
                let variant = &constructor
                    .as_ref()
                    .expect("Constructor not found for variable usage")
                    .variant;
                match variant {
                    ValueConstructorVariant::ModuleConstant { literal, .. } => {
                        const_inline(literal, self.environment)
                    }
                    ValueConstructorVariant::LocalVariable { .. }
                    | ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::Record { .. } => {
                        self.environment.local_var_name(name)
                    }
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
                        return self.bit_array_size_divide(left, right, "div");
                    }
                    IntOperator::Remainder => {
                        return self.bit_array_size_divide(left, right, "rem");
                    }
                };

                docvec![
                    self.bit_array_size(left),
                    operator,
                    self.bit_array_size(right)
                ]
            }
        }
    }

    fn bit_array_size_divide(
        &mut self,
        left: &'a TypedBitArraySize,
        right: &'a TypedBitArraySize,
        operator: &'static str,
    ) -> Document<'a> {
        if right.non_zero_compile_time_number() {
            return self.bit_array_size_operator(left, operator, right);
        }

        let left = self.bit_array_size(left);
        let right = self.bit_array_size(right);
        let denominator = self.environment.next_local_var_name("gleam@denominator");
        let clauses = docvec![
            line(),
            "0 -> 0;",
            line(),
            denominator.clone(),
            " -> ",
            binop_documents(left, operator, denominator)
        ];
        docvec!["case ", right, " of", clauses.nest(INDENT), line(), "end"]
    }

    fn bit_array_size_operator(
        &mut self,
        left: &'a TypedBitArraySize,
        operator: &'static str,
        right: &'a TypedBitArraySize,
    ) -> Document<'a> {
        let left = if let BitArraySize::BinaryOperator { .. } = left {
            self.bit_array_size(left).surround("(", ")")
        } else {
            self.bit_array_size(left)
        };
        let right = if let BitArraySize::BinaryOperator { .. } = right {
            self.bit_array_size(right).surround("(", ")")
        } else {
            self.bit_array_size(right)
        };
        binop_documents(left, operator, right)
    }

    fn tag_tuple_pattern(
        &mut self,
        name: &'a str,
        arguments: &'a [CallArg<TypedPattern>],
    ) -> Document<'a> {
        if arguments.is_empty() {
            atom_string(to_snake_case(name))
        } else {
            tuple(
                [atom_string(to_snake_case(name))]
                    .into_iter()
                    .chain(arguments.iter().map(|argument| self.print(&argument.value))),
            )
        }
    }

    fn pattern_list(
        &mut self,
        elements: &'a [TypedPattern],
        tail: Option<&'a TypedTailPattern>,
    ) -> Document<'a> {
        let elements = join(
            elements.iter().map(|element| self.print(element)),
            break_(",", ", "),
        );
        let tail = tail.map(|tail| self.print(&tail.pattern));
        list(elements, tail)
    }

    fn pattern_segment(
        &mut self,
        value: &'a TypedPattern,
        options: &'a [BitArrayOption<TypedPattern>],
    ) -> Document<'a> {
        let pattern_is_a_string_literal = matches!(value, Pattern::String { .. });
        let pattern_is_a_discard = matches!(value, Pattern::Discard { .. });

        let create_document = |this: &mut PatternPrinter<'a, 'env>| match value {
            Pattern::String { value, .. } => string_inner(value).surround("\"", "\""),
            Pattern::Discard { .. }
            | Pattern::Variable { .. }
            | Pattern::Int { .. }
            | Pattern::Float { .. } => this.print(value),

            Pattern::Assign { name, pattern, .. } => {
                this.variables.push(name);
                let variable_name = this.environment.next_local_var_name(name);

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
                        this.guards
                            .push(docvec![variable_name.clone(), " =:= ", string(value)]);
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

        let size = |value: &'a TypedPattern, this: &mut PatternPrinter<'a, 'env>| {
            Some(":".to_doc().append(this.print(value)))
        };

        let unit = |value: &'a u8| Some(eco_format!("unit:{value}").to_doc());

        bit_array_segment(
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
