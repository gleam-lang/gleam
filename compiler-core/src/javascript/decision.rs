// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

use super::{
    INDENT, bit_array_segment_int_value_to_bytes,
    expression::{self, Generator, Ordering, Scope, float, float_from_value},
};
use crate::{
    ast::{AssignmentKind, Endianness, TypedClause, TypedExpr, TypedPattern},
    exhaustiveness::{
        BitArrayMatchedValue, BitArrayTest, Body, BoundValue, CompiledCase, Decision,
        FallbackCheck, MatchTest, Offset, ReadAction, ReadSize, ReadType, RuntimeCheck,
        SizeOperator, SizeTest, Variable, VariableUsage,
    },
    javascript::{
        TypeVariant,
        expression::{eco_string_int, string},
        maybe_escape_property,
    },
    strings::{convert_string_escape_chars, length_utf16},
};
use ecow::{EcoString, eco_format};
use itertools::Itertools;
use num_bigint::BigInt;
use pretty_arena::*;
use src_span::SrcSpan;
use std::{collections::HashMap, sync::OnceLock};

pub const ASSIGNMENT_VAR: &str = "$";

pub fn case<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    compiled_case: &'a CompiledCase,
    clauses: &'a [TypedClause],
    subjects: &'a [TypedExpr],
    expression_generator: &mut Generator<'_, 'a, 'doc>,
) -> Document<'a, 'doc> {
    let scope_position = expression_generator.scope_position.clone();
    let mut variables = Variables::new(expression_generator, VariableAssignment::Declare);
    let assignments = variables.assign_case_subjects(arena, compiled_case, subjects);
    let mut printer = CasePrinter {
        variables,
        assignments: &assignments,
        kind: DecisionKind::Case { clauses },
    };

    let decision = match &compiled_case.tree {
        // Printing needs extra care if we're dealing with a sort of "degenerate"
        // tree that immediately starts with a guard node.
        // Code generation for guard nodes require defining variables outside of
        // the safe scope of the generated `if` statement. So if we were to just
        // generate code like usual we run the risk of leaking variables in the
        // outer scope:
        //
        // ```case
        // case 11 {
        //   n if n == 10 -> todo
        //   _ -> todo
        // }
        //
        // let n = 12
        // ```
        //
        // That case would have us generate something like this:
        //
        // ```js
        // let n = 11
        // if (n === 10) { todo } else { todo }
        //
        // // If we don't wrap it in a block that `n = 11` definition that was
        // // introduced would end up clashing with the `let n = 12` that comes
        // // later!
        // ```
        //
        // So in this special case we have to wrap everything in a block.
        tree @ Decision::Guard { .. } if !scope_position.is_tail() => break_block(
            arena,
            printer
                .inside_new_scope(|this| this.decision(arena, tree))
                .into_doc(arena),
        ),

        tree @ (Decision::Run { .. }
        | Decision::Guard { .. }
        | Decision::Switch { .. }
        | Decision::Fail) => printer.decision(arena, tree).into_doc(arena),
    };
    docvec![
        arena,
        subjects
            .first()
            .map(|subject| expression_generator.source_map_tracker(arena, subject.location().start))
            .unwrap_or(EMPTY_DOCUMENT),
        assignments_to_doc(arena, &mut *expression_generator, assignments),
        decision
    ]
    .force_break(arena)
}

/// The generated code for a decision tree.
enum CaseBody<'a, 'doc> {
    /// A JavaScript `if`` statement by itself. This can be merged with any
    /// preceding `else` statements to form an `else if` construct.
    If {
        check: Document<'a, 'doc>,
        body: Document<'a, 'doc>,
    },
    /// A sequence of statements. This must be wrapped as the body of an `if` or
    /// `else` statement.
    Statements(Document<'a, 'doc>),

    /// A JavaScript `if` statement followed by a single `else` clause. In some
    /// cases this can be flattened to reduce the size of the generated decision
    /// tree.
    IfElse {
        check: Document<'a, 'doc>,
        if_body: Document<'a, 'doc>,
        else_body: Document<'a, 'doc>,
        /// The decision in the tree that is used to generate the code for the
        /// `else` clause of this statement. If this is the same as another `if`-
        /// `else` statement, the two can be merged into one.
        fallback_decision: &'a Decision,
    },

    /// A JavaScript `if` statement followed by more than one `else` clause. This
    /// can sometimes be merged with preceding `else` statements in the same way
    /// that `if` can.
    IfElseChain(Document<'a, 'doc>),
}

impl<'a, 'doc> CaseBody<'a, 'doc> {
    fn into_doc(self, arena: &'doc DocumentArena<'a, 'doc>) -> Document<'a, 'doc> {
        match self {
            CaseBody::If { check, body } => docvec![
                arena,
                IF_SPACE_OPEN_PAREN_DOCUMENT,
                EMPTY_BREAK_DOCUMENT
                    .append(arena, check)
                    .nest(arena, INDENT)
                    .append(arena, EMPTY_BREAK_DOCUMENT)
                    .group(arena),
                CLOSE_PAREN_SPACE_DOCUMENT,
                break_block(arena, body)
            ],
            // If we have some code like the following:
            // ```javascript
            // if (some_condition) {
            //
            // } else {
            //   fallback()
            // }
            // ```
            //
            // Here, the body of the `if` statement is empty. This can happen
            // sometimes when generating decision trees for `let assert`.
            //
            // Instead, we can write this more concisely:
            // ```javascript
            // if (!some_condition) {
            //   fallback()
            // }
            // ```
            CaseBody::IfElse {
                check,
                if_body,
                else_body,
                ..
            } if if_body.is_empty() => docvec![
                arena,
                IF_SPACE_OPEN_PAREN_EXCLAMATION_OPEN_PAREN_DOCUMENT,
                EMPTY_BREAK_DOCUMENT
                    .append(arena, check)
                    .nest(arena, INDENT)
                    .append(arena, EMPTY_BREAK_DOCUMENT)
                    .group(arena),
                DOUBLE_CLOSE_PAREN_SPACE_DOCUMENT,
                else_body,
            ],
            CaseBody::IfElse {
                check,
                if_body,
                else_body,
                ..
            } => docvec![
                arena,
                IF_SPACE_OPEN_PAREN_DOCUMENT,
                EMPTY_BREAK_DOCUMENT
                    .append(arena, check)
                    .nest(arena, INDENT)
                    .append(arena, EMPTY_BREAK_DOCUMENT)
                    .group(arena),
                CLOSE_PAREN_SPACE_DOCUMENT,
                break_block(arena, if_body),
                SPACE_ELSE_SPACE_DOCUMENT,
                else_body,
            ],
            CaseBody::Statements(document) | CaseBody::IfElseChain(document) => document,
        }
    }

    /// Convert this value into the required document to put directly after an
    /// `else` keyword.
    fn document_after_else(self, arena: &'doc DocumentArena<'a, 'doc>) -> Document<'a, 'doc> {
        match self {
            // `if` and `if-else` statements can come directly after an `else` keyword
            CaseBody::If { .. } | CaseBody::IfElse { .. } => self.into_doc(arena),
            CaseBody::IfElseChain(document) => document,
            // Lists of statements must be wrapped in a block
            CaseBody::Statements(document) => break_block(arena, document),
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            CaseBody::If { .. } | CaseBody::IfElse { .. } => false,
            CaseBody::Statements(document) | CaseBody::IfElseChain(document) => document.is_empty(),
        }
    }
}

struct CasePrinter<'module, 'generator, 'a, 'assignments, 'doc> {
    variables: Variables<'generator, 'module, 'a, 'doc>,
    assignments: &'assignments Vec<SubjectAssignment<'a, 'doc>>,
    kind: DecisionKind<'a>,
}

/// Information specific to the different kinds of decision trees: `case`
/// expressions and `let assert` statements.
enum DecisionKind<'a> {
    Case {
        clauses: &'a [TypedClause],
    },
    LetAssert {
        kind: &'a AssignmentKind<TypedExpr>,
        subject_location: SrcSpan,
        pattern_location: SrcSpan,
        subject: EcoString,
    },
}

enum BodyExpression<'a, 'doc> {
    /// This happens when a case expression branch returns the same value that
    /// is being matched on. So instead of rebuilding it from scratch we can
    /// return the case subject directly. For example:
    /// `Ok(1) -> Ok(1)`
    /// `a -> a`
    /// `[1, ..rest] -> [1, ..rest]`
    ///
    Variable(Document<'a, 'doc>),

    /// This happens when a case expression has a complex body that is not just
    /// returning the matched subject. For example:
    /// `Ok(1) -> Ok(2)`
    /// `_ -> [1, 2, 3]`
    /// `1 -> "wibble"`
    ///
    Expressions(Document<'a, 'doc>),
}

/// Code generation for decision trees can look a bit daunting at a first glance
/// so let's go over the big idea to hopefully make it easier to understand why
/// the code is organised the way it is :)
///
/// > It might be helpful to go over the `exhaustiveness` module first and get
/// > familiar with the structure of the decision tree!
///
/// A decision tree has nodes that perform checks on pattern variables until
/// it reaches a body with an expression to run. This will be turned into a
/// series of if-else checks.
///
/// While on the surface it might sound pretty straightforward, the code generation
/// needs to take care of a couple of tricky aspects: when a check succeeds it's
/// not just allowing us to move to the next check, but it also introduces new
/// variables in the scope that we can reference. Let's look at an example:
///
/// ```gleam
/// case value {
///   [1, ..rest] -> rest
///   _ -> []
/// }
/// ```
///
/// Here we will first need to check that the list is not empty:
///
/// ```js
/// if (value instanceof $NonEmptyList) {
///   // ...
/// } else {
///   return [];
/// }
/// ```
///
/// Once that check succeeds we know that now we can access two new values: the
/// first element of the list and the rest of the list! So we need to keep track
/// of that in case further checks need to use those values; and in the example
/// above they actually do! The second check we need to perform will be on the
/// first item of the list, so we need to actually create a variable for it:
///
/// ```js
/// if (value instanceof $NonEmptyList) {
///   let $ = value.head;
///   if ($ === 1) {
///     // ...
///   } else {
///     // ...
///   }
/// } else {
///   return [];
/// }
/// ```
///
/// So, as we're generating code for each check and move further down the decision
/// tree, we will have to keep track of all the variables that we've discovered
/// after each successful check.
///
/// In order to do that we'll be using a `Variables` data structure to hold all
/// this information about the current scope.
///
impl<'a, 'doc> CasePrinter<'_, '_, 'a, '_, 'doc> {
    fn decision(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        decision: &'a Decision,
    ) -> CaseBody<'a, 'doc> {
        match decision {
            Decision::Fail => {
                if let DecisionKind::LetAssert {
                    kind,
                    subject_location,
                    pattern_location,
                    subject,
                } = &self.kind
                {
                    CaseBody::Statements(self.assignment_no_match(
                        arena,
                        subject.to_doc(arena),
                        kind,
                        *subject_location,
                        *pattern_location,
                    ))
                } else {
                    unreachable!("Invalid decision tree reached code generation")
                }
            }
            Decision::Run { body } => {
                let location = match self.kind {
                    DecisionKind::Case { clauses } => clauses
                        .get(body.clause_index)
                        .expect("invalid clause index")
                        .location(),
                    DecisionKind::LetAssert {
                        subject_location,
                        pattern_location,
                        ..
                    } => match self.variables.variable_assignment {
                        // When the variables are being declared, they
                        // correspond to the pattern.
                        VariableAssignment::Declare => pattern_location,
                        // When the variables are being assigned, they
                        // correspond to the subject.
                        VariableAssignment::Reassign => subject_location,
                    },
                };
                let source_map_tracker = self
                    .variables
                    .expression_generator
                    .source_map_tracker(arena, location.start);
                let bindings = docvec![
                    arena,
                    source_map_tracker,
                    self.variables.bindings_doc(arena, &body.bindings)
                ];
                let body = self.body_expression(arena, body.clause_index);
                let body = match body {
                    BodyExpression::Variable(variable) => variable,
                    BodyExpression::Expressions(body) => join_with_line(arena, bindings, body),
                };
                CaseBody::Statements(body)
            }
            Decision::Switch {
                var,
                choices,
                fallback,
                fallback_check,
            } => self.switch(arena, var, choices, fallback, fallback_check),
            Decision::Guard {
                guard,
                if_true,
                if_false,
            } => self.decision_guard(arena, *guard, if_true, if_false),
        }
    }

    fn body_expression(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        clause_index: usize,
    ) -> BodyExpression<'a, 'doc> {
        // If we are not in a `case` expression, there is no additional code to
        // execute when a branch matches; we only assign variables bound in the
        // pattern.
        let DecisionKind::Case { clauses } = &self.kind else {
            return BodyExpression::Expressions(EMPTY_DOCUMENT);
        };

        let clause = &clauses.get(clause_index).expect("invalid clause index");
        let body = &clause.then;

        if let Some(subject_index) = clause.returned_subject() {
            let variable = self
                .assignments
                .get(subject_index)
                .expect("case with no subjects")
                .name();

            BodyExpression::Variable(
                self.variables
                    .expression_generator
                    .wrap_return(arena, variable.to_doc(arena)),
            )
        } else {
            BodyExpression::Expressions(
                self.variables
                    .expression_generator
                    .expression_flattening_blocks(arena, body),
            )
        }
    }

    fn switch(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        var: &'a Variable,
        choices: &'a [(RuntimeCheck, Decision)],
        fallback: &'a Decision,
        fallback_check: &'a FallbackCheck,
    ) -> CaseBody<'a, 'doc> {
        // If there's just a single choice we can just generate the code for
        // it: no need to do any checking, we know it must match!
        if choices.is_empty() {
            // However, if the choice had an associated check (that is, it was
            // not just a simple catch all) we need to keep track of all the
            // variables brought into scope by the (always) successfull check.
            if let FallbackCheck::RuntimeCheck { check } = fallback_check {
                self.variables.record_check_assignments(arena, var, check);
            }

            // We can't use `inside_new_scope` here without care: the
            // code we generate goes directly into the enclosing scope
            // (there's no wrapping if-else block). User variables bound
            // in the branch go out of scope at its end, so their counters
            // are reset and later references resolve to the outer bindings.
            // Compiler synthesised variables (the `$` case subjects, `_pipe`,
            // `_block`, ...) can't be referenced by user code and their
            // declarations leak into this scope. Restoring only the user-variable
            // counters leaves the synthesised counters advanced, so later code
            // can't redeclare one of them.
            let old_user_variables = match &self.kind {
                DecisionKind::Case { .. } => self
                    .variables
                    .expression_generator
                    .current_scope
                    .user_variables()
                    .clone(),
                DecisionKind::LetAssert { .. } => im::HashMap::new(),
            };
            let old_names = self.variables.scoped_variable_names.clone();
            let old_segments = self.variables.segment_values.clone();
            let old_segment_names = self.variables.scoped_segment_names.clone();

            let result = self.decision(arena, fallback);

            match &self.kind {
                DecisionKind::Case { .. } => {
                    // Restore the user variables that were in scope before the
                    // branch. The synthesised counters are left advanced, and
                    // the high-water marks keep any user variable that leaked
                    // out of the branch from being redeclared by a later `let`.
                    self.variables
                        .expression_generator
                        .current_scope
                        .restore_user_variables(&old_user_variables);
                }
                DecisionKind::LetAssert { .. } => {}
            }

            self.variables.scoped_variable_names = old_names;
            self.variables.segment_values = old_segments;
            self.variables.scoped_segment_names = old_segment_names;
            return result;
        }

        // Otherwise we'll have to generate a series of if-else to check which
        // pattern is going to match!
        let mut assignments = vec![];
        if !self.variables.is_bound_in_scope(var) {
            // If the variable we need to perform a check on is not already bound
            // in scope we will be binding it to a new made up name. This way we
            // can also reference this exact name in further checks instead of
            // recomputing the value each time.
            let name = self.variables.next_local_var(&ASSIGNMENT_VAR.into());
            let value = self.variables.get_value(var);
            self.variables.bind(name.clone(), var);
            assignments.push(let_doc(arena, name, value.to_doc(arena)));
        }

        // Variable storing the character code for the first character of a string.
        // This is only declared if multiple patterns match on just the first
        // character, as it allows us to avoid calling `.startsWith` multiple times
        // and just call `.charCodeAt(0)` once, which is much faster.
        let first_character_variable = if multiple_single_character_prefix_checks(choices) {
            let name = self.variables.next_local_var(&ASSIGNMENT_VAR.into());
            let string = self.variables.get_value(var);
            let first_character = docvec![arena, string, DOT_CHAR_CODE_AT_ZERO_DOCUMENT];
            assignments.push(let_doc(arena, name.clone(), first_character));
            Some(name)
        } else {
            None
        };

        let mut if_ = CaseBody::Statements(EMPTY_DOCUMENT);
        for (i, (check, decision)) in choices.iter().enumerate() {
            self.variables.record_check_assignments(arena, var, check);

            // For each check we generate:
            // - the document to perform such check
            // - the body to run if the check is successful
            // - the assignments we need to bring all the bit array segments
            //   referenced by this check
            let (check_doc, body, mut segment_assignments) = self.inside_new_scope(|this| {
                let segment_assignments =
                    this.variables.bit_array_segment_assignments(arena, check);

                // If the pattern matches on a single character, use the character
                // code instead of `.startsWith`.
                let check_doc = if let Some(code) = single_character_prefix_code(check) {
                    let first_character = if let Some(variable) = &first_character_variable {
                        variable.to_doc(arena)
                    } else {
                        // This is the only single-character match in this `case`
                        // expression, so we don't bind it to a variable and just
                        // call `.charCodeAt` inline. This is still faster than
                        // `.startsWith`.
                        let string = this.variables.get_value(var);
                        docvec![arena, string, DOT_CHAR_CODE_AT_ZERO_DOCUMENT]
                    };

                    docvec![
                        arena,
                        first_character,
                        SPACE_TRIPLE_EQUAL_SPACE_DOCUMENT,
                        code
                    ]
                } else {
                    this.variables.runtime_check(arena, var, check)
                };

                let body = this.decision(arena, decision);
                (check_doc, body, segment_assignments)
            });
            assignments.append(&mut segment_assignments);

            let (check_doc, body) = match body {
                // If we have a statement like this:
                // ```javascript
                // if (x) {
                //   if (y) {
                //     ...
                //   }
                // }
                // ```
                //
                // We can transform it into:
                // ```javascript
                // if (x && y) {
                //   ...
                // }
                // ```
                CaseBody::If { check, body } => (
                    docvec![
                        arena,
                        check_doc,
                        SPACE_DOUBLE_AMPERSAND_BREAK_DOCUMENT,
                        check
                    ],
                    body,
                ),

                // The following code is a pretty common pattern in the code
                // generated by decision trees:
                //
                // ```javascript
                // if (something) {
                //   if (something_else) {
                //     do_thing()
                //   } else {
                //     do_fallback()
                //   }
                // } else {
                //   do_fallback()
                // }
                // ```
                //
                // Here, the `do_fallback()` branch is repeated, which we want
                // to avoid if possible. In this case, we can transform the above
                // code into the following:
                //
                // ```javascript
                // if (something && something_else) {
                //   do_thing()
                // } else {
                //   do_fallback()
                // }
                // ```
                //
                // This only works if both `else` branches run the same code,
                // otherwise we would be losing information.
                // It also only works if the inner statement has only a single
                // `else` clause, and not multiple `else if`s.
                CaseBody::IfElse {
                    check,
                    if_body,
                    fallback_decision: decision,
                    ..
                } if decision == fallback => (
                    docvec![
                        arena,
                        check_doc,
                        SPACE_DOUBLE_AMPERSAND_BREAK_DOCUMENT,
                        check
                    ],
                    if_body,
                ),

                if_else @ CaseBody::IfElse { .. } => (check_doc, if_else.into_doc(arena)),

                CaseBody::Statements(document) | CaseBody::IfElseChain(document) => {
                    (check_doc, document)
                }
            };

            if_ = match if_ {
                // The first statement will always be an `if`
                _ if i == 0 => CaseBody::If {
                    check: check_doc,
                    body,
                },
                // If this is the second check, the `if` becomes `else if`
                CaseBody::If { .. } | CaseBody::IfElse { .. } => CaseBody::IfElseChain(docvec![
                    arena,
                    if_.into_doc(arena),
                    " else if (",
                    EMPTY_BREAK_DOCUMENT
                        .append(arena, check_doc)
                        .nest(arena, INDENT)
                        .append(arena, EMPTY_BREAK_DOCUMENT)
                        .group(arena),
                    CLOSE_PAREN_SPACE_DOCUMENT,
                    break_block(arena, body)
                ]),
                CaseBody::IfElseChain(document) | CaseBody::Statements(document) => {
                    CaseBody::IfElseChain(docvec![
                        arena,
                        document,
                        " else if (",
                        EMPTY_BREAK_DOCUMENT
                            .append(arena, check_doc)
                            .nest(arena, INDENT)
                            .append(arena, EMPTY_BREAK_DOCUMENT)
                            .group(arena),
                        CLOSE_PAREN_SPACE_DOCUMENT,
                        break_block(arena, body)
                    ])
                }
            };
        }

        // In case there's some new variables we can extract after the
        // successful final check we store those. But we don't need to perform
        // the check itself: the type system ensures that, if we ever get here,
        // the check is going to match no matter what!
        if let FallbackCheck::RuntimeCheck { check } = fallback_check {
            self.variables.record_check_assignments(arena, var, check);
        }

        let else_body = self.inside_new_scope(|this| this.decision(arena, fallback));
        let document = if else_body.is_empty() {
            if_
        } else if let CaseBody::If {
            check,
            body: if_body,
        } = if_
        {
            CaseBody::IfElse {
                check,
                if_body,
                else_body: else_body.document_after_else(arena),
                fallback_decision: fallback,
            }
        } else {
            CaseBody::IfElseChain(docvec![
                arena,
                if_.into_doc(arena),
                SPACE_ELSE_SPACE_DOCUMENT,
                else_body.document_after_else(arena)
            ])
        };

        if assignments.is_empty() {
            document
        } else {
            CaseBody::Statements(join_with_line(
                arena,
                arena.join(assignments, LINE_DOCUMENT),
                document.into_doc(arena),
            ))
        }
    }

    fn inside_new_scope<A, F>(&mut self, run: F) -> A
    where
        F: Fn(&mut Self) -> A,
    {
        // Since we use reassignment for `let assert`, we can't reset the scope
        // as it loses data about the assigned variables.
        let old_scope = match &self.kind {
            DecisionKind::Case { .. } => self.variables.expression_generator.current_scope.clone(),
            DecisionKind::LetAssert { .. } => Scope::default(),
        };

        let old_names = self.variables.scoped_variable_names.clone();
        let old_segments = self.variables.segment_values.clone();
        let old_segment_names = self.variables.scoped_segment_names.clone();
        let output = run(self);

        match &self.kind {
            DecisionKind::Case { .. } => {
                self.variables.expression_generator.current_scope = old_scope;
            }
            DecisionKind::LetAssert { .. } => {}
        }

        self.variables.scoped_variable_names = old_names;
        self.variables.segment_values = old_segments;
        self.variables.scoped_segment_names = old_segment_names;
        output
    }

    fn decision_guard(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        guard: usize,
        if_true: &'a Body,
        if_false: &'a Decision,
    ) -> CaseBody<'a, 'doc> {
        let DecisionKind::Case { clauses } = &self.kind else {
            unreachable!("Guards cannot appear in let assert decision trees")
        };

        let guard = clauses
            .get(guard)
            .expect("invalid clause index")
            .guard
            .as_ref()
            .expect("missing guard");

        // Before generating the if-else condition we want to generate all the
        // assignments that will be needed by the guard condition so we can rest
        // assured they are in scope and the guard check can use those.
        let guard_variables = guard.referenced_variables();
        let (check_bindings, if_true_bindings): (Vec<_>, Vec<_>) = if_true
            .bindings
            .iter()
            .partition(|(variable, _)| guard_variables.contains(variable));

        let (check_bindings, check, if_true) = self.inside_new_scope(|this| {
            // check_bindings and if_true generation have to be in this scope so that pattern-bound
            // variables used in guards don't leak into other case branches (if_false).
            let check_bindings = this.variables.bindings_ref_doc(arena, &check_bindings);
            let check = this.variables.expression_generator.guard(arena, guard);
            // All the other bindings that are not needed by the guard check will
            // end up directly in the body of the if clause.
            let if_true_bindings = this.variables.bindings_ref_doc(arena, &if_true_bindings);
            let if_true_body = this.body_expression(arena, if_true.clause_index);
            let if_true = match if_true_body {
                BodyExpression::Variable(variable) => variable,
                BodyExpression::Expressions(if_true_body) => {
                    join_with_line(arena, if_true_bindings, if_true_body)
                }
            };
            (check_bindings, check, if_true)
        });

        let if_false_body = self.inside_new_scope(|this| this.decision(arena, if_false));

        // We can now piece everything together into a case body!
        let if_ = if if_false_body.is_empty() {
            CaseBody::If {
                check,
                body: if_true,
            }
        } else {
            CaseBody::IfElse {
                check,
                if_body: if_true,
                else_body: if_false_body.document_after_else(arena),
                fallback_decision: if_false,
            }
        };

        if check_bindings.is_empty() {
            if_
        } else {
            CaseBody::Statements(join_with_line(arena, check_bindings, if_.into_doc(arena)))
        }
    }

    fn assignment_no_match(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        subject: Document<'a, 'doc>,
        kind: &'a AssignmentKind<TypedExpr>,
        subject_location: SrcSpan,
        pattern_location: SrcSpan,
    ) -> Document<'a, 'doc> {
        let AssignmentKind::Assert {
            location, message, ..
        } = kind
        else {
            unreachable!("inexhaustive let made it to code generation");
        };

        let generator = &mut self.variables.expression_generator;
        let message = match message {
            None => string(arena, "Pattern match failed, no pattern matched the value."),
            Some(message) => generator.not_in_tail_position(Some(Ordering::Strict), |this| {
                this.wrap_expression(arena, message)
            }),
        };
        generator.throw_error(
            arena,
            "let_assert",
            &message,
            *location,
            [
                ("value", subject),
                ("start", location.start.to_doc(arena)),
                ("end", subject_location.end.to_doc(arena)),
                ("pattern_start", pattern_location.start.to_doc(arena)),
                ("pattern_end", pattern_location.end.to_doc(arena)),
            ],
        )
    }
}

/// Returns the character code for the character being matched for patterns matching
/// on single-character string prefixes.
fn single_character_prefix_code(check: &RuntimeCheck) -> Option<u32> {
    match check {
        // On JavaScript, a single "character" is one that can be represented as
        // a single UTF-16 codepoint.
        RuntimeCheck::StringPrefix { prefix, .. } if utf16_no_escape_len(prefix) == 1 => {
            convert_string_escape_chars(prefix)
                .chars()
                .next()
                .map(|first| first as u32)
        }
        RuntimeCheck::Int { .. }
        | RuntimeCheck::Float { .. }
        | RuntimeCheck::String { .. }
        | RuntimeCheck::StringPrefix { .. }
        | RuntimeCheck::Tuple { .. }
        | RuntimeCheck::BitArray { .. }
        | RuntimeCheck::Variant { .. }
        | RuntimeCheck::NonEmptyList { .. }
        | RuntimeCheck::EmptyList => None,
    }
}

/// Returns whether a `case` expression contains multiple patterns matching on
/// the first character of a string.
fn multiple_single_character_prefix_checks(choices: &[(RuntimeCheck, Decision)]) -> bool {
    let mut encountered_check = false;

    for (check, _) in choices.iter() {
        if let RuntimeCheck::StringPrefix { prefix, .. } = check
            && utf16_no_escape_len(prefix) == 1
        {
            if encountered_check {
                return true;
            } else {
                encountered_check = true;
            }
        }
    }

    false
}

pub fn let_<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    compiled_case: &'a CompiledCase,
    subject: &'a TypedExpr,
    kind: &'a AssignmentKind<TypedExpr>,
    expression_generator: &mut Generator<'_, 'a, 'doc>,
    pattern: &'a TypedPattern,
) -> Document<'a, 'doc> {
    let scope_position = expression_generator.scope_position.clone();
    let variable_assignment_kind = match &compiled_case.tree {
        // If the binding is exhaustive (so no runtime checks need to be done), then
        // assignments don't need to be separate from declarations.
        Decision::Switch {
            choices, fallback, ..
        } if choices.is_empty() && matches!(**fallback, Decision::Run { .. }) => {
            VariableAssignment::Declare
        }
        Decision::Run { .. }
        | Decision::Guard { .. }
        | Decision::Switch { .. }
        | Decision::Fail => VariableAssignment::Reassign,
    };
    let mut variables = Variables::new(expression_generator, variable_assignment_kind);

    let assignment = variables.assign_let_subject(arena, compiled_case, subject);
    let assignment_name = assignment.name();
    let assignments = vec![assignment];
    let pattern_location = pattern.location();
    let decision = CasePrinter {
        variables,
        assignments: &assignments,
        kind: DecisionKind::LetAssert {
            kind,
            subject_location: subject.location(),
            pattern_location,
            subject: assignment_name.clone(),
        },
    }
    .decision(arena, &compiled_case.tree);

    let assignments_doc = assignments_to_doc(arena, expression_generator, assignments);

    let beginning_assignments = match variable_assignment_kind {
        // If the decision tree will generate declarations, don't declare them here.
        VariableAssignment::Declare => EMPTY_DOCUMENT,
        // If the decision tree will only assign, declare the variables here.
        VariableAssignment::Reassign => {
            // When we generate `let assert` statements, we want to produce code like
            // this:
            // ```javascript
            // let some_var;
            // let other_var;
            // if (condition_to_check_pattern) {
            //   some_var = x;
            //   other_var = y;
            // }
            // ```
            // This generates the code for binding the initial variables before the
            // check so the scoping of them is correct.
            //
            // We must generate this after we generate the code for the decision tree
            // itself as we might be re-binding variables which are used in the checks
            // to determine whether the pattern matches or not.
            arena.concat(pattern.bound_variables().into_iter().map(|bound_variable| {
                docvec![
                    arena,
                    LET_SPACE_DOCUMENT,
                    expression_generator.local_var(&bound_variable.name()),
                    SEMICOLON_DOCUMENT,
                    LINE_DOCUMENT
                ]
            }))
        }
    };

    let doc = docvec![
        arena,
        assignments_doc,
        beginning_assignments,
        decision.into_doc(arena)
    ];

    match scope_position {
        expression::Position::Expression(_) | expression::Position::Statement => doc,
        expression::Position::Tail => {
            docvec![
                arena,
                doc,
                LINE_DOCUMENT,
                "return ",
                assignment_name,
                SEMICOLON_DOCUMENT
            ]
        }
        expression::Position::Assign(variable) => {
            docvec![
                arena,
                doc,
                LINE_DOCUMENT,
                variable,
                SPACE_EQUAL_SPACE_DOCUMENT,
                assignment_name,
                SEMICOLON_DOCUMENT
            ]
        }
    }
}

#[derive(Copy, Clone)]
enum VariableAssignment {
    Declare,
    Reassign,
}

/// This is a useful piece of state that is kept separate from the generator
/// itself so we can reuse it both with `case`s and `let`s without rewriting
/// everything from scratch.
///
struct Variables<'generator, 'module, 'a, 'doc> {
    expression_generator: &'generator mut Generator<'module, 'a, 'doc>,

    /// Whether to bind variables using `let` as we do in `case` expressions,
    /// or to reassign them as we do in `let assert` statements.
    variable_assignment: VariableAssignment,

    /// All the pattern variables will be assigned a specific value: being bound
    /// to a constructor field, tuple element and so on. Pattern variables never
    /// end up in the generated code but we replace them with their actual value.
    /// We store those values as `EcoString`s in this map; the key is the pattern
    /// variable's unique id.
    ///
    variable_values: HashMap<usize, EcoString>,

    /// The same happens for bit array segments. Unlike pattern variables, we
    /// identify those using their names and store their value as a `Document`.
    segment_values: HashMap<EcoString, Document<'a, 'doc>>,

    /// When we discover new variables after a runtime check we don't immediately
    /// generate assignments for each of them, because that could lead to wasted
    /// work. Let's consider the following check:
    ///
    /// ```txt
    /// a is Wibble(3, c, 1) -> c
    /// a is _ -> 1
    /// ```
    ///
    /// If we generated variables for it as soon as we enter its corresponding
    /// branch we would find ourselves with this piece of code:
    ///
    /// ```js
    /// if (a instanceof Wibble) {
    ///   let a$0 = wibble.0;
    ///   let a$1 = wibble.1;
    ///   let a$2 = wibble.2;
    ///
    ///   // and now we go on checking these new variables
    /// }
    /// ```
    ///
    /// However, by extracting all the fields immediately we might end up doing
    /// wasted work: as soon as we find out that `a$0 != 3` we don't even need
    /// to check the other fields, we know the pattern can't match! So we
    /// extracted two fields we're not even checking.
    ///
    /// To avoid this situation, we only bind a variable to a name right before
    /// we're checking it so we're sure we're never generating useless bindings.
    /// The previous example would become something like this:
    ///
    /// ```js
    /// if (a instanceof Wibble) {
    ///   let a$0 = wibble.0;
    ///   if (a$0 === 3) {
    ///     let a$2 = wibble.2
    ///     // further checks
    ///   } else {
    ///     return 1;
    ///   }
    /// }
    /// ```
    ///
    /// In this map we store the name a variable is bound to in the current
    /// scope. For example here we know that `wibble.0` is bound to the name
    /// `a$0`.
    ///
    scoped_variable_names: HashMap<usize, EcoString>,

    /// Once again, this is the same as `scoped_variable_names` with the
    /// difference that a segment is identified by its name.
    ///
    scoped_segment_names: HashMap<EcoString, EcoString>,
}

impl<'generator, 'module, 'a, 'doc> Variables<'generator, 'module, 'a, 'doc> {
    fn new(
        expression_generator: &'generator mut Generator<'module, 'a, 'doc>,
        variable_assignment: VariableAssignment,
    ) -> Self {
        Variables {
            expression_generator,
            variable_assignment,
            variable_values: HashMap::new(),
            scoped_variable_names: HashMap::new(),
            segment_values: HashMap::new(),
            scoped_segment_names: HashMap::new(),
        }
    }

    /// Give a unique name to each of the subjects of a case expression and keep
    /// track of each of those names in case it needs to be referenced later.
    ///
    fn assign_case_subjects(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        compiled_case: &'a CompiledCase,
        subjects: &'a [TypedExpr],
    ) -> Vec<SubjectAssignment<'a, 'doc>> {
        let assignments = subjects
            .iter()
            .map(|subject| {
                assign_subject(arena, self.expression_generator, subject, Ordering::Strict)
            })
            .collect_vec();

        for (variable, assignment) in compiled_case
            .subject_variables
            .iter()
            .zip(assignments.iter())
        {
            // We need to record the fact that each subject corresponds to a
            // pattern variable.
            self.set_value(variable, assignment.name());
            self.bind(assignment.name(), variable);
        }

        assignments
    }

    /// Give a unique name to the subject of a let expression (if it needs one
    /// and it's not already a variable) and keep track of that name in case it
    /// needs to be referenced later.
    ///
    fn assign_let_subject(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        compiled_case: &'a CompiledCase,
        subject: &'a TypedExpr,
    ) -> SubjectAssignment<'a, 'doc> {
        let variable = compiled_case
            .subject_variables
            .first()
            .expect("decision tree with no subjects");
        let assignment = assign_subject(arena, self.expression_generator, subject, Ordering::Loose);
        self.set_value(variable, assignment.name());
        self.bind(assignment.name(), variable);
        assignment
    }

    fn local_var(&mut self, name: &EcoString) -> EcoString {
        self.expression_generator.local_var(name)
    }

    fn next_local_var(&mut self, name: &EcoString) -> EcoString {
        self.expression_generator.next_local_var(name)
    }

    /// Records that a given pattern `variable` has been assigned a runtime
    /// `value`. For example if we had something like this:
    ///
    /// ```txt
    /// a is Wibble(1, b) -> todo
    /// ```
    ///
    /// After a successful `is Wibble` check, we know we'd end up with two
    /// additional checks that look like this:
    ///
    /// ```txt
    /// a0 is 1, a1 is b -> todo
    /// ```
    ///
    /// But what's the runtime value of `a0` and `a1`? To get those we'd have to
    /// extract the two fields from `a`, so they would have a value that looks
    /// like this: `a[0]` and `a[1]`; these values are set with this `set_value`
    /// function as we discover them.
    ///
    fn set_value(&mut self, variable: &Variable, value: EcoString) {
        let _ = self.variable_values.insert(variable.id, value);
    }

    /// This is conceptually the same as set value, but it's for bit array
    /// segments instead of pattern variables.
    fn set_segment_value(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        bit_array: &Variable,
        segment_name: EcoString,
        read_action: &ReadAction,
    ) {
        let value = self.read_action_to_doc(arena, bit_array, read_action);
        let _ = self.segment_values.insert(segment_name, value);
    }

    /// During the code generation process we might end up having to generate
    /// code to materialises one of the pattern variables and gives it a name to
    /// be used to avoid repeating it every single time.
    ///
    /// For example if a pattern variable is referencing the fifth element in a
    /// list it's runtime value would look something like this:
    /// `list.tail.tail.tail.tail.head`; if we where to perform additional
    /// checks on this value, it would be quite wasteful to recompute it every
    /// single time. Imagine this piece of code:
    ///
    /// ```gleam
    /// case list {
    ///   [_, _, _, _, 1] -> todo
    ///   [_, _, _, _, 2] -> todo
    ///   // ...
    ///   _ -> todo
    /// }
    /// ```
    ///
    /// The corresponding check would end up looking something like this:
    ///
    /// ```js
    /// if (list.tail.tail.tail.tail.head === 1) {}
    /// else if (list.tail.tail.tail.tail.head === 2) {}
    /// // ...
    /// else {}
    /// ```
    ///
    /// So before a check we might want to bind a pattern variable to a name so
    /// we can use that to reference it in the check:
    ///
    /// ```js
    /// let $ = list.tail.tail.tail.tail.head;
    /// if ($ === 1) {}
    /// else if ($ === 2) {}
    /// // ...
    /// else {}
    /// ```
    ///
    /// This makes for neater code! These bindings are kept track of with this
    /// function.
    ///
    fn bind(&mut self, name: EcoString, variable: &Variable) {
        let _ = self.scoped_variable_names.insert(variable.id, name);
    }

    /// This has the exact same purpose as `bind` but works with bit array
    /// segments instead of pattern variables introduced during the decision
    /// tree compilation.
    ///
    fn bind_segment(&mut self, bound_to_variable: EcoString, segment: EcoString) {
        let _ = self.scoped_segment_names.insert(segment, bound_to_variable);
    }

    fn bindings_doc(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        bindings: &'a [(EcoString, BoundValue)],
    ) -> Document<'a, 'doc> {
        let bindings = (bindings.iter())
            .map(|(variable, value)| self.body_binding_doc(arena, variable, value));
        arena.join(bindings, LINE_DOCUMENT)
    }

    fn bindings_ref_doc(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        bindings: &[&'a (EcoString, BoundValue)],
    ) -> Document<'a, 'doc> {
        let bindings = (bindings.iter())
            .map(|(variable, value)| self.body_binding_doc(arena, variable, value));
        arena.join(bindings, LINE_DOCUMENT)
    }

    fn body_binding_doc(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        variable_name: &'a EcoString,
        value: &'a BoundValue,
    ) -> Document<'a, 'doc> {
        let local_variable_name = self.next_local_var(variable_name);
        let assigned_value = match value {
            BoundValue::Variable(variable) => self.get_value(variable).to_doc(arena),
            BoundValue::LiteralString(value) => string(arena, value),
            BoundValue::LiteralFloat(value) => float(arena, value),
            BoundValue::LiteralInt(value) => eco_string_int(arena, eco_format!("{value}")),
            BoundValue::BitArraySlice {
                bit_array,
                read_action,
            } => self
                .get_segment_value(arena, variable_name)
                .unwrap_or_else(|| self.read_action_to_doc(arena, bit_array, read_action)),
            BoundValue::StringSlice { subject, prefix } => {
                let subject_value = self.get_value(subject);
                let prefix_size = utf16_no_escape_len(prefix);
                docvec![arena, subject_value, ".slice(", prefix_size, ")"]
            }
        };

        match self.variable_assignment {
            VariableAssignment::Declare => let_doc(arena, local_variable_name, assigned_value),
            VariableAssignment::Reassign => {
                reassignment_doc(arena, local_variable_name, assigned_value)
            }
        }
    }

    /// Generates the document to perform a (possibly negated) runtime check on
    /// the given variable.
    ///
    fn runtime_check(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        variable: &Variable,
        runtime_check: &'a RuntimeCheck,
    ) -> Document<'a, 'doc> {
        let value = self.get_value(variable);

        match runtime_check {
            RuntimeCheck::String { value: expected } => {
                docvec![
                    arena,
                    value,
                    SPACE_TRIPLE_EQUAL_SPACE_DOCUMENT,
                    string(arena, expected)
                ]
            }
            RuntimeCheck::Float {
                float_value: expected,
            } => docvec![
                arena,
                value,
                SPACE_TRIPLE_EQUAL_SPACE_DOCUMENT,
                float_from_value(arena, expected.value())
            ],
            RuntimeCheck::Int {
                int_value: expected,
            } => docvec![
                arena,
                value,
                SPACE_TRIPLE_EQUAL_SPACE_DOCUMENT,
                expected.clone()
            ],
            RuntimeCheck::StringPrefix { prefix, .. } => {
                docvec![
                    arena,
                    value,
                    DOT_STARTS_WITH_OPEN_PAREN_DOCUMENT,
                    string(arena, prefix),
                    CLOSE_PAREN_DOCUMENT
                ]
            }

            RuntimeCheck::BitArray { test } => match test {
                // In this case we need to check that the remaining part of the
                // bit array has a whole number of bytes.
                BitArrayTest::CatchAllIsBytes { size_so_far } => {
                    if size_so_far.is_zero() {
                        docvec![
                            arena,
                            value,
                            DOT_BIT_SIZE_MODULO_8_DOCUMENT,
                            SPACE_TRIPLE_EQUAL_SPACE_DOCUMENT,
                            ZERO_DOCUMENT,
                        ]
                    } else {
                        let size_so_far = self.offset_to_doc(arena, size_so_far, true);
                        let remaining_bits =
                            docvec![arena, value, DOT_BIT_SIZE_MINUS_SPACE_DOCUMENT, size_so_far];
                        docvec![
                            arena,
                            OPEN_PAREN_DOCUMENT,
                            remaining_bits,
                            CLOSE_PAREN_MODULO_8_DOCUMENT,
                            SPACE_TRIPLE_EQUAL_SPACE_DOCUMENT,
                            ZERO_DOCUMENT
                        ]
                    }
                }

                BitArrayTest::ReadSizeIsNotNegative { size } => {
                    docvec![
                        arena,
                        self.read_size_to_doc(arena, size).expect("empty size"),
                        SPACE_GT_EQ_ZERO_DOCUMENT
                    ]
                }

                BitArrayTest::SegmentIsFiniteFloat {
                    read_action:
                        ReadAction {
                            from: start,
                            size,
                            endianness,
                            ..
                        },
                } => {
                    let start_doc = self.offset_to_doc(arena, start, false);
                    let end = match (start.constant_bits(), size.constant_bits()) {
                        (Some(start), _) if start == BigInt::ZERO => self
                            .read_size_to_doc(arena, size)
                            .expect("unexpected catch all size"),
                        (Some(start), Some(end)) => (start + end).to_doc(arena),
                        (_, _) => {
                            docvec![
                                arena,
                                start_doc,
                                SPACE_PLUS_SPACE_DOCUMENT,
                                self.read_size_to_doc(arena, size).expect("empty size")
                            ]
                        }
                    };
                    let check =
                        self.bit_array_slice_to_float(arena, value, start_doc, end, endianness);

                    docvec![
                        arena,
                        NUMBER_DOT_IS_FINITE_OPEN_PAREN_DOCUMENT,
                        check,
                        CLOSE_PAREN_DOCUMENT
                    ]
                }

                // Here we need to make sure that the bit array has a specific
                // size.
                BitArrayTest::Size(SizeTest { operator, size }) => {
                    let operator = match operator {
                        SizeOperator::GreaterEqual => SPACE_GT_EQ_SPACE_DOCUMENT,
                        SizeOperator::Equal => SPACE_TRIPLE_EQUAL_SPACE_DOCUMENT,
                    };
                    let size = self.offset_to_doc(arena, size, false);
                    docvec![arena, value, DOT_BIT_SIZE_DOCUMENT, operator, size]
                }

                // Finally, here we need to check that a given portion of the
                // bit array matches a given value.
                BitArrayTest::Match(MatchTest {
                    value: expected,
                    read_action,
                }) => match expected {
                    BitArrayMatchedValue::LiteralString {
                        value: _,
                        encoding: _,
                        bytes: expected,
                    } => {
                        self.literal_string_segment_bytes_check(arena, value, expected, read_action)
                    }
                    BitArrayMatchedValue::LiteralFloat(expected) => {
                        self.literal_float_segment_bytes_check(arena, value, expected, read_action)
                    }
                    BitArrayMatchedValue::LiteralInt {
                        value: expected, ..
                    } => self.literal_int_segment_bytes_check(
                        arena,
                        value,
                        expected.clone(),
                        read_action,
                    ),
                    BitArrayMatchedValue::Variable(..)
                    | BitArrayMatchedValue::Discard(..)
                    | BitArrayMatchedValue::Assign { .. } => {
                        panic!("unreachable")
                    }
                },
            },

            // When checking on a tuple there's always going to be a single choice
            // and the code generation will always skip generating the check for it
            // as the type system ensures it must match.
            RuntimeCheck::Tuple { .. } => unreachable!("tried generating runtime check for tuple"),

            // Some variants like `Bool` and `Result` are special cased and checked
            // in a different way from all other variants.
            RuntimeCheck::Variant { match_, .. } if variable.type_.is_bool() => {
                match match_.used_name().as_str() {
                    "True" => value.to_doc(arena),
                    _ => docvec![arena, EXCLAMATION_MARK_DOCUMENT, value],
                }
            }

            RuntimeCheck::Variant {
                match_,
                index,
                fields,
                ..
            } => {
                if variable.type_.is_result() && match_.module().is_none() {
                    if *index == 0 {
                        self.expression_generator.tracker.ok_used = true;
                    } else {
                        self.expression_generator.tracker.error_used = true;
                    }
                }

                let qualification = match_
                    .module()
                    .map(|module| eco_format!("${module}."))
                    .unwrap_or_default();

                // If this variant has no fields, register it as being used
                // so that we know to import it.
                if fields.is_empty()
                    && let Some((package, module, type_name)) =
                        variable.type_.named_type_name_and_package()
                {
                    _ = self
                        .expression_generator
                        .tracker
                        .variants_used_in_instanceof
                        .insert(TypeVariant {
                            package,
                            module,
                            type_name,
                            name: match_.variant_name(),
                        });
                }

                docvec![
                    arena,
                    value,
                    SPACE_INSTANCE_OF_SPACE_DOCUMENT,
                    qualification,
                    match_.used_name()
                ]
            }

            RuntimeCheck::NonEmptyList { .. } => {
                self.expression_generator.tracker.list_non_empty_class_used = true;
                docvec![arena, value, SPACE_INSTANCE_OF_NON_EMPTY_DOCUMENT]
            }

            RuntimeCheck::EmptyList => {
                self.expression_generator.tracker.list_empty_class_used = true;
                docvec![arena, value, SPACE_INSTANCE_OF_EMPTY_DOCUMENT]
            }
        }
    }

    /// Turns a read action into a document that can be used to extract the
    /// corresponding value from the given bit array and assign it to a
    /// variable.
    ///
    fn read_action_to_doc(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        bit_array: &Variable,
        read_action: &ReadAction,
    ) -> Document<'a, 'doc> {
        let ReadAction {
            from,
            size,
            type_,
            endianness,
            signed,
        } = read_action;
        let bit_array = self.get_value(bit_array);
        let from_bits = from.constant_bits();

        // There's two special cases we need to take care of:
        match (size, &from_bits) {
            // If we're reading a single byte as un unsigned int from a byte aligned
            // offset then we can optimise this call as a `.byteAt` call!
            (ReadSize::ConstantBits(size), Some(from_bits))
                if type_.is_int()
                    && *size == BigInt::from(8)
                    && !signed
                    && from_bits.clone() % 8 == BigInt::ZERO =>
            {
                let from_byte: BigInt = from_bits / 8;
                return docvec![
                    arena,
                    bit_array,
                    DOT_BYTE_AT_OPEN_PAREN_DOCUMENT,
                    from_byte,
                    CLOSE_PAREN_DOCUMENT
                ];
            }

            // If we're reading all the remaining bits/bytes of an array we'll
            // take the remaining slice.
            (ReadSize::RemainingBits | ReadSize::RemainingBytes, _) => {
                return self.bit_array_slice(arena, bit_array, from);
            }

            _ => (),
        }

        // Otherwise we'll take a regular slice out of the bit array, depending
        // on the type of the segment.
        let (start, end) =
            if let (ReadSize::ConstantBits(size), Some(from_bits)) = (size, from_bits) {
                // If both the start and and are known at compile time we can use
                // those directly in the slice call and perform no addition at
                // runtime.
                let start = from_bits.clone().to_doc(arena);
                let end = (from_bits + size).to_doc(arena);
                (start, end)
            } else {
                // Otherwise we'll have to sum the variable part and the constant
                // one to tell how long the slice should be.
                let size = self
                    .read_size_to_doc(arena, size)
                    .expect("no variable size");
                let start = self.offset_to_doc(arena, from, false);
                let end = if from.is_zero() {
                    size
                } else {
                    docvec![arena, start, SPACE_PLUS_SPACE_DOCUMENT, size]
                };
                (start, end)
            };

        match type_ {
            ReadType::Int => {
                self.bit_array_slice_to_int(arena, bit_array, start, end, endianness, *signed)
            }
            ReadType::Float => {
                self.bit_array_slice_to_float(arena, bit_array, start, end, endianness)
            }
            ReadType::BitArray => self.bit_array_slice_with_end(arena, bit_array, from, end),
            ReadType::String | ReadType::UtfCodepoint => {
                panic!("invalid slice type made it to code generation: {type_:#?}")
            }
        }
    }

    fn offset_to_doc(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        offset: &Offset,
        parenthesise: bool,
    ) -> Document<'a, 'doc> {
        if offset.is_zero() {
            return ZERO_DOCUMENT;
        }

        let mut pieces = vec![];
        if offset.constant != BigInt::ZERO {
            pieces.push(eco_string_int(arena, offset.constant.to_string().into()));
        }

        for (variable, times) in offset
            .variables
            .iter()
            .sorted_by(|(one, _), (other, _)| one.name().cmp(other.name()))
        {
            let mut variable = match variable {
                VariableUsage::PatternSegment(segment_name, _) => self
                    .get_segment_value(arena, segment_name)
                    .expect("segment referenced in a check before being created"),
                VariableUsage::OutsideVariable(name) => self.local_var(name).to_doc(arena),
            };
            if *times != 1 {
                variable = variable
                    .append(arena, SPACE_TIMES_SPACE_DOCUMENT)
                    .append(arena, *times);
            }
            pieces.push(variable.to_doc(arena));
        }

        for calculation in offset.calculations.iter() {
            let left = self.offset_to_doc(arena, &calculation.left, true);
            let right = self.offset_to_doc(arena, &calculation.right, true);

            let calculation = self.expression_generator.bin_op_with_doc_operands(
                arena,
                calculation.operator.to_bin_op(),
                left,
                right,
                &crate::type_::int(),
            );

            if parenthesise {
                pieces.push(calculation.surround(arena, OPEN_PAREN_DOCUMENT, CLOSE_PAREN_DOCUMENT));
            } else {
                pieces.push(calculation);
            }
        }

        if pieces.len() > 1 && parenthesise {
            docvec![
                arena,
                OPEN_PAREN_DOCUMENT,
                arena.join(pieces, SPACE_PLUS_SPACE_DOCUMENT),
                CLOSE_PAREN_DOCUMENT
            ]
        } else {
            arena.join(pieces, SPACE_PLUS_SPACE_DOCUMENT)
        }
    }

    /// If the read size has a constant value (that is, it's not a "read all the
    /// remaining bits/bytes") this returns a document representing that size.
    /// Otherwise it returns an empty document.
    ///
    fn read_size_to_doc(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        size: &ReadSize,
    ) -> Option<Document<'a, 'doc>> {
        match size {
            ReadSize::ConstantBits(value) => Some(value.clone().to_doc(arena)),
            ReadSize::VariableBits { variable, unit } => {
                let variable = self.local_var(variable.name());
                Some(if *unit == 1 {
                    variable.to_doc(arena)
                } else {
                    docvec![arena, variable, SPACE_TIMES_SPACE_DOCUMENT, *unit as i64]
                })
            }
            ReadSize::RemainingBits | ReadSize::RemainingBytes => None,

            ReadSize::BinaryOperator {
                left,
                right,
                operator,
            } => {
                let left = if self.read_size_must_be_wrapped(left) {
                    self.read_size_to_doc(arena, left)?.surround(
                        arena,
                        OPEN_PAREN_DOCUMENT,
                        CLOSE_PAREN_DOCUMENT,
                    )
                } else {
                    self.read_size_to_doc(arena, left)?
                };
                let right = if self.read_size_must_be_wrapped(right) {
                    self.read_size_to_doc(arena, right)?.surround(
                        arena,
                        OPEN_PAREN_DOCUMENT,
                        CLOSE_PAREN_DOCUMENT,
                    )
                } else {
                    self.read_size_to_doc(arena, right)?
                };

                Some(self.expression_generator.bin_op_with_doc_operands(
                    arena,
                    operator.to_bin_op(),
                    left,
                    right,
                    &crate::type_::int(),
                ))
            }
        }
    }

    fn read_size_must_be_wrapped(&self, size: &ReadSize) -> bool {
        match size {
            ReadSize::ConstantBits(_) | ReadSize::RemainingBits | ReadSize::RemainingBytes => false,

            ReadSize::VariableBits { unit, .. } => *unit != 1,
            ReadSize::BinaryOperator { .. } => true,
        }
    }

    /// Generates the document that calls the `bitArraySliceToInt` function, with
    /// the given arguments.
    ///
    fn bit_array_slice_to_int(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        bit_array: impl Documentable<'a, 'doc>,
        start: impl Documentable<'a, 'doc>,
        end: impl Documentable<'a, 'doc>,
        endianness: &Endianness,
        signed: bool,
    ) -> Document<'a, 'doc> {
        self.expression_generator
            .tracker
            .bit_array_slice_to_int_used = true;

        let endianness = match endianness {
            Endianness::Big => TRUE_LOWERCASE_DOCUMENT,
            Endianness::Little => FALSE_LOWERCASE_DOCUMENT,
        };
        let signed = if signed {
            TRUE_LOWERCASE_DOCUMENT
        } else {
            FALSE_LOWERCASE_DOCUMENT
        };
        let arguments = arena.join(
            [
                bit_array.to_doc(arena),
                start.to_doc(arena),
                end.to_doc(arena),
                endianness.to_doc(arena),
                signed.to_doc(arena),
            ],
            COMMA_SPACE_DOCUMENT,
        );
        docvec![
            arena,
            BIT_ARRAY_SLICE_TO_INT_OPEN_PAREN_DOCUMENT,
            arguments,
            CLOSE_PAREN_DOCUMENT
        ]
    }

    /// Generates the document that calls the `bitArraySliceToFloat` function,
    /// with the given arguments.
    ///
    fn bit_array_slice_to_float(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        bit_array: impl Documentable<'a, 'doc>,
        start: impl Documentable<'a, 'doc>,
        end: impl Documentable<'a, 'doc>,
        endianness: &Endianness,
    ) -> Document<'a, 'doc> {
        self.expression_generator
            .tracker
            .bit_array_slice_to_float_used = true;

        let endianness = match endianness {
            Endianness::Big => TRUE_LOWERCASE_DOCUMENT,
            Endianness::Little => FALSE_LOWERCASE_DOCUMENT,
        };
        let arguments = arena.join(
            [
                bit_array.to_doc(arena),
                start.to_doc(arena),
                end.to_doc(arena),
                endianness.to_doc(arena),
            ],
            COMMA_SPACE_DOCUMENT,
        );
        docvec![
            arena,
            BIT_ARRAY_SLICE_TO_FLOAT_OPEN_PAREN_DOCUMENT,
            arguments,
            CLOSE_PAREN_DOCUMENT
        ]
    }

    /// Generates the document that calls the `bitArraySlice` function, with
    /// an end argument as well. If you need to take a slice that starts at a
    /// given offset and read the entire array you can use `bit_array_slice`.
    ///
    fn bit_array_slice_with_end(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        bit_array: impl Documentable<'a, 'doc>,
        from: &Offset,
        end: impl Documentable<'a, 'doc>,
    ) -> Document<'a, 'doc> {
        self.expression_generator.tracker.bit_array_slice_used = true;
        let from = self.offset_to_doc(arena, from, false);
        docvec![
            arena,
            BIT_ARRAY_SLICE_OPEN_PAREN_DOCUMENT,
            bit_array,
            COMMA_SPACE_DOCUMENT,
            from,
            COMMA_SPACE_DOCUMENT,
            end,
            CLOSE_PAREN_DOCUMENT
        ]
    }

    /// Generates the document that calls the `bitArraySlice` function, starting
    /// at a given offset. This will read the entire remaining bit of the array,
    /// if you know that the slice should end at a given offset you can use
    /// `bit_array_slice_with_end` instead.
    ///
    fn bit_array_slice(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        bit_array: impl Documentable<'a, 'doc>,
        from: &Offset,
    ) -> Document<'a, 'doc> {
        self.expression_generator.tracker.bit_array_slice_used = true;
        let from = self.offset_to_doc(arena, from, false);
        docvec![
            arena,
            BIT_ARRAY_SLICE_OPEN_PAREN_DOCUMENT,
            bit_array,
            COMMA_SPACE_DOCUMENT,
            from,
            CLOSE_PAREN_DOCUMENT
        ]
    }

    /// This generates all the checks that need to be performed to make sure a
    /// bit array segment (obtained with the read action passed as argument)
    /// matches with a literal string.
    ///
    fn literal_string_segment_bytes_check(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        // A string representing the bit array value we read bits from.
        bit_array: EcoString,
        // The bytes of the literal string we should be matching on.
        string_bytes: &Vec<u8>,
        read_action: &ReadAction,
    ) -> Document<'a, 'doc> {
        let ReadAction {
            from: start,
            endianness,
            signed,
            ..
        } = read_action;
        let mut checks = vec![];

        let equality = SPACE_TRIPLE_EQUAL_SPACE_DOCUMENT;

        let bytes = string_bytes.as_slice();

        if let Some(mut from_byte) = start.constant_bytes() {
            // If the string starts at a compile-time known byte, then we can
            // optimise this by reading all the subsequent bytes and checking
            // they have a specific value.
            for byte in bytes {
                let byte_access = docvec![
                    arena,
                    bit_array.clone(),
                    DOT_BYTE_AT_OPEN_PAREN_DOCUMENT,
                    from_byte.clone(),
                    CLOSE_PAREN_DOCUMENT
                ];
                checks.push(docvec![arena, byte_access, equality, *byte]);
                from_byte += 1;
            }
        } else {
            let mut start = start.clone();

            // If the string doesn't start at a byte aligned offset then we'll
            // have to take slices out of it to check that each byte matches.
            for byte in bytes {
                let start_doc = self.offset_to_doc(arena, &start, false);
                let end = start.add_constant(8);
                let end_doc = self.offset_to_doc(arena, &end, false);
                let byte_access = self.bit_array_slice_to_int(
                    arena, &bit_array, start_doc, end_doc, endianness, *signed,
                );
                checks.push(docvec![arena, byte_access, equality, *byte]);
                start = end;
            }
        }

        // Otherwise the check succeeds if all the byte checks succeed.
        arena
            .join(checks, SPACE_DOUBLE_AMPERSAND_BREAK_DOCUMENT)
            .nest(arena, INDENT)
            .group(arena)
    }

    /// This generates all the checks that need to be performed to make sure a
    /// bit array segment (obtained with the read action passed as argument)
    /// matches with a literal int.
    ///
    fn literal_int_segment_bytes_check(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        // A string representing the bit array value we read bits from.
        bit_array: EcoString,
        literal_int: BigInt,
        read_action: &ReadAction,
    ) -> Document<'a, 'doc> {
        let ReadAction {
            from: start,
            size,
            endianness,
            signed,
            ..
        } = read_action;

        if let (Some(mut from_byte), Some(size)) = (start.constant_bytes(), size.constant_bytes()) {
            // If the number starts at a byte-aligned offset and is made of a
            // whole number of bytes then we can optimise this by checking that
            // all the bytes starting at the given offset match the int bytes.
            let mut checks = vec![];
            for byte in bit_array_segment_int_value_to_bytes(literal_int, size * 8, *endianness) {
                let byte_access = docvec![
                    arena,
                    bit_array.clone(),
                    DOT_BYTE_AT_OPEN_PAREN_DOCUMENT,
                    from_byte.clone(),
                    CLOSE_PAREN_DOCUMENT
                ];
                checks.push(docvec![
                    arena,
                    byte_access,
                    SPACE_TRIPLE_EQUAL_SPACE_DOCUMENT,
                    byte
                ]);
                from_byte += 1;
            }

            arena
                .join(checks, SPACE_DOUBLE_AMPERSAND_BREAK_DOCUMENT)
                .nest(arena, INDENT)
                .group(arena)
        } else {
            // Otherwise we have to take an int slice out of the bit array and
            // check it matches the expected value.
            let start_doc = self.offset_to_doc(arena, start, false);
            let end = match (start.constant_bits(), size.constant_bits()) {
                (Some(start), _) if start == BigInt::ZERO => self
                    .read_size_to_doc(arena, size)
                    .expect("unexpected catch all size"),
                (Some(start), Some(end)) => (start + end).to_doc(arena),
                (_, _) => docvec![
                    arena,
                    start_doc,
                    SPACE_PLUS_SPACE_DOCUMENT,
                    self.read_size_to_doc(arena, size).expect("empty size")
                ],
            };
            let check =
                self.bit_array_slice_to_int(arena, bit_array, start_doc, end, endianness, *signed);
            docvec![arena, check, SPACE_TRIPLE_EQUAL_SPACE_DOCUMENT, literal_int]
        }
    }

    /// This generates all the checks that need to be performed to make sure a
    /// bit array segment (obtained with the read action passed as argument)
    /// matches with a literal float.
    ///
    fn literal_float_segment_bytes_check(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        // A string representing the bit array value we read bits from.
        bit_array: EcoString,
        expected: &EcoString,
        read_action: &ReadAction,
    ) -> Document<'a, 'doc> {
        let ReadAction {
            from: start,
            size,
            endianness,
            ..
        } = read_action;

        let equality = SPACE_TRIPLE_EQUAL_SPACE_DOCUMENT;

        // Unlike literal integers and strings, for now we don't try and apply any
        // optimisation in the way we match on those: we take an entire slice,
        // convert it to a float and check if it matches the expected value.
        let start_doc = self.offset_to_doc(arena, start, false);
        let end = match (start.constant_bits(), size.constant_bits()) {
            (Some(start), _) if start == BigInt::ZERO => self
                .read_size_to_doc(arena, size)
                .expect("unexpected catch all size"),
            (Some(start), Some(end)) => (start + end).to_doc(arena),
            (_, _) => docvec![
                arena,
                start_doc,
                SPACE_PLUS_SPACE_DOCUMENT,
                self.read_size_to_doc(arena, size).expect("empty size")
            ],
        };
        let check = self.bit_array_slice_to_float(arena, bit_array, start_doc, end, endianness);
        docvec![arena, check, equality, expected]
    }

    #[must_use]
    fn is_bound_in_scope(&self, variable: &Variable) -> bool {
        self.scoped_variable_names.contains_key(&variable.id)
    }

    #[must_use]
    fn segment_is_bound_in_scope(&self, segment_name: &EcoString) -> bool {
        self.scoped_segment_names.contains_key(segment_name)
    }

    /// In case the check introduces new variables, this will record their
    /// actual value to be used by later checks and assignments.
    ///
    fn record_check_assignments(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        variable: &Variable,
        check: &RuntimeCheck,
    ) {
        let value = self.get_value(variable);
        match check {
            RuntimeCheck::Int { .. }
            | RuntimeCheck::Float { .. }
            | RuntimeCheck::String { .. }
            | RuntimeCheck::EmptyList => (),

            RuntimeCheck::BitArray { test } => {
                for (segment_name, read_action) in test.referenced_segment_patterns() {
                    self.set_segment_value(arena, variable, segment_name.clone(), read_action);
                }
            }

            RuntimeCheck::StringPrefix { rest, prefix } => {
                let prefix_size = utf16_no_escape_len(prefix);
                self.set_value(rest, eco_format!("{value}.slice({prefix_size})"));
            }

            RuntimeCheck::Tuple { elements, .. } => {
                for (i, element) in elements.iter().enumerate() {
                    self.set_value(element, eco_format!("{value}[{i}]"));
                }
            }

            RuntimeCheck::Variant { fields, labels, .. } => {
                for (i, field) in fields.iter().enumerate() {
                    let access = match labels.get(&i) {
                        Some(label) => eco_format!("{value}.{}", maybe_escape_property(label)),
                        None => eco_format!("{value}[{i}]"),
                    };
                    self.set_value(field, access);
                }
            }

            RuntimeCheck::NonEmptyList { first, rest } => {
                self.set_value(first, eco_format!("{value}.head"));
                self.set_value(rest, eco_format!("{value}.tail"));
            }
        }
    }

    /// A runtime check might need to reference some bit array segments in its
    /// check (for example if a bit array length depends on a previous segment).
    /// This function returns a vector with all the assignments needed to bring
    /// the referenced segments into scope, so they're available to use for the
    /// runtime check.
    ///
    fn bit_array_segment_assignments(
        &mut self,
        arena: &'doc DocumentArena<'a, 'doc>,
        check: &RuntimeCheck,
    ) -> Vec<Document<'a, 'doc>> {
        let mut check_assignments = vec![];
        for (segment, _) in check.referenced_segment_patterns() {
            // If the segment was already bound to a variable in this scope we
            // don't need to generate any further assignment for it. We will just
            // reuse that existing variable when we need to access this segment
            if self.segment_is_bound_in_scope(segment) {
                continue;
            }

            let variable_name = self.next_local_var(segment);
            let segment_value = self
                .get_segment_value(arena, segment)
                .expect("segment referenced in a check before being created");
            self.bind_segment(variable_name.clone(), segment.clone());
            check_assignments.push(let_doc(arena, variable_name, segment_value));
        }
        check_assignments
    }

    /// Returns a string representing the value of a pattern variable: it might
    /// be the code needed to obtain such variable (for example accessing a
    /// list item `wibble.head`), or it could be a name this variable was bound
    /// to in the current scope to avoid doing any repeated work!
    ///
    fn get_value(&self, variable: &Variable) -> EcoString {
        // If the pattern variable was already assigned to a variable that is
        // in scope we use that variable name!
        if let Some(name) = self.scoped_variable_names.get(&variable.id) {
            return name.clone();
        }

        // Otherwise we fallback to using its value directly.
        self.variable_values
            .get(&variable.id)
            .expect("pattern variable used before assignment")
            .clone()
    }

    fn get_segment_value(
        &self,
        arena: &'doc DocumentArena<'a, 'doc>,
        segment_name: &EcoString,
    ) -> Option<Document<'a, 'doc>> {
        // If the segment was already assigned to a variable that is in scope
        // we use that variable name!
        if let Some(name) = self.scoped_segment_names.get(segment_name) {
            return Some(name.clone().to_doc(arena));
        }

        // Otherwise we fallback to using its value directly.
        self.segment_values.get(segment_name).cloned()
    }
}

/// When going over the subjects of a case expression/let we might end up in two
/// situation: the subject might be a variable or it could be a more complex
/// expression (like a function call, a complex expression, ...).
///
/// ```gleam
/// case a_variable { ... }
/// case a_function_call(wobble) { ... }
/// ```
///
/// When checking on a case we might end up repeating the subjects multiple times
/// (as they need to appear in various checks), this means that if we ended up
/// doing the simple thing of just repeating the subject as it is, we might end
/// up dramatically changing the meaning of the program when the subject is a
/// complex expression! Imagine this example:
///
/// ```gleam
/// case wibble("a") {
///   1 -> todo
///   2 -> todo
///   _ -> todo
/// }
/// ```
///
/// If we just repeated the subject every time we need to check it, the decision
/// tree would end up looking something like this:
///
/// ```js
/// if (wibble("a") === 1) {}
/// else if (wibble("a") === 2) {}
/// else {}
/// ```
///
/// It would be quite bad as we would end up running the same function multiple
/// times instead of just once!
///
/// So we need to split each subject in two categories: if it is a simple
/// variable already, it's no big deal and we can repeat that name as many times
/// as we want; however, if it's anything else we first need to bind that subject
/// to a variable we can then reference multiple times.
///
enum SubjectAssignment<'a, 'doc> {
    /// The subject is a complex expression with a `value` that has to be
    /// assigned to a variable with the given `name` as repeating the `value`
    /// multiple times could possibly change the meaning of the program.
    BindToVariable {
        name: EcoString,
        value: Document<'a, 'doc>,
        location: SrcSpan,
    },
    /// The subject is already a simple variable with the given name, we will
    /// keep using that name to reference it.
    AlreadyAVariable { name: EcoString },
}

impl SubjectAssignment<'_, '_> {
    fn name(&self) -> EcoString {
        match self {
            SubjectAssignment::BindToVariable {
                name,
                value: _,
                location: _,
            }
            | SubjectAssignment::AlreadyAVariable { name } => name.clone(),
        }
    }
}

fn assign_subject<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    expression_generator: &mut Generator<'_, 'a, 'doc>,
    subject: &'a TypedExpr,
    ordering: Ordering,
) -> SubjectAssignment<'a, 'doc> {
    static ASSIGNMENT_VAR_ECO_STR: OnceLock<EcoString> = OnceLock::new();

    // If the value is a variable we don't need to assign it to a new
    // variable, we can use the value expression safely without worrying about
    // performing computation or side effects multiple times.
    if let TypedExpr::Var {
        name, constructor, ..
    } = subject
        && constructor.is_local_variable()
    {
        SubjectAssignment::AlreadyAVariable {
            name: expression_generator.local_var(name),
        }
    } else {
        // If it's not a variable we need to assign it to a variable
        // to avoid rendering the subject expression multiple times
        let name = expression_generator
            .next_local_var(ASSIGNMENT_VAR_ECO_STR.get_or_init(|| ASSIGNMENT_VAR.into()));
        let value = expression_generator
            .not_in_tail_position(Some(ordering), |this| this.wrap_expression(arena, subject));

        SubjectAssignment::BindToVariable {
            value,
            name,
            location: subject.location(),
        }
    }
}

fn assignments_to_doc<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    expression_generator: &mut Generator<'_, 'a, 'doc>,
    assignments: Vec<SubjectAssignment<'a, 'doc>>,
) -> Document<'a, 'doc> {
    arena.concat(assignments.into_iter().filter_map(|assignment| {
        let SubjectAssignment::BindToVariable {
            name,
            value,
            location,
        } = assignment
        else {
            return None;
        };

        Some(docvec![
            arena,
            expression_generator.source_map_tracker(arena, location.start),
            let_doc(arena, name, value),
            LINE_DOCUMENT
        ])
    }))
}

/// Appends the second document to the first one separating the two with a newline.
/// However, if the second document is empty the empty line is not added.
///
fn join_with_line<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    one: Document<'a, 'doc>,
    other: Document<'a, 'doc>,
) -> Document<'a, 'doc> {
    if one.is_empty() {
        other
    } else if other.is_empty() {
        one
    } else {
        docvec![arena, one, LINE_DOCUMENT, other]
    }
}

fn reassignment_doc<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    variable_name: EcoString,
    value: Document<'a, 'doc>,
) -> Document<'a, 'doc> {
    docvec![
        arena,
        variable_name,
        SPACE_EQUAL_SPACE_DOCUMENT,
        value,
        SEMICOLON_DOCUMENT
    ]
}

fn let_doc<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    variable_name: EcoString,
    value: Document<'a, 'doc>,
) -> Document<'a, 'doc> {
    docvec![
        arena,
        LET_SPACE_DOCUMENT,
        variable_name,
        SPACE_EQUAL_SPACE_DOCUMENT,
        value,
        SEMICOLON_DOCUMENT
    ]
}

/// Calculates the length of str as utf16 without escape characters.
///
fn utf16_no_escape_len(str: &EcoString) -> usize {
    length_utf16(&convert_string_escape_chars(str))
}

pub fn break_block<'a, 'doc>(
    arena: &'doc DocumentArena<'a, 'doc>,
    doc: Document<'a, 'doc>,
) -> Document<'a, 'doc> {
    docvec![
        arena,
        OPEN_CURLY_DOCUMENT,
        docvec![arena, LINE_DOCUMENT, doc].nest(arena, INDENT),
        LINE_DOCUMENT,
        CLOSE_CURLY_DOCUMENT
    ]
    .force_break(arena)
}
