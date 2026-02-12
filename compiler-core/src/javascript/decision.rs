use super::{
    INDENT, bit_array_segment_int_value_to_bytes,
    expression::{self, Generator, Ordering, float, float_from_value},
};
use crate::{
    ast::{AssignmentKind, Endianness, SrcSpan, TypedClause, TypedExpr, TypedPattern},
    docvec,
    exhaustiveness::{
        BitArrayMatchedValue, BitArrayTest, Body, BoundValue, CompiledCase, Decision,
        FallbackCheck, MatchTest, Offset, ReadAction, ReadSize, ReadType, RuntimeCheck,
        SizeOperator, SizeTest, Variable, VariableUsage,
    },
    format::break_block,
    javascript::{
        expression::{eco_string_int, string},
        maybe_escape_property,
    },
    pretty::{Document, Documentable, break_, concat, join, line, nil},
    strings::{convert_string_escape_chars, length_utf16},
};
use ecow::{EcoString, eco_format};
use itertools::Itertools;
use num_bigint::BigInt;
use std::{collections::HashMap, sync::OnceLock};

pub static ASSIGNMENT_VAR: &str = "$";

pub fn case<'a>(
    compiled_case: &'a CompiledCase,
    clauses: &'a [TypedClause],
    subjects: &'a [TypedExpr],
    expression_generator: &mut Generator<'_, 'a>,
) -> Document<'a> {
    let scope_position = expression_generator.scope_position.clone();
    let mut variables = Variables::new(expression_generator, VariableAssignment::Declare);
    let assignments = variables.assign_case_subjects(compiled_case, subjects);
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
            printer
                .inside_new_scope(|this| this.decision(tree))
                .into_doc(),
        ),

        tree @ (Decision::Run { .. }
        | Decision::Guard { .. }
        | Decision::Switch { .. }
        | Decision::Fail) => printer.decision(tree).into_doc(),
    };
    docvec![assignments_to_doc(assignments), decision].force_break()
}

/// The generated code for a decision tree.
enum CaseBody<'a> {
    /// A JavaScript `if`` statement by itself. This can be merged with any
    /// preceding `else` statements to form an `else if` construct.
    If {
        check: Document<'a>,
        body: Document<'a>,
    },
    /// A sequence of statements. This must be wrapped as the body of an `if` or
    /// `else` statement.
    Statements(Document<'a>),

    /// A JavaScript `if` statement followed by a single `else` clause. In some
    /// cases this can be flattened to reduce the size of the generated decision
    /// tree.
    IfElse {
        check: Document<'a>,
        if_body: Document<'a>,
        else_body: Document<'a>,
        /// The decision in the tree that is used to generate the code for the
        /// `else` clause of this statement. If this is the same as another `if`-
        /// `else` statement, the two can be merged into one.
        fallback_decision: &'a Decision,
    },

    /// A JavaScript `if` statement followed by more than one `else` clause. This
    /// can sometimes be merged with preceding `else` statements in the same way
    /// that `if` can.
    IfElseChain(Document<'a>),
}

impl<'a> CaseBody<'a> {
    fn into_doc(self) -> Document<'a> {
        match self {
            CaseBody::If { check, body } => docvec![
                "if (",
                break_("", "")
                    .append(check)
                    .nest(INDENT)
                    .append(break_("", ""))
                    .group(),
                ") ",
                break_block(body)
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
                "if (!(",
                break_("", "")
                    .append(check)
                    .nest(INDENT)
                    .append(break_("", ""))
                    .group(),
                ")) ",
                else_body,
            ],
            CaseBody::IfElse {
                check,
                if_body,
                else_body,
                ..
            } => docvec![
                "if (",
                break_("", "")
                    .append(check)
                    .nest(INDENT)
                    .append(break_("", ""))
                    .group(),
                ") ",
                break_block(if_body),
                " else ",
                else_body,
            ],
            CaseBody::Statements(document) | CaseBody::IfElseChain(document) => document,
        }
    }

    /// Convert this value into the required document to put directly after an
    /// `else` keyword.
    fn document_after_else(self) -> Document<'a> {
        match self {
            // `if` and `if-else` statements can come directly after an `else` keyword
            CaseBody::If { .. } | CaseBody::IfElse { .. } => self.into_doc(),
            CaseBody::IfElseChain(document) => document,
            // Lists of statements must be wrapped in a block
            CaseBody::Statements(document) => break_block(document),
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            CaseBody::If { .. } | CaseBody::IfElse { .. } => false,
            CaseBody::Statements(document) | CaseBody::IfElseChain(document) => document.is_empty(),
        }
    }
}

struct CasePrinter<'module, 'generator, 'a, 'assignments> {
    variables: Variables<'generator, 'module, 'a>,
    assignments: &'assignments Vec<SubjectAssignment<'a>>,
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

enum BodyExpression<'a> {
    /// This happens when a case expression branch returns the same value that
    /// is being matched on. So instead of rebuilding it from scratch we can
    /// return the case subject directly. For example:
    /// `Ok(1) -> Ok(1)`
    /// `a -> a`
    /// `[1, ..rest] -> [1, ..rest]`
    ///
    Variable(Document<'a>),

    /// This happens when a case expression has a complex body that is not just
    /// returning the matched subject. For example:
    /// `Ok(1) -> Ok(2)`
    /// `_ -> [1, 2, 3]`
    /// `1 -> "wibble"`
    ///
    Expressions(Document<'a>),
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
/// if (value instanceOf $NonEmptyList) {
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
/// if (value instanceOf $NonEmptyList) {
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
impl<'a> CasePrinter<'_, '_, 'a, '_> {
    fn decision(&mut self, decision: &'a Decision) -> CaseBody<'a> {
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
                        subject.to_doc(),
                        kind,
                        *subject_location,
                        *pattern_location,
                    ))
                } else {
                    unreachable!("Invalid decision tree reached code generation")
                }
            }
            Decision::Run { body } => {
                let bindings = self.variables.bindings_doc(&body.bindings);
                let body = self.body_expression(body.clause_index);
                let body = match body {
                    BodyExpression::Variable(variable) => variable,
                    BodyExpression::Expressions(body) => join_with_line(bindings, body),
                };
                CaseBody::Statements(body)
            }
            Decision::Switch {
                var,
                choices,
                fallback,
                fallback_check,
            } => self.switch(var, choices, fallback, fallback_check),
            Decision::Guard {
                guard,
                if_true,
                if_false,
            } => self.decision_guard(*guard, if_true, if_false),
        }
    }

    fn body_expression(&mut self, clause_index: usize) -> BodyExpression<'a> {
        // If we are not in a `case` expression, there is no additional code to
        // execute when a branch matches; we only assign variables bound in the
        // pattern.
        let DecisionKind::Case { clauses } = &self.kind else {
            return BodyExpression::Expressions(nil());
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
                    .wrap_return(variable.to_doc()),
            )
        } else {
            BodyExpression::Expressions(
                self.variables
                    .expression_generator
                    .expression_flattening_blocks(body),
            )
        }
    }

    fn switch(
        &mut self,
        var: &'a Variable,
        choices: &'a [(RuntimeCheck, Decision)],
        fallback: &'a Decision,
        fallback_check: &'a FallbackCheck,
    ) -> CaseBody<'a> {
        // If there's just a single choice we can just generate the code for
        // it: no need to do any checking, we know it must match!
        if choices.is_empty() {
            // However, if the choice had an associated check (that is, it was
            // not just a simple catch all) we need to keep track of all the
            // variables brought into scope by the (always) successfull check.
            if let FallbackCheck::RuntimeCheck { check } = fallback_check {
                self.variables.record_check_assignments(var, check);
            }
            return self.inside_new_scope(|this| this.decision(fallback));
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
            assignments.push(let_doc(name, value.to_doc()))
        };

        let mut if_ = CaseBody::Statements(nil());
        for (i, (check, decision)) in choices.iter().enumerate() {
            self.variables.record_check_assignments(var, check);

            // For each check we generate:
            // - the document to perform such check
            // - the body to run if the check is successful
            // - the assignments we need to bring all the bit array segments
            //   referenced by this check
            let (check_doc, body, mut segment_assignments) = self.inside_new_scope(|this| {
                let segment_assignments = this.variables.bit_array_segment_assignments(check);
                let check_doc = this.variables.runtime_check(var, check);
                let body = this.decision(decision);
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
                CaseBody::If { check, body } => {
                    (docvec![check_doc, break_(" &&", " && "), check], body)
                }

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
                } if decision == fallback => {
                    (docvec![check_doc, break_(" &&", " && "), check], if_body)
                }

                if_else @ CaseBody::IfElse { .. } => (check_doc, if_else.into_doc()),

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
                    if_.into_doc(),
                    " else if (",
                    break_("", "")
                        .append(check_doc)
                        .nest(INDENT)
                        .append(break_("", ""))
                        .group(),
                    ") ",
                    break_block(body)
                ]),
                CaseBody::IfElseChain(document) | CaseBody::Statements(document) => {
                    CaseBody::IfElseChain(docvec![
                        document,
                        " else if (",
                        break_("", "")
                            .append(check_doc)
                            .nest(INDENT)
                            .append(break_("", ""))
                            .group(),
                        ") ",
                        break_block(body)
                    ])
                }
            };
        }

        // In case there's some new variables we can extract after the
        // successful final check we store those. But we don't need to perform
        // the check itself: the type system ensures that, if we ever get here,
        // the check is going to match no matter what!
        if let FallbackCheck::RuntimeCheck { check } = fallback_check {
            self.variables.record_check_assignments(var, check);
        }

        let else_body = self.inside_new_scope(|this| this.decision(fallback));
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
                else_body: else_body.document_after_else(),
                fallback_decision: fallback,
            }
        } else {
            CaseBody::IfElseChain(docvec![
                if_.into_doc(),
                " else ",
                else_body.document_after_else()
            ])
        };

        if assignments.is_empty() {
            document
        } else {
            CaseBody::Statements(join_with_line(
                join(assignments, line()),
                document.into_doc(),
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
            DecisionKind::Case { .. } => self
                .variables
                .expression_generator
                .current_scope_vars
                .clone(),
            DecisionKind::LetAssert { .. } => Default::default(),
        };

        let old_names = self.variables.scoped_variable_names.clone();
        let old_segments = self.variables.segment_values.clone();
        let old_segment_names = self.variables.scoped_segment_names.clone();
        let output = run(self);

        match &self.kind {
            DecisionKind::Case { .. } => {
                self.variables.expression_generator.current_scope_vars = old_scope
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
        guard: usize,
        if_true: &'a Body,
        if_false: &'a Decision,
    ) -> CaseBody<'a> {
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
            let check_bindings = this.variables.bindings_ref_doc(&check_bindings);
            let check = this.variables.expression_generator.guard(guard);
            // All the other bindings that are not needed by the guard check will
            // end up directly in the body of the if clause.
            let if_true_bindings = this.variables.bindings_ref_doc(&if_true_bindings);
            let if_true_body = this.body_expression(if_true.clause_index);
            let if_true = match if_true_body {
                BodyExpression::Variable(variable) => variable,
                BodyExpression::Expressions(if_true_body) => {
                    join_with_line(if_true_bindings, if_true_body)
                }
            };
            (check_bindings, check, if_true)
        });

        let if_false_body = self.inside_new_scope(|this| this.decision(if_false));

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
                else_body: if_false_body.document_after_else(),
                fallback_decision: if_false,
            }
        };

        if check_bindings.is_empty() {
            if_
        } else {
            CaseBody::Statements(join_with_line(check_bindings, if_.into_doc()))
        }
    }

    fn assignment_no_match(
        &mut self,
        subject: Document<'a>,
        kind: &'a AssignmentKind<TypedExpr>,
        subject_location: SrcSpan,
        pattern_location: SrcSpan,
    ) -> Document<'a> {
        let AssignmentKind::Assert {
            location, message, ..
        } = kind
        else {
            unreachable!("inexhaustive let made it to code generation");
        };

        let generator = &mut self.variables.expression_generator;
        let message = match message {
            None => string("Pattern match failed, no pattern matched the value."),
            Some(message) => generator
                .not_in_tail_position(Some(Ordering::Strict), |this| this.wrap_expression(message)),
        };
        generator.throw_error(
            "let_assert",
            &message,
            *location,
            [
                ("value", subject),
                ("start", location.start.to_doc()),
                ("end", subject_location.end.to_doc()),
                ("pattern_start", pattern_location.start.to_doc()),
                ("pattern_end", pattern_location.end.to_doc()),
            ],
        )
    }
}

pub fn let_<'a>(
    compiled_case: &'a CompiledCase,
    subject: &'a TypedExpr,
    kind: &'a AssignmentKind<TypedExpr>,
    expression_generator: &mut Generator<'_, 'a>,
    pattern: &'a TypedPattern,
) -> Document<'a> {
    let scope_position = expression_generator.scope_position.clone();
    let mut variables = Variables::new(expression_generator, VariableAssignment::Reassign);

    let assignment = variables.assign_let_subject(compiled_case, subject);
    let assignment_name = assignment.name();
    let assignments = vec![assignment];

    let decision = CasePrinter {
        variables,
        assignments: &assignments,
        kind: DecisionKind::LetAssert {
            kind,
            subject_location: subject.location(),
            pattern_location: pattern.location(),
            subject: assignment_name.clone(),
        },
    }
    .decision(&compiled_case.tree);

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
    let beginning_assignments = pattern.bound_variables().into_iter().map(|bound_variable| {
        docvec![
            "let ",
            expression_generator.local_var(&bound_variable.name()),
            ";",
            line()
        ]
    });

    let doc = docvec![
        assignments_to_doc(assignments),
        concat(beginning_assignments),
        decision.into_doc()
    ];

    match scope_position {
        expression::Position::Expression(_) | expression::Position::Statement => doc,
        expression::Position::Tail => docvec![doc, line(), "return ", assignment_name, ";"],
        expression::Position::Assign(variable) => {
            docvec![doc, line(), variable, " = ", assignment_name, ";"]
        }
    }
}

enum VariableAssignment {
    Declare,
    Reassign,
}

/// This is a useful piece of state that is kept separate from the generator
/// itself so we can reuse it both with `case`s and `let`s without rewriting
/// everything from scratch.
///
struct Variables<'generator, 'module, 'a> {
    expression_generator: &'generator mut Generator<'module, 'a>,

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
    segment_values: HashMap<EcoString, Document<'a>>,

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

impl<'generator, 'module, 'a> Variables<'generator, 'module, 'a> {
    fn new(
        expression_generator: &'generator mut Generator<'module, 'a>,
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
        compiled_case: &'a CompiledCase,
        subjects: &'a [TypedExpr],
    ) -> Vec<SubjectAssignment<'a>> {
        let assignments = subjects
            .iter()
            .map(|subject| assign_subject(self.expression_generator, subject, Ordering::Strict))
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
        compiled_case: &'a CompiledCase,
        subject: &'a TypedExpr,
    ) -> SubjectAssignment<'a> {
        let variable = compiled_case
            .subject_variables
            .first()
            .expect("decision tree with no subjects");
        let assignment = assign_subject(self.expression_generator, subject, Ordering::Loose);
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
        bit_array: &Variable,
        segment_name: EcoString,
        read_action: &ReadAction,
    ) {
        let value = self.read_action_to_doc(bit_array, read_action);
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

    fn bindings_doc(&mut self, bindings: &'a [(EcoString, BoundValue)]) -> Document<'a> {
        let bindings =
            (bindings.iter()).map(|(variable, value)| self.body_binding_doc(variable, value));
        join(bindings, line())
    }

    fn bindings_ref_doc(&mut self, bindings: &[&'a (EcoString, BoundValue)]) -> Document<'a> {
        let bindings =
            (bindings.iter()).map(|(variable, value)| self.body_binding_doc(variable, value));
        join(bindings, line())
    }

    fn body_binding_doc(
        &mut self,
        variable_name: &'a EcoString,
        value: &'a BoundValue,
    ) -> Document<'a> {
        let local_variable_name = self.next_local_var(variable_name);
        let assigned_value = match value {
            BoundValue::Variable(variable) => self.get_value(variable).to_doc(),
            BoundValue::LiteralString(value) => string(value),
            BoundValue::LiteralFloat(value) => float(value),
            BoundValue::LiteralInt(value) => eco_string_int(eco_format!("{value}")),
            BoundValue::BitArraySlice {
                bit_array,
                read_action,
            } => self
                .get_segment_value(variable_name)
                .unwrap_or_else(|| self.read_action_to_doc(bit_array, read_action)),
        };

        match self.variable_assignment {
            VariableAssignment::Declare => let_doc(local_variable_name.clone(), assigned_value),
            VariableAssignment::Reassign => {
                reassignment_doc(local_variable_name.clone(), assigned_value)
            }
        }
    }

    /// Generates the document to perform a (possibly negated) runtime check on
    /// the given variable.
    ///
    fn runtime_check(
        &mut self,
        variable: &Variable,
        runtime_check: &'a RuntimeCheck,
    ) -> Document<'a> {
        let value = self.get_value(variable);

        let equality = " === ";

        match runtime_check {
            RuntimeCheck::String { value: expected } => docvec![value, equality, string(expected)],
            RuntimeCheck::Float {
                float_value: expected,
            } => docvec![value, equality, float_from_value(expected.value())],
            RuntimeCheck::Int {
                int_value: expected,
            } => docvec![value, equality, expected.clone()],
            RuntimeCheck::StringPrefix { prefix, .. } => {
                docvec![value, ".startsWith(", string(prefix), ")"]
            }

            RuntimeCheck::BitArray { test } => match test {
                // In this case we need to check that the remaining part of the
                // bit array has a whole number of bytes.
                BitArrayTest::CatchAllIsBytes { size_so_far } => {
                    if size_so_far.is_zero() {
                        docvec![value, ".bitSize % 8", equality, "0"]
                    } else {
                        let size_so_far = self.offset_to_doc(size_so_far, true);
                        let remaining_bits = docvec![value, ".bitSize - ", size_so_far];
                        docvec!["(", remaining_bits, ") % 8", equality, "0"]
                    }
                }

                BitArrayTest::ReadSizeIsNotNegative { size } => {
                    docvec![self.read_size_to_doc(size), " >= 0"]
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
                    let start_doc = self.offset_to_doc(start, false);
                    let end = match (start.constant_bits(), size.constant_bits()) {
                        (Some(start), _) if start == BigInt::ZERO => self
                            .read_size_to_doc(size)
                            .expect("unexpected catch all size"),
                        (Some(start), Some(end)) => (start + end).to_doc(),
                        (_, _) => docvec![start_doc.clone(), " + ", self.read_size_to_doc(size)],
                    };
                    let check = self.bit_array_slice_to_float(value, start_doc, end, endianness);

                    docvec!["Number.isFinite(", check, ")"]
                }

                // Here we need to make sure that the bit array has a specific
                // size.
                BitArrayTest::Size(SizeTest { operator, size }) => {
                    let operator = match operator {
                        SizeOperator::GreaterEqual => " >= ",
                        SizeOperator::Equal => equality,
                    };
                    let size = self.offset_to_doc(size, false);
                    docvec![value, ".bitSize", operator, size]
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
                    } => self.literal_string_segment_bytes_check(value, expected, read_action),
                    BitArrayMatchedValue::LiteralFloat(expected) => {
                        self.literal_float_segment_bytes_check(value, expected, read_action)
                    }
                    BitArrayMatchedValue::LiteralInt {
                        value: expected, ..
                    } => self.literal_int_segment_bytes_check(value, expected.clone(), read_action),
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
                match match_.name().as_str() {
                    "True" => value.to_doc(),
                    _ => docvec!["!", value],
                }
            }

            RuntimeCheck::Variant { match_, index, .. } => {
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

                docvec![value, " instanceof ", qualification, match_.name()]
            }

            RuntimeCheck::NonEmptyList { .. } => {
                self.expression_generator.tracker.list_non_empty_class_used = true;
                docvec![value, " instanceof $NonEmpty"]
            }

            RuntimeCheck::EmptyList => {
                self.expression_generator.tracker.list_empty_class_used = true;
                docvec![value, " instanceof $Empty"]
            }
        }
    }

    /// Turns a read action into a document that can be used to extract the
    /// corresponding value from the given bit array and assign it to a
    /// variable.
    ///
    fn read_action_to_doc(
        &mut self,
        bit_array: &Variable,
        read_action: &ReadAction,
    ) -> Document<'a> {
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
                return docvec![bit_array, ".byteAt(", from_byte, ")"];
            }

            // If we're reading all the remaining bits/bytes of an array we'll
            // take the remaining slice.
            (ReadSize::RemainingBits | ReadSize::RemainingBytes, _) => {
                return self.bit_array_slice(bit_array, from);
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
                let start = from_bits.clone().to_doc();
                let end = (from_bits + size).to_doc();
                (start, end)
            } else {
                // Otherwise we'll have to sum the variable part and the constant
                // one to tell how long the slice should be.
                let size = self.read_size_to_doc(size).expect("no variable size");
                let start = self.offset_to_doc(from, false);
                let end = if from.is_zero() {
                    size
                } else {
                    docvec![start.clone(), " + ", size]
                };
                (start, end)
            };

        match type_ {
            ReadType::Int => {
                self.bit_array_slice_to_int(bit_array, start, end, endianness, *signed)
            }
            ReadType::Float => self.bit_array_slice_to_float(bit_array, start, end, endianness),
            ReadType::BitArray => self.bit_array_slice_with_end(bit_array, from, end),
            ReadType::String | ReadType::UtfCodepoint => {
                panic!("invalid slice type made it to code generation: {type_:#?}")
            }
        }
    }

    fn offset_to_doc(&mut self, offset: &Offset, parenthesise: bool) -> Document<'a> {
        if offset.is_zero() {
            return "0".to_doc();
        }

        let mut pieces = vec![];
        if offset.constant != BigInt::ZERO {
            pieces.push(eco_string_int(offset.constant.to_string().into()));
        }

        for (variable, times) in offset
            .variables
            .iter()
            .sorted_by(|(one, _), (other, _)| one.name().cmp(other.name()))
        {
            let mut variable = match variable {
                VariableUsage::PatternSegment(segment_name, _) => self
                    .get_segment_value(segment_name)
                    .expect("segment referenced in a check before being created"),
                VariableUsage::OutsideVariable(name) => self.local_var(name).to_doc(),
            };
            if *times != 1 {
                variable = variable.append(" * ").append(*times)
            }
            pieces.push(variable.to_doc())
        }

        for calculation in offset.calculations.iter() {
            let left = self.offset_to_doc(&calculation.left, true);
            let right = self.offset_to_doc(&calculation.right, true);

            let calculation = self.expression_generator.bin_op_with_doc_operands(
                calculation.operator.to_bin_op(),
                left,
                right,
                &crate::type_::int(),
            );

            if parenthesise {
                pieces.push(calculation.surround("(", ")"))
            } else {
                pieces.push(calculation)
            }
        }

        if pieces.len() > 1 && parenthesise {
            docvec!["(", join(pieces, " + ".to_doc()), ")"]
        } else {
            join(pieces, " + ".to_doc())
        }
    }

    /// If the read size has a constant value (that is, it's not a "read all the
    /// remaining bits/bytes") this returns a document representing that size.
    ///
    fn read_size_to_doc(&mut self, size: &ReadSize) -> Option<Document<'a>> {
        match size {
            ReadSize::ConstantBits(value) => Some(value.clone().to_doc()),
            ReadSize::VariableBits { variable, unit } => {
                let variable = self.local_var(variable.name());
                Some(if *unit == 1 {
                    variable.to_doc()
                } else {
                    docvec![variable, " * ", *unit as i64]
                })
            }
            ReadSize::RemainingBits | ReadSize::RemainingBytes => None,

            ReadSize::BinaryOperator {
                left,
                right,
                operator,
            } => {
                let left = if self.read_size_must_be_wrapped(left) {
                    self.read_size_to_doc(left)?.surround("(", ")")
                } else {
                    self.read_size_to_doc(left)?
                };
                let right = if self.read_size_must_be_wrapped(right) {
                    self.read_size_to_doc(right)?.surround("(", ")")
                } else {
                    self.read_size_to_doc(right)?
                };

                Some(self.expression_generator.bin_op_with_doc_operands(
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
        bit_array: impl Documentable<'a>,
        start: impl Documentable<'a>,
        end: impl Documentable<'a>,
        endianness: &Endianness,
        signed: bool,
    ) -> Document<'a> {
        self.expression_generator
            .tracker
            .bit_array_slice_to_int_used = true;

        let endianness = match endianness {
            Endianness::Big => "true",
            Endianness::Little => "false",
        };
        let signed = if signed { "true" } else { "false" };
        let arguments = join(
            [
                bit_array.to_doc(),
                start.to_doc(),
                end.to_doc(),
                endianness.to_doc(),
                signed.to_doc(),
            ],
            ", ".to_doc(),
        );
        docvec!["bitArraySliceToInt(", arguments, ")"]
    }

    /// Generates the document that calls the `bitArraySliceToFloat` function,
    /// with the given arguments.
    ///
    fn bit_array_slice_to_float(
        &mut self,
        bit_array: impl Documentable<'a>,
        start: impl Documentable<'a>,
        end: impl Documentable<'a>,
        endianness: &Endianness,
    ) -> Document<'a> {
        self.expression_generator
            .tracker
            .bit_array_slice_to_float_used = true;

        let endianness = match endianness {
            Endianness::Big => "true",
            Endianness::Little => "false",
        };
        let arguments = join(
            [
                bit_array.to_doc(),
                start.to_doc(),
                end.to_doc(),
                endianness.to_doc(),
            ],
            ", ".to_doc(),
        );
        docvec!["bitArraySliceToFloat(", arguments, ")"]
    }

    /// Generates the document that calls the `bitArraySlice` function, with
    /// an end argument as well. If you need to take a slice that starts at a
    /// given offset and read the entire array you can use `bit_array_slice`.
    ///
    fn bit_array_slice_with_end(
        &mut self,
        bit_array: impl Documentable<'a>,
        from: &Offset,
        end: impl Documentable<'a>,
    ) -> Document<'a> {
        self.expression_generator.tracker.bit_array_slice_used = true;
        let from = self.offset_to_doc(from, false);
        docvec!["bitArraySlice(", bit_array, ", ", from, ", ", end, ")"]
    }

    /// Generates the document that calls the `bitArraySlice` function, starting
    /// at a given offset. This will read the entire remaining bit of the array,
    /// if you know that the slice should end at a given offset you can use
    /// `bit_array_slice_with_end` instead.
    ///
    fn bit_array_slice(&mut self, bit_array: impl Documentable<'a>, from: &Offset) -> Document<'a> {
        self.expression_generator.tracker.bit_array_slice_used = true;
        let from = self.offset_to_doc(from, false);
        docvec!["bitArraySlice(", bit_array, ", ", from, ")"]
    }

    /// This generates all the checks that need to be performed to make sure a
    /// bit array segment (obtained with the read action passed as argument)
    /// matches with a literal string.
    ///
    fn literal_string_segment_bytes_check(
        &mut self,
        // A string representing the bit array value we read bits from.
        bit_array: EcoString,
        // The bytes of the literal string we should be matching on.
        string_bytes: &Vec<u8>,
        read_action: &ReadAction,
    ) -> Document<'a> {
        let ReadAction {
            from: start,
            endianness,
            signed,
            ..
        } = read_action;
        let mut checks = vec![];

        let equality = " === ";

        let bytes = string_bytes.as_slice();

        if let Some(mut from_byte) = start.constant_bytes() {
            // If the string starts at a compile-time known byte, then we can
            // optimise this by reading all the subsequent bytes and checking
            // they have a specific value.
            for byte in bytes {
                let byte_access = docvec![bit_array.clone(), ".byteAt(", from_byte.clone(), ")"];
                checks.push(docvec![byte_access, equality, byte]);
                from_byte += 1;
            }
        } else {
            let mut start = start.clone();

            // If the string doesn't start at a byte aligned offset then we'll
            // have to take slices out of it to check that each byte matches.
            for byte in bytes {
                let start_doc = self.offset_to_doc(&start, false);
                let end = start.add_constant(8);
                let end_doc = self.offset_to_doc(&end, false);
                let byte_access = self
                    .bit_array_slice_to_int(&bit_array, start_doc, end_doc, endianness, *signed);
                checks.push(docvec![byte_access, equality, byte]);
                start = end;
            }
        }

        // Otherwise the check succeeds if all the byte checks succeed.
        join(checks, break_(" &&", " && ")).nest(INDENT).group()
    }

    /// This generates all the checks that need to be performed to make sure a
    /// bit array segment (obtained with the read action passed as argument)
    /// matches with a literal int.
    ///
    fn literal_int_segment_bytes_check(
        &mut self,
        // A string representing the bit array value we read bits from.
        bit_array: EcoString,
        literal_int: BigInt,
        read_action: &ReadAction,
    ) -> Document<'a> {
        let ReadAction {
            from: start,
            size,
            endianness,
            signed,
            ..
        } = read_action;

        let equality = " === ";

        if let (Some(mut from_byte), Some(size)) = (start.constant_bytes(), size.constant_bytes()) {
            // If the number starts at a byte-aligned offset and is made of a
            // whole number of bytes then we can optimise this by checking that
            // all the bytes starting at the given offset match the int bytes.
            let mut checks = vec![];
            for byte in bit_array_segment_int_value_to_bytes(literal_int, size * 8, *endianness) {
                let byte_access = docvec![bit_array.clone(), ".byteAt(", from_byte.clone(), ")"];
                checks.push(docvec![byte_access, equality, byte]);
                from_byte += 1;
            }

            join(checks, break_(" &&", " && ")).nest(INDENT).group()
        } else {
            // Otherwise we have to take an int slice out of the bit array and
            // check it matches the expected value.
            let start_doc = self.offset_to_doc(start, false);
            let end = match (start.constant_bits(), size.constant_bits()) {
                (Some(start), _) if start == BigInt::ZERO => self
                    .read_size_to_doc(size)
                    .expect("unexpected catch all size"),
                (Some(start), Some(end)) => (start + end).to_doc(),
                (_, _) => docvec![start_doc.clone(), " + ", self.read_size_to_doc(size)],
            };
            let check = self.bit_array_slice_to_int(bit_array, start_doc, end, endianness, *signed);
            docvec![check, equality, literal_int]
        }
    }

    /// This generates all the checks that need to be performed to make sure a
    /// bit array segment (obtained with the read action passed as argument)
    /// matches with a literal float.
    ///
    fn literal_float_segment_bytes_check(
        &mut self,
        // A string representing the bit array value we read bits from.
        bit_array: EcoString,
        expected: &EcoString,
        read_action: &ReadAction,
    ) -> Document<'a> {
        let ReadAction {
            from: start,
            size,
            endianness,
            ..
        } = read_action;

        let equality = " === ";

        // Unlike literal integers and strings, for now we don't try and apply any
        // optimisation in the way we match on those: we take an entire slice,
        // convert it to a float and check if it matches the expected value.
        let start_doc = self.offset_to_doc(start, false);
        let end = match (start.constant_bits(), size.constant_bits()) {
            (Some(start), _) if start == BigInt::ZERO => self
                .read_size_to_doc(size)
                .expect("unexpected catch all size"),
            (Some(start), Some(end)) => (start + end).to_doc(),
            (_, _) => docvec![start_doc.clone(), " + ", self.read_size_to_doc(size)],
        };
        let check = self.bit_array_slice_to_float(bit_array, start_doc, end, endianness);
        docvec![check, equality, expected]
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
    fn record_check_assignments(&mut self, variable: &Variable, check: &RuntimeCheck) {
        let value = self.get_value(variable);
        match check {
            RuntimeCheck::Int { .. }
            | RuntimeCheck::Float { .. }
            | RuntimeCheck::String { .. }
            | RuntimeCheck::EmptyList => (),

            RuntimeCheck::BitArray { test } => {
                for (segment_name, read_action) in test.referenced_segment_patterns() {
                    self.set_segment_value(variable, segment_name.clone(), read_action)
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
    fn bit_array_segment_assignments(&mut self, check: &RuntimeCheck) -> Vec<Document<'a>> {
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
                .get_segment_value(segment)
                .expect("segment referenced in a check before being created");
            self.bind_segment(variable_name.clone(), segment.clone());
            check_assignments.push(let_doc(variable_name, segment_value))
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

    fn get_segment_value(&self, segment_name: &EcoString) -> Option<Document<'a>> {
        // If the segment was already assigned to a variable that is in scope
        // we use that variable name!
        if let Some(name) = self.scoped_segment_names.get(segment_name) {
            return Some(name.clone().to_doc());
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
enum SubjectAssignment<'a> {
    /// The subject is a complex expression with a `value` that has to be
    /// assigned to a variable with the given `name` as repeating the `value`
    /// multiple times could possibly change the meaning of the program.
    BindToVariable {
        name: EcoString,
        value: Document<'a>,
    },
    /// The subject is already a simple variable with the given name, we will
    /// keep using that name to reference it.
    AlreadyAVariable { name: EcoString },
}

impl SubjectAssignment<'_> {
    fn name(&self) -> EcoString {
        match self {
            SubjectAssignment::BindToVariable { name, value: _ }
            | SubjectAssignment::AlreadyAVariable { name } => name.clone(),
        }
    }
}

fn assign_subject<'a>(
    expression_generator: &mut Generator<'_, 'a>,
    subject: &'a TypedExpr,
    ordering: Ordering,
) -> SubjectAssignment<'a> {
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
            .not_in_tail_position(Some(ordering), |this| this.wrap_expression(subject));

        SubjectAssignment::BindToVariable { value, name }
    }
}

fn assignments_to_doc(assignments: Vec<SubjectAssignment<'_>>) -> Document<'_> {
    let mut assignments_docs = vec![];
    for assignment in assignments.into_iter() {
        let SubjectAssignment::BindToVariable { name, value } = assignment else {
            continue;
        };
        assignments_docs.push(docvec![let_doc(name, value), line()])
    }
    assignments_docs.to_doc()
}

/// Appends the second document to the first one separating the two with a newline.
/// However, if the second document is empty the empty line is not added.
///
fn join_with_line<'a>(one: Document<'a>, other: Document<'a>) -> Document<'a> {
    if one.is_empty() {
        other
    } else if other.is_empty() {
        one
    } else {
        docvec![one, line(), other]
    }
}

fn reassignment_doc(variable_name: EcoString, value: Document<'_>) -> Document<'_> {
    docvec![variable_name, " = ", value, ";"]
}

fn let_doc(variable_name: EcoString, value: Document<'_>) -> Document<'_> {
    docvec!["let ", variable_name, " = ", value, ";"]
}

/// Calculates the length of str as utf16 without escape characters.
///
fn utf16_no_escape_len(str: &EcoString) -> usize {
    length_utf16(&convert_string_escape_chars(str))
}
