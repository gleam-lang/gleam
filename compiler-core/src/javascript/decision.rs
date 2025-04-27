use super::{
    Error, INDENT, Output, bit_array_segment_int_value_to_bytes,
    expression::{self, Generator, Ordering, float, int},
};
use crate::{
    ast::{AssignmentKind, Endianness, TypedClause, TypedExpr},
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
    pretty::{Document, Documentable, break_, join, line, nil},
    strings::convert_string_escape_chars,
};
use ecow::{EcoString, eco_format};
use itertools::Itertools;
use num_bigint::BigInt;
use std::{
    collections::{HashMap, VecDeque},
    sync::OnceLock,
};

pub static ASSIGNMENT_VAR: &str = "$";

pub fn case<'a>(
    compiled_case: &'a CompiledCase,
    clauses: &'a [TypedClause],
    subjects: &'a [TypedExpr],
    expression_generator: &mut Generator<'_, 'a>,
) -> Output<'a> {
    let mut variables = Variables::new(expression_generator);
    let assignments = variables.assign_subjects(compiled_case, subjects)?;
    let decision = CasePrinter { clauses, variables }.decision(&compiled_case.tree)?;
    Ok(docvec![assignments_to_doc(assignments), decision].force_break())
}

struct CasePrinter<'module, 'generator, 'a> {
    clauses: &'a [TypedClause],
    variables: Variables<'generator, 'module, 'a>,
}

/// Code generation for decision trees can look a bit daunting at a first glance
/// so let's go over the big idea to hopefully make it easier to understand why
/// the code is organised the way it is :)
///
/// > It might be helpful to go over the `exhaustiveness` module first and get
/// > familiar with the structure of the decision tree.
///
/// A decision tree has nodes that perform checks on pattern variables until
/// it reaches a body with an expression to run. This will be turned into a
///
/// TODO)) finire la documentazione
///
impl<'a> CasePrinter<'_, '_, 'a> {
    fn decision(&mut self, decision: &'a Decision) -> Output<'a> {
        match decision {
            Decision::Fail => unreachable!("Invalid decision tree reached code generation"),
            Decision::Run { body } => {
                let bindings = self.variables.bindings_doc(&body.bindings);
                let body = self.body_expression(body.clause_index)?;
                Ok(join_with_line(bindings, body))
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

    fn body_expression(&mut self, clause_index: usize) -> Output<'a> {
        let body = &self
            .clauses
            .get(clause_index)
            .expect("invalid clause index")
            .then;

        self.variables
            .expression_generator
            .expression_flattening_blocks(body)
    }

    fn switch(
        &mut self,
        var: &'a Variable,
        choices: &'a [(RuntimeCheck, Box<Decision>)],
        fallback: &'a Decision,
        fallback_check: &'a FallbackCheck,
    ) -> Output<'a> {
        // If there's just a single choice we can just generate the code for
        // it: no need to do any checking, we know it must match!
        if choices.is_empty() {
            if let FallbackCheck::RuntimeCheck { check } = fallback_check {
                self.variables.record_check_assignments(var, check);
            }
            return self.decision(fallback);
        }

        // Otherwise we'll have to generate a series of if-else to check which
        // pattern is going to match!
        let mut assignments = vec![];
        if !self.variables.is_bound_in_scope(var) {
            // If the variable is not bound already we will be binding it to a
            // new made up name so we can use this variable to reference it and
            // never do any duplicate work recomputing its value every time
            // this pattern variable is used.
            let name = self.variables.next_local_var(&ASSIGNMENT_VAR.into());
            let value = self.variables.get_value(var);
            self.variables.bind(name.clone(), var);
            assignments.push(let_doc(name, value.to_doc()))
        };

        let mut if_ = nil();
        for (i, (check, decision)) in choices.iter().enumerate() {
            self.variables.record_check_assignments(var, check);

            let (check_doc, body, mut check_assignments) = self.inside_new_scope(|this| {
                let mut check_assignments = vec![];
                for (segment_name, _) in check.referenced_segment_patterns() {
                    // TODO)) Aggiungi documentazione!
                    if this.variables.segment_is_bound_in_scope(segment_name) {
                        continue;
                    }

                    let segment_local_name = this.variables.next_local_var(&segment_name);
                    let segment_value = this
                        .variables
                        .get_segment_value(segment_name)
                        .expect("segment referenced in a check before being created");

                    this.variables
                        .bind_segment(segment_name.clone(), segment_local_name.clone());
                    check_assignments.push(let_doc(segment_local_name, segment_value))
                }

                let check_doc =
                    this.variables
                        .runtime_check(var, check, CheckNegation::NotNegated)?;
                let body = this.decision(decision);
                Ok((check_doc, body, check_assignments))
            })?;

            assignments.append(&mut check_assignments);

            let branch = if i == 0 {
                docvec!["if (", check_doc, ") "]
            } else {
                docvec![" else if (", check_doc, ") "]
            };
            if_ = if_.append(docvec![branch, break_block(body?)]);
        }

        // In case there's some new variables we can extract after the
        // successful final check we store those. But we don't need to perform
        // the check itself: the type system makes sure that, if we ever
        // get here, the check is going to match no matter what!
        if let FallbackCheck::RuntimeCheck { check } = fallback_check {
            self.variables.record_check_assignments(var, check);
        }

        let body = self.inside_new_scope(|this| this.decision(fallback))?;
        let if_ = join_with_line(join(assignments, line()), if_);
        if body.is_empty() {
            Ok(if_)
        } else {
            let else_ = docvec![" else ", break_block(body)];
            Ok(docvec![if_, else_])
        }
    }

    fn inside_new_scope<A, F>(&mut self, run: F) -> A
    where
        F: Fn(&mut Self) -> A,
    {
        let old_scope = self
            .variables
            .expression_generator
            .current_scope_vars
            .clone();
        let old_names = self.variables.scoped_variable_names.clone();
        let old_segments = self.variables.segment_values.clone();
        let old_segment_names = self.variables.scoped_segment_names.clone();
        let output = run(self);
        self.variables.expression_generator.current_scope_vars = old_scope;
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
    ) -> Output<'a> {
        let guard = self
            .clauses
            .get(guard)
            .expect("invalid clause index")
            .guard
            .as_ref()
            .expect("missing guard");

        // Before generating the if-else condition we want to generate all the
        // assignments that will be needed by the guard condition so we can rest
        // assured they are in scope and the if condition can use those.
        let guard_variables = guard.referenced_variables();
        let (check_bindings, if_true_bindings): (Vec<_>, Vec<_>) = if_true
            .bindings
            .iter()
            .partition(|(variable, _)| guard_variables.contains(variable));

        let check_bindings = self.variables.bindings_ref_doc(&check_bindings);
        let check = self.variables.expression_generator.guard(guard)?;
        let if_true = self.inside_new_scope(|this| {
            // All the other bindings are not needed by the guard check will
            // end up directly in the body of the if clause to avoid doing any
            // extra work before making sure the condition is true and they are
            // actually needed.
            let if_true_bindings = this.variables.bindings_ref_doc(&if_true_bindings);
            let if_true_body = this.body_expression(if_true.clause_index)?;
            Ok(join_with_line(if_true_bindings, if_true_body))
        })?;
        let if_false = self.inside_new_scope(|this| this.decision(if_false))?;

        // We can now piece everything together into a single document!
        let if_ = docvec!["if (", check, ") ", break_block(if_true)];
        let if_ = join_with_line(check_bindings, if_);
        if if_false.is_empty() {
            Ok(docvec![if_])
        } else {
            let else_ = docvec![" else ", break_block(if_false)];
            Ok(docvec![if_, else_])
        }
    }
}

/// Prints the code for a let assignment (it could either be a let assert or
/// just a destructuring let, either cases are handled correctly by the decision
/// tree!)
///
pub fn let_<'a>(
    compiled_case: &'a CompiledCase,
    subject: &'a TypedExpr,
    kind: &'a AssignmentKind<TypedExpr>,
    expression_generator: &mut Generator<'_, 'a>,
) -> Output<'a> {
    let scope_position = expression_generator.scope_position.clone();
    let mut variables = Variables::new(expression_generator);
    let assignment = variables.assign_subject(compiled_case, subject)?;
    let assignment_name = assignment.name();
    let decision = LetPrinter::new(variables, kind)
        .decision(assignment_name.clone().to_doc(), &compiled_case.tree)?;

    let doc = docvec![assignments_to_doc(vec![assignment]), decision];
    Ok(match scope_position {
        expression::Position::NotTail(_ordering) => doc,
        expression::Position::Tail => docvec![doc, line(), "return ", assignment_name, ";"],
        expression::Position::Assign(variable) => {
            docvec![doc, line(), variable, " = ", assignment_name, ";"]
        }
    })
}

struct LetPrinter<'generator, 'module, 'a> {
    variables: Variables<'generator, 'module, 'a>,
    kind: &'a AssignmentKind<TypedExpr>,
    is_redundant: bool,
}

impl<'generator, 'module, 'a> LetPrinter<'generator, 'module, 'a> {
    fn new(
        variables: Variables<'generator, 'module, 'a>,
        kind: &'a AssignmentKind<TypedExpr>,
    ) -> Self {
        Self {
            variables,
            kind,
            is_redundant: true,
        }
    }

    fn decision(&mut self, subject: Document<'a>, decision: &'a Decision) -> Output<'a> {
        let Some(ChecksAndBindings { checks, bindings }) =
            self.positive_checks_and_bindings(decision)
        else {
            // In case we never reach a body, we know that this let assert will
            // always throw an exception!
            self.variables.expression_generator.statement_always_throws = true;
            return self.assignment_no_match(subject);
        };

        for (variable, check) in checks.iter() {
            self.variables.record_check_assignments(variable, check);
        }

        let checks = if self.is_redundant {
            nil()
        } else {
            // TODO)) Find the size checks on all bit arrays and only keep the
            // most restrictive one. All the other ones can be ignored as they
            // are already checked by checking the most restrictive one is not
            // violated.
            //
            // Most restrictive means: `==` over `>=`, and `>= a` over `>= b`
            // if `a > b`.
            //
            let checks = checks.iter().filter_map(|(variable, check)| {
                self.variables.record_check_assignments(variable, check);
                match check {
                    RuntimeCheck::Tuple { .. } => None,
                    _ => self
                        .variables
                        .runtime_check(variable, check, CheckNegation::Negated)
                        .ok(),
                }
            });
            docvec![break_("", ""), join(checks, break_(" ||", " || "))]
                .nest(INDENT)
                .append(break_("", ""))
                .group()
        };

        let doc = if checks.is_empty() {
            nil()
        } else {
            let exception = self.assignment_no_match(subject)?;
            docvec!["if (", checks, ") ", break_block(exception)]
        };

        let body_bindings = bindings
            .iter()
            .map(|(name, value)| self.variables.body_binding_doc(name, value));
        let body_bindings = join(body_bindings, line());
        Ok(join_with_line(doc, body_bindings))
    }

    fn assignment_no_match(&mut self, subject: Document<'a>) -> Output<'a> {
        let AssignmentKind::Assert { location, message } = self.kind else {
            unreachable!("inexhaustive let made it to code generation");
        };

        let generator = &mut self.variables.expression_generator;
        let message = match message {
            None => string("Pattern match failed, no pattern matched the value."),
            Some(message) => generator.not_in_tail_position(Some(Ordering::Strict), |this| {
                this.wrap_expression(message)
            })?,
        };
        Ok(generator.throw_error("let_assert", &message, *location, [("value", subject)]))
    }

    /// A let decision tree has a very precise structure since it's made of a
    /// single pattern. It will always be a very narrow tree that has a singe
    /// success node with a series of checks leading down to it. If any of those
    /// checks fails it immediately leads to a `Fail` node that has to result
    /// in an exception. For example:
    ///
    /// ```gleam
    /// let assert [1, 2, ..] = list
    /// ```
    ///
    /// Will have to check that the first item is `1` and the second one is `2`,
    /// if any of those checks fail the entire assignment fails.
    ///
    /// So we can traverse such a decision tree and collect the sequence of checks
    /// that need to succeed in order for the assignment to succeed. We also return
    /// all the bindings that we discover in the final `Run` block so they can be
    /// used by later code generation steps without having to traverse the whole
    /// decision tree once again!
    ///
    /// If there's no `Run` node it means that this pattern will _always_ fail
    /// and there's no useful data we could ever return.
    /// This could happen due to variant inference (e.g. `let assert Wibble = Wobble`).
    ///
    fn positive_checks_and_bindings(
        &mut self,
        decision: &'a Decision,
    ) -> Option<ChecksAndBindings<'a>> {
        match decision {
            Decision::Run { body, .. } => Some(ChecksAndBindings::new(&body.bindings)),
            Decision::Guard { .. } => unreachable!("guard in let assert decision tree"),
            Decision::Fail => {
                // If the check could fail it means that the entire let is not
                // redundant!
                self.is_redundant = false;
                None
            }
            Decision::Switch {
                var,
                choices,
                fallback,
                fallback_check,
            } => {
                let mut result = None;

                // We go over all the decision to record all the bindings and
                // see if we can get to a failing node. We will only keep the
                // results coming from the positive path!
                for (check, decision) in choices {
                    if let Some(mut checks) = self.positive_checks_and_bindings(decision) {
                        checks.add_check(var.clone(), check);
                        result = Some(checks);
                    };
                }

                // Important not to forget the fallback check, that's a path we
                // might have to go down to as well!
                if let Some(mut checks) = self.positive_checks_and_bindings(fallback) {
                    if let FallbackCheck::RuntimeCheck { check } = fallback_check {
                        checks.add_check(var.clone(), check);
                        result = Some(checks);
                    };
                };

                result
            }
        }
    }
}

/// The result we get from inspecting a `let`'s decision tree: it contains all
/// the checks that lead down to the only possible successfull `Body` node and
/// the bindings found inside it.
///
struct ChecksAndBindings<'a> {
    checks: VecDeque<(Variable, &'a RuntimeCheck)>,
    bindings: &'a Vec<(EcoString, BoundValue)>,
}

impl<'a> ChecksAndBindings<'a> {
    fn new(bindings: &'a Vec<(EcoString, BoundValue)>) -> Self {
        Self {
            checks: VecDeque::new(),
            bindings,
        }
    }

    fn add_check(&mut self, variable: Variable, check: &'a RuntimeCheck) {
        self.checks.push_front((variable, check));
    }
}

/// This is a useful piece of state that is kept separate from the generator
/// itself so we can reuse it both with cases and let asserts without rewriting
/// everything from scratch.
///
struct Variables<'generator, 'module, 'a> {
    expression_generator: &'generator mut Generator<'module, 'a>,

    /// All the pattern variables will be assigned a specific value: being bound
    /// to a constructor field, tuple element and so on. Pattern variables never
    /// end up in the generated code but we replace them with their actual value.
    /// We store those values as we find them in this map.
    ///
    variable_values: HashMap<usize, EcoString>,

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

    // TODO)) Aggiungere documentazione
    segment_values: HashMap<EcoString, Document<'a>>,
    scoped_segment_names: HashMap<EcoString, EcoString>,
}

impl<'generator, 'module, 'a> Variables<'generator, 'module, 'a> {
    fn new(expression_generator: &'generator mut Generator<'module, 'a>) -> Self {
        Variables {
            expression_generator,
            variable_values: HashMap::new(),
            scoped_variable_names: HashMap::new(),
            segment_values: HashMap::new(),
            scoped_segment_names: HashMap::new(),
        }
    }

    fn assign_subjects(
        &mut self,
        compiled_case: &'a CompiledCase,
        subjects: &'a [TypedExpr],
    ) -> Result<Vec<SubjectAssignment<'a>>, Error> {
        let assignments = assign_subjects(self.expression_generator, subjects)?;
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
        Ok(assignments)
    }

    fn assign_subject(
        &mut self,
        compiled_case: &'a CompiledCase,
        subject: &'a TypedExpr,
    ) -> Result<SubjectAssignment<'a>, Error> {
        let variable = compiled_case
            .subject_variables
            .first()
            .expect("decision tree with no subjects");
        let assignment = assign_subject(self.expression_generator, subject)?;
        self.set_value(variable, assignment.name());
        self.bind(assignment.name(), variable);
        Ok(assignment)
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
    /// code to materialese one of the pattern variables and give it a name to
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

    fn bind_segment(&mut self, segment_name: EcoString, bound_to_variable: EcoString) {
        let _ = self
            .scoped_segment_names
            .insert(segment_name, bound_to_variable);
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
            BoundValue::BitArraySlice {
                bit_array,
                read_action,
            } => self
                .get_segment_value(&variable_name)
                .unwrap_or_else(|| self.read_action_to_doc(bit_array, read_action)),
        };
        let_doc(local_variable_name.clone(), assigned_value)
    }

    fn runtime_check(
        &mut self,
        variable: &Variable,
        runtime_check: &'a RuntimeCheck,
        negation: CheckNegation,
    ) -> Output<'a> {
        let value = self.get_value(variable);

        let equality = if negation.is_negated() {
            " !== "
        } else {
            " === "
        };

        let result = match runtime_check {
            RuntimeCheck::String { value: expected } => docvec![value, equality, string(expected)],
            RuntimeCheck::Float { value: expected } => docvec![value, equality, float(expected)],
            RuntimeCheck::Int { value: expected } => docvec![value, equality, int(expected)],
            RuntimeCheck::StringPrefix { prefix, .. } => {
                let negation = if negation.is_negated() {
                    "!".to_doc()
                } else {
                    nil()
                };

                docvec![negation, value, ".startsWith(", string(prefix), ")"]
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

                // Here we need to make sure that the bit array has a specific
                // size.
                BitArrayTest::Size(SizeTest { operator, size }) => {
                    let operator = match operator {
                        SizeOperator::GreaterEqual if negation.is_negated() => " < ",
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
                    BitArrayMatchedValue::LiteralString(expected) => self
                        .literal_string_segment_bytes_check(value, expected, read_action, negation),
                    BitArrayMatchedValue::LiteralFloat(expected) => self
                        .literal_float_segment_bytes_check(value, expected, read_action, negation),
                    BitArrayMatchedValue::LiteralInt(expected) => self
                        .literal_int_segment_bytes_check(
                            value,
                            expected.clone(),
                            read_action,
                            negation,
                        )?,
                    BitArrayMatchedValue::Variable(..) | BitArrayMatchedValue::Discard(..) => {
                        panic!("unreachable")
                    }
                },
            },

            // When checking on a tuple there's always going to be a single choice
            // and the code generation will always skip generating the check for it
            // as the type system ensures it must match.
            RuntimeCheck::Tuple { .. } => unreachable!("tried generating runtime check for tuple"),

            RuntimeCheck::Variant { match_, .. } if variable.type_.is_bool() => {
                match (match_.name().as_str(), negation) {
                    ("True", CheckNegation::NotNegated) => value.to_doc(),
                    ("True", CheckNegation::Negated) => docvec!["!", value],
                    (_, CheckNegation::NotNegated) => docvec!["!", value],
                    (_, CheckNegation::Negated) => value.to_doc(),
                }
            }

            RuntimeCheck::Variant { match_, .. } if variable.type_.is_result() => {
                match (match_.name().as_str(), negation) {
                    ("Ok", CheckNegation::NotNegated) => docvec![value, ".isOk()"],
                    ("Ok", CheckNegation::Negated) => docvec!["!", value, ".isOk()"],
                    (_, CheckNegation::NotNegated) => docvec!["!", value, ".isOk()"],
                    (_, CheckNegation::Negated) => docvec![value, ".isOk()"],
                }
            }

            RuntimeCheck::Variant { .. } if variable.type_.is_nil() && negation.is_negated() => {
                "false".to_doc()
            }

            RuntimeCheck::Variant { match_, .. } => {
                let qualification = match_
                    .module()
                    .map(|module| eco_format!("${module}."))
                    .unwrap_or_default();

                let check = docvec![value, " instanceof ", qualification, match_.name()];
                if negation.is_negated() {
                    docvec!["!(", check, ")"]
                } else {
                    check
                }
            }

            RuntimeCheck::NonEmptyList { .. } if negation.is_negated() => {
                self.expression_generator.tracker.list_empty_class_used = true;
                docvec![value, " instanceof $Empty"]
            }

            RuntimeCheck::NonEmptyList { .. } => {
                self.expression_generator.tracker.list_non_empty_class_used = true;
                docvec![value, " instanceof $NonEmpty"]
            }

            RuntimeCheck::EmptyList if negation.is_negated() => {
                self.expression_generator.tracker.list_non_empty_class_used = true;
                docvec![value, " instanceof $NonEmpty"]
            }

            RuntimeCheck::EmptyList => {
                self.expression_generator.tracker.list_empty_class_used = true;
                docvec![value, " instanceof $Empty"]
            }
        };

        Ok(result)
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

        match (size, from.constant_bits()) {
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

            // If we're reading all the remaining bits/bytes of an array.
            (ReadSize::RemainingBits | ReadSize::RemainingBytes, _) => {
                self.bit_array_slice(bit_array, from)
            }

            // If both the start and and are known at compile time we can use
            // those directly in the slice call.
            (ReadSize::ConstantBits(size), Some(from_bits)) => {
                let start = from_bits.clone();
                let end = from_bits + size;
                match type_ {
                    ReadType::Int => {
                        self.bit_array_slice_to_int(bit_array, start, end, endianness, *signed)
                    }
                    ReadType::Float => {
                        self.bit_array_slice_to_float(bit_array, start, end, endianness)
                    }
                    ReadType::BitArray => self.bit_array_slice_with_end(bit_array, from, end),
                    _ => panic!(
                        "invalid slice type made it to code generation: {:#?}",
                        type_
                    ),
                }
            }

            // Otherwise we need to add the variable and constant parts together
            // in order to take a slice out of the bit array.
            (size, _) => {
                let size = self.read_size_to_doc(size).expect("no variable size");
                let start = self.offset_to_doc(from, false);
                let end = if from.is_zero() {
                    size
                } else {
                    docvec![start.clone(), " + ", size]
                };

                match type_ {
                    ReadType::Int => {
                        self.bit_array_slice_to_int(bit_array, start, end, endianness, *signed)
                    }
                    ReadType::Float => {
                        self.bit_array_slice_to_float(bit_array, start, end, endianness)
                    }
                    ReadType::BitArray => self.bit_array_slice_with_end(bit_array, from, end),
                    _ => panic!("invalid slice type made it to code generation"),
                }
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
                    docvec![variable, " * ", unit]
                })
            }
            ReadSize::RemainingBits | ReadSize::RemainingBytes => None,
        }
    }

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
        let args = join(
            [
                bit_array.to_doc(),
                start.to_doc(),
                end.to_doc(),
                endianness.to_doc(),
                signed.to_doc(),
            ],
            ", ".to_doc(),
        );
        docvec!["bitArraySliceToInt(", args, ")"]
    }

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
        let args = join(
            [
                bit_array.to_doc(),
                start.to_doc(),
                end.to_doc(),
                endianness.to_doc(),
            ],
            ", ".to_doc(),
        );
        docvec!["bitArraySliceToFloat(", args, ")"]
    }

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

    fn bit_array_slice(&mut self, bit_array: impl Documentable<'a>, from: &Offset) -> Document<'a> {
        self.expression_generator.tracker.bit_array_slice_used = true;
        let from = self.offset_to_doc(from, false);
        docvec!["bitArraySlice(", bit_array, ", ", from, ")"]
    }

    fn literal_string_segment_bytes_check(
        &mut self,
        // A document representing the bit array value we read bits from.
        bit_array: EcoString,
        literal_string: &EcoString,
        read_action: &ReadAction,
        check_negation: CheckNegation,
    ) -> Document<'a> {
        let ReadAction {
            from: start,
            endianness,
            signed,
            ..
        } = read_action;
        let mut checks = vec![];

        let equality = if check_negation.is_negated() {
            " !== "
        } else {
            " === "
        };

        if let Some(mut from_byte) = start.constant_bytes() {
            for byte in convert_string_escape_chars(literal_string).as_bytes() {
                let byte_access = docvec![bit_array.clone(), ".byteAt(", from_byte.clone(), ")"];
                checks.push(docvec![byte_access, equality, byte]);
                from_byte += 1;
            }
        } else {
            for byte in convert_string_escape_chars(literal_string).as_bytes() {
                let end = self.offset_to_doc(&start.add_constant(8), false);
                let from = self.offset_to_doc(start, false);
                let byte_access =
                    self.bit_array_slice_to_int(&bit_array, from, end, endianness, *signed);
                checks.push(docvec![byte_access, equality, byte]);
            }
        }

        if check_negation.is_negated() {
            join(checks, break_(" ||", " || ")).group()
        } else {
            join(checks, break_(" &&", " && ")).nest(INDENT).group()
        }
    }

    fn literal_int_segment_bytes_check(
        &mut self,
        // A document representing the bit array value we read bits from.
        bit_array: EcoString,
        literal_int: BigInt,
        read_action: &ReadAction,
        check_negation: CheckNegation,
    ) -> Output<'a> {
        let ReadAction {
            from: start,
            size,
            endianness,
            signed,
            ..
        } = read_action;

        let equality = if check_negation.is_negated() {
            " !== "
        } else {
            " === "
        };

        if let (Some(mut from_byte), Some(size)) = (start.constant_bytes(), size.constant_bytes()) {
            let mut checks = vec![];
            for byte in bit_array_segment_int_value_to_bytes(literal_int, size * 8, *endianness)? {
                let byte_access = docvec![bit_array.clone(), ".byteAt(", from_byte.clone(), ")"];
                checks.push(docvec![byte_access, equality, byte]);
                from_byte += 1;
            }

            Ok(if check_negation.is_negated() {
                join(checks, break_(" ||", " || ")).group()
            } else {
                join(checks, break_(" &&", " && ")).nest(INDENT).group()
            })
        } else {
            let start_doc = self.offset_to_doc(start, false);
            let end = match (start.constant_bits(), size.constant_bits()) {
                (Some(start), _) if start == BigInt::ZERO => self
                    .read_size_to_doc(size)
                    .expect("unexpected catch all size"),
                (Some(start), Some(end)) => (start + end).to_doc(),
                (_, _) => docvec![start_doc.clone(), " + ", self.read_size_to_doc(size)],
            };
            let check = self.bit_array_slice_to_int(bit_array, start_doc, end, endianness, *signed);
            Ok(docvec![check, equality, literal_int])
        }
    }

    fn literal_float_segment_bytes_check(
        &mut self,
        // A document representing the bit array value we read bits from.
        bit_array: EcoString,
        expected: &EcoString,
        read_action: &ReadAction,
        check_negation: CheckNegation,
    ) -> Document<'a> {
        let ReadAction {
            from: start,
            size,
            endianness,
            ..
        } = read_action;

        let equality = if check_negation.is_negated() {
            " !== "
        } else {
            " === "
        };

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
    /// actual value to be used by later checks and assignments
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

#[derive(Eq, PartialEq)]
enum CheckNegation {
    Negated,
    NotNegated,
}

impl CheckNegation {
    fn is_negated(&self) -> bool {
        match self {
            CheckNegation::Negated => true,
            CheckNegation::NotNegated => false,
        }
    }
}

/// When going over the subjects of a case expression/let we might end up in two
/// situation: the subject might be a variable or it could be a more complex
/// expression (like a function call, math expression, ...).
///
/// ```gleam
/// case a_variable { ... }
/// case a_function_call(wobble) { ... }
/// ```
///
/// When checking on a case we might end up repeating the subjects multiple times
/// (as they need to appear in various checks), this means that if we ended up
/// doing the simple thing of just repeating the subject as it is we might end
/// up dramatically changin the meaning of the program when the subject is a
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
/// So we need to categorize each subject in two categories: if it is a simple
/// variable already, it's no big deal and we can repeat that name as many times
/// as we want; however if it's anything else we first need to bind that subject
/// to a variable we can than reference multiple times.
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

fn assign_subjects<'a>(
    expression_generator: &mut Generator<'_, 'a>,
    subjects: &'a [TypedExpr],
) -> Result<Vec<SubjectAssignment<'a>>, Error> {
    let mut out = Vec::with_capacity(subjects.len());
    for subject in subjects {
        out.push(assign_subject(expression_generator, subject)?)
    }
    Ok(out)
}

fn assign_subject<'a>(
    expression_generator: &mut Generator<'_, 'a>,
    subject: &'a TypedExpr,
) -> Result<SubjectAssignment<'a>, Error> {
    static ASSIGNMENT_VAR_ECO_STR: OnceLock<EcoString> = OnceLock::new();

    match subject {
        // If the value is a variable we don't need to assign it to a new
        // variable, we can use the value expression safely without worrying about
        // performing computation or side effects multiple times.
        TypedExpr::Var {
            name, constructor, ..
        } if constructor.is_local_variable() => Ok(SubjectAssignment::AlreadyAVariable {
            name: expression_generator.local_var(name),
        }),

        // If it's not a variable we need to assign it to a variable
        // to avoid rendering the subject expression multiple times
        _ => {
            let name = expression_generator
                .next_local_var(ASSIGNMENT_VAR_ECO_STR.get_or_init(|| ASSIGNMENT_VAR.into()));
            let value = expression_generator
                .not_in_tail_position(Some(Ordering::Strict), |this| {
                    this.wrap_expression(subject)
                })?;

            Ok(SubjectAssignment::BindToVariable { value, name })
        }
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

fn let_doc(variable_name: EcoString, value: Document<'_>) -> Document<'_> {
    docvec!["let ", variable_name, " = ", value, ";"]
}

/// Calculates the length of str as utf16 without escape characters.
///
fn utf16_no_escape_len(str: &EcoString) -> usize {
    convert_string_escape_chars(str).encode_utf16().count()
}
