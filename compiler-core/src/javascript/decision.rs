use super::{
    Output,
    expression::{Generator, Ordering},
};
use crate::{
    ast::{TypedClauseGuard, TypedExpr},
    docvec,
    exhaustiveness::{
        Body, BoundValue, CompiledCase, Decision, FallbackCheck, RuntimeCheck, Variable,
    },
    format::break_block,
    javascript::expression::{string, string_from_eco},
    pretty::{Document, Documentable, join, line, nil},
    strings::convert_string_escape_chars,
};
use ecow::{EcoString, eco_format};
use std::{collections::HashMap, sync::OnceLock};

pub static ASSIGNMENT_VAR: &str = "$";

pub fn print<'a>(
    compiled_case: &'a CompiledCase,
    clauses: Vec<(&'a TypedExpr, Option<&'a TypedClauseGuard>)>,
    subjects: &'a [TypedExpr],
    expression_generator: &mut Generator<'_, 'a>,
) -> Output<'a> {
    // The case subjects might be repeated in the generated code, so we want to
    // assign those to variables (if they're not already ones) and use those;
    // otherwise we'd end up calling the same functions multiple times, which
    // would change the program's meaning!
    let subjects_assignments = assign_subjects(expression_generator, subjects);

    let mut printer = DecisionPrinter {
        clauses,
        expression_generator,
        variable_values: HashMap::new(),
        scoped_variable_names: HashMap::new(),
    };

    // Might have to add those to the scope!!!!
    for (var, (subject_value, assignment)) in compiled_case
        .subject_variables
        .iter()
        .zip(subjects_assignments.iter())
    {
        printer.set_pattern_variable_value(var, subject_value.clone());
        if let Some(name) = assignment {
            printer.bind_variable(name.clone(), var);
        } else {
            printer.bind_variable(subject_value.clone(), var);
        }
    }

    let decision = printer.decision(&compiled_case.tree)?;

    // Then if there's any assignment we write those before the generated
    // decision tree.
    let mut subject_assignments_docs = vec![];
    for ((_, assignment), subject) in subjects_assignments.into_iter().zip(subjects) {
        let Some(var) = assignment else { continue };
        let value = expression_generator
            .not_in_tail_position(Some(Ordering::Strict), |this| this.wrap_expression(subject))?;
        subject_assignments_docs.push(let_(var, value).append(line()))
    }

    Ok(docvec![subject_assignments_docs, decision].force_break())
}

pub struct DecisionPrinter<'module, 'generator, 'a> {
    clauses: Vec<(&'a TypedExpr, Option<&'a TypedClauseGuard>)>,
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
}

impl<'a> DecisionPrinter<'_, '_, 'a> {
    fn set_pattern_variable_value(&mut self, variable: &Variable, value: EcoString) {
        let _ = self.variable_values.insert(variable.id, value);
    }

    fn bind_variable(&mut self, name: EcoString, variable: &Variable) {
        let _ = self.scoped_variable_names.insert(variable.id, name);
    }

    #[must_use]
    fn is_bound_in_scope(&self, variable: &Variable) -> bool {
        self.scoped_variable_names.contains_key(&variable.id)
    }

    fn get_variable_value(&self, variable: &Variable) -> EcoString {
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

    fn decision(&mut self, decision: &'a Decision) -> Output<'a> {
        match decision {
            Decision::Fail => unreachable!("Invalid decision tree reached code generation"),
            Decision::Run { body } => {
                let bindings = self.bindings(&body.bindings);
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

    fn bindings(&mut self, bindings: &'a [(EcoString, BoundValue)]) -> Document<'a> {
        let bindings = (bindings.iter()).map(|(variable, value)| self.binding(variable, value));
        join(bindings, line())
    }

    fn bindings_ref(&mut self, bindings: &[&'a (EcoString, BoundValue)]) -> Document<'a> {
        let bindings = (bindings.iter()).map(|(variable, value)| self.binding(variable, value));
        join(bindings, line())
    }

    fn body_expression(&mut self, clause_index: usize) -> Output<'a> {
        let (body, _) = &self
            .clauses
            .get(clause_index)
            .expect("invalid clause index");

        self.expression_generator.expression_flattening_blocks(body)
    }

    fn binding(&mut self, variable_name: &'a EcoString, value: &'a BoundValue) -> Document<'a> {
        let variable_name = self.expression_generator.next_local_var(variable_name);
        let assigned_value = match value {
            BoundValue::Variable(variable) => self.get_variable_value(variable).to_doc(),
            BoundValue::LiteralString(value) => string(value),
        };
        let_(variable_name.clone(), assigned_value)
    }

    fn switch(
        &mut self,
        var: &'a Variable,
        choices: &'a [(RuntimeCheck, Box<Decision>)],
        fallback: &'a Decision,
        fallback_check: &'a FallbackCheck,
    ) -> Output<'a> {
        match choices {
            // If there's just a single choice we can just generate the code for
            // it: no need to do any checking, we know it must match!
            [] => {
                if let FallbackCheck::RuntimeCheck { check } = fallback_check {
                    self.record_check_assignments(var, check);
                }
                self.decision(fallback)
            }
            _ => {
                let mut if_ = if self.is_bound_in_scope(var) {
                    // If the variable is already bound to a name in the current
                    // scope we don't have to generate any additional code...
                    nil()
                } else {
                    // ... if it's not we will be binding the value to a variable
                    // name so we can use this variable to reference it and never
                    // do any duplicate work recomputing its value every time
                    // this pattern variable is used.
                    let name = self.expression_generator.next_local_var(&"pattern".into());
                    let value = self.get_variable_value(var);
                    self.bind_variable(name.clone(), var);
                    docvec![let_(name, value.to_doc()), line()]
                };

                for (i, (check, decision)) in choices.iter().enumerate() {
                    self.record_check_assignments(var, check);
                    let check_doc = self.runtime_check(var, check);
                    let body = self.inside_new_scope(|this| this.decision(decision))?;

                    let branch = if i == 0 {
                        docvec!["if (", check_doc, ") "]
                    } else {
                        docvec![" else if (", check_doc, ") "]
                    };
                    if_ = if_.append(docvec![branch, break_block(body)]);
                }

                // In case there's some new variables we can extract after the
                // successful check we store those. But we don't need to perform
                // the check itself: the type system makes sure that, if we ever
                // get here, the check is going to match no matter what!
                if let FallbackCheck::RuntimeCheck { check } = fallback_check {
                    self.record_check_assignments(var, check);
                }

                let body = self.inside_new_scope(|this| this.decision(fallback))?;
                if body.is_empty() {
                    Ok(if_)
                } else {
                    let else_ = docvec![" else ", break_block(body)];
                    Ok(docvec![if_, else_])
                }
            }
        }
    }

    fn inside_new_scope<A, F>(&mut self, run: F) -> A
    where
        F: Fn(&mut Self) -> A,
    {
        let old_scope = self.expression_generator.current_scope_vars.clone();
        let old_names = self.scoped_variable_names.clone();
        let output = run(self);
        self.expression_generator.current_scope_vars = old_scope;
        self.scoped_variable_names = old_names;
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
            .1
            .expect("missing guard");

        // Before generating the if-else condition we want to generate all the
        // assignments that will be needed by the guard condition so we can rest
        // assured they are in scope for the if condition to use those.
        let guard_variables = guard.referenced_variables();
        let (check_bindings, if_true_bindings): (Vec<_>, Vec<_>) = if_true
            .bindings
            .iter()
            .partition(|(variable, _)| guard_variables.contains(variable));

        let check_bindings = self.bindings_ref(&check_bindings);
        let check = self.expression_generator.guard(guard)?;
        let if_true = self.inside_new_scope(|this| {
            // All the other bindings are not needed by the guard check will
            // end up directly in the body of the if clause to avoid doing any
            // extra work before making sure the condition is true and they are
            // actually needed.
            let if_true_bindings = this.bindings_ref(&if_true_bindings);
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

    fn runtime_check(&mut self, var: &Variable, runtime_check: &RuntimeCheck) -> Document<'a> {
        let var_value = self.get_variable_value(var);
        match runtime_check {
            RuntimeCheck::String { value } => {
                docvec![var_value, " === ", string_from_eco(value.clone())]
            }

            RuntimeCheck::Int { value } | RuntimeCheck::Float { value } => {
                docvec![var_value, " === ", value]
            }

            RuntimeCheck::StringPrefix { prefix, .. } => docvec![
                var_value,
                ".startsWith(",
                string_from_eco(prefix.clone()),
                ")"
            ],

            RuntimeCheck::BitArray { value } => docvec!["TODO"],

            // When checking on a tuple there's always going to be a single choice
            // and the code generation will always skip generating the check for it
            // as the type system ensures it must match.
            RuntimeCheck::Tuple { .. } => unreachable!("tried generating runtime check for tuple"),

            RuntimeCheck::Variant { match_, .. } if var.type_.is_bool() => {
                if match_.name() == "True" {
                    var_value.to_doc()
                } else {
                    docvec!["!", var_value]
                }
            }

            RuntimeCheck::Variant { match_, .. } if var.type_.is_result() => {
                docvec![var_value, ".is", match_.name(), "()"]
            }

            RuntimeCheck::Variant { match_, .. } => {
                let qualification = match_
                    .module()
                    .map(|module| eco_format!("${module}."))
                    .unwrap_or_default();

                docvec![var_value, " instanceof ", qualification, match_.name()]
            }
            RuntimeCheck::NonEmptyList { .. } => docvec!["!", var_value, ".hasLength(0)"],
            RuntimeCheck::EmptyList => docvec![var_value, ".hasLength(0)"],
        }
    }

    /// In case the check introduces new variables, this will record their
    /// actual value to be used by later checks and assignments
    ///
    fn record_check_assignments(&mut self, var: &Variable, check: &RuntimeCheck) {
        let value = self.get_variable_value(var);
        match check {
            RuntimeCheck::Int { .. }
            | RuntimeCheck::Float { .. }
            | RuntimeCheck::String { .. }
            | RuntimeCheck::BitArray { .. }
            | RuntimeCheck::EmptyList => (),

            RuntimeCheck::StringPrefix { rest, prefix } => {
                let prefix_size = utf16_no_escape_len(prefix);
                self.set_pattern_variable_value(rest, eco_format!("{value}.slice({prefix_size})"));
            }

            RuntimeCheck::Tuple { elements, .. } => {
                for (i, element) in elements.iter().enumerate() {
                    self.set_pattern_variable_value(element, eco_format!("{value}[{i}]"));
                }
            }

            RuntimeCheck::Variant { fields, labels, .. } => {
                for (i, field) in fields.iter().enumerate() {
                    let access = match labels.get(&i) {
                        Some(label) => eco_format!("{value}.{label}"),
                        None => eco_format!("{value}[{i}]"),
                    };
                    self.set_pattern_variable_value(field, access);
                }
            }

            RuntimeCheck::NonEmptyList { first, rest } => {
                self.set_pattern_variable_value(first, eco_format!("{value}.head"));
                self.set_pattern_variable_value(rest, eco_format!("{value}.tail"));
            }
        }
    }
}

fn let_(variable_name: EcoString, value: Document<'_>) -> Document<'_> {
    docvec!["let ", variable_name, " = ", value, ";"]
}

/// Calculates the length of str as utf16 without escape characters.
///
fn utf16_no_escape_len(str: &EcoString) -> usize {
    convert_string_escape_chars(str).encode_utf16().count()
}

fn assign_subjects<'a>(
    expression_generator: &mut Generator<'_, 'a>,
    subjects: &'a [TypedExpr],
) -> Vec<(EcoString, Option<EcoString>)> {
    let mut out = Vec::with_capacity(subjects.len());
    for subject in subjects {
        out.push(assign_subject(expression_generator, subject))
    }
    out
}

fn assign_subject<'a>(
    expression_generator: &mut Generator<'_, 'a>,
    subject: &'a TypedExpr,
) -> (EcoString, Option<EcoString>) {
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
