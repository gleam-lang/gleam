use super::{
    Error, INDENT, Output,
    expression::{Generator, Ordering, float, int},
};
use crate::{
    ast::{SrcSpan, TypedClause, TypedExpr},
    docvec,
    exhaustiveness::{
        Body, BoundValue, CompiledCase, Decision, FallbackCheck, RuntimeCheck, Variable,
    },
    format::break_block,
    javascript::{expression::string, maybe_escape_property},
    pretty::{Document, Documentable, break_, join, line, nil},
    strings::convert_string_escape_chars,
};
use ecow::{EcoString, eco_format};
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

impl<'a> CasePrinter<'_, '_, 'a> {
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
        let body = &self
            .clauses
            .get(clause_index)
            .expect("invalid clause index")
            .then;

        self.variables
            .expression_generator
            .expression_flattening_blocks(body)
    }

    fn binding(&mut self, variable_name: &'a EcoString, value: &'a BoundValue) -> Document<'a> {
        let variable_name = self.variables.next_local_var(variable_name);
        let assigned_value = match value {
            BoundValue::Variable(variable) => self.variables.get_value(variable).to_doc(),
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

        let mut if_ = if self.variables.is_bound_in_scope(var) {
            // If the variable is already bound to a name in the current
            // scope we don't have to generate any additional code...
            nil()
        } else {
            // ... if it's not we will be binding the value to a variable
            // name so we can use this variable to reference it and never
            // do any duplicate work recomputing its value every time
            // this pattern variable is used.
            let name = self.variables.next_local_var(&ASSIGNMENT_VAR.into());
            let value = self.variables.get_value(var);
            self.variables.bind(name.clone(), var);
            docvec![let_(name, value.to_doc()), line()]
        };

        for (i, (check, decision)) in choices.iter().enumerate() {
            self.variables.record_check_assignments(var, check);
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
        // successful final check we store those. But we don't need to perform
        // the check itself: the type system makes sure that, if we ever
        // get here, the check is going to match no matter what!
        if let FallbackCheck::RuntimeCheck { check } = fallback_check {
            self.variables.record_check_assignments(var, check);
        }

        let body = self.inside_new_scope(|this| this.decision(fallback))?;
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
        let output = run(self);
        self.variables.expression_generator.current_scope_vars = old_scope;
        self.variables.scoped_variable_names = old_names;
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
        // assured they are in scope for the if condition to use those.
        let guard_variables = guard.referenced_variables();
        let (check_bindings, if_true_bindings): (Vec<_>, Vec<_>) = if_true
            .bindings
            .iter()
            .partition(|(variable, _)| guard_variables.contains(variable));

        let check_bindings = self.bindings_ref(&check_bindings);
        let check = self.variables.expression_generator.guard(guard)?;
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

    fn runtime_check(
        &mut self,
        variable: &Variable,
        runtime_check: &'a RuntimeCheck,
    ) -> Document<'a> {
        let value = self.variables.get_value(variable);
        match runtime_check {
            RuntimeCheck::String { value: literal } => docvec![value, " === ", string(&literal)],
            RuntimeCheck::Float { value: literal } => docvec![value, " === ", float(literal)],
            RuntimeCheck::Int { value: literal } => docvec![value, " === ", int(literal)],
            RuntimeCheck::StringPrefix { prefix, .. } => {
                docvec![value, ".startsWith(", string(&prefix), ")"]
            }

            RuntimeCheck::BitArray { value } => todo!(),

            // When checking on a tuple there's always going to be a single choice
            // and the code generation will always skip generating the check for it
            // as the type system ensures it must match.
            RuntimeCheck::Tuple { .. } => unreachable!("tried generating runtime check for tuple"),

            RuntimeCheck::Variant { match_, .. } if variable.type_.is_bool() => {
                if match_.name() == "True" {
                    value.to_doc()
                } else {
                    docvec!["!", value]
                }
            }

            RuntimeCheck::Variant { match_, .. } if variable.type_.is_result() => {
                if match_.name() == "Ok" {
                    docvec![value, ".isOk()"]
                } else {
                    docvec!["!", value, ".isOk()"]
                }
            }

            RuntimeCheck::Variant { match_, .. } => {
                let qualification = match_
                    .module()
                    .map(|module| eco_format!("${module}."))
                    .unwrap_or_default();

                docvec![value, " instanceof ", qualification, match_.name()]
            }
            RuntimeCheck::NonEmptyList { .. } => {
                self.variables
                    .expression_generator
                    .tracker
                    .list_non_empty_class_used = true;
                docvec![value, " instanceof $NonEmpty"]
            }
            RuntimeCheck::EmptyList => {
                self.variables
                    .expression_generator
                    .tracker
                    .list_empty_class_used = true;
                docvec![value, " instanceof $Empty"]
            }
        }
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
}

impl<'generator, 'module, 'a> Variables<'generator, 'module, 'a> {
    fn new(expression_generator: &'generator mut Generator<'module, 'a>) -> Self {
        Variables {
            expression_generator,
            variable_values: HashMap::new(),
            scoped_variable_names: HashMap::new(),
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
            self.set_value(variable, assignment.name());
            self.bind(assignment.name(), variable);
        }
        Ok(assignments)
    }

    fn next_local_var(&mut self, name: &EcoString) -> EcoString {
        self.expression_generator.next_local_var(name)
    }

    fn set_value(&mut self, variable: &Variable, value: EcoString) {
        let _ = self.variable_values.insert(variable.id, value);
    }

    fn bind(&mut self, name: EcoString, variable: &Variable) {
        let _ = self.scoped_variable_names.insert(variable.id, name);
    }

    #[must_use]
    fn is_bound_in_scope(&self, variable: &Variable) -> bool {
        self.scoped_variable_names.contains_key(&variable.id)
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
            | RuntimeCheck::BitArray { .. }
            | RuntimeCheck::EmptyList => (),

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
                        Some(label) => eco_format!("{value}.{}", maybe_escape_property(&label)),
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
}

enum SubjectAssignment<'a> {
    BindToVariable {
        name: EcoString,
        value: Document<'a>,
    },
    AlreadyAVariable {
        name: EcoString,
    },
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
    // Each of the subjects might end up being stored in a variable before the
    // code of the decision tree can run. This is needed because we can't just
    // repeat a subject code multiple times if the decision tree needs it.
    // Imagine this example:
    // ```gleam
    // case wibble("a") {
    //   1 -> todo
    //   2 -> todo
    //   _ -> todo
    // }
    // ```
    // If the decision tree ended up looking something like this:
    // ```js
    // if (wibble("a") === 1) {}
    // else if (wibble("a") === 2) {}
    // else {}
    // ```
    // It would be quite bad as we would end up running the same function
    // multiple times instead of just once!
    //
    // So if `setup_subjects` decided to bind a subject into a variable so that
    // can be reused to safely refer to a subject's value, we'll have to generate
    // the code for that binding:
    //
    // ```js
    // // Now this is safe!
    // let $ = wibble("a")
    // if ($ === 1) {}
    // else if ($ === 2) {}
    // else {}
    // ```
    let mut assignments_docs = vec![];
    for assignment in assignments.into_iter() {
        let SubjectAssignment::BindToVariable { name, value } = assignment else {
            continue;
        };
        assignments_docs.push(docvec![let_(name, value), line()])
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

fn let_(variable_name: EcoString, value: Document<'_>) -> Document<'_> {
    docvec!["let ", variable_name, " = ", value, ";"]
}

/// Calculates the length of str as utf16 without escape characters.
///
fn utf16_no_escape_len(str: &EcoString) -> usize {
    convert_string_escape_chars(str).encode_utf16().count()
}
