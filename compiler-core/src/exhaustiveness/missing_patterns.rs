use super::{CompileCaseResult, Decision, FallbackCheck, RuntimeCheck, Variable, printer::Printer};
use crate::type_::environment::Environment;
use ecow::EcoString;
use indexmap::IndexSet;
use std::collections::HashMap;

/// Returns a list of patterns not covered by the match expression.
pub fn missing_patterns(
    result: &CompileCaseResult,
    environment: &Environment<'_>,
) -> Vec<EcoString> {
    let subjects = &result.compiled_case.subject_variables;
    let mut generator = MissingPatternsGenerator::new(subjects, environment);
    generator.add_missing_patterns(&result.compiled_case.tree);

    generator.missing.into_iter().collect()
}

#[derive(Debug, Clone)]
pub struct VariantField {
    pub label: Option<EcoString>,
    pub variable: Variable,
}

/// Information about a single constructor/value (aka term) being tested, used
/// to build a list of names of missing patterns.
#[derive(Debug)]
pub enum Term {
    Variant {
        variable: Variable,
        name: EcoString,
        module: EcoString,
        fields: Vec<VariantField>,
    },
    Tuple {
        variable: Variable,
        elements: Vec<Variable>,
    },
    Infinite {
        variable: Variable,
    },
    EmptyList {
        variable: Variable,
    },
    List {
        variable: Variable,
        first: Variable,
        rest: Variable,
    },
}

impl Term {
    pub fn variable(&self) -> &Variable {
        match self {
            Term::Variant { variable, .. } => variable,
            Term::Tuple { variable, .. } => variable,
            Term::Infinite { variable } => variable,
            Term::EmptyList { variable } => variable,
            Term::List { variable, .. } => variable,
        }
    }
}

struct MissingPatternsGenerator<'a, 'env> {
    subjects: &'a Vec<Variable>,
    terms: Vec<Term>,
    missing: IndexSet<EcoString>,
    environment: &'a Environment<'env>,
    printer: Printer<'a>,
}

impl<'a, 'env> MissingPatternsGenerator<'a, 'env> {
    fn new(subjects: &'a Vec<Variable>, environment: &'a Environment<'env>) -> Self {
        MissingPatternsGenerator {
            subjects,
            terms: vec![],
            missing: IndexSet::new(),
            environment,
            printer: Printer::new(&environment.names),
        }
    }

    fn print_terms(&self, mapping: HashMap<usize, usize>) -> EcoString {
        self.printer
            .print_terms(self.subjects, &self.terms, &mapping)
    }

    fn add_missing_patterns(&mut self, node: &Decision) {
        match node {
            Decision::Run { .. } => {}

            Decision::Guard { if_false, .. } => self.add_missing_patterns(if_false),

            Decision::Fail => {
                // At this point the terms stack looks something like this:
                // `[term, term + arguments, term, ...]`. To construct a pattern
                // name from this stack, we first map all variables to their
                // term indexes. This is needed because when a term defines
                // arguments, the terms for those arguments don't necessarily
                // appear in order in the term stack.
                //
                // This mapping is then used when (recursively) generating a
                // pattern name.
                //
                // This approach could probably be done more efficiently, so if
                // you're reading this and happen to know of a way, please
                // submit a merge request :)
                let mut mapping = HashMap::new();
                for (index, step) in self.terms.iter().enumerate() {
                    _ = mapping.insert(step.variable().id, index);
                }
                let pattern = self.print_terms(mapping);

                _ = self.missing.insert(pattern);
            }

            Decision::Switch {
                var,
                choices,
                fallback,
                fallback_check,
            } => {
                for (check, body) in choices {
                    self.add_missing_patterns_after_check(var, check, body);
                }

                match fallback_check.as_ref() {
                    FallbackCheck::InfiniteCatchAll => {
                        self.add_missing_patterns(fallback);
                    }
                    FallbackCheck::RuntimeCheck { check } => {
                        self.add_missing_patterns_after_check(var, check, fallback)
                    }
                    FallbackCheck::CatchAll { ignored_checks } => {
                        for check in ignored_checks {
                            self.add_missing_patterns_after_check(var, check, fallback);
                        }
                    }
                };
            }
        }
    }

    fn add_missing_patterns_after_check(
        &mut self,
        var: &Variable,
        check: &RuntimeCheck,
        body: &Decision,
    ) {
        let term = self.check_to_term(var.clone(), check);
        self.terms.push(term);
        self.add_missing_patterns(body);
        _ = self.terms.pop();
    }

    fn check_to_term(&self, variable: Variable, check: &RuntimeCheck) -> Term {
        match check {
            RuntimeCheck::Int { .. }
            | RuntimeCheck::Float { .. }
            | RuntimeCheck::String { .. }
            | RuntimeCheck::BitArray { .. }
            | RuntimeCheck::StringPrefix { .. } => Term::Infinite { variable },

            RuntimeCheck::Tuple { elements, .. } => Term::Tuple {
                variable,
                elements: elements.clone(),
            },

            RuntimeCheck::Variant {
                index,
                fields,
                labels,
                ..
            } => {
                let (module, name) = variable
                    .type_
                    .named_type_name()
                    .expect("Should be a named type");

                let name = self
                    .environment
                    .get_constructors_for_type(&module, &name)
                    .expect("Custom type constructor must have custom type kind")
                    .variants
                    .get(*index)
                    .expect("Custom type constructor exist for type")
                    .name
                    .clone();

                let fields = fields
                    .iter()
                    .enumerate()
                    .map(|(index, variable)| VariantField {
                        label: labels.get(&index).cloned(),
                        variable: variable.clone(),
                    })
                    .collect();

                Term::Variant {
                    variable,
                    name,
                    module,
                    fields,
                }
            }

            RuntimeCheck::NonEmptyList { first, rest } => Term::List {
                variable,
                first: first.clone(),
                rest: rest.clone(),
            },

            RuntimeCheck::EmptyList => Term::EmptyList { variable },
        }
    }
}
