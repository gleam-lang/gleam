use super::{Constructor, Decision, Match, Variable, printer::Printer};
use crate::type_::environment::Environment;
use ecow::EcoString;
use std::collections::{HashMap, HashSet};

/// Returns a list of patterns not covered by the match expression.
pub fn missing_patterns(matches: &Match, environment: &Environment<'_>) -> Vec<EcoString> {
    let mut names = HashSet::new();
    let mut steps = Vec::new();

    add_missing_patterns(
        &matches.tree,
        &matches.subject_variables,
        &mut steps,
        &mut names,
        environment,
    );

    let mut missing: Vec<EcoString> = names.into_iter().collect();

    // Sorting isn't necessary, but it makes it a bit easier to write tests.
    missing.sort();
    missing
}

/// Information about a single constructor/value (aka term) being tested, used
/// to build a list of names of missing patterns.
#[derive(Debug)]
pub enum Term {
    Variant {
        variable: Variable,
        name: EcoString,
        module: EcoString,
        arguments: Vec<Variable>,
    },
    Tuple {
        variable: Variable,
        arguments: Vec<Variable>,
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

fn add_missing_patterns(
    node: &Decision,
    subjects: &Vec<Variable>,
    terms: &mut Vec<Term>,
    missing: &mut HashSet<EcoString>,
    environment: &Environment<'_>,
) {
    match node {
        Decision::Success(_) => {}

        Decision::Failure => {
            let mut mapping = HashMap::new();
            let printer = Printer::new(&environment.names);

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
            for (index, step) in terms.iter().enumerate() {
                _ = mapping.insert(step.variable().id, index);
            }

            let pattern = printer.print_terms(subjects, terms, &mapping);

            _ = missing.insert(pattern);
        }

        Decision::Guard(_, _, fallback) => {
            add_missing_patterns(fallback, subjects, terms, missing, environment);
        }

        Decision::Switch(variable, cases, fallback) => {
            for case in cases {
                match &case.constructor {
                    Constructor::Int(_)
                    | Constructor::Float(_)
                    | Constructor::String(_)
                    | Constructor::BitArray
                    | Constructor::StringPrefix => {
                        terms.push(Term::Infinite {
                            variable: variable.clone(),
                        });
                    }

                    Constructor::Tuple(_) => {
                        let arguments = case.arguments.clone();
                        terms.push(Term::Tuple {
                            variable: variable.clone(),
                            arguments,
                        });
                    }

                    Constructor::Variant { type_, index } => {
                        let (module, name) =
                            type_.named_type_name().expect("Should be a named type");
                        let name = environment
                            .get_constructors_for_type(&module, &name, type_.fn_arity())
                            .expect("Custom type constructor must have custom type kind")
                            .variants
                            .get(*index as usize)
                            .expect("Custom type constructor exist for type")
                            .name
                            .clone();
                        terms.push(Term::Variant {
                            variable: variable.clone(),
                            name,
                            module,
                            arguments: case.arguments.clone(),
                        });
                    }
                }

                add_missing_patterns(&case.body, subjects, terms, missing, environment);
                _ = terms.pop();
            }

            if let Some(node) = fallback {
                add_missing_patterns(node, subjects, terms, missing, environment);
            }
        }

        Decision::List {
            variable,
            empty,
            non_empty,
        } => {
            terms.push(Term::EmptyList {
                variable: variable.clone(),
            });
            add_missing_patterns(empty, subjects, terms, missing, environment);
            _ = terms.pop();

            terms.push(Term::List {
                variable: variable.clone(),
                first: non_empty.first.clone(),
                rest: non_empty.rest.clone(),
            });
            add_missing_patterns(&non_empty.decision, subjects, terms, missing, environment);
            _ = terms.pop();
        }
    }
}
