use super::{CompileCaseResult, Decision, RuntimeCheck, Variable, printer::Printer};
use crate::type_::environment::Environment;
use ecow::EcoString;
use std::collections::{HashMap, HashSet};

/// Returns a list of patterns not covered by the match expression.
pub fn missing_patterns(
    result: &CompileCaseResult,
    environment: &Environment<'_>,
) -> Vec<EcoString> {
    let mut names = HashSet::new();
    let mut steps = Vec::new();

    add_missing_patterns(
        &result.compiled_case.tree,
        &result.compiled_case.subject_variables,
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
        fields: Vec<Variable>,
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

fn add_missing_patterns(
    node: &Decision,
    subjects: &Vec<Variable>,
    terms: &mut Vec<Term>,
    missing: &mut HashSet<EcoString>,
    environment: &Environment<'_>,
) {
    match node {
        Decision::Run { .. } => {}

        Decision::Fail => {
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

        Decision::Guard { if_false, .. } => {
            add_missing_patterns(if_false, subjects, terms, missing, environment);
        }

        Decision::Switch {
            var,
            choices,
            fallback,
        } => {
            for (check, body) in choices {
                let term = check_to_term(var.clone(), check, environment);
                terms.push(term);
                add_missing_patterns(body, subjects, terms, missing, environment);
                _ = terms.pop();
            }

            add_missing_patterns(fallback, subjects, terms, missing, environment);
        }

        Decision::ExhaustiveSwitch { var, choices } => {
            for (check, body) in choices {
                let term = check_to_term(var.clone(), check, environment);
                terms.push(term);
                add_missing_patterns(body, subjects, terms, missing, environment);
                _ = terms.pop();
            }
        }
    }
}

fn check_to_term(variable: Variable, check: &RuntimeCheck, env: &Environment<'_>) -> Term {
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
            labels: _,
            match_: _,
        } => {
            let (module, name) = variable
                .type_
                .named_type_name()
                .expect("Should be a named type");

            let name = env
                .get_constructors_for_type(&module, &name)
                .expect("Custom type constructor must have custom type kind")
                .variants
                .get(*index)
                .expect("Custom type constructor exist for type")
                .name
                .clone();

            Term::Variant {
                variable,
                name,
                module,
                fields: fields.clone(),
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
