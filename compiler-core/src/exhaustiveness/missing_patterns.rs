use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use smol_str::SmolStr;

use crate::type_::Type;

use super::{Constructor, Decision, Match, NamedTypeInfo, Variable};

/// Returns a list of patterns not covered by the match expression.
pub fn missing_patterns(matches: &Match<'_>) -> Vec<SmolStr> {
    let mut names = HashSet::new();
    let mut steps = Vec::new();

    add_missing_patterns(matches, &matches.tree, &mut steps, &mut names);

    let mut missing: Vec<SmolStr> = names.into_iter().collect();

    // Sorting isn't necessary, but it makes it a bit easier to write tests.
    missing.sort();
    missing
}

/// Information about a single constructor/value (aka term) being tested, used
/// to build a list of names of missing patterns.
#[derive(Debug)]
enum Term {
    Variant {
        variable: Variable,
        name: SmolStr,
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
    fn variable(&self) -> &Variable {
        match self {
            Term::Variant { variable, .. } => variable,
            Term::Infinite { variable } => variable,
            Term::EmptyList { variable } => variable,
            Term::List { variable, .. } => variable,
        }
    }

    fn pattern_string(&self, terms: &[Term], mapping: &HashMap<usize, usize>) -> SmolStr {
        match self {
            Term::Variant {
                variable,
                name,
                arguments,
            } => {
                if arguments.is_empty() {
                    return name.clone();
                }
                let args = arguments
                    .iter()
                    .map(|variable| {
                        mapping
                            .get(&variable.id)
                            .map(|&idx| {
                                terms
                                    .get(idx)
                                    .expect("Term must exist")
                                    .pattern_string(terms, mapping)
                            })
                            .unwrap_or_else(|| "_".into())
                    })
                    .join(", ");
                format!("{}({})", name, args).into()
            }

            Term::Infinite { variable } => "_".into(),

            Term::EmptyList { variable } => "[]".into(),

            Term::List { .. } => format!("[{}]", self.list_pattern_string(terms, mapping)).into(),
        }
    }

    fn list_pattern_string(&self, terms: &[Term], mapping: &HashMap<usize, usize>) -> SmolStr {
        match self {
            Term::Infinite { .. } | Term::Variant { .. } => "_".into(),

            Term::EmptyList { .. } => "".into(),

            Term::List {
                variable,
                first,
                rest,
            } => {
                let first = mapping
                    .get(&first.id)
                    .map(|&idx| {
                        terms
                            .get(idx)
                            .expect("Term must exist")
                            .list_pattern_string(terms, mapping)
                    })
                    .unwrap_or_else(|| "_".into());
                let rest = mapping
                    .get(&rest.id)
                    .map(|&idx| {
                        terms
                            .get(idx)
                            .expect("Term must exist")
                            .list_pattern_string(terms, mapping)
                    })
                    .unwrap_or_else(|| "_".into());

                match rest.as_str() {
                    "" => first,
                    "_" => format!("{first}, ..").into(),
                    _ => format!("{first}, {rest}").into(),
                }
            }
        }
    }
}

fn add_missing_patterns(
    matches: &Match<'_>,
    node: &Decision,
    terms: &mut Vec<Term>,
    missing: &mut HashSet<SmolStr>,
) {
    match node {
        Decision::Success(_) => {}

        Decision::Failure => {
            let mut mapping = HashMap::new();

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

            let name = terms
                .first()
                .map(|term| term.pattern_string(terms, &mapping))
                .unwrap_or_else(|| "_".into());

            _ = missing.insert(name);
        }

        Decision::Guard(_, _, fallback) => {
            add_missing_patterns(matches, fallback, terms, missing);
        }

        Decision::Switch(variable, cases, fallback) => {
            for case in cases {
                match &case.constructor {
                    Constructor::Int(_)
                    | Constructor::Float(_)
                    | Constructor::String(_)
                    | Constructor::BitString
                    | Constructor::StringPrefix => {
                        terms.push(Term::Infinite {
                            variable: variable.clone(),
                        });
                    }

                    Constructor::Tuple(_) => {
                        let arguments = case.arguments.clone();
                        terms.push(Term::Variant {
                            variable: variable.clone(),
                            name: "#".into(),
                            arguments,
                        });
                    }

                    Constructor::Variant(type_, index) => {
                        let name = custom_type_info(matches, type_)
                            .expect("Custom type constructor must have custom type kind")
                            .constructors
                            .get(*index)
                            .expect("Custom type constructor exist for type")
                            .0
                            .clone();
                        terms.push(Term::Variant {
                            variable: variable.clone(),
                            name,
                            arguments: case.arguments.clone(),
                        });
                    }
                }

                add_missing_patterns(matches, &case.body, terms, missing);
                _ = terms.pop();
            }

            if let Some(node) = fallback {
                add_missing_patterns(matches, node, terms, missing);
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
            add_missing_patterns(matches, empty, terms, missing);
            _ = terms.pop();

            terms.push(Term::List {
                variable: variable.clone(),
                first: non_empty.first.clone(),
                rest: non_empty.rest.clone(),
            });
            add_missing_patterns(matches, &non_empty.decision, terms, missing);
            _ = terms.pop();
        }
    }
}

fn custom_type_info<'a>(matches: &'a Match<'_>, type_: &Type) -> Option<&'a NamedTypeInfo> {
    let (module, name) = type_.named_type_name()?;
    matches
        .modules
        .get(&module)?
        .types_value_constructors
        .get(&name)
}
