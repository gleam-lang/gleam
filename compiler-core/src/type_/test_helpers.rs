use super::*;

pub fn env_types_with(things: &[&str]) -> Vec<String> {
    let mut types: Vec<_> = env_types();
    for &thing in things {
        types.push(thing.to_string());
    }
    types
}

pub fn env_types() -> Vec<String> {
    let mut modules = HashMap::new();
    let mut uid = 0;
    // DUPE: preludeinsertion
    // TODO: Currently we do this here and also in the tests. It would be better
    // to have one place where we create all this required state for use in each
    // place.
    let _ = modules.insert("gleam".to_string(), build_prelude(&mut uid));
    Environment::new(&mut 0, &[], &modules, &mut vec![])
        .module_types
        .keys()
        .map(|s| s.to_string())
        .collect()
}

pub fn sort_options(e: Error) -> Error {
    match e {
        Error::UnknownType {
            location,
            name,
            mut types,
        } => {
            types.sort();
            Error::UnknownType {
                location,
                name,
                types,
            }
        }

        Error::UnknownVariable {
            location,
            name,
            mut variables,
        } => {
            variables.sort();
            Error::UnknownVariable {
                location,
                name,
                variables,
            }
        }

        Error::UnknownLabels {
            unknown,
            mut valid,
            supplied,
        } => {
            valid.sort();
            Error::UnknownLabels {
                unknown,
                valid,
                supplied,
            }
        }

        _ => e,
    }
}
