use super::*;

pub fn env_types_with(things: &[&str]) -> Vec<String> {
    let mut types: Vec<_> = env_types();
    for thing in things {
        types.push(thing.to_string());
    }
    types
}

pub fn env_types() -> Vec<String> {
    Environment::new(&mut 0, &[], &HashMap::new(), &mut vec![])
        .module_types
        .keys()
        .map(|s| s.to_string())
        .collect()
}

pub fn env_vars_with(things: &[&str]) -> Vec<String> {
    let mut types: Vec<_> = env_vars();
    for thing in things {
        types.push(thing.to_string());
    }
    types
}

pub fn env_vars() -> Vec<String> {
    Environment::new(&mut 0, &[], &HashMap::new(), &mut vec![])
        .local_values
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
