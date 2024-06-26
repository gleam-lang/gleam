use ecow::EcoString;

use crate::ast::{ArgNames, SrcSpan};

use super::{BadNameKind, Error};
use heck::{ToSnakeCase, ToUpperCamelCase};

type BadName = (SrcSpan, EcoString);

pub fn check_valid_name(
    location: SrcSpan,
    name: &EcoString,
    kind: BadNameKind,
) -> Option<(Error, BadName)> {
    check_bad_name(location, name, kind)
        .map(|error| (error, (location, EcoString::from(name.to_snake_case()))))
}

pub fn check_valid_discard_name(location: SrcSpan, name: &EcoString) -> Option<(Error, BadName)> {
    check_bad_name(location, name, BadNameKind::Discard).map(|error| {
        (
            error,
            (
                location,
                EcoString::from(format!("_{}", name.to_snake_case())),
            ),
        )
    })
}

pub fn check_valid_upname(
    location: SrcSpan,
    name: &EcoString,
    kind: BadNameKind,
) -> Option<(Error, BadName)> {
    if name.contains('_') {
        return Some((
            Error::BadName {
                location,
                kind,
                name: name.clone(),
            },
            (location, EcoString::from(name.to_upper_camel_case())),
        ));
    }
    None
}

pub fn check_valid_argument(names: &ArgNames) -> (Vec<Error>, Vec<BadName>) {
    let mut errors = Vec::new();
    let mut bad_names = Vec::new();
    match names {
        ArgNames::Discard { name, location } => {
            if let Some((error, bad_name)) = check_valid_discard_name(*location, name) {
                errors.push(error);
                bad_names.push(bad_name);
            }
        }
        ArgNames::LabelledDiscard {
            label,
            label_location,
            name,
            name_location,
        } => {
            if let Some((error, bad_name)) =
                check_valid_name(*label_location, label, BadNameKind::Label)
            {
                errors.push(error);
                bad_names.push(bad_name);
            }
            if let Some((error, bad_name)) = check_valid_discard_name(*name_location, name) {
                errors.push(error);
                bad_names.push(bad_name);
            }
        }
        ArgNames::Named { name, location } => {
            if let Some((error, bad_name)) =
                check_valid_name(*location, name, BadNameKind::Argument)
            {
                errors.push(error);
                bad_names.push(bad_name);
            }
        }
        ArgNames::NamedLabelled {
            name,
            name_location,
            label,
            label_location,
        } => {
            if let Some((error, bad_name)) =
                check_valid_name(*label_location, label, BadNameKind::Label)
            {
                errors.push(error);
                bad_names.push(bad_name);
            }
            if let Some((error, bad_name)) =
                check_valid_name(*name_location, name, BadNameKind::Argument)
            {
                errors.push(error);
                bad_names.push(bad_name);
            }
        }
    }
    (errors, bad_names)
}

fn check_bad_name(location: SrcSpan, name: &EcoString, kind: BadNameKind) -> Option<Error> {
    if name.contains(|c: char| c.is_ascii_uppercase()) {
        return Some(Error::BadName {
            location,
            kind,
            name: name.clone(),
        });
    }
    None
}
