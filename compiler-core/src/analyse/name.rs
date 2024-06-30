use ecow::{eco_format, EcoString};
use regex::Regex;

use crate::ast::{ArgNames, SrcSpan};

use super::{BadNameKind, Error};
use heck::{ToSnakeCase, ToUpperCamelCase};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameCorrection {
    pub location: SrcSpan,
    pub correction: EcoString,
}

pub fn check_valid_name(
    location: SrcSpan,
    name: &EcoString,
    kind: BadNameKind,
) -> Option<(Error, NameCorrection)> {
    // Some of the internally generated variables (such as `_capture` and `_use0`)
    // start with underscores, so we allow underscores here.
    let valid_name_regex = Regex::new("^_?[a-z][a-z0-9_]*$").expect("Regex is correct");
    if !valid_name_regex.is_match(name) {
        return Some((
            Error::BadName {
                location,
                kind,
                name: name.clone(),
            },
            NameCorrection {
                location,
                correction: name.to_snake_case().into(),
            },
        ));
    }
    None
}

pub fn check_valid_discard_name(
    location: SrcSpan,
    name: &EcoString,
) -> Option<(Error, NameCorrection)> {
    let valid_discard_regex = Regex::new("^_[a-z0-9_]*$").expect("Regex is correct");
    if !valid_discard_regex.is_match(name) {
        return Some((
            Error::BadName {
                location,
                kind: BadNameKind::Discard,
                name: name.clone(),
            },
            NameCorrection {
                location,
                correction: eco_format!("_{}", name.to_snake_case()),
            },
        ));
    }
    None
}

pub fn check_valid_upname(
    location: SrcSpan,
    name: &EcoString,
    kind: BadNameKind,
) -> Option<(Error, NameCorrection)> {
    let valid_upname_regex = Regex::new("^[A-Z][A-Za-z0-9]*$").expect("Regex is correct");
    if !valid_upname_regex.is_match(name) {
        return Some((
            Error::BadName {
                location,
                kind,
                name: name.clone(),
            },
            NameCorrection {
                location,
                correction: name.to_upper_camel_case().into(),
            },
        ));
    }
    None
}

pub fn check_valid_argument(
    names: &ArgNames,
    errors: &mut Vec<Error>,
    name_corrections: &mut Vec<NameCorrection>,
) {
    match names {
        ArgNames::Discard { name, location } => {
            if let Some((error, correction)) = check_valid_discard_name(*location, name) {
                errors.push(error);
                name_corrections.push(correction);
            }
        }
        ArgNames::LabelledDiscard {
            label,
            label_location,
            name,
            name_location,
        } => {
            if let Some((error, correction)) =
                check_valid_name(*label_location, label, BadNameKind::Label)
            {
                errors.push(error);
                name_corrections.push(correction);
            }
            if let Some((error, correction)) = check_valid_discard_name(*name_location, name) {
                errors.push(error);
                name_corrections.push(correction);
            }
        }
        ArgNames::Named { name, location } => {
            if let Some((error, correction)) =
                check_valid_name(*location, name, BadNameKind::Argument)
            {
                errors.push(error);
                name_corrections.push(correction);
            }
        }
        ArgNames::NamedLabelled {
            name,
            name_location,
            label,
            label_location,
        } => {
            if let Some((error, correction)) =
                check_valid_name(*label_location, label, BadNameKind::Label)
            {
                errors.push(error);
                name_corrections.push(correction);
            }
            if let Some((error, correction)) =
                check_valid_name(*name_location, name, BadNameKind::Argument)
            {
                errors.push(error);
                name_corrections.push(correction);
            }
        }
    }
}
