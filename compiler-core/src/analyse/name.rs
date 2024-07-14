use std::sync::OnceLock;

use ecow::{eco_format, EcoString};
use regex::Regex;

use crate::ast::{ArgNames, SrcSpan};

use super::{Error, Named};
use heck::{ToSnakeCase, ToUpperCamelCase};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameCorrection {
    pub location: SrcSpan,
    pub correction: EcoString,
}

static VALID_NAME_PATTERN: OnceLock<Regex> = OnceLock::new();

fn valid_name(name: &EcoString) -> bool {
    // Some of the internally generated variables (such as `_capture` and `_use0`)
    // start with underscores, so we allow underscores here.
    let valid_name_pattern = VALID_NAME_PATTERN
        .get_or_init(|| Regex::new("^_?[a-z][a-z0-9_]*$").expect("Regex is correct"));
    valid_name_pattern.is_match(name)
}

static VALID_DISCARD_PATTERN: OnceLock<Regex> = OnceLock::new();

fn valid_discard_name(name: &EcoString) -> bool {
    let valid_discard_pattern = VALID_DISCARD_PATTERN
        .get_or_init(|| Regex::new("^_[a-z0-9_]*$").expect("Regex is correct"));
    valid_discard_pattern.is_match(name)
}

static VALID_UPNAME_PATTERN: OnceLock<Regex> = OnceLock::new();

fn valid_upname(name: &EcoString) -> bool {
    let valid_upname_pattern = VALID_UPNAME_PATTERN
        .get_or_init(|| Regex::new("^[A-Z][A-Za-z0-9]*$").expect("Regex is correct"));
    valid_upname_pattern.is_match(name)
}

pub fn check_name_case(location: SrcSpan, name: &EcoString, kind: Named) -> Result<(), Error> {
    let valid = match kind {
        Named::Type | Named::TypeVariable | Named::CustomTypeVariant => valid_upname(name),
        Named::Variable | Named::Argument | Named::Label | Named::Constant | Named::Function => {
            valid_name(name)
        }
        Named::Discard => valid_discard_name(name),
    };

    if valid {
        return Ok(());
    }

    Err(Error::BadName {
        location,
        kind,
        name: name.clone(),
    })
}

pub fn correct_name_case(location: SrcSpan, name: &EcoString, kind: Named) -> NameCorrection {
    let correction = match kind {
        Named::Type | Named::TypeVariable | Named::CustomTypeVariant => {
            name.to_upper_camel_case().into()
        }
        Named::Variable | Named::Argument | Named::Label | Named::Constant | Named::Function => {
            name.to_snake_case().into()
        }
        Named::Discard => eco_format!("_{}", name.to_snake_case()),
    };
    NameCorrection {
        location,
        correction,
    }
}

pub fn check_argument_names(
    names: &ArgNames,
    errors: &mut Vec<Error>,
    name_corrections: &mut Vec<NameCorrection>,
) {
    match names {
        ArgNames::Discard { name, location } => {
            if let Err(error) = check_name_case(*location, name, Named::Discard) {
                errors.push(error);
                name_corrections.push(correct_name_case(*location, name, Named::Discard));
            }
        }
        ArgNames::LabelledDiscard {
            label,
            label_location,
            name,
            name_location,
        } => {
            if let Err(error) = check_name_case(*label_location, label, Named::Label) {
                errors.push(error);
                name_corrections.push(correct_name_case(*label_location, label, Named::Label));
            }
            if let Err(error) = check_name_case(*name_location, name, Named::Discard) {
                errors.push(error);
                name_corrections.push(correct_name_case(*name_location, name, Named::Discard));
            }
        }
        ArgNames::Named { name, location } => {
            if let Err(error) = check_name_case(*location, name, Named::Argument) {
                errors.push(error);
                name_corrections.push(correct_name_case(*location, name, Named::Argument));
            }
        }
        ArgNames::NamedLabelled {
            name,
            name_location,
            label,
            label_location,
        } => {
            if let Err(error) = check_name_case(*label_location, label, Named::Label) {
                errors.push(error);
                name_corrections.push(correct_name_case(*label_location, label, Named::Label));
            }
            if let Err(error) = check_name_case(*name_location, name, Named::Argument) {
                errors.push(error);
                name_corrections.push(correct_name_case(*name_location, name, Named::Argument));
            }
        }
    }
}
