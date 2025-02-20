use std::sync::OnceLock;

use ecow::{EcoString, eco_format};
use regex::Regex;

use crate::{
    ast::{ArgNames, SrcSpan},
    type_::Problems,
};

use super::{Error, Named};
use heck::{ToSnakeCase, ToUpperCamelCase};

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
        Named::Type | Named::TypeAlias | Named::CustomTypeVariant => valid_upname(name),
        Named::Variable
        | Named::TypeVariable
        | Named::Argument
        | Named::Label
        | Named::Constant
        | Named::Function => valid_name(name),
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

pub fn correct_name_case(name: &EcoString, kind: Named) -> EcoString {
    match kind {
        Named::Type | Named::TypeAlias | Named::CustomTypeVariant => {
            name.to_upper_camel_case().into()
        }
        Named::Variable
        | Named::TypeVariable
        | Named::Argument
        | Named::Label
        | Named::Constant
        | Named::Function => name.to_snake_case().into(),
        Named::Discard => eco_format!("_{}", name.to_snake_case()),
    }
}

pub fn check_argument_names(names: &ArgNames, problems: &mut Problems) {
    match names {
        ArgNames::Discard { name, location } => {
            if let Err(error) = check_name_case(*location, name, Named::Discard) {
                problems.error(error);
            }
        }
        ArgNames::LabelledDiscard {
            label,
            label_location,
            name,
            name_location,
        } => {
            if let Err(error) = check_name_case(*label_location, label, Named::Label) {
                problems.error(error);
            }
            if let Err(error) = check_name_case(*name_location, name, Named::Discard) {
                problems.error(error);
            }
        }
        ArgNames::Named { name, location } => {
            if let Err(error) = check_name_case(*location, name, Named::Argument) {
                problems.error(error);
            }
        }
        ArgNames::NamedLabelled {
            name,
            name_location,
            label,
            label_location,
        } => {
            if let Err(error) = check_name_case(*label_location, label, Named::Label) {
                problems.error(error);
            }
            if let Err(error) = check_name_case(*name_location, name, Named::Argument) {
                problems.error(error);
            }
        }
    }
}
