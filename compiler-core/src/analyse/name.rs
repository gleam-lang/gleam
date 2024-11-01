use ecow::{eco_format, EcoString};

use crate::{
    ast::{ArgNames, SrcSpan},
    type_::Problems,
};

use super::{Error, Named};
use heck::{ToSnakeCase, ToUpperCamelCase};

fn valid_name(name: &EcoString) -> bool {
    // ^_?[a-z][a-z0-9_]*$
    let mut chars = name.chars();
    if chars.clone().next() == Some('_') {
        let _ = chars.next();
    }
    if chars.next().map(|ch| ch.is_ascii_lowercase()) != Some(true) {
        return false;
    }
    chars.all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit() || ch == '_')
}

fn valid_discard_name(name: &EcoString) -> bool {
    // ^_[a-z0-9_]*$
    let mut chars = name.chars();
    if chars.next() != Some('_') {
        return false;
    }
    chars.all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit() || ch == '_')
}

fn valid_upname(name: &EcoString) -> bool {
    // ^[A-Z][A-Za-z0-9]*$
    let mut chars = name.chars();
    if chars.next().map(|ch| ch.is_ascii_uppercase()) != Some(true) {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric())
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
