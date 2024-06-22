use ecow::EcoString;

use crate::ast::{ArgNames, SrcSpan};

use super::{BadNameKind, Error};
use heck::{ToSnakeCase, ToUpperCamelCase};

pub trait NameChecker {
    fn push_bad_name(&mut self, error: Error, bad_name: (SrcSpan, EcoString));

    fn check_valid_name(&mut self, location: SrcSpan, name: &EcoString, kind: BadNameKind) {
        if let Err(e) = check_valid_name(location, name, kind) {
            self.push_bad_name(e, (location, EcoString::from(name.to_snake_case())));
        }
    }

    fn check_valid_discard_name(&mut self, location: SrcSpan, name: &EcoString) {
        if let Err(e) = check_valid_name(location, name, BadNameKind::Discard) {
            self.push_bad_name(
                e,
                (
                    location,
                    EcoString::from(format!("_{}", name.to_snake_case())),
                ),
            );
        }
    }

    fn check_valid_upname(&mut self, location: SrcSpan, name: &EcoString, kind: BadNameKind) {
        if name.contains('_') {
            self.push_bad_name(
                Error::BadName {
                    location,
                    kind,
                    name: name.clone(),
                },
                (location, EcoString::from(name.to_upper_camel_case())),
            );
        }
    }

    fn check_valid_argument(&mut self, names: &ArgNames) {
        match names {
            ArgNames::Discard { name, location } => {
                self.check_valid_discard_name(*location, name);
            }
            ArgNames::LabelledDiscard {
                label,
                label_location,
                name,
                name_location,
            } => {
                self.check_valid_name(*label_location, label, BadNameKind::Label);
                self.check_valid_discard_name(*name_location, name);
            }
            ArgNames::Named { name, location } => {
                self.check_valid_name(*location, name, BadNameKind::Argument);
            }
            ArgNames::NamedLabelled {
                name,
                name_location,
                label,
                label_location,
            } => {
                self.check_valid_name(*label_location, label, BadNameKind::Label);
                self.check_valid_name(*name_location, name, BadNameKind::Argument);
            }
        }
    }
}

fn check_valid_name(location: SrcSpan, name: &EcoString, kind: BadNameKind) -> Result<(), Error> {
    if name.contains(|c: char| c.is_ascii_uppercase()) {
        return Err(Error::BadName {
            location,
            kind,
            name: name.clone(),
        });
    }
    Ok(())
}
