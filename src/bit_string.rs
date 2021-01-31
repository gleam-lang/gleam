use crate::ast::{BitStringSegmentOption, SrcSpan};
use crate::typ::Type;
use std::sync::Arc;

//
//  Public Interface
//

pub fn type_options_for_value<T>(
    input_options: &[BitStringSegmentOption<T>],
) -> Result<Arc<Type>, Error> {
    type_options(input_options, true, false)
}

pub fn type_options_for_pattern<T>(
    input_options: &[BitStringSegmentOption<T>],
    must_have_size: bool,
) -> Result<Arc<Type>, Error> {
    type_options(input_options, false, must_have_size)
}

struct SegmentOptionCategories<'a, T> {
    typ: Option<&'a BitStringSegmentOption<T>>,
    signed: Option<&'a BitStringSegmentOption<T>>,
    endian: Option<&'a BitStringSegmentOption<T>>,
    unit: Option<&'a BitStringSegmentOption<T>>,
    size: Option<&'a BitStringSegmentOption<T>>,
}

impl<T> SegmentOptionCategories<'_, T> {
    fn new() -> Self {
        SegmentOptionCategories {
            typ: None,
            signed: None,
            endian: None,
            unit: None,
            size: None,
        }
    }

    fn segment_type(&self) -> Arc<Type> {
        match self.typ {
            Some(BitStringSegmentOption::Integer { .. }) => crate::typ::int(),
            Some(BitStringSegmentOption::Float { .. }) => crate::typ::float(),
            Some(BitStringSegmentOption::Binary { .. }) => crate::typ::bit_string(),
            Some(BitStringSegmentOption::BitString { .. }) => crate::typ::bit_string(),
            Some(BitStringSegmentOption::UTF8 { .. }) => crate::typ::string(),
            Some(BitStringSegmentOption::UTF16 { .. }) => crate::typ::string(),
            Some(BitStringSegmentOption::UTF32 { .. }) => crate::typ::string(),
            Some(BitStringSegmentOption::UTF8Codepoint { .. }) => crate::typ::utf_codepoint(),
            Some(BitStringSegmentOption::UTF16Codepoint { .. }) => crate::typ::utf_codepoint(),
            Some(BitStringSegmentOption::UTF32Codepoint { .. }) => crate::typ::utf_codepoint(),
            None => crate::typ::int(),
            Some(_) => crate::error::fatal_compiler_bug(
                "Tried to type a non type kind BitString segment option.",
            ),
        }
    }
}

fn type_options<T>(
    input_options: &[BitStringSegmentOption<T>],
    value_mode: bool,
    must_have_size: bool,
) -> Result<Arc<Type>, Error> {
    let mut categories = SegmentOptionCategories::new();
    // Basic category checking
    for option in input_options.iter() {
        match option {
            BitStringSegmentOption::Binary { .. }
            | BitStringSegmentOption::Integer { .. }
            | BitStringSegmentOption::Float { .. }
            | BitStringSegmentOption::BitString { .. }
            | BitStringSegmentOption::UTF8 { .. }
            | BitStringSegmentOption::UTF16 { .. }
            | BitStringSegmentOption::UTF32 { .. }
            | BitStringSegmentOption::UTF8Codepoint { .. }
            | BitStringSegmentOption::UTF16Codepoint { .. }
            | BitStringSegmentOption::UTF32Codepoint { .. } => {
                if let Some(previous) = categories.typ {
                    return err(
                        ErrorType::ConflictingTypeOptions {
                            existing_type: previous.label(),
                        },
                        option.location(),
                    );
                } else {
                    categories.typ = Some(option.clone());
                }
            }
            BitStringSegmentOption::Signed { .. } | BitStringSegmentOption::Unsigned { .. } => {
                if let Some(previous) = categories.signed {
                    return err(
                        ErrorType::ConflictingSignednessOptions {
                            existing_signed: previous.label(),
                        },
                        option.location(),
                    );
                } else {
                    categories.signed = Some(option.clone());
                }
            }
            BitStringSegmentOption::Big { .. }
            | BitStringSegmentOption::Little { .. }
            | BitStringSegmentOption::Native { .. } => {
                if let Some(previous) = categories.endian {
                    return err(
                        ErrorType::ConflictingEndiannessOptions {
                            existing_endianness: previous.label(),
                        },
                        option.location(),
                    );
                } else {
                    categories.endian = Some(option.clone());
                }
            }

            BitStringSegmentOption::Size { .. } => {
                if let Some(_) = categories.size {
                    return err(ErrorType::ConflictingSizeOptions, option.location());
                } else {
                    categories.size = Some(option.clone());
                }
            }

            BitStringSegmentOption::Unit { .. } => {
                if let Some(_) = categories.unit {
                    return err(ErrorType::ConflictingUnitOptions, option.location());
                } else {
                    categories.unit = Some(option.clone());
                }
            }
        };
    }

    // Some options are not allowed in value mode
    if value_mode == true {
        match categories {
            SegmentOptionCategories {
                signed: Some(opt), ..
            }
            | SegmentOptionCategories {
                typ: Some(opt @ BitStringSegmentOption::Binary { .. }),
                ..
            } => return err(ErrorType::OptionNotAllowedInValue, opt.location()),

            _ => {}
        }
    }

    // All but the last segment in a pattern must have an exact size
    if must_have_size {
        match categories.typ {
            Some(opt @ BitStringSegmentOption::Binary { .. })
            | Some(opt @ BitStringSegmentOption::BitString { .. }) => {
                if categories.size.is_none() {
                    return err(ErrorType::SegmentMustHaveSize, opt.location());
                }
            }
            _ => {}
        }
    }

    // Endianness is only valid for int, utf6, utf32 and float
    if let Some(endian) = categories.endian {
        match categories.typ {
            None
            | Some(BitStringSegmentOption::Integer { .. })
            | Some(BitStringSegmentOption::UTF16 { .. })
            | Some(BitStringSegmentOption::UTF32 { .. })
            | Some(BitStringSegmentOption::Float { .. }) => {}

            _ => return err(ErrorType::InvalidEndianness, endian.location()),
        }
    };

    // signed and unsigned can only be used with int types
    match categories.typ {
        None | Some(BitStringSegmentOption::Integer { .. }) => {}

        Some(opt) => {
            if let Some(sign) = categories.signed {
                return err(
                    ErrorType::SignednessUsedOnNonInt { typ: opt.label() },
                    sign.location(),
                );
            }
        }
    };

    // utf8, utf16, utf32 exclude unit and size
    match categories {
        SegmentOptionCategories {
            typ: Some(typ),
            unit: Some(_),
            ..
        } => {
            if is_unicode(typ) {
                return err(
                    ErrorType::TypeDoesNotAllowUnit { typ: typ.label() },
                    typ.location().clone(),
                );
            }
        }
        SegmentOptionCategories {
            typ: Some(typ),
            size: Some(_),
            ..
        } => {
            if is_unicode(typ) {
                return err(
                    ErrorType::TypeDoesNotAllowSize { typ: typ.label() },
                    typ.location().clone(),
                );
            }
        }
        _ => {}
    }

    // if unit specified, size must be specified
    if let Some(unit) = categories.unit {
        if categories.size.is_none() {
            return err(ErrorType::UnitMustHaveSize, unit.location().clone());
        };
    };

    // size cannot be used with float
    match categories {
        SegmentOptionCategories {
            typ: Some(BitStringSegmentOption::Float { .. }),
            size: Some(opt),
            ..
        } => return err(ErrorType::FloatWithSize, opt.location().clone()),
        _ => {}
    }

    Ok(categories.segment_type())
}

fn is_unicode<T>(opt: &BitStringSegmentOption<T>) -> bool {
    matches!(opt,
        BitStringSegmentOption::UTF8 { .. }
        | BitStringSegmentOption::UTF16 { .. }
        | BitStringSegmentOption::UTF32 { .. }
        | BitStringSegmentOption::UTF8Codepoint { .. }
        | BitStringSegmentOption::UTF16Codepoint { .. }
        | BitStringSegmentOption::UTF32Codepoint { .. }
    )
}

fn err<A>(error: ErrorType, location: SrcSpan) -> Result<A, Error> {
    Err(Error { location, error })
}

pub struct Error {
    pub location: SrcSpan,
    pub error: ErrorType,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ErrorType {
    ConflictingEndiannessOptions { existing_endianness: String },
    ConflictingSignednessOptions { existing_signed: String },
    ConflictingSizeOptions,
    ConflictingTypeOptions { existing_type: String },
    ConflictingUnitOptions,
    FloatWithSize,
    InvalidEndianness,
    OptionNotAllowedInValue,
    SegmentMustHaveSize,
    SignednessUsedOnNonInt { typ: String },
    TypeDoesNotAllowSize { typ: String },
    TypeDoesNotAllowUnit { typ: String },
    UnitMustHaveSize,
    VaribleUTFSegmentInPatten,
}
