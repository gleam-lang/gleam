use crate::ast::{BitStringSegmentOption, SrcSpan};
use crate::type_::Type;
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
            Some(BitStringSegmentOption::Int { .. }) => crate::type_::int(),
            Some(BitStringSegmentOption::Float { .. }) => crate::type_::float(),
            Some(BitStringSegmentOption::Binary { .. }) => crate::type_::bit_string(),
            Some(BitStringSegmentOption::BitString { .. }) => crate::type_::bit_string(),
            Some(BitStringSegmentOption::Utf8 { .. }) => crate::type_::string(),
            Some(BitStringSegmentOption::Utf16 { .. }) => crate::type_::string(),
            Some(BitStringSegmentOption::Utf32 { .. }) => crate::type_::string(),
            Some(BitStringSegmentOption::Utf8Codepoint { .. }) => crate::type_::utf_codepoint(),
            Some(BitStringSegmentOption::Utf16Codepoint { .. }) => crate::type_::utf_codepoint(),
            Some(BitStringSegmentOption::Utf32Codepoint { .. }) => crate::type_::utf_codepoint(),
            None => crate::type_::int(),
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
            | BitStringSegmentOption::Int { .. }
            | BitStringSegmentOption::Float { .. }
            | BitStringSegmentOption::BitString { .. }
            | BitStringSegmentOption::Utf8 { .. }
            | BitStringSegmentOption::Utf16 { .. }
            | BitStringSegmentOption::Utf32 { .. }
            | BitStringSegmentOption::Utf8Codepoint { .. }
            | BitStringSegmentOption::Utf16Codepoint { .. }
            | BitStringSegmentOption::Utf32Codepoint { .. } => {
                if let Some(previous) = categories.typ {
                    return err(
                        ErrorType::ConflictingTypeOptions {
                            existing_type: previous.label(),
                        },
                        option.location(),
                    );
                } else {
                    categories.typ = Some(option);
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
                    categories.signed = Some(option);
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
                    categories.endian = Some(option);
                }
            }

            BitStringSegmentOption::Size { .. } => {
                if categories.size.is_some() {
                    return err(ErrorType::ConflictingSizeOptions, option.location());
                } else {
                    categories.size = Some(option);
                }
            }

            BitStringSegmentOption::Unit { .. } => {
                if categories.unit.is_some() {
                    return err(ErrorType::ConflictingUnitOptions, option.location());
                } else {
                    categories.unit = Some(option);
                }
            }
        };
    }

    // Some options are not allowed in value mode
    if value_mode {
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
            | Some(BitStringSegmentOption::Int { .. })
            | Some(BitStringSegmentOption::Utf16 { .. })
            | Some(BitStringSegmentOption::Utf32 { .. })
            | Some(BitStringSegmentOption::Float { .. }) => {}

            _ => return err(ErrorType::InvalidEndianness, endian.location()),
        }
    };

    // signed and unsigned can only be used with int types
    match categories.typ {
        None | Some(BitStringSegmentOption::Int { .. }) => {}

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
                    typ.location(),
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
                    typ.location(),
                );
            }
        }
        _ => {}
    }

    // if unit specified, size must be specified
    if let Some(unit) = categories.unit {
        if categories.size.is_none() {
            return err(ErrorType::UnitMustHaveSize, unit.location());
        };
    };

    // size cannot be used with float
    match categories {
        SegmentOptionCategories {
            typ: Some(BitStringSegmentOption::Float { .. }),
            size: Some(opt),
            ..
        } => err(ErrorType::FloatWithSize, opt.location()),
        _ => Ok(categories.segment_type()),
    }
}

fn is_unicode<T>(opt: &BitStringSegmentOption<T>) -> bool {
    matches!(
        opt,
        BitStringSegmentOption::Utf8 { .. }
            | BitStringSegmentOption::Utf16 { .. }
            | BitStringSegmentOption::Utf32 { .. }
            | BitStringSegmentOption::Utf8Codepoint { .. }
            | BitStringSegmentOption::Utf16Codepoint { .. }
            | BitStringSegmentOption::Utf32Codepoint { .. }
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
    VariableUtfSegmentInPattern,
}
