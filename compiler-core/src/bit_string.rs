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
        use BitStringSegmentOption::*;

        match self.typ {
            Some(Int { .. }) => crate::type_::int(),
            Some(Float { .. }) => crate::type_::float(),
            Some(Binary { .. }) => crate::type_::bit_string(),
            Some(BitString { .. }) => crate::type_::bit_string(),
            Some(Utf8 { .. }) => crate::type_::string(),
            Some(Utf16 { .. }) => crate::type_::string(),
            Some(Utf32 { .. }) => crate::type_::string(),
            Some(Utf8Codepoint { .. }) => crate::type_::utf_codepoint(),
            Some(Utf16Codepoint { .. }) => crate::type_::utf_codepoint(),
            Some(Utf32Codepoint { .. }) => crate::type_::utf_codepoint(),
            None => crate::type_::int(),
            _ => panic!("Tried to type a non type kind BitString segment option.",),
        }
    }
}

fn type_options<T>(
    input_options: &[BitStringSegmentOption<T>],
    value_mode: bool,
    must_have_size: bool,
) -> Result<Arc<Type>, Error> {
    use BitStringSegmentOption::*;

    let mut categories = SegmentOptionCategories::new();
    // Basic category checking
    for option in input_options {
        match option {
            Binary { .. }
            | Int { .. }
            | Float { .. }
            | BitString { .. }
            | Utf8 { .. }
            | Utf16 { .. }
            | Utf32 { .. }
            | Utf8Codepoint { .. }
            | Utf16Codepoint { .. }
            | Utf32Codepoint { .. } => {
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

            Signed { .. } | Unsigned { .. } => {
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

            Big { .. } | Little { .. } | Native { .. } => {
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

            Size { .. } => {
                if categories.size.is_some() {
                    return err(ErrorType::ConflictingSizeOptions, option.location());
                } else {
                    categories.size = Some(option);
                }
            }

            Unit { .. } => {
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
        if let SegmentOptionCategories {
            signed: Some(opt), ..
        }
        | SegmentOptionCategories {
            typ: Some(opt @ Binary { .. }),
            ..
        } = categories
        {
            return err(ErrorType::OptionNotAllowedInValue, opt.location());
        }
    }

    // All but the last segment in a pattern must have an exact size
    if must_have_size {
        if let SegmentOptionCategories {
            typ: Some(opt @ (Binary { .. } | BitString { .. })),
            size: None,
            ..
        } = categories
        {
            return err(ErrorType::SegmentMustHaveSize, opt.location());
        }
    }

    // Endianness is only valid for int, utf6, utf32 and float
    match categories {
        SegmentOptionCategories {
            typ: None | Some(Int { .. } | Utf16 { .. } | Utf32 { .. } | Float { .. }),
            ..
        } => {}

        SegmentOptionCategories {
            endian: Some(endian),
            ..
        } => return err(ErrorType::InvalidEndianness, endian.location()),

        _ => {}
    }

    // signed and unsigned can only be used with int types
    match categories {
        SegmentOptionCategories {
            typ: None | Some(Int { .. }),
            ..
        } => {}

        SegmentOptionCategories {
            typ: Some(opt),
            signed: Some(sign),
            ..
        } => {
            return err(
                ErrorType::SignednessUsedOnNonInt { typ: opt.label() },
                sign.location(),
            );
        }

        _ => {}
    }

    // utf8, utf16, utf32 exclude unit and size
    match categories {
        SegmentOptionCategories {
            typ: Some(typ),
            unit: Some(_),
            ..
        } if is_unicode(typ) => {
            return err(
                ErrorType::TypeDoesNotAllowUnit { typ: typ.label() },
                typ.location(),
            );
        }

        SegmentOptionCategories {
            typ: Some(typ),
            size: Some(_),
            ..
        } if is_unicode(typ) => {
            return err(
                ErrorType::TypeDoesNotAllowSize { typ: typ.label() },
                typ.location(),
            );
        }

        _ => {}
    }

    // if unit specified, size must be specified
    if let SegmentOptionCategories {
        unit: Some(unit),
        size: None,
        ..
    } = categories
    {
        return err(ErrorType::UnitMustHaveSize, unit.location());
    }

    // size cannot be used with float
    match categories {
        SegmentOptionCategories {
            typ: Some(Float { .. }),
            size: Some(opt),
            ..
        } => err(ErrorType::FloatWithSize, opt.location()),
        _ => Ok(categories.segment_type()),
    }
}

fn is_unicode<T>(opt: &BitStringSegmentOption<T>) -> bool {
    use BitStringSegmentOption::*;

    matches!(
        opt,
        Utf8 { .. }
            | Utf16 { .. }
            | Utf32 { .. }
            | Utf8Codepoint { .. }
            | Utf16Codepoint { .. }
            | Utf32Codepoint { .. }
    )
}

fn err<A>(error: ErrorType, location: SrcSpan) -> Result<A, Error> {
    Err(Error { location, error })
}

#[derive(Debug)]
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
