use ecow::EcoString;

use crate::ast::{BitArrayOption, SrcSpan};
use crate::type_::Type;
use std::sync::Arc;

//
//  Public Interface
//

pub fn type_options_for_value<TypedValue>(
    input_options: &[BitArrayOption<TypedValue>],
) -> Result<Arc<Type>, Error>
where
    TypedValue: GetLiteralValue,
{
    type_options(input_options, true, false)
}

pub fn type_options_for_pattern<TypedValue>(
    input_options: &[BitArrayOption<TypedValue>],
    must_have_size: bool,
) -> Result<Arc<Type>, Error>
where
    TypedValue: GetLiteralValue,
{
    type_options(input_options, false, must_have_size)
}

struct SegmentOptionCategories<'a, T> {
    typ: Option<&'a BitArrayOption<T>>,
    signed: Option<&'a BitArrayOption<T>>,
    endian: Option<&'a BitArrayOption<T>>,
    unit: Option<&'a BitArrayOption<T>>,
    size: Option<&'a BitArrayOption<T>>,
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
        use BitArrayOption::*;
        let default = Int {
            location: SrcSpan::default(),
        };

        match self.typ.unwrap_or(&default) {
            Int { .. } => crate::type_::int(),
            Float { .. } => crate::type_::float(),
            Utf8 { .. } | Utf16 { .. } | Utf32 { .. } => crate::type_::string(),
            Bytes { .. } | Bits { .. } => crate::type_::bits(),
            Utf8Codepoint { .. } | Utf16Codepoint { .. } | Utf32Codepoint { .. } => {
                crate::type_::utf_codepoint()
            }

            Signed { .. }
            | Unsigned { .. }
            | Big { .. }
            | Little { .. }
            | Native { .. }
            | Size { .. }
            | Unit { .. } => panic!("Tried to type a non type kind BitArray option."),
        }
    }
}

fn type_options<TypedValue>(
    input_options: &[BitArrayOption<TypedValue>],
    value_mode: bool,
    must_have_size: bool,
) -> Result<Arc<Type>, Error>
where
    TypedValue: GetLiteralValue,
{
    use BitArrayOption::*;

    let mut categories = SegmentOptionCategories::new();
    // Basic category checking
    for option in input_options {
        match option {
            Bytes { .. }
            | Int { .. }
            | Float { .. }
            | Bits { .. }
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
        match categories {
            SegmentOptionCategories {
                signed: Some(opt), ..
            }
            | SegmentOptionCategories {
                typ: Some(opt @ Bytes { .. }),
                ..
            } => return err(ErrorType::OptionNotAllowedInValue, opt.location()),
            _ => (),
        }
    }

    // All but the last segment in a pattern must have an exact size
    if must_have_size {
        if let SegmentOptionCategories {
            typ: Some(opt @ (Bytes { .. } | Bits { .. })),
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

    // float only 16/32/64
    if let SegmentOptionCategories {
        typ: Some(Float { .. }),
        size: Some(size),
        ..
    } = categories
    {
        if let Some(abox) = size.value() {
            match abox.as_int_literal() {
                None => (),
                Some(16) => (),
                Some(32) => (),
                Some(64) => (),
                _ => return err(ErrorType::FloatWithSize, size.location()),
            }
        }
    }

    Ok(categories.segment_type())
}

pub trait GetLiteralValue {
    fn as_int_literal(&self) -> Option<i64>;
}

impl GetLiteralValue for crate::ast::Pattern<Arc<Type>> {
    fn as_int_literal(&self) -> Option<i64> {
        match self {
            crate::ast::Pattern::Int { value, .. } => {
                if let Ok(val) = value.parse::<i64>() {
                    return Some(val);
                }
            }
            crate::ast::Pattern::VarUsage { .. } => return None,
            _ => (),
        }
        None
    }
}

fn is_unicode<T>(opt: &BitArrayOption<T>) -> bool {
    use BitArrayOption::*;

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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorType {
    ConflictingEndiannessOptions { existing_endianness: EcoString },
    ConflictingSignednessOptions { existing_signed: EcoString },
    ConflictingSizeOptions,
    ConflictingTypeOptions { existing_type: EcoString },
    ConflictingUnitOptions,
    FloatWithSize,
    InvalidEndianness,
    OptionNotAllowedInValue,
    SegmentMustHaveSize,
    SignednessUsedOnNonInt { typ: EcoString },
    TypeDoesNotAllowSize { typ: EcoString },
    TypeDoesNotAllowUnit { typ: EcoString },
    UnitMustHaveSize,
    VariableUtfSegmentInPattern,
}
