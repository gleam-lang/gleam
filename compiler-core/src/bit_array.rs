use ecow::EcoString;
use num_bigint::BigInt;

use crate::ast::{self, BitArrayOption, SrcSpan};
use crate::build::Target;
use crate::type_::Type;
use std::sync::Arc;

//
//  Public Interface
//

pub fn type_options_for_value<TypedValue>(
    input_options: &[BitArrayOption<TypedValue>],
    target: Target,
) -> Result<Arc<Type>, Error>
where
    TypedValue: GetLiteralValue,
{
    type_options(input_options, TypeOptionsMode::Expression, false, target)
}

pub fn type_options_for_pattern<TypedValue>(
    input_options: &[BitArrayOption<TypedValue>],
    must_have_size: bool,
    target: Target,
) -> Result<Arc<Type>, Error>
where
    TypedValue: GetLiteralValue,
{
    type_options(
        input_options,
        TypeOptionsMode::Pattern,
        must_have_size,
        target,
    )
}

struct SegmentOptionCategories<'a, T> {
    type_: Option<&'a BitArrayOption<T>>,
    signed: Option<&'a BitArrayOption<T>>,
    endian: Option<&'a BitArrayOption<T>>,
    unit: Option<&'a BitArrayOption<T>>,
    size: Option<&'a BitArrayOption<T>>,
}

impl<T> SegmentOptionCategories<'_, T> {
    fn new() -> Self {
        SegmentOptionCategories {
            type_: None,
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

        match self.type_.unwrap_or(&default) {
            Int { .. } => crate::type_::int(),
            Float { .. } => crate::type_::float(),
            Utf8 { .. } | Utf16 { .. } | Utf32 { .. } => crate::type_::string(),
            Bytes { .. } | Bits { .. } => crate::type_::bit_array(),
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

#[derive(Debug, PartialEq, Eq)]
/// Whether we're typing options for a bit array segment that's part of a pattern
/// or an expression.
///
enum TypeOptionsMode {
    Expression,
    Pattern,
}

fn type_options<TypedValue>(
    input_options: &[BitArrayOption<TypedValue>],
    mode: TypeOptionsMode,
    must_have_size: bool,
    target: Target,
) -> Result<Arc<Type>, Error>
where
    TypedValue: GetLiteralValue,
{
    use BitArrayOption::*;

    let mut categories = SegmentOptionCategories::new();
    // Basic category checking
    for option in input_options {
        match option {
            Utf8Codepoint { .. } | Utf16Codepoint { .. } | Utf32Codepoint { .. }
                if mode == TypeOptionsMode::Pattern && target == Target::JavaScript =>
            {
                return err(
                    ErrorType::OptionNotSupportedForTarget {
                        target,
                        option: UnsupportedOption::UtfCodepointPattern,
                    },
                    option.location(),
                );
            }

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
                if let Some(previous) = categories.type_ {
                    return err(
                        ErrorType::ConflictingTypeOptions {
                            existing_type: previous.label(),
                        },
                        option.location(),
                    );
                } else {
                    categories.type_ = Some(option);
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

            Native { .. } if target == Target::JavaScript => {
                return err(
                    ErrorType::OptionNotSupportedForTarget {
                        target,
                        option: UnsupportedOption::NativeEndianness,
                    },
                    option.location(),
                );
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
    if mode == TypeOptionsMode::Expression {
        match categories {
            SegmentOptionCategories {
                signed: Some(opt), ..
            }
            | SegmentOptionCategories {
                type_: Some(opt @ Bytes { .. }),
                ..
            } => return err(ErrorType::OptionNotAllowedInValue, opt.location()),
            _ => (),
        }
    }

    // All but the last segment in a pattern must have an exact size
    if must_have_size
        && let SegmentOptionCategories {
            type_: Some(opt @ (Bytes { .. } | Bits { .. })),
            size: None,
            ..
        } = categories
    {
        return err(ErrorType::SegmentMustHaveSize, opt.location());
    }

    // Endianness is only valid for int, utf16, utf16_codepoint, utf32,
    // utf32_codepoint and float
    match categories {
        SegmentOptionCategories {
            type_:
                None
                | Some(
                    Int { .. }
                    | Utf16 { .. }
                    | Utf32 { .. }
                    | Utf16Codepoint { .. }
                    | Utf32Codepoint { .. }
                    | Float { .. },
                ),
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
            type_: None | Some(Int { .. }),
            ..
        } => {}

        SegmentOptionCategories {
            type_: Some(opt),
            signed: Some(sign),
            ..
        } => {
            return err(
                ErrorType::SignednessUsedOnNonInt { type_: opt.label() },
                sign.location(),
            );
        }

        _ => {}
    }

    // utf8, utf16, utf32 exclude unit and size
    match categories {
        SegmentOptionCategories {
            type_: Some(type_),
            unit: Some(unit),
            ..
        } if is_unicode(type_) => {
            return err(
                ErrorType::TypeDoesNotAllowUnit {
                    type_: type_.label(),
                },
                unit.location(),
            );
        }

        SegmentOptionCategories {
            type_: Some(type_),
            size: Some(size),
            ..
        } if is_unicode(type_) => {
            return err(
                ErrorType::TypeDoesNotAllowSize {
                    type_: type_.label(),
                },
                size.location(),
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
        type_: Some(Float { .. }),
        size: Some(size),
        ..
    } = categories
        && let Some(abox) = size.value()
    {
        match abox.as_int_literal() {
            None => (),
            Some(value) if value == 16.into() || value == 32.into() || value == 64.into() => (),
            _ => return err(ErrorType::FloatWithSize, size.location()),
        }
    }

    // Segment patterns with a zero or negative constant size must be rejected,
    // we know they will never match!
    // A negative size is still allowed in expressions as it will just result
    // in an empty segment.
    if let (Some(size @ Size { value, .. }), TypeOptionsMode::Pattern) = (categories.size, mode) {
        match value.as_int_literal() {
            Some(n) if n <= BigInt::ZERO => {
                return err(ErrorType::ConstantSizeNotPositive, size.location());
            }
            Some(_) | None => (),
        }
    }

    Ok(categories.segment_type())
}

pub trait GetLiteralValue {
    fn as_int_literal(&self) -> Option<BigInt>;
}

impl GetLiteralValue for ast::TypedPattern {
    fn as_int_literal(&self) -> Option<BigInt> {
        match self {
            ast::Pattern::Int { int_value, .. }
            | ast::Pattern::BitArraySize(ast::BitArraySize::Int { int_value, .. }) => {
                Some(int_value.clone())
            }
            ast::Pattern::Float { .. }
            | ast::Pattern::String { .. }
            | ast::Pattern::Variable { .. }
            | ast::Pattern::BitArraySize(_)
            | ast::Pattern::Assign { .. }
            | ast::Pattern::Discard { .. }
            | ast::Pattern::List { .. }
            | ast::Pattern::Constructor { .. }
            | ast::Pattern::Tuple { .. }
            | ast::Pattern::BitArray { .. }
            | ast::Pattern::StringPrefix { .. }
            | ast::Pattern::Invalid { .. } => None,
        }
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
    ConflictingEndiannessOptions {
        existing_endianness: EcoString,
    },
    ConflictingSignednessOptions {
        existing_signed: EcoString,
    },
    ConflictingSizeOptions,
    ConflictingTypeOptions {
        existing_type: EcoString,
    },
    ConflictingUnitOptions,
    FloatWithSize,
    InvalidEndianness,
    OptionNotAllowedInValue,
    SegmentMustHaveSize,
    SignednessUsedOnNonInt {
        type_: EcoString,
    },
    TypeDoesNotAllowSize {
        type_: EcoString,
    },
    TypeDoesNotAllowUnit {
        type_: EcoString,
    },
    UnitMustHaveSize,
    VariableUtfSegmentInPattern,
    ConstantSizeNotPositive,
    OptionNotSupportedForTarget {
        target: Target,
        option: UnsupportedOption,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnsupportedOption {
    UtfCodepointPattern,
    NativeEndianness,
}
