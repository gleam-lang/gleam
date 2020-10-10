use crate::ast::{BitStringSegmentOption, SegmentOptionCategory as Category, SrcSpan};
use crate::typ::Type;
use std::sync::Arc;

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryTypeSpecifier<T> {
    pub typ: Option<BitStringSegmentOption<T>>,
    signedness: Option<BitStringSegmentOption<T>>,
    endianness: Option<BitStringSegmentOption<T>>,
    unit: Option<BitStringSegmentOption<T>>,
    size: Option<BitStringSegmentOption<T>>,
}

impl<T> BinaryTypeSpecifier<T> {
    pub fn new(options: &[BitStringSegmentOption<T>], must_have_size: bool) -> Result<Self, Error>
    where
        T: Clone,
    {
        let empty_bts = BinaryTypeSpecifier {
            typ: None,
            signedness: None,
            endianness: None,
            unit: None,
            size: None,
        };

        let parsed_bts = options.iter().try_fold(empty_bts, |mut bts, option| {
            match (option.category(), &bts) {
                (Category::Type, Self { typ: None, .. }) => {
                    bts.typ = Some(option.clone());
                    Ok(bts)
                }

                (Category::Type, Self { typ: Some(t), .. }) => Err(Error::ConflictingTypeOptions {
                    previous_location: t.location().clone(),
                    location: option.location().clone(),
                    name: t.label(),
                }),

                (
                    Category::Signedness,
                    Self {
                        signedness: None, ..
                    },
                ) => {
                    bts.signedness = Some(option.clone());
                    Ok(bts)
                }

                (
                    Category::Signedness,
                    Self {
                        signedness: Some(s),
                        ..
                    },
                ) => Err(Error::ConflictingSignednessOptions {
                    previous_location: s.location().clone(),
                    location: option.location().clone(),
                    name: s.label(),
                }),

                (
                    Category::Endianness,
                    Self {
                        endianness: None, ..
                    },
                ) => {
                    bts.endianness = Some(option.clone());
                    Ok(bts)
                }

                (
                    Category::Endianness,
                    Self {
                        endianness: Some(e),
                        ..
                    },
                ) => Err(Error::ConflictingEndiannessOptions {
                    previous_location: e.location().clone(),
                    location: option.location().clone(),
                    name: e.label(),
                }),

                (Category::Size, Self { size: None, .. }) => {
                    bts.size = Some(option.clone());
                    Ok(bts)
                }

                (Category::Size, Self { size: Some(s), .. }) => {
                    Err(Error::ConflictingSizeOptions {
                        previous_location: s.location().clone(),
                        location: option.location().clone(),
                    })
                }

                (Category::Unit, Self { unit: None, .. }) => {
                    bts.unit = Some(option.clone());
                    Ok(bts)
                }

                (Category::Unit, Self { unit: Some(u), .. }) => {
                    Err(Error::ConflictingUnitOptions {
                        previous_location: u.location().clone(),
                        location: option.location().clone(),
                    })
                }

                _ => Ok(bts),
            }
        })?;

        match parsed_bts {
            Self {
                typ: Some(t),
                unit: Some(u),
                ..
            } if !t.unit_is_allowed() => Err(Error::TypeDoesNotAllowUnit {
                location: u.location().clone(),
                typ: t.label(),
            }),

            Self {
                size: None,
                typ: Some(BitStringSegmentOption::Binary { .. }),
                ..
            }
            | Self {
                size: None,
                typ: Some(BitStringSegmentOption::BitString { .. }),
                ..
            } if must_have_size => Err(Error::SegmentMustHaveSize),

            _ => Ok(parsed_bts),
        }
    }

    pub fn typ(&self) -> Option<Arc<Type>> {
        match self.typ {
            Some(BitStringSegmentOption::Integer { .. }) => Some(crate::typ::int()),
            Some(BitStringSegmentOption::Float { .. }) => Some(crate::typ::float()),
            Some(BitStringSegmentOption::Binary { .. }) => Some(crate::typ::bit_string()),
            Some(BitStringSegmentOption::BitString { .. }) => Some(crate::typ::bit_string()),
            Some(BitStringSegmentOption::UTF8 { .. }) => Some(crate::typ::string()),
            Some(BitStringSegmentOption::UTF16 { .. }) => Some(crate::typ::string()),
            Some(BitStringSegmentOption::UTF32 { .. }) => Some(crate::typ::string()),
            Some(BitStringSegmentOption::UTF8Codepoint { .. }) => Some(crate::typ::utf_codepoint()),
            Some(BitStringSegmentOption::UTF16Codepoint { .. }) => {
                Some(crate::typ::utf_codepoint())
            }
            Some(BitStringSegmentOption::UTF32Codepoint { .. }) => {
                Some(crate::typ::utf_codepoint())
            }
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    ConflictingTypeOptions {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: String,
    },

    ConflictingSignednessOptions {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: String,
    },

    ConflictingEndiannessOptions {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: String,
    },

    ConflictingSizeOptions {
        location: SrcSpan,
        previous_location: SrcSpan,
    },

    ConflictingUnitOptions {
        location: SrcSpan,
        previous_location: SrcSpan,
    },

    TypeDoesNotAllowUnit {
        location: SrcSpan,
        typ: String,
    },

    SegmentMustHaveSize,
}

impl<A> BitStringSegmentOption<A> {
    pub fn unit_is_allowed(&self) -> bool {
        !matches!(self,
            BitStringSegmentOption::UTF8 { .. }
            | BitStringSegmentOption::UTF16 { .. }
            | BitStringSegmentOption::UTF32 { .. }
        )
    }
}
