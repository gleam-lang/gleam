use crate::ast::{BinSegmentOption, SegmentOptionCategory as Category, SrcSpan};
use crate::typ::Type;
use std::sync::Arc;

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryTypeSpecifier<T> {
    typ: Option<BinSegmentOption<T>>,
    signedness: Option<BinSegmentOption<T>>,
    endianness: Option<BinSegmentOption<T>>,
    unit: Option<BinSegmentOption<T>>,
    size: Option<BinSegmentOption<T>>,
}

impl<T> BinaryTypeSpecifier<T> {
    pub fn new(options: &Vec<BinSegmentOption<T>>, must_have_size: bool) -> Result<Self, Error>
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
                typ: Some(BinSegmentOption::Binary { .. }),
                ..
            }
            | Self {
                size: None,
                typ: Some(BinSegmentOption::BitString { .. }),
                ..
            } if must_have_size => Err(Error::SegmentMustHaveSize),

            _ => Ok(parsed_bts),
        }
    }

    pub fn typ(&self) -> Option<Arc<Type>> {
        match self.typ {
            Some(BinSegmentOption::Integer { .. }) => Some(crate::typ::int()),
            Some(BinSegmentOption::Float { .. }) => Some(crate::typ::float()),
            Some(BinSegmentOption::Binary { .. }) => Some(crate::typ::bit_string()),
            Some(BinSegmentOption::BitString { .. }) => Some(crate::typ::bit_string()),
            Some(BinSegmentOption::UTF8 { .. }) => Some(crate::typ::string()),
            Some(BinSegmentOption::UTF16 { .. }) => Some(crate::typ::bit_string()),
            Some(BinSegmentOption::UTF32 { .. }) => Some(crate::typ::bit_string()),
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

impl<A> BinSegmentOption<A> {
    pub fn unit_is_allowed(&self) -> bool {
        match self {
            BinSegmentOption::UTF8 { .. }
            | BinSegmentOption::UTF16 { .. }
            | BinSegmentOption::UTF32 { .. } => false,
            _ => true,
        }
    }
}
