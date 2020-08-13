use crate::{ast::SrcSpan, bit_string::Error as BinaryError, typ::Type};

use std::sync::Arc;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    UnknownLabels {
        unknown: Vec<(String, SrcSpan)>,
        valid: Vec<String>,
        supplied: Vec<String>,
    },

    UnknownVariable {
        location: SrcSpan,
        name: String,
        variables: Vec<String>,
    },

    UnknownType {
        location: SrcSpan,
        name: String,
        types: Vec<String>,
    },

    UnknownModule {
        location: SrcSpan,
        name: String,
        imported_modules: Vec<String>,
    },

    UnknownModuleType {
        location: SrcSpan,
        name: String,
        module_name: Vec<String>,
        type_constructors: Vec<String>,
    },

    UnknownModuleValue {
        location: SrcSpan,
        name: String,
        module_name: Vec<String>,
        value_constructors: Vec<String>,
    },

    UnknownModuleField {
        location: SrcSpan,
        name: String,
        module_name: Vec<String>,
        value_constructors: Vec<String>,
        type_constructors: Vec<String>,
    },

    NotFn {
        location: SrcSpan,
        typ: Arc<Type>,
    },

    UnknownField {
        location: SrcSpan,
        typ: Arc<Type>,
        label: String,
        fields: Vec<String>,
    },

    IncorrectArity {
        location: SrcSpan,
        expected: usize,
        given: usize,
        labels: Vec<String>,
    },

    UnnecessarySpreadOperator {
        location: SrcSpan,
        arity: usize,
    },

    IncorrectTypeArity {
        location: SrcSpan,
        name: String,
        expected: usize,
        given: usize,
    },

    CouldNotUnify {
        location: SrcSpan,
        expected: Arc<Type>,
        given: Arc<Type>,
    },

    RecursiveType {
        location: SrcSpan,
    },

    DuplicateName {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: String,
    },

    DuplicateTypeName {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: String,
    },

    DuplicateArgument {
        location: SrcSpan,
        label: String,
    },

    DuplicateField {
        location: SrcSpan,
        label: String,
    },

    PrivateTypeLeak {
        location: SrcSpan,
        leaked: Type,
    },

    UnexpectedLabelledArg {
        location: SrcSpan,
        label: String,
    },

    PositionalArgumentAfterLabelled {
        location: SrcSpan,
    },

    IncorrectNumClausePatterns {
        location: SrcSpan,
        expected: usize,
        given: usize,
    },

    NonLocalClauseGuardVariable {
        location: SrcSpan,
        name: String,
    },

    ExtraVarInAlternativePattern {
        location: SrcSpan,
        name: String,
    },

    DuplicateVarInPattern {
        location: SrcSpan,
        name: String,
    },

    OutOfBoundsTupleIndex {
        location: SrcSpan,
        index: u64,
        size: usize,
    },

    NotATuple {
        location: SrcSpan,
        given: Arc<Type>,
    },

    NotATupleUnbound {
        location: SrcSpan,
    },

    RecordAccessUnknownType {
        location: SrcSpan,
    },

    RecordUpdateInvalidConstructor {
        location: SrcSpan,
    },

    ConflictingBinaryTypeOptions {
        previous_location: SrcSpan,
        location: SrcSpan,
        name: String,
    },

    ConflictingBinarySignednessOptions {
        previous_location: SrcSpan,
        location: SrcSpan,
        name: String,
    },

    ConflictingBinaryEndiannessOptions {
        previous_location: SrcSpan,
        location: SrcSpan,
        name: String,
    },

    ConflictingBinarySizeOptions {
        previous_location: SrcSpan,
        location: SrcSpan,
    },

    ConflictingBinaryUnitOptions {
        previous_location: SrcSpan,
        location: SrcSpan,
    },

    BinaryTypeDoesNotAllowUnit {
        location: SrcSpan,
        typ: String,
    },

    BinarySegmentMustHaveSize {
        location: SrcSpan,
    },

    InvalidBinarySegmentOption {
        location: SrcSpan,
        label: String,
    },

    UnexpectedTypeHole {
        location: SrcSpan,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Warning {
    Todo { location: SrcSpan, typ: Arc<Type> },

    ImplicitlyDiscardedResult { location: SrcSpan },

    NoFieldsRecordUpdate { location: SrcSpan },

    AllFieldsRecordUpdate { location: SrcSpan },
}

#[derive(Debug, PartialEq)]
pub enum GetValueConstructorError {
    UnknownVariable {
        name: String,
        variables: Vec<String>,
    },

    UnknownModule {
        name: String,
        imported_modules: Vec<String>,
    },

    UnknownModuleValue {
        name: String,
        module_name: Vec<String>,
        value_constructors: Vec<String>,
    },
}

pub fn convert_get_value_constructor_error(
    e: GetValueConstructorError,
    location: &SrcSpan,
) -> Error {
    match e {
        GetValueConstructorError::UnknownVariable { name, variables } => Error::UnknownVariable {
            location: location.clone(),
            name,
            variables,
        },

        GetValueConstructorError::UnknownModule {
            name,
            imported_modules,
        } => Error::UnknownModule {
            location: location.clone(),
            name,
            imported_modules,
        },

        GetValueConstructorError::UnknownModuleValue {
            name,
            module_name,
            value_constructors,
        } => Error::UnknownModuleValue {
            location: location.clone(),
            name,
            module_name,
            value_constructors,
        },
    }
}

#[derive(Debug, PartialEq)]
pub enum GetTypeConstructorError {
    UnknownType {
        name: String,
        type_constructors: Vec<String>,
    },

    UnknownModule {
        name: String,
        imported_modules: Vec<String>,
    },

    UnknownModuleType {
        name: String,
        module_name: Vec<String>,
        type_constructors: Vec<String>,
    },
}

pub fn convert_get_type_constructor_error(e: GetTypeConstructorError, location: &SrcSpan) -> Error {
    match e {
        GetTypeConstructorError::UnknownType {
            name,
            type_constructors,
        } => Error::UnknownType {
            location: location.clone(),
            name,
            types: type_constructors,
        },

        GetTypeConstructorError::UnknownModule {
            name,
            imported_modules,
        } => Error::UnknownModule {
            location: location.clone(),
            name,
            imported_modules,
        },

        GetTypeConstructorError::UnknownModuleType {
            name,
            module_name,
            type_constructors,
        } => Error::UnknownModuleType {
            location: location.clone(),
            name,
            module_name,
            type_constructors,
        },
    }
}

#[derive(Debug)]
pub enum MatchFunTypeError {
    IncorrectArity { expected: usize, given: usize },
    NotFn { typ: Arc<Type> },
}

pub fn convert_not_fun_error(
    e: MatchFunTypeError,
    fn_location: &SrcSpan,
    call_location: &SrcSpan,
) -> Error {
    match e {
        MatchFunTypeError::IncorrectArity { expected, given } => Error::IncorrectArity {
            labels: vec![],
            location: call_location.clone(),
            expected,
            given,
        },

        MatchFunTypeError::NotFn { typ } => Error::NotFn {
            location: fn_location.clone(),
            typ,
        },
    }
}

pub fn flip_unify_error(e: UnifyError) -> UnifyError {
    match e {
        UnifyError::CouldNotUnify { expected, given } => UnifyError::CouldNotUnify {
            expected: given,
            given: expected,
        },
        other => other,
    }
}

pub fn unify_enclosed_type(
    e1: Arc<Type>,
    e2: Arc<Type>,
    result: Result<(), UnifyError>,
) -> Result<(), UnifyError> {
    // If types cannot unify, show the type error with the enclosing types, e1 and e2.
    match result {
        Err(UnifyError::CouldNotUnify { .. }) => Err(UnifyError::CouldNotUnify {
            expected: e1,
            given: e2,
        }),

        _ => result,
    }
}

#[derive(Debug, PartialEq)]
pub enum UnifyError {
    CouldNotUnify {
        expected: Arc<Type>,
        given: Arc<Type>,
    },

    ExtraVarInAlternativePattern {
        name: String,
    },

    DuplicateVarInPattern {
        name: String,
    },

    RecursiveType,
}

pub fn convert_binary_error(e: crate::bit_string::Error, location: &SrcSpan) -> Error {
    match e {
        BinaryError::ConflictingSignednessOptions {
            location,
            previous_location,
            name,
        } => Error::ConflictingBinarySignednessOptions {
            location: location.clone(),
            previous_location: previous_location.clone(),
            name: name.clone(),
        },

        BinaryError::ConflictingEndiannessOptions {
            location,
            previous_location,
            name,
        } => Error::ConflictingBinaryEndiannessOptions {
            location: location.clone(),
            previous_location: previous_location.clone(),
            name: name.clone(),
        },

        BinaryError::ConflictingTypeOptions {
            location,
            previous_location,
            name,
        } => Error::ConflictingBinaryTypeOptions {
            location: location.clone(),
            previous_location: previous_location.clone(),
            name: name.clone(),
        },

        BinaryError::ConflictingSizeOptions {
            location,
            previous_location,
        } => Error::ConflictingBinarySizeOptions {
            location: location.clone(),
            previous_location: previous_location.clone(),
        },

        BinaryError::ConflictingUnitOptions {
            location,
            previous_location,
        } => Error::ConflictingBinaryUnitOptions {
            location: location.clone(),
            previous_location: previous_location.clone(),
        },

        BinaryError::TypeDoesNotAllowUnit { location, typ } => Error::BinaryTypeDoesNotAllowUnit {
            location: location.clone(),
            typ: typ.clone(),
        },

        BinaryError::SegmentMustHaveSize => Error::BinarySegmentMustHaveSize {
            location: location.clone(),
        },
    }
}

pub fn convert_unify_error(e: UnifyError, location: &SrcSpan) -> Error {
    match e {
        UnifyError::CouldNotUnify { expected, given } => Error::CouldNotUnify {
            location: location.clone(),
            expected,
            given,
        },

        UnifyError::ExtraVarInAlternativePattern { name } => Error::ExtraVarInAlternativePattern {
            location: location.clone(),
            name,
        },

        UnifyError::DuplicateVarInPattern { name } => Error::DuplicateVarInPattern {
            location: location.clone(),
            name,
        },

        UnifyError::RecursiveType => Error::RecursiveType {
            location: location.clone(),
        },
    }
}
