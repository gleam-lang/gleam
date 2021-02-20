use crate::{
    ast::{BinOp, SrcSpan},
    typ::Type,
};

use std::sync::Arc;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    BitStringSegmentError {
        error: crate::bit_string::ErrorType,
        location: SrcSpan,
    },
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
        situation: Option<UnifyErrorSituation>,
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

    UnexpectedTypeHole {
        location: SrcSpan,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Warning {
    Todo {
        location: SrcSpan,
        typ: Arc<Type>,
    },

    ImplicitlyDiscardedResult {
        location: SrcSpan,
    },

    UnusedLiteral {
        location: SrcSpan,
    },

    NoFieldsRecordUpdate {
        location: SrcSpan,
    },

    AllFieldsRecordUpdate {
        location: SrcSpan,
    },

    UnusedType {
        location: SrcSpan,
        imported: bool,
        name: String,
    },

    UnusedConstructor {
        location: SrcSpan,
        imported: bool,
        name: String,
    },

    UnusedImportedValue {
        location: SrcSpan,
        name: String,
    },

    UnusedPrivateModuleConstant {
        location: SrcSpan,
        name: String,
    },

    UnusedPrivateFunction {
        location: SrcSpan,
        name: String,
    },

    UnusedVariable {
        location: SrcSpan,
        name: String,
    },
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
    location: SrcSpan,
) -> Error {
    match e {
        GetValueConstructorError::UnknownVariable { name, variables } => Error::UnknownVariable {
            location,
            name,
            variables,
        },

        GetValueConstructorError::UnknownModule {
            name,
            imported_modules,
        } => Error::UnknownModule {
            location,
            name,
            imported_modules,
        },

        GetValueConstructorError::UnknownModuleValue {
            name,
            module_name,
            value_constructors,
        } => Error::UnknownModuleValue {
            location,
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
            location: *location,
            name,
            types: type_constructors,
        },

        GetTypeConstructorError::UnknownModule {
            name,
            imported_modules,
        } => Error::UnknownModule {
            location: *location,
            name,
            imported_modules,
        },

        GetTypeConstructorError::UnknownModuleType {
            name,
            module_name,
            type_constructors,
        } => Error::UnknownModuleType {
            location: *location,
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
    fn_location: SrcSpan,
    call_location: SrcSpan,
) -> Error {
    match e {
        MatchFunTypeError::IncorrectArity { expected, given } => Error::IncorrectArity {
            labels: vec![],
            location: call_location,
            expected,
            given,
        },

        MatchFunTypeError::NotFn { typ } => Error::NotFn {
            location: fn_location,
            typ,
        },
    }
}

pub fn flip_unify_error(e: UnifyError) -> UnifyError {
    match e {
        UnifyError::CouldNotUnify {
            expected,
            given,
            situation: note,
        } => UnifyError::CouldNotUnify {
            expected: given,
            given: expected,
            situation: note,
        },
        other => other,
    }
}

#[test]
fn flip_unify_error_test() {
    assert_eq!(
        UnifyError::CouldNotUnify {
            expected: crate::typ::int(),
            given: crate::typ::float(),
            situation: Some(UnifyErrorSituation::CaseClauseMismatch),
        },
        flip_unify_error(UnifyError::CouldNotUnify {
            expected: crate::typ::float(),
            given: crate::typ::int(),
            situation: Some(UnifyErrorSituation::CaseClauseMismatch),
        })
    );
}

pub fn unify_enclosed_type(
    e1: Arc<Type>,
    e2: Arc<Type>,
    result: Result<(), UnifyError>,
) -> Result<(), UnifyError> {
    // If types cannot unify, show the type error with the enclosing types, e1 and e2.
    match result {
        Err(UnifyError::CouldNotUnify {
            situation: note, ..
        }) => Err(UnifyError::CouldNotUnify {
            expected: e1,
            given: e2,
            situation: note,
        }),

        _ => result,
    }
}

#[test]
fn unify_enclosed_type_test() {
    assert_eq!(
        Err(UnifyError::CouldNotUnify {
            expected: crate::typ::int(),
            given: crate::typ::float(),
            situation: Some(UnifyErrorSituation::CaseClauseMismatch)
        }),
        unify_enclosed_type(
            crate::typ::int(),
            crate::typ::float(),
            Err(UnifyError::CouldNotUnify {
                expected: crate::typ::string(),
                given: crate::typ::bit_string(),
                situation: Some(UnifyErrorSituation::CaseClauseMismatch)
            })
        )
    );
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnifyErrorSituation {
    CaseClauseMismatch,
    ReturnAnnotationMismatch,
    Operator(BinOp),
}

impl UnifyErrorSituation {
    pub fn description(&self) -> &'static str {
        match self {
            Self::CaseClauseMismatch => {
                "This case clause was found to return a different type than the previous
one, but all case clauses must return the same type."
            }
            Self::ReturnAnnotationMismatch => {
                "The type of this returned value doesn't match the return type 
annotation of this function."
            }
            Self::Operator(_op) => {
                todo!()
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum UnifyError {
    CouldNotUnify {
        expected: Arc<Type>,
        given: Arc<Type>,
        situation: Option<UnifyErrorSituation>,
    },

    ExtraVarInAlternativePattern {
        name: String,
    },

    DuplicateVarInPattern {
        name: String,
    },

    RecursiveType,
}

impl UnifyError {
    pub fn with_unify_error_situation(self, situation: UnifyErrorSituation) -> Self {
        match self {
            Self::CouldNotUnify {
                expected, given, ..
            } => Self::CouldNotUnify {
                expected,
                given,
                situation: Some(situation),
            },
            other => other,
        }
    }

    pub fn case_clause_mismatch(self) -> Self {
        self.with_unify_error_situation(UnifyErrorSituation::CaseClauseMismatch)
    }

    pub fn return_annotation_mismatch(self) -> Self {
        self.with_unify_error_situation(UnifyErrorSituation::ReturnAnnotationMismatch)
    }

    pub fn operator_situation(self, binop: BinOp) -> Self {
        self.with_unify_error_situation(UnifyErrorSituation::Operator(binop))
    }

    pub fn to_error(self, location: SrcSpan) -> Error {
        match self {
            Self::CouldNotUnify {
                expected,
                given,
                situation: note,
            } => Error::CouldNotUnify {
                location,
                expected,
                given,
                situation: note,
            },

            Self::ExtraVarInAlternativePattern { name } => {
                Error::ExtraVarInAlternativePattern { location, name }
            }

            Self::DuplicateVarInPattern { name } => Error::DuplicateVarInPattern { location, name },

            Self::RecursiveType => Error::RecursiveType { location },
        }
    }
}

pub fn convert_unify_error(e: UnifyError, location: SrcSpan) -> Error {
    e.to_error(location)
}
