use crate::{
    ast::{BinOp, SrcSpan, TodoKind},
    type_::Type,
};

use std::{path::PathBuf, sync::Arc};

#[cfg(test)]
use pretty_assertions::assert_eq;
use smol_str::SmolStr;

use super::FieldAccessUsage;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Error {
    SrcImportingTest {
        location: SrcSpan,
        src_module: crate::error::Name,
        test_module: crate::error::Name,
    },

    BitStringSegmentError {
        error: crate::bit_string::ErrorType,
        location: SrcSpan,
    },
    UnknownLabels {
        unknown: Vec<(SmolStr, SrcSpan)>,
        valid: Vec<SmolStr>,
        supplied: Vec<SmolStr>,
    },

    UnknownVariable {
        location: SrcSpan,
        name: SmolStr,
        variables: Vec<SmolStr>,
    },

    UnknownType {
        location: SrcSpan,
        name: SmolStr,
        types: Vec<SmolStr>,
    },

    UnknownModule {
        location: SrcSpan,
        name: SmolStr,
        imported_modules: Vec<SmolStr>,
    },

    UnknownModuleType {
        location: SrcSpan,
        name: SmolStr,
        module_name: SmolStr,
        type_constructors: Vec<SmolStr>,
    },

    UnknownModuleValue {
        location: SrcSpan,
        name: SmolStr,
        module_name: SmolStr,
        value_constructors: Vec<SmolStr>,
    },

    UnknownModuleField {
        location: SrcSpan,
        name: SmolStr,
        module_name: SmolStr,
        value_constructors: Vec<SmolStr>,
        type_constructors: Vec<SmolStr>,
    },

    NotFn {
        location: SrcSpan,
        typ: Arc<Type>,
    },

    UnknownRecordField {
        location: SrcSpan,
        typ: Arc<Type>,
        label: SmolStr,
        fields: Vec<SmolStr>,
        usage: FieldAccessUsage,
    },

    IncorrectArity {
        location: SrcSpan,
        expected: usize,
        given: usize,
        labels: Vec<SmolStr>,
    },

    UpdateMultiConstructorType {
        location: SrcSpan,
    },

    UnnecessarySpreadOperator {
        location: SrcSpan,
        arity: usize,
    },

    IncorrectTypeArity {
        location: SrcSpan,
        name: SmolStr,
        expected: usize,
        given: usize,
    },

    CouldNotUnify {
        location: SrcSpan,
        situation: Option<UnifyErrorSituation>,
        expected: Arc<Type>,
        given: Arc<Type>,
        rigid_type_names: im::HashMap<u64, SmolStr>,
    },

    RecursiveType {
        location: SrcSpan,
    },

    DuplicateName {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: SmolStr,
    },

    DuplicateImport {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: SmolStr,
    },

    DuplicateTypeName {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: SmolStr,
    },

    DuplicateArgument {
        location: SrcSpan,
        label: SmolStr,
    },

    DuplicateField {
        location: SrcSpan,
        label: SmolStr,
    },

    PrivateTypeLeak {
        location: SrcSpan,
        leaked: Type,
    },

    UnexpectedLabelledArg {
        location: SrcSpan,
        label: SmolStr,
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
        name: SmolStr,
    },

    ExtraVarInAlternativePattern {
        location: SrcSpan,
        name: SmolStr,
    },

    MissingVarInAlternativePattern {
        location: SrcSpan,
        name: SmolStr,
    },

    DuplicateVarInPattern {
        location: SrcSpan,
        name: SmolStr,
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

    ReservedModuleName {
        name: SmolStr,
    },

    KeywordInModuleName {
        name: SmolStr,
        keyword: SmolStr,
    },

    NotExhaustivePatternMatch {
        location: SrcSpan,
        unmatched: Vec<SmolStr>,
        kind: PatternMatchKind,
    },

    /// A function was defined with multiple arguments with the same name
    ///
    /// # Examples
    ///
    /// ```gleam
    /// fn main(x, x) { Nil }
    /// ```
    /// ```gleam
    /// fn main() {
    ///   fn(x, x) { Nil }
    /// }
    /// ```
    ArgumentNameAlreadyUsed {
        location: SrcSpan,
        name: SmolStr,
    },

    /// A function was defined with an unlabelled argument after a labelled one.
    UnlabelledAfterlabelled {
        location: SrcSpan,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatternMatchKind {
    Case,
    Assignment,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Warning {
    Todo {
        kind: TodoKind,
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
        name: SmolStr,
    },

    UnusedConstructor {
        location: SrcSpan,
        imported: bool,
        name: SmolStr,
    },

    UnusedImportedValue {
        location: SrcSpan,
        name: SmolStr,
    },

    UnusedImportedModule {
        location: SrcSpan,
        name: SmolStr,
    },

    UnusedPrivateModuleConstant {
        location: SrcSpan,
        name: SmolStr,
    },

    UnusedPrivateFunction {
        location: SrcSpan,
        name: SmolStr,
    },

    UnusedVariable {
        location: SrcSpan,
        name: SmolStr,
    },
}

impl Error {
    pub fn with_unify_error_situation(mut self, new_situation: UnifyErrorSituation) -> Self {
        match self {
            Error::CouldNotUnify {
                ref mut situation, ..
            } => {
                *situation = Some(new_situation);
                self
            }
            _ => self,
        }
    }

    pub fn with_unify_error_rigid_names(mut self, new_names: &im::HashMap<u64, SmolStr>) -> Self {
        match self {
            Error::CouldNotUnify {
                rigid_type_names: ref mut annotated_names,
                ..
            } => {
                *annotated_names = new_names.clone();
                self
            }
            _ => self,
        }
    }
}

impl Warning {
    pub fn into_warning(self, path: PathBuf, src: SmolStr) -> crate::Warning {
        crate::Warning::Type {
            path,
            src,
            warning: self,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnknownValueConstructorError {
    Variable {
        name: SmolStr,
        variables: Vec<SmolStr>,
    },

    Module {
        name: SmolStr,
        imported_modules: Vec<SmolStr>,
    },

    ModuleValue {
        name: SmolStr,
        module_name: SmolStr,
        value_constructors: Vec<SmolStr>,
    },
}

pub fn convert_get_value_constructor_error(
    e: UnknownValueConstructorError,
    location: SrcSpan,
) -> Error {
    match e {
        UnknownValueConstructorError::Variable { name, variables } => Error::UnknownVariable {
            location,
            name,
            variables,
        },

        UnknownValueConstructorError::Module {
            name,
            imported_modules,
        } => Error::UnknownModule {
            location,
            name,
            imported_modules,
        },

        UnknownValueConstructorError::ModuleValue {
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

#[derive(Debug, PartialEq, Eq)]
pub enum UnknownTypeConstructorError {
    Type {
        name: SmolStr,
        type_constructors: Vec<SmolStr>,
    },

    Module {
        name: SmolStr,
        imported_modules: Vec<SmolStr>,
    },

    ModuleType {
        name: SmolStr,
        module_name: SmolStr,
        type_constructors: Vec<SmolStr>,
    },
}

pub fn convert_get_type_constructor_error(
    e: UnknownTypeConstructorError,
    location: &SrcSpan,
) -> Error {
    match e {
        UnknownTypeConstructorError::Type {
            name,
            type_constructors,
        } => Error::UnknownType {
            location: *location,
            name,
            types: type_constructors,
        },

        UnknownTypeConstructorError::Module {
            name,
            imported_modules,
        } => Error::UnknownModule {
            location: *location,
            name,
            imported_modules,
        },

        UnknownTypeConstructorError::ModuleType {
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
            expected: crate::type_::int(),
            given: crate::type_::float(),
            situation: Some(UnifyErrorSituation::CaseClauseMismatch),
        },
        flip_unify_error(UnifyError::CouldNotUnify {
            expected: crate::type_::float(),
            given: crate::type_::int(),
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
            expected: crate::type_::int(),
            given: crate::type_::float(),
            situation: Some(UnifyErrorSituation::CaseClauseMismatch)
        }),
        unify_enclosed_type(
            crate::type_::int(),
            crate::type_::float(),
            Err(UnifyError::CouldNotUnify {
                expected: crate::type_::string(),
                given: crate::type_::bit_string(),
                situation: Some(UnifyErrorSituation::CaseClauseMismatch)
            })
        )
    );
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnifyErrorSituation {
    /// Clauses in a case expression were found to return different types.
    CaseClauseMismatch,

    /// A function was found to return a value that did not match its return
    /// annotation.
    ReturnAnnotationMismatch,

    PipeTypeMismatch,

    /// The operands of a binary operator were incorrect.
    Operator(BinOp),

    /// A try expression returned a different error type to the previous try.
    TryErrorMismatch,

    /// The final value of a try expression was not a Result.
    TryReturnResult,
}

impl UnifyErrorSituation {
    pub fn description(&self) -> Option<&'static str> {
        match self {
            Self::CaseClauseMismatch => Some(
                "This case clause was found to return a different type than the previous
one, but all case clauses must return the same type.",
            ),
            Self::ReturnAnnotationMismatch => Some(
                "The type of this returned value doesn't match the return type 
annotation of this function.",
            ),
            Self::PipeTypeMismatch => {
                Some("This function cannot handle the argument sent through the (|>) pipe:")
            }
            Self::Operator(_op) => None,

            UnifyErrorSituation::TryErrorMismatch => Some(
                "This returned value has a type incompatible with the previous try expression.
All the try expressions in a block and the final result value must have
the same error type.",
            ),

            UnifyErrorSituation::TryReturnResult => Some(
                "This returned value has a type incompatible with the previous try expression.
The returned value after a try must be of type Result.",
            ),
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
        name: SmolStr,
    },

    MissingVarInAlternativePattern {
        name: SmolStr,
    },

    DuplicateVarInPattern {
        name: SmolStr,
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

    pub fn inconsistent_try(self, return_value_is_result: bool) -> Self {
        self.with_unify_error_situation(if return_value_is_result {
            UnifyErrorSituation::TryErrorMismatch
        } else {
            UnifyErrorSituation::TryReturnResult
        })
    }

    pub fn into_error(self, location: SrcSpan) -> Error {
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
                rigid_type_names: im::hashmap![],
            },

            Self::ExtraVarInAlternativePattern { name } => {
                Error::ExtraVarInAlternativePattern { location, name }
            }

            Self::MissingVarInAlternativePattern { name } => {
                Error::MissingVarInAlternativePattern { location, name }
            }

            Self::DuplicateVarInPattern { name } => Error::DuplicateVarInPattern { location, name },

            Self::RecursiveType => Error::RecursiveType { location },
        }
    }
}

pub fn convert_unify_error(e: UnifyError, location: SrcSpan) -> Error {
    e.into_error(location)
}
