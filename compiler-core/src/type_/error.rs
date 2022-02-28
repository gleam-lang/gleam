use crate::{
    ast::{BinOp, SrcSpan},
    diagnostic::{Diagnostic, Label, Location},
    type_::Type,
};

use std::{path::PathBuf, sync::Arc};

#[cfg(test)]
use pretty_assertions::assert_eq;

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

    UnknownRecordField {
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
        rigid_type_names: im::HashMap<u64, String>,
    },

    RecursiveType {
        location: SrcSpan,
    },

    DuplicateName {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: String,
    },

    DuplicateImport {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: String,
    },

    DuplicateTypeName {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: String,
    },

    DuplicateConstName {
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

    MissingVarInAlternativePattern {
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

    ReservedModuleName {
        name: String,
    },

    KeywordInModuleName {
        name: String,
        keyword: String,
    },

    NotExhaustivePatternMatch {
        location: SrcSpan,
        unmatched: Vec<String>,
    },
}

impl Error {
    pub fn to_diagnostic(&self, src: String, path: PathBuf) -> Diagnostic {
        match &self {
            Error::UnknownModule { location, name, .. } => Diagnostic {
                title: "Unknown module".to_string(),
                text: format!(
                    "Unresolved module {name}\nModule {name} not found",
                    name = name
                ),
                level: crate::diagnostic::Level::Error,
                location: Some(Location {
                    src,
                    path,
                    label: Label {
                        text: None,
                        span: *location,
                    },
                    extra_labels: Vec::new(),
                }),
            },
            Error::UnknownVariable { location, name, .. } => Diagnostic {
                title: "Unknown variable".to_string(),
                text: format!("Variable `{}` not found in this scope", name),
                level: crate::diagnostic::Level::Error,
                location: Some(Location {
                    src,
                    path,
                    label: Label {
                        text: None,
                        span: *location,
                    },
                    extra_labels: Vec::new(),
                }),
            },

            Error::UnknownType { location, name, .. } => Diagnostic {
                title: "Unknown type".to_string(),
                text: format!("Type `{}` not found in this scope", name),
                level: crate::diagnostic::Level::Error,
                location: Some(Location {
                    src,
                    path,
                    label: Label {
                        text: None,
                        span: *location,
                    },
                    extra_labels: Vec::new(),
                }),
            },

            Error::UnknownModuleType { location, name, .. } => Diagnostic {
                title: "Unknown module type".to_string(),
                text: format!("Module type `{}` not found in this scope", name),
                level: crate::diagnostic::Level::Error,
                location: Some(Location {
                    src,
                    path,
                    label: Label {
                        text: None,
                        span: *location,
                    },
                    extra_labels: Vec::new(),
                }),
            },
            _ => Diagnostic {
                title: "TODO".to_string(),
                text: "TODO".to_string(),
                level: crate::diagnostic::Level::Error,
                location: None,
            },
            // Error::UnknownModuleValue { location, name, module_name, value_constructors } => todo!(),
            // Error::UnknownModuleField { location, name, module_name, value_constructors, type_constructors } => todo!(),
            // Error::NotFn { location, typ } => todo!(),
            // Error::UnknownRecordField { location, typ, label, fields } => todo!(),
            // Error::IncorrectArity { location, expected, given, labels } => todo!(),
            // Error::UnnecessarySpreadOperator { location, arity } => todo!(),
            // Error::IncorrectTypeArity { location, name, expected, given } => todo!(),
            // Error::CouldNotUnify { location, situation, expected, given, rigid_type_names } => todo!(),
            // Error::RecursiveType { location } => todo!(),
            // Error::DuplicateName { location, previous_location, name } => todo!(),
            // Error::DuplicateImport { location, previous_location, name } => todo!(),
            // Error::DuplicateTypeName { location, previous_location, name } => todo!(),
            // Error::DuplicateConstName { location, previous_location, name } => todo!(),
            // Error::DuplicateArgument { location, label } => todo!(),
            // Error::DuplicateField { location, label } => todo!(),
            // Error::PrivateTypeLeak { location, leaked } => todo!(),
            // Error::UnexpectedLabelledArg { location, label } => todo!(),
            // Error::PositionalArgumentAfterLabelled { location } => todo!(),
            // Error::IncorrectNumClausePatterns { location, expected, given } => todo!(),
            // Error::NonLocalClauseGuardVariable { location, name } => todo!(),
            // Error::ExtraVarInAlternativePattern { location, name } => todo!(),
            // Error::MissingVarInAlternativePattern { location, name } => todo!(),
            // Error::DuplicateVarInPattern { location, name } => todo!(),
            // Error::OutOfBoundsTupleIndex { location, index, size } => todo!(),
            // Error::NotATuple { location, given } => todo!(),
            // Error::NotATupleUnbound { location } => todo!(),
            // Error::RecordAccessUnknownType { location } => todo!(),
            // Error::RecordUpdateInvalidConstructor { location } => todo!(),
            // Error::UnexpectedTypeHole { location } => todo!(),
            // Error::ReservedModuleName { name } => todo!(),
            // Error::KeywordInModuleName { name, keyword } => todo!(),
            // Error::NotExhaustivePatternMatch { location, unmatched } => todo!(),
            // Error::BitStringSegmentError { error, location } => todo!(),
            // Error::UnknownLabels { unknown, valid, supplied } => todo!(),
            // _ => "".to_string()
        }
    }
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

    pub fn with_unify_error_rigid_names(mut self, new_names: &im::HashMap<u64, String>) -> Self {
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
    pub fn into_warning(self, path: PathBuf, src: String) -> crate::Warning {
        crate::Warning::Type {
            path,
            src,
            warning: self,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum UnknownValueConstructorError {
    Variable {
        name: String,
        variables: Vec<String>,
    },

    Module {
        name: String,
        imported_modules: Vec<String>,
    },

    ModuleValue {
        name: String,
        module_name: Vec<String>,
        value_constructors: Vec<String>,
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

#[derive(Debug, PartialEq)]
pub enum UnknownTypeConstructorError {
    Type {
        name: String,
        type_constructors: Vec<String>,
    },

    Module {
        name: String,
        imported_modules: Vec<String>,
    },

    ModuleType {
        name: String,
        module_name: Vec<String>,
        type_constructors: Vec<String>,
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
        name: String,
    },

    MissingVarInAlternativePattern {
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
