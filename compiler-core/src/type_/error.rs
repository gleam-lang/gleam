use crate::{
    ast::{BinOp, SrcSpan, TodoKind},
    build::Target,
    type_::Type,
};

use camino::Utf8PathBuf;
use std::sync::Arc;

use crate::ast::Layer;
use ecow::EcoString;
#[cfg(test)]
use pretty_assertions::assert_eq;

use super::FieldAccessUsage;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UnknownType {
    pub location: SrcSpan,
    pub name: EcoString,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Error {
    SrcImportingTest {
        location: SrcSpan,
        src_module: crate::error::Name,
        test_module: crate::error::Name,
    },

    BitArraySegmentError {
        error: crate::bit_array::ErrorType,
        location: SrcSpan,
    },

    UnknownLabels {
        unknown: Vec<(EcoString, SrcSpan)>,
        valid: Vec<EcoString>,
        supplied: Vec<EcoString>,
    },

    UnknownVariable {
        location: SrcSpan,
        name: EcoString,
        variables: Vec<EcoString>,
    },

    UnknownType {
        location: SrcSpan,
        name: EcoString,
        hint: UnknownTypeHint,
    },

    UnknownModule {
        location: SrcSpan,
        name: EcoString,
        imported_modules: Vec<EcoString>,
    },

    UnknownModuleType {
        location: SrcSpan,
        name: EcoString,
        module_name: EcoString,
        type_constructors: Vec<EcoString>,
    },

    UnknownModuleValue {
        location: SrcSpan,
        name: EcoString,
        module_name: EcoString,
        value_constructors: Vec<EcoString>,
    },

    UnknownModuleField {
        location: SrcSpan,
        name: EcoString,
        module_name: EcoString,
        value_constructors: Vec<EcoString>,
        type_constructors: Vec<EcoString>,
    },

    NotFn {
        location: SrcSpan,
        typ: Arc<Type>,
    },

    UnknownRecordField {
        location: SrcSpan,
        typ: Arc<Type>,
        label: EcoString,
        fields: Vec<EcoString>,
        usage: FieldAccessUsage,
    },

    IncorrectArity {
        location: SrcSpan,
        expected: usize,
        given: usize,
        labels: Vec<EcoString>,
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
        name: EcoString,
        expected: usize,
        given: usize,
    },

    CouldNotUnify {
        location: SrcSpan,
        situation: Option<UnifyErrorSituation>,
        expected: Arc<Type>,
        given: Arc<Type>,
        rigid_type_names: im::HashMap<u64, EcoString>,
    },

    RecursiveType {
        location: SrcSpan,
    },

    DuplicateName {
        location_a: SrcSpan,
        location_b: SrcSpan,
        name: EcoString,
    },

    DuplicateImport {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: EcoString,
    },

    DuplicateTypeName {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: EcoString,
    },

    DuplicateArgument {
        location: SrcSpan,
        label: EcoString,
    },

    DuplicateField {
        location: SrcSpan,
        label: EcoString,
    },

    PrivateTypeLeak {
        location: SrcSpan,
        leaked: Type,
    },

    UnexpectedLabelledArg {
        location: SrcSpan,
        label: EcoString,
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
        name: EcoString,
    },

    ExtraVarInAlternativePattern {
        location: SrcSpan,
        name: EcoString,
    },

    MissingVarInAlternativePattern {
        location: SrcSpan,
        name: EcoString,
    },

    DuplicateVarInPattern {
        location: SrcSpan,
        name: EcoString,
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
        name: EcoString,
    },

    KeywordInModuleName {
        name: EcoString,
        keyword: EcoString,
    },

    NotExhaustivePatternMatch {
        location: SrcSpan,
        unmatched: Vec<EcoString>,
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
        name: EcoString,
    },

    /// A function was defined with an unlabelled argument after a labelled one.
    UnlabelledAfterlabelled {
        location: SrcSpan,
    },

    /// A type alias was defined directly or indirectly in terms of itself, which would
    /// cause it to expand to infinite size.
    /// e.g.
    ///     type ForkBomb = #(ForkBomb, ForkBomb)
    RecursiveTypeAlias {
        location: SrcSpan,
        cycle: Vec<EcoString>,
    },

    /// A function has been given an external implementation but not all the
    /// type annotations have been given. The annotations are required as we
    /// cannot infer the types of external implementations.
    ExternalMissingAnnotation {
        location: SrcSpan,
        kind: MissingAnnotation,
    },

    /// A function has been given without either a Gleam implementation or an
    /// external one.
    NoImplementation {
        location: SrcSpan,
    },

    /// A function/constant that is used doesn't have an implementation for the
    /// current compilation target.
    UnsupportedExpressionTarget {
        location: SrcSpan,
        target: Target,
    },

    /// A function's JavaScript implementation has been given but it does not
    /// have a valid module name.
    InvalidExternalJavascriptModule {
        location: SrcSpan,
        module: EcoString,
        name: EcoString,
    },

    /// A function's JavaScript implementation has been given but it does not
    /// have a valid function name.
    InvalidExternalJavascriptFunction {
        location: SrcSpan,
        function: EcoString,
        name: EcoString,
    },

    /// A case expression is missing one or more patterns to match all possible
    /// values of the type.
    InexhaustiveCaseExpression {
        location: SrcSpan,
        missing: Vec<EcoString>,
    },

    /// Let assignment's pattern does not match all possible values of the type.
    InexhaustiveLetAssignment {
        location: SrcSpan,
        missing: Vec<EcoString>,
    },

    /// A type alias has a type variable but it is not used in the definition.
    ///
    /// For example, here `unused` is not used
    ///
    /// ```gleam
    /// pub type Wibble(unused) =
    ///   Int
    /// ```
    UnusedTypeAliasParameter {
        location: SrcSpan,
        name: EcoString,
    },

    /// A definition has two type parameters with the same name.
    ///
    /// ```gleam
    /// pub type Wibble(a, a) =
    ///   Int
    /// ```
    /// ```gleam
    /// pub type Wibble(a, a) {
    ///   Wibble
    /// }
    /// ```
    DuplicateTypeParameter {
        location: SrcSpan,
        name: EcoString,
    },

    /// A public function doesn't have an implementation for the current target.
    /// This is only raised when compiling a package with `TargetSupport::Enforced`, which is
    /// typically the root package, deps not being enforced.
    ///
    /// For example, if compiling to Erlang:
    ///
    /// ```gleam
    /// @external(javascript, "one", "two")
    /// pub fn wobble() -> Int
    /// ```
    UnsupportedPublicFunctionTarget {
        target: Target,
        name: EcoString,
        location: SrcSpan,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MissingAnnotation {
    Parameter,
    Return,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatternMatchKind {
    Case,
    Assignment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmptyListCheckKind {
    Empty,
    NonEmpty,
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
        name: EcoString,
    },

    UnusedConstructor {
        location: SrcSpan,
        imported: bool,
        name: EcoString,
    },

    UnusedImportedValue {
        location: SrcSpan,
        name: EcoString,
    },

    UnusedImportedModule {
        location: SrcSpan,
        name: EcoString,
    },

    UnusedImportedModuleAlias {
        location: SrcSpan,
        alias: EcoString,
        module_name: EcoString,
    },

    UnusedPrivateModuleConstant {
        location: SrcSpan,
        name: EcoString,
    },

    UnusedPrivateFunction {
        location: SrcSpan,
        name: EcoString,
    },

    UnusedVariable {
        location: SrcSpan,
        name: EcoString,
    },

    UnnecessaryDoubleIntNegation {
        location: SrcSpan,
    },

    UnnecessaryDoubleBoolNegation {
        location: SrcSpan,
    },

    InefficientEmptyListCheck {
        location: SrcSpan,
        kind: EmptyListCheckKind,
    },

    TransitiveDependencyImported {
        location: SrcSpan,
        module: EcoString,
        package: EcoString,
    },

    DeprecatedItem {
        location: SrcSpan,
        message: EcoString,
        layer: Layer,
    },

    UnreachableCaseClause {
        location: SrcSpan,
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

    pub fn with_unify_error_rigid_names(mut self, new_names: &im::HashMap<u64, EcoString>) -> Self {
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
    pub fn into_warning(self, path: Utf8PathBuf, src: EcoString) -> crate::Warning {
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
        name: EcoString,
        variables: Vec<EcoString>,
    },

    Module {
        name: EcoString,
        imported_modules: Vec<EcoString>,
    },

    ModuleValue {
        name: EcoString,
        module_name: EcoString,
        value_constructors: Vec<EcoString>,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnknownTypeHint {
    AlternativeTypes(Vec<EcoString>),
    ValueInScopeWithSameName,
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnknownTypeConstructorError {
    Type {
        name: EcoString,
        hint: UnknownTypeHint,
    },

    Module {
        name: EcoString,
        imported_modules: Vec<EcoString>,
    },

    ModuleType {
        name: EcoString,
        module_name: EcoString,
        type_constructors: Vec<EcoString>,
    },
}

pub fn convert_get_type_constructor_error(
    e: UnknownTypeConstructorError,
    location: &SrcSpan,
) -> Error {
    match e {
        UnknownTypeConstructorError::Type { name, hint } => Error::UnknownType {
            location: *location,
            name,
            hint,
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
                given: crate::type_::bits(),
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

    /// One of the elements of a list was not the same type as the others.
    ListElementMismatch,

    /// The tail of the list is not the same type as the other elements.
    ListTailMismatch,
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

            Self::TryErrorMismatch => Some(
                "This returned value has a type incompatible with the previous try expression.
All the try expressions in a block and the final result value must have
the same error type.",
            ),

            Self::TryReturnResult => Some(
                "This returned value has a type incompatible with the previous try expression.
The returned value after a try must be of type Result.",
            ),

            Self::ListElementMismatch => Some(
                "All elements of a list must be the same type, but this one doesn't
match the one before it.",
            ),

            Self::ListTailMismatch => Some(
                "All elements in a list must have the same type, but the elements of
this list don't match the type of the elements being prepended to it.",
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
        name: EcoString,
    },

    MissingVarInAlternativePattern {
        name: EcoString,
    },

    DuplicateVarInPattern {
        name: EcoString,
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

    pub fn list_element_mismatch(self) -> Self {
        self.with_unify_error_situation(UnifyErrorSituation::ListElementMismatch)
    }

    pub fn list_tail_mismatch(self) -> Self {
        self.with_unify_error_situation(UnifyErrorSituation::ListTailMismatch)
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
