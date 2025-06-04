use super::{
    FieldAccessUsage,
    expression::{ArgumentKind, CallKind},
};
use crate::{
    ast::{BinOp, BitArraySegmentTruncation, Layer, SrcSpan, TodoKind},
    build::Target,
    type_::Type,
};

use camino::Utf8PathBuf;
use ecow::EcoString;
use hexpm::version::Version;
use num_bigint::BigInt;
#[cfg(test)]
use pretty_assertions::assert_eq;
use std::sync::Arc;

/// Errors and warnings discovered when compiling a module.
///
#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct Problems {
    errors: Vec<Error>,
    warnings: Vec<Warning>,
}

impl Problems {
    pub fn new() -> Self {
        Default::default()
    }

    /// Sort the warnings and errors by their location.
    ///
    pub fn sort(&mut self) {
        self.errors.sort_by_key(|e| e.start_location());
        self.warnings.sort_by_key(|w| w.location().start);
    }

    /// Register an error.
    ///
    pub fn error(&mut self, error: Error) {
        self.errors.push(error)
    }

    /// Register a warning.
    ///
    pub fn warning(&mut self, warning: Warning) {
        self.warnings.push(warning)
    }

    /// Take all the errors, leaving an empty vector in its place.
    ///
    pub fn take_errors(&mut self) -> Vec<Error> {
        std::mem::take(&mut self.errors)
    }

    /// Take all the warnings, leaving an empty vector in its place.
    ///
    pub fn take_warnings(&mut self) -> Vec<Warning> {
        std::mem::take(&mut self.warnings)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UnknownType {
    pub location: SrcSpan,
    pub name: EcoString,
}

/// This is used by the unknown record field error to tell if an unknown field
/// is a field appearing in another variant of the same type to provide a better
/// error message explaining why it can't be accessed:
///
/// ```gleam
/// pub type Wibble {
///   Wibble(field: Int)
///   Wobble(thing: String)
/// }
///
/// Wobble("hello").field
/// //             ^^^^^^
/// ```
///
/// Here the error can be extra useful and explain that to access `field` all
/// variants should have that field at the same position and with the same type.
///
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum UnknownField {
    /// The field we're trying to access appears in at least a variant, so it
    /// can be useful to explain why it cannot be accessed and how to fix it
    /// (adding it to all variants/making sure it has the same type/making sure
    /// it's in the same position).
    ///
    AppearsInAVariant,

    /// The field we are trying to access appears in a variant, but we can
    /// infer that the value we are accessing on is never the one that this
    /// value is, so we can give information accordingly.
    AppearsInAnImpossibleVariant,

    /// The field is not in any of the variants, this might truly be a typo and
    /// there's no need to add further explanations.
    ///
    TrulyUnknown,

    /// The type that the user is trying to access has no fields whatsoever,
    /// such as Int or fn(..) -> _
    NoFields,
}

/// A suggestion for an unknown module
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ModuleSuggestion {
    /// A module which which has a similar name, and an
    /// exported value matching the one being accessed
    Importable(EcoString),
    /// A module already imported in the current scope
    Imported(EcoString),
}

impl ModuleSuggestion {
    pub fn suggestion(&self, module: &str) -> String {
        match self {
            ModuleSuggestion::Importable(name) => {
                // Add a little extra information if the names don't match
                let imported_name = self.last_name_component();
                if module == imported_name {
                    format!("Did you mean to import `{name}`?")
                } else {
                    format!("Did you mean to import `{name}` and reference `{imported_name}`?")
                }
            }
            ModuleSuggestion::Imported(name) => format!("Did you mean `{name}`?"),
        }
    }

    pub fn last_name_component(&self) -> &str {
        match self {
            ModuleSuggestion::Imported(name) | ModuleSuggestion::Importable(name) => {
                name.split('/').next_back().unwrap_or(name)
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Error {
    InvalidImport {
        location: SrcSpan,
        importing_module: EcoString,
        imported_module: EcoString,
        kind: InvalidImportKind,
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
        type_with_name_in_scope: bool,
    },

    UnknownType {
        location: SrcSpan,
        name: EcoString,
        hint: UnknownTypeHint,
    },

    UnknownModule {
        location: SrcSpan,
        name: EcoString,
        suggestions: Vec<ModuleSuggestion>,
    },

    UnknownModuleType {
        location: SrcSpan,
        name: EcoString,
        module_name: EcoString,
        type_constructors: Vec<EcoString>,
        value_with_same_name: bool,
    },

    UnknownModuleValue {
        location: SrcSpan,
        name: EcoString,
        module_name: EcoString,
        value_constructors: Vec<EcoString>,
        type_with_same_name: bool,
        context: ModuleValueUsageContext,
    },

    ModuleAliasUsedAsName {
        location: SrcSpan,
        name: EcoString,
    },

    NotFn {
        location: SrcSpan,
        type_: Arc<Type>,
    },

    UnknownRecordField {
        location: SrcSpan,
        type_: Arc<Type>,
        label: EcoString,
        fields: Vec<EcoString>,
        usage: FieldAccessUsage,
        unknown_field: UnknownField,
    },

    IncorrectArity {
        location: SrcSpan,
        expected: usize,
        given: usize,
        labels: Vec<EcoString>,
    },

    UnsafeRecordUpdate {
        location: SrcSpan,
        reason: UnsafeRecordUpdateReason,
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

    /// A case expression is missing its body.
    MissingCaseBody {
        location: SrcSpan,
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

    /// When there's something that is not a function to the left of the `<-`
    /// operator in a use expression:
    ///
    /// For example:
    ///
    /// ```gleam
    /// use <- "wibble"
    /// todo
    /// ```
    NotFnInUse {
        location: SrcSpan,
        type_: Arc<Type>,
    },

    /// When the function to the right hand side of `<-` in a `use` expression
    /// is called with the wrong number of arguments (given already takes into
    /// account the use callback passed to the function).
    ///
    UseFnIncorrectArity {
        location: SrcSpan,
        expected: usize,
        given: usize,
    },

    /// When on the left hand side of `<-` in a `use` expression there is the
    /// wrong number of patterns.
    ///
    /// For example:
    ///
    /// ```gleam
    /// use _, _ <- result.try(res)
    /// todo
    /// ```
    ///
    UseCallbackIncorrectArity {
        call_location: SrcSpan,
        pattern_location: SrcSpan,
        expected: usize,
        given: usize,
    },

    /// When on the right hand side of use there is a function that doesn't take
    /// a callback function as its last argument.
    ///
    /// For example:
    ///
    /// ```gleam
    /// use <- io.println
    /// ```
    ///
    UseFnDoesntTakeCallback {
        location: SrcSpan,
        actual_type: Option<Type>,
    },

    /// When the name assigned to a variable or function doesn't follow the gleam
    /// naming conventions.
    ///
    /// For example:
    ///
    /// ```gleam
    /// let myBadName = 42
    /// ```
    BadName {
        location: SrcSpan,
        kind: Named,
        name: EcoString,
    },

    /// Occurs when all the variant types of a custom type are deprecated
    ///
    /// ```gleam
    /// type Wibble {
    ///     @deprecated("1")
    ///     Wobble1
    ///     @deprecated("1")
    ///     Wobble1
    /// }
    /// ```
    AllVariantsDeprecated {
        location: SrcSpan,
    },

    /// Occers when any varient of a custom type is deprecated while
    /// the custom type itself is deprecated
    DeprecatedVariantOnDeprecatedType {
        location: SrcSpan,
    },

    ErlangFloatUnsafe {
        location: SrcSpan,
    },

    /// When the echo keyword is not followed by an expression to be printed.
    /// The only place where echo is allowed to appear on its own is as a step
    /// of a pipeline, otherwise omitting the expression will result in this
    /// error. For example:
    ///
    /// ```gleam
    /// call(echo, 1, 2)
    /// //   ^^^^ Error!
    /// ```
    ///
    EchoWithNoFollowingExpression {
        location: SrcSpan,
    },
    /// When someone tries concatenating two string values using the `+` operator.
    ///
    /// ```gleam
    /// "aaa" + "bbb"
    /// //    ^ We wont to suggest using `<>` instead!
    /// ```
    StringConcatenationWithAddInt {
        location: SrcSpan,
    },
    /// When someone tries using an int operator on two floats.
    ///
    /// ```gleam
    /// 1 +. 3
    /// //^ We wont to suggest using `+` instead!
    /// ```
    FloatOperatorOnInts {
        operator: BinOp,
        location: SrcSpan,
    },
    /// When someone tries using an int operator on two floats.
    ///
    /// ```gleam
    /// 1.2 + 1.0
    /// //  ^ We wont to suggest using `+.` instead!
    /// ```
    IntOperatorOnFloats {
        operator: BinOp,
        location: SrcSpan,
    },

    DoubleVariableAssignmentInBitArray {
        location: SrcSpan,
    },

    NonUtf8StringAssignmentInBitArray {
        location: SrcSpan,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModuleValueUsageContext {
    UnqualifiedImport,
    ModuleAccess,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum EmptyListCheckKind {
    Empty,
    NonEmpty,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum LiteralCollectionKind {
    List,
    Tuple,
    Record,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InvalidImportKind {
    SrcImportingTest,
    SrcImportingDev,
    DevImportingTest,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Named {
    Type,
    TypeAlias,
    TypeVariable,
    CustomTypeVariant,
    Variable,
    Argument,
    Label,
    Constant,
    Function,
    Discard,
}

impl Named {
    pub fn as_str(self) -> &'static str {
        match self {
            Named::Type => "Type",
            Named::TypeAlias => "Type alias",
            Named::TypeVariable => "Type variable",
            Named::CustomTypeVariant => "Type variant",
            Named::Variable => "Variable",
            Named::Argument => "Argument",
            Named::Label => "Label",
            Named::Constant => "Constant",
            Named::Function => "Function",
            Named::Discard => "Discard",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
/// The origin of a variable. Used to determine how it can be ignored when unused.
pub enum VariableOrigin {
    /// A variable that can be ignored by prefixing with an underscore, `_name`
    Variable(EcoString),
    /// A variable from label shorthand syntax, which can be ignored with an underscore: `label: _`
    LabelShorthand(EcoString),
    /// A variable from an assignment pattern, which can be ignored by removing `as name`,
    AssignmentPattern,
    /// A variable generated by the compiler. This should never need to be ignored
    Generated,
}

impl VariableOrigin {
    pub fn how_to_ignore(&self) -> Option<String> {
        match self {
            VariableOrigin::Variable(name) => {
                Some(format!("You can ignore it with an underscore: `_{name}`."))
            }
            VariableOrigin::LabelShorthand(label) => Some(format!(
                "You can ignore it with an underscore: `{label}: _`."
            )),
            VariableOrigin::AssignmentPattern => Some("You can safely remove it.".to_string()),
            VariableOrigin::Generated => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub enum Warning {
    Todo {
        kind: TodoKind,
        location: SrcSpan,
        type_: Arc<Type>,
    },

    ImplicitlyDiscardedResult {
        location: SrcSpan,
    },

    UnusedLiteral {
        location: SrcSpan,
    },

    UnusedValue {
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
        origin: VariableOrigin,
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

    UnreachableCasePattern {
        location: SrcSpan,
        reason: UnreachablePatternReason,
    },

    /// This happens when someone tries to write a case expression where one of
    /// the subjects is a literal tuple, list or bit array for example:
    ///
    /// ```gleam
    /// case #(wibble, wobble) { ... }
    /// ```
    ///
    /// Matching on a literal collection of elements is redundant since we can
    /// always pass the single items it's made of separated by a comma:
    ///
    /// ```gleam
    /// case wibble, wobble { ... }
    /// ```
    ///
    CaseMatchOnLiteralCollection {
        kind: LiteralCollectionKind,
        location: SrcSpan,
    },

    /// This happens if someone tries to match on some kind of literal value
    /// like an Int, a String, an empty List etc.
    ///
    /// ```gleam
    /// case #() { ... }
    /// ```
    ///
    /// The whole match becomes redundant since one can already tell beforehand
    /// the structure of the value being matched.
    ///
    /// Note: for non-empty literal collection of values we want to provide a
    ///       better error message that suggests to drop the wrapper, for that
    ///       we have the `CaseMatchOnLiteralCollection` variant.
    ///
    CaseMatchOnLiteralValue {
        location: SrcSpan,
    },

    /// This happens when someone defines an external type (with no
    /// constructors) and marks it as opqaue:
    ///
    /// ```gleam
    /// opaque type External
    /// ```
    ///
    /// Since an external type already has no constructors, marking it as
    /// opaque is redundant.
    ///
    OpaqueExternalType {
        location: SrcSpan,
    },

    /// This happens when an internal type is accidentally exposed in the public
    /// API. Since internal types are excluded from documentation, completions
    /// and the package interface, this would lead to poor developer experience.
    ///
    /// ```gleam
    /// @internal type Wibble
    ///
    /// pub fn wibble(thing: Wibble) { todo }
    /// //            ^^^^^^^^^^^^^ There would be no documentation
    /// //                          explaining what `Wibble` is in the
    /// //                          package's doc site.
    /// ```
    InternalTypeLeak {
        location: SrcSpan,
        leaked: Type,
    },

    RedundantAssertAssignment {
        location: SrcSpan,
    },

    AssertAssignmentOnInferredVariant {
        location: SrcSpan,
    },

    /// When a `todo` or `panic` is used as a function instead of providing the
    /// error message with the `as` syntax.
    ///
    /// ```gleam
    /// todo("this won't appear in the error message")
    /// ```
    ///
    TodoOrPanicUsedAsFunction {
        kind: TodoOrPanic,
        location: SrcSpan,
        args_location: Option<SrcSpan>,
        args: usize,
    },

    UnreachableCodeAfterPanic {
        location: SrcSpan,
        panic_position: PanicPosition,
    },

    /// When a function capture is used in a pipe to pipe into the first
    /// argument of a function:
    ///
    /// ```gleam
    /// wibble |> wobble(_, 1)
    ///                  ^ Redundant and can be removed
    /// ```
    ///
    RedundantPipeFunctionCapture {
        location: SrcSpan,
    },

    /// When the `gleam` range specified in the package's `gleam.toml` is too
    /// low and would include a version that's too low to support this feature.
    ///
    /// For example, let's say that a package is saying `gleam = ">=1.1.0"`
    /// but it is using label shorthand syntax: `wibble(label:)`.
    /// That requires a version that is `>=1.4.0`, so the constraint expressed
    /// in the `gleam.toml` is too permissive and if someone were to run this
    /// code with v1.1.0 they would run into compilation errors since the
    /// compiler cannot know of label shorthands!
    ///
    FeatureRequiresHigherGleamVersion {
        location: SrcSpan,
        minimum_required_version: Version,
        wrongfully_allowed_version: Version,
        feature_kind: FeatureKind,
    },

    /// When targeting JavaScript and an `Int` value is specified that lies
    /// outside the range `Number.MIN_SAFE_INTEGER` - `Number.MAX_SAFE_INTEGER`.
    ///
    JavaScriptIntUnsafe {
        location: SrcSpan,
    },

    /// When we are trying to use bool assert on a literal value. For example:
    /// ```gleam
    /// assert True
    ///        ^ The programmer knows this will never panic, so it's useless
    /// ```
    AssertLiteralValue {
        location: SrcSpan,
    },

    /// When a segment has a constant value that is bigger than its size and we
    /// know for certain is going to be truncated.
    ///
    BitArraySegmentTruncatedValue {
        truncation: BitArraySegmentTruncation,
        location: SrcSpan,
    },

    /// In Gleam v1 it is possible to import one module twice using different aliases.
    /// This is deprecated, and likely would be removed in a Gleam v2.
    ModuleImportedTwice {
        name: EcoString,
        first: SrcSpan,
        second: SrcSpan,
    },
}

#[derive(Debug, Eq, Copy, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub enum FeatureKind {
    LabelShorthandSyntax,
    ConstantStringConcatenation,
    ArithmeticInGuards,
    UnannotatedUtf8StringSegment,
    UnannotatedFloatSegment,
    NestedTupleAccess,
    InternalAnnotation,
    AtInJavascriptModules,
    RecordUpdateVariantInference,
    RecordAccessVariantInference,
    LetAssertWithMessage,
    VariantWithDeprecatedAnnotation,
    JavaScriptUnalignedBitArray,
    BoolAssert,
}

impl FeatureKind {
    pub fn required_version(&self) -> Version {
        match self {
            FeatureKind::InternalAnnotation | FeatureKind::NestedTupleAccess => {
                Version::new(1, 1, 0)
            }

            FeatureKind::AtInJavascriptModules => Version::new(1, 2, 0),

            FeatureKind::ArithmeticInGuards => Version::new(1, 3, 0),

            FeatureKind::LabelShorthandSyntax | FeatureKind::ConstantStringConcatenation => {
                Version::new(1, 4, 0)
            }

            FeatureKind::UnannotatedUtf8StringSegment => Version::new(1, 5, 0),

            FeatureKind::RecordUpdateVariantInference
            | FeatureKind::RecordAccessVariantInference => Version::new(1, 6, 0),

            FeatureKind::VariantWithDeprecatedAnnotation | FeatureKind::LetAssertWithMessage => {
                Version::new(1, 7, 0)
            }

            FeatureKind::JavaScriptUnalignedBitArray => Version::new(1, 9, 0),
            FeatureKind::UnannotatedFloatSegment => Version::new(1, 10, 0),

            FeatureKind::BoolAssert => Version::new(1, 11, 0),
        }
    }
}
#[derive(Debug, Eq, PartialEq, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum PanicPosition {
    /// When the unreachable part is a function argument, this means that one
    /// of the previous arguments must be a panic.
    PreviousFunctionArgument,

    /// When the unreachable part is a function call, this means that its last
    /// argument must be a panic.
    LastFunctionArgument,

    /// When the expression to be printed by echo panics.
    EchoExpression,

    /// Any expression that doesn't fall in the previous two categories
    PreviousExpression,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum TodoOrPanic {
    Todo,
    Panic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum UnreachablePatternReason {
    /// The clause is unreachable because a previous pattern
    /// matches the same case.
    DuplicatePattern,
    /// The clause is unreachable because we have inferred the variant
    /// of the custom type that we are matching on, and this matches
    /// against one of the variants we know it isn't.
    ImpossibleVariant,
}

impl Error {
    // Location where the error started
    pub fn start_location(&self) -> u32 {
        match self {
            Error::InvalidImport { location, .. }
            | Error::BitArraySegmentError { location, .. }
            | Error::UnknownVariable { location, .. }
            | Error::UnknownType { location, .. }
            | Error::UnknownModule { location, .. }
            | Error::UnknownModuleType { location, .. }
            | Error::UnknownModuleValue { location, .. }
            | Error::ModuleAliasUsedAsName { location, .. }
            | Error::NotFn { location, .. }
            | Error::UnknownRecordField { location, .. }
            | Error::IncorrectArity { location, .. }
            | Error::UnsafeRecordUpdate { location, .. }
            | Error::UnnecessarySpreadOperator { location, .. }
            | Error::IncorrectTypeArity { location, .. }
            | Error::CouldNotUnify { location, .. }
            | Error::RecursiveType { location, .. }
            | Error::DuplicateName {
                location_a: location,
                ..
            }
            | Error::DuplicateImport { location, .. }
            | Error::DuplicateTypeName { location, .. }
            | Error::DuplicateArgument { location, .. }
            | Error::DuplicateField { location, .. }
            | Error::PrivateTypeLeak { location, .. }
            | Error::UnexpectedLabelledArg { location, .. }
            | Error::PositionalArgumentAfterLabelled { location, .. }
            | Error::IncorrectNumClausePatterns { location, .. }
            | Error::NonLocalClauseGuardVariable { location, .. }
            | Error::ExtraVarInAlternativePattern { location, .. }
            | Error::MissingVarInAlternativePattern { location, .. }
            | Error::DuplicateVarInPattern { location, .. }
            | Error::OutOfBoundsTupleIndex { location, .. }
            | Error::NotATuple { location, .. }
            | Error::NotATupleUnbound { location, .. }
            | Error::RecordAccessUnknownType { location, .. }
            | Error::RecordUpdateInvalidConstructor { location, .. }
            | Error::UnexpectedTypeHole { location, .. }
            | Error::NotExhaustivePatternMatch { location, .. }
            | Error::ArgumentNameAlreadyUsed { location, .. }
            | Error::UnlabelledAfterlabelled { location, .. }
            | Error::RecursiveTypeAlias { location, .. }
            | Error::ExternalMissingAnnotation { location, .. }
            | Error::NoImplementation { location, .. }
            | Error::UnsupportedExpressionTarget { location, .. }
            | Error::InvalidExternalJavascriptModule { location, .. }
            | Error::InvalidExternalJavascriptFunction { location, .. }
            | Error::InexhaustiveCaseExpression { location, .. }
            | Error::MissingCaseBody { location }
            | Error::InexhaustiveLetAssignment { location, .. }
            | Error::UnusedTypeAliasParameter { location, .. }
            | Error::DuplicateTypeParameter { location, .. }
            | Error::UnsupportedPublicFunctionTarget { location, .. }
            | Error::NotFnInUse { location, .. }
            | Error::UseCallbackIncorrectArity {
                pattern_location: location,
                ..
            }
            | Error::UseFnDoesntTakeCallback { location, .. }
            | Error::UseFnIncorrectArity { location, .. }
            | Error::BadName { location, .. }
            | Error::AllVariantsDeprecated { location }
            | Error::EchoWithNoFollowingExpression { location }
            | Error::DeprecatedVariantOnDeprecatedType { location }
            | Error::ErlangFloatUnsafe { location }
            | Error::FloatOperatorOnInts { location, .. }
            | Error::IntOperatorOnFloats { location, .. }
            | Error::StringConcatenationWithAddInt { location }
            | Error::DoubleVariableAssignmentInBitArray { location }
            | Error::NonUtf8StringAssignmentInBitArray { location } => location.start,

            Error::UnknownLabels { unknown, .. } => {
                unknown.iter().map(|(_, s)| s.start).min().unwrap_or(0)
            }
            Error::ReservedModuleName { .. } => 0,
            Error::KeywordInModuleName { .. } => 0,
        }
    }

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
}

impl Warning {
    pub fn into_warning(self, path: Utf8PathBuf, src: EcoString) -> crate::Warning {
        crate::Warning::Type {
            path,
            src,
            warning: self,
        }
    }

    pub(crate) fn location(&self) -> SrcSpan {
        match self {
            Warning::Todo { location, .. }
            | Warning::ImplicitlyDiscardedResult { location, .. }
            | Warning::UnusedLiteral { location, .. }
            | Warning::UnusedValue { location, .. }
            | Warning::NoFieldsRecordUpdate { location, .. }
            | Warning::AllFieldsRecordUpdate { location, .. }
            | Warning::UnusedType { location, .. }
            | Warning::UnusedConstructor { location, .. }
            | Warning::UnusedImportedValue { location, .. }
            | Warning::UnusedImportedModule { location, .. }
            | Warning::UnusedImportedModuleAlias { location, .. }
            | Warning::UnusedPrivateModuleConstant { location, .. }
            | Warning::UnusedPrivateFunction { location, .. }
            | Warning::UnusedVariable { location, .. }
            | Warning::UnnecessaryDoubleIntNegation { location, .. }
            | Warning::UnnecessaryDoubleBoolNegation { location, .. }
            | Warning::InefficientEmptyListCheck { location, .. }
            | Warning::TransitiveDependencyImported { location, .. }
            | Warning::DeprecatedItem { location, .. }
            | Warning::UnreachableCasePattern { location, .. }
            | Warning::CaseMatchOnLiteralCollection { location, .. }
            | Warning::CaseMatchOnLiteralValue { location, .. }
            | Warning::OpaqueExternalType { location, .. }
            | Warning::InternalTypeLeak { location, .. }
            | Warning::RedundantAssertAssignment { location, .. }
            | Warning::AssertAssignmentOnInferredVariant { location, .. }
            | Warning::TodoOrPanicUsedAsFunction { location, .. }
            | Warning::UnreachableCodeAfterPanic { location, .. }
            | Warning::RedundantPipeFunctionCapture { location, .. }
            | Warning::FeatureRequiresHigherGleamVersion { location, .. }
            | Warning::JavaScriptIntUnsafe { location, .. }
            | Warning::AssertLiteralValue { location, .. }
            | Warning::BitArraySegmentTruncatedValue { location, .. }
            | Warning::ModuleImportedTwice {
                second: location, ..
            } => *location,
        }
    }

    pub(crate) fn is_todo(&self) -> bool {
        match self {
            Self::Todo { .. } => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnknownValueConstructorError {
    Variable {
        name: EcoString,
        variables: Vec<EcoString>,
        type_with_name_in_scope: bool,
    },

    Module {
        name: EcoString,
        suggestions: Vec<ModuleSuggestion>,
    },

    ModuleValue {
        name: EcoString,
        module_name: EcoString,
        value_constructors: Vec<EcoString>,
        imported_value_as_type: bool,
    },
}

pub fn convert_get_value_constructor_error(
    e: UnknownValueConstructorError,
    location: SrcSpan,
    module_location: Option<SrcSpan>,
) -> Error {
    match e {
        UnknownValueConstructorError::Variable {
            name,
            variables,
            type_with_name_in_scope,
        } => Error::UnknownVariable {
            location,
            name,
            variables,
            type_with_name_in_scope,
        },

        UnknownValueConstructorError::Module { name, suggestions } => Error::UnknownModule {
            location: module_location.unwrap_or(location),
            name,
            suggestions,
        },

        UnknownValueConstructorError::ModuleValue {
            name,
            module_name,
            value_constructors,
            imported_value_as_type,
        } => Error::UnknownModuleValue {
            location,
            name,
            module_name,
            value_constructors,
            type_with_same_name: imported_value_as_type,
            context: ModuleValueUsageContext::ModuleAccess,
        },
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum UnsafeRecordUpdateReason {
    UnknownVariant {
        constructed_variant: EcoString,
    },
    WrongVariant {
        constructed_variant: EcoString,
        spread_variant: EcoString,
    },
    IncompatibleFieldTypes {
        constructed_variant: Arc<Type>,
        record_variant: Arc<Type>,
        expected_field_type: Arc<Type>,
        record_field_type: Arc<Type>,
        field_name: EcoString,
    },
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
        suggestions: Vec<ModuleSuggestion>,
    },

    ModuleType {
        name: EcoString,
        module_name: EcoString,
        type_constructors: Vec<EcoString>,
        imported_type_as_value: bool,
    },
}

pub fn convert_get_type_constructor_error(
    e: UnknownTypeConstructorError,
    location: &SrcSpan,
    module_location: Option<SrcSpan>,
) -> Error {
    match e {
        UnknownTypeConstructorError::Type { name, hint } => Error::UnknownType {
            location: *location,
            name,
            hint,
        },

        UnknownTypeConstructorError::Module { name, suggestions } => Error::UnknownModule {
            location: module_location.unwrap_or(*location),
            name,
            suggestions,
        },

        UnknownTypeConstructorError::ModuleType {
            name,
            module_name,
            type_constructors,
            imported_type_as_value,
        } => Error::UnknownModuleType {
            location: *location,
            name,
            module_name,
            type_constructors,
            value_with_same_name: imported_type_as_value,
        },
    }
}

#[derive(Debug, Clone)]
pub enum MatchFunTypeError {
    IncorrectArity {
        expected: usize,
        given: usize,
        args: Vec<Arc<Type>>,
        return_type: Arc<Type>,
    },
    NotFn {
        type_: Arc<Type>,
    },
}

pub fn convert_not_fun_error(
    e: MatchFunTypeError,
    fn_location: SrcSpan,
    call_location: SrcSpan,
    call_kind: CallKind,
) -> Error {
    match (call_kind, e) {
        (
            CallKind::Function,
            MatchFunTypeError::IncorrectArity {
                expected, given, ..
            },
        ) => Error::IncorrectArity {
            labels: vec![],
            location: call_location,
            expected,
            given,
        },

        (CallKind::Function, MatchFunTypeError::NotFn { type_ }) => Error::NotFn {
            location: fn_location,
            type_,
        },

        (
            CallKind::Use { call_location, .. },
            MatchFunTypeError::IncorrectArity {
                expected, given, ..
            },
        ) => Error::UseFnIncorrectArity {
            location: call_location,
            expected,
            given,
        },

        (CallKind::Use { call_location, .. }, MatchFunTypeError::NotFn { type_ }) => {
            Error::NotFnInUse {
                location: call_location,
                type_,
            }
        }
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
            situation: Some(UnifyErrorSituation::CaseClauseMismatch {
                clause_location: SrcSpan::default()
            }),
        },
        flip_unify_error(UnifyError::CouldNotUnify {
            expected: crate::type_::float(),
            given: crate::type_::int(),
            situation: Some(UnifyErrorSituation::CaseClauseMismatch {
                clause_location: SrcSpan::default()
            }),
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
            situation: Some(UnifyErrorSituation::CaseClauseMismatch {
                clause_location: SrcSpan::default()
            })
        }),
        unify_enclosed_type(
            crate::type_::int(),
            crate::type_::float(),
            Err(UnifyError::CouldNotUnify {
                expected: crate::type_::string(),
                given: crate::type_::bit_array(),
                situation: Some(UnifyErrorSituation::CaseClauseMismatch {
                    clause_location: SrcSpan::default()
                })
            })
        )
    );
}

pub fn unify_wrong_arity(
    t1: &Arc<Type>,
    arity1: usize,
    t2: &Arc<Type>,
    arity2: usize,
) -> UnifyError {
    UnifyError::CouldNotUnify {
        expected: t1.clone(),
        given: t2.clone(),
        situation: Some(UnifyErrorSituation::FunctionsMismatch {
            reason: FunctionsMismatchReason::Arity {
                expected: arity1,
                given: arity2,
            },
        }),
    }
}

pub fn unify_wrong_arguments(
    expected: &Arc<Type>,
    expected_arg: &Arc<Type>,
    given: &Arc<Type>,
    given_arg: &Arc<Type>,
    position: usize,
) -> UnifyError {
    UnifyError::CouldNotUnify {
        expected: expected.clone(),
        given: given.clone(),
        situation: Some(UnifyErrorSituation::FunctionsMismatch {
            reason: FunctionsMismatchReason::Argument {
                expected: expected_arg.clone(),
                given: given_arg.clone(),
                position,
            },
        }),
    }
}

pub fn unify_wrong_returns(
    expected: &Arc<Type>,
    expected_return: &Arc<Type>,
    given: &Arc<Type>,
    given_return: &Arc<Type>,
) -> UnifyError {
    UnifyError::CouldNotUnify {
        expected: expected.clone(),
        given: given.clone(),
        situation: Some(UnifyErrorSituation::FunctionsMismatch {
            reason: FunctionsMismatchReason::Results {
                expected: expected_return.clone(),
                given: given_return.clone(),
            },
        }),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnifyErrorSituation {
    /// Clauses in a case expression were found to return different types.
    CaseClauseMismatch {
        clause_location: SrcSpan,
    },

    /// A function was found to return a value that did not match its return
    /// annotation.
    ReturnAnnotationMismatch,

    PipeTypeMismatch,

    /// The operands of a binary operator were incorrect.
    Operator(BinOp),

    /// One of the elements of a list was not the same type as the others.
    ListElementMismatch,

    /// The tail of the list is not the same type as the other elements.
    ListTailMismatch,

    /// When two functions cannot be unified.
    FunctionsMismatch {
        reason: FunctionsMismatchReason,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionsMismatchReason {
    Results {
        expected: Arc<Type>,
        given: Arc<Type>,
    },
    Arity {
        expected: usize,
        given: usize,
    },
    Argument {
        expected: Arc<Type>,
        given: Arc<Type>,
        position: usize,
    },
}

impl UnifyErrorSituation {
    pub fn description(&self) -> Option<&'static str> {
        match self {
            Self::CaseClauseMismatch { clause_location: _ } => Some(
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

            Self::ListElementMismatch => Some(
                "All elements of a list must be the same type, but this one doesn't
match the one before it.",
            ),

            Self::ListTailMismatch => Some(
                "All elements in a list must have the same type, but the elements of
this list don't match the type of the elements being prepended to it.",
            ),

            Self::FunctionsMismatch { .. } => None,
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

    pub fn case_clause_mismatch(self, clause_location: SrcSpan) -> Self {
        self.with_unify_error_situation(UnifyErrorSituation::CaseClauseMismatch { clause_location })
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

    pub fn into_use_unify_error(
        self,
        function_location: SrcSpan,
        pattern_location: SrcSpan,
        last_statement_location: SrcSpan,
        body_location: SrcSpan,
    ) -> Error {
        match &self {
            // If the expected value is not a function, that means we're trying
            // to pass something that doesn't take a function as callback to the
            // right hand side of use.
            Self::CouldNotUnify {
                expected,
                given: _,
                situation: _,
            } if !expected.as_ref().is_fun() => Error::UseFnDoesntTakeCallback {
                location: function_location,
                actual_type: Some(expected.as_ref().clone()),
            },

            Self::CouldNotUnify {
                situation: Some(UnifyErrorSituation::FunctionsMismatch { reason }),
                ..
            } => match reason {
                // If both the expected and given values are functions we can be a
                // bit more specific with the error and highlight the reason of the
                // problem instead of having a generic type mismatch error.
                FunctionsMismatchReason::Results {
                    expected: one,
                    given: other,
                } => Error::CouldNotUnify {
                    location: last_statement_location,
                    expected: one.clone(),
                    given: other.clone(),
                    situation: None,
                },

                FunctionsMismatchReason::Arity {
                    expected: one,
                    given: other,
                } => Error::UseCallbackIncorrectArity {
                    call_location: function_location,
                    pattern_location,
                    expected: *one,
                    given: *other,
                },

                // For this one we just fallback to the generic cannot unify error
                // as it is already plenty clear in a `use` expression.
                FunctionsMismatchReason::Argument { .. } => self.into_error(body_location),
            },

            // In all other cases we fallback to the generic cannot unify error.
            _ => self.into_error(body_location),
        }
    }
}

pub fn convert_unify_error(e: UnifyError, location: SrcSpan) -> Error {
    e.into_error(location)
}

pub fn convert_unify_call_error(e: UnifyError, location: SrcSpan, kind: ArgumentKind) -> Error {
    match kind {
        ArgumentKind::UseCallback {
            function_location,
            assignments_location,
            last_statement_location,
        } => e.into_use_unify_error(
            function_location,
            assignments_location,
            last_statement_location,
            location,
        ),
        ArgumentKind::Regular => convert_unify_error(e, location),
    }
}

/// When targeting JavaScript, adds a warning if the given Int value is outside the range of
/// safe integers as defined by Number.MIN_SAFE_INTEGER and Number.MAX_SAFE_INTEGER.
///
pub fn check_javascript_int_safety(int_value: &BigInt, location: SrcSpan, problems: &mut Problems) {
    let js_min_safe_integer = -9007199254740991i64;
    let js_max_safe_integer = 9007199254740991i64;

    if *int_value < js_min_safe_integer.into() || *int_value > js_max_safe_integer.into() {
        problems.warning(Warning::JavaScriptIntUnsafe { location });
    }
}

/// When targeting Erlang, adds an error if the given Float value is outside the range
/// -1.7976931348623157e308 to 1.7976931348623157e308 which is the allowed range for
/// Erlang's floating point numbers
///
pub fn check_erlang_float_safety(
    string_value: &EcoString,
    location: SrcSpan,
    problems: &mut Problems,
) {
    let erl_min_float = -1.7976931348623157e308f64;
    let erl_max_float = 1.7976931348623157e308f64;

    let float_value: f64 = string_value
        .replace("_", "")
        .parse()
        .expect("Unable to parse string to floating point value");

    if float_value < erl_min_float || float_value > erl_max_float {
        problems.error(Error::ErlangFloatUnsafe { location });
    }
}
