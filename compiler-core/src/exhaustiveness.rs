//! An implementation of the algorithm described in:
//!
//! - How to compile pattern matching, Jules Jacobs.
//!   <https://julesjacobs.com/notes/patternmatching/patternmatching.pdf>
//!
//! - Efficient manipulation of binary data using pattern matching,
//!   Per Gustafsson and Konstantinos Sagonas.
//!   <https://user.it.uu.se/~kostis/Papers/JFP_06.pdf>
//!
//! Adapted from Yorick Peterse's implementation at
//! <https://github.com/yorickpeterse/pattern-matching-in-rust>.
//! Thank you Yorick!
//!
//! > This module comment (and all the following doc comments) are a rough
//! > explanation. It's great to set some expectations on what to expect from
//! > the following code and why the data looks the way it does.
//! > If you want a more detailed explanation, the original paper is a lot more
//! > detailed!
//!
//! A case to be compiled looks a bit different from the case expressions we're
//! used to in Gleam: instead of having a variable to match on and a series of
//! branches, a `CaseToCompile` is made up of a series of branches that can each
//! contain multiple pattern checks. With a psedo-Gleam syntax, this is what it
//! would look like:
//!
//! ```text
//! case {
//!   a is Some, b is 1, c is _ -> todo
//!   a is wibble -> todo
//! }
//! ```
//!
//! > You may wonder, why are we writing branches like this? Usually a case
//! > expression matches on a single variable and each branch refers to it. For
//! > example in gleam you'd write:
//! >
//! > ```gleam
//! > case a {
//! >   Some(_) -> todo
//! >   None -> todo
//! > }
//! > ```
//! >
//! > In out representation that would turn into:
//! >
//! > ```text
//! > case {
//! >   a is Some(_) -> todo
//! >   a is None -> todo
//! > }
//! > ```
//! >
//! > This change makes it way easier to compile the pattern matching into a
//! > decision tree, because now we can add multiple checks on different
//! > variables in each branch.
//!
//! Starting from this data structure, we'll be splitting all the branches into
//! a decision tree that can be used to perform exhaustiveness checking and code
//! generation.
//!

mod missing_patterns;
pub mod printer;

use crate::{
    ast::{
        self, AssignName, BitArraySize, Endianness, IntOperator, TypedBitArraySize, TypedClause,
        TypedPattern, TypedPatternBitArraySegment,
    },
    strings::{convert_string_escape_chars, length_utf16, length_utf32},
    type_::{
        Environment, Opaque, Type, TypeValueConstructor, TypeValueConstructorField, TypeVar,
        TypeVariantConstructors, collapse_links, error::UnreachablePatternReason,
        is_prelude_module, string,
    },
};
use ecow::EcoString;
use id_arena::{Arena, Id};
use itertools::Itertools;
use num_bigint::BigInt;
use radix_trie::{Trie, TrieCommon};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
    sync::Arc,
};

/// A single branch composing a `case` expression to be compiled into a decision
/// tree.
///
/// As shown in the module documentation, branches are a bit different from the
/// usual branches we see in Gleam's case expressions. Each branch can perform
/// multiple checks (each on a different variable, which appears in the check
/// itself!):
///
/// ```text
/// a is Some, b is 1 if condition -> todo
/// ─┬───────  ─┬──── ─┬──────────    ─┬──
///  │          │      │               ╰── body: an arbitrary expression
///  │          │      ╰── guard: an additional boolean condition
///  ╰──────────┴── checks: check that a variable matches with a pattern
/// ─┬────────────────────────────────────
///  ╰── branch: one of the branches making up a pattern matching expression
/// ```
///
/// As shown here a branch can also optionally include a guard with a boolean
/// condition and is followed by a body that is to be executed if all the checks
/// match (and the guard evaluates to true).
///
#[derive(Clone, Eq, PartialEq, Debug)]
struct Branch {
    /// Each branch is identified by a numeric index, so we can nicely
    /// report errors once we find something's wrong with a branch.
    ///
    clause_index: usize,

    /// Each alternative pattern in an alternative pattern matching (e.g.
    /// `one | two | three -> todo`) gets turned into its own branch in this
    /// internal representation. So we also keep track of the index of the
    /// alternative this comes from (0 being the first one and so on...)
    ///
    alternative_index: usize,
    checks: Vec<PatternCheck>,
    guard: Option<usize>,
    body: Body,
}

impl Branch {
    fn new(
        clause_index: usize,
        alternative_index: usize,
        checks: Vec<PatternCheck>,
        has_guard: bool,
    ) -> Self {
        Self {
            clause_index,
            alternative_index,
            checks,
            guard: if has_guard { Some(clause_index) } else { None },
            body: Body::new(clause_index),
        }
    }

    /// Removes and returns a `PatternCheck` on the given variable from this
    /// branch.
    ///
    fn pop_check_on_var(&mut self, var: &Variable) -> Option<PatternCheck> {
        let index = self.checks.iter().position(|check| check.var == *var)?;
        Some(self.checks.remove(index))
    }

    fn add_check(&mut self, check: PatternCheck) {
        self.checks.push(check);
    }

    /// To simplify compiling the pattern we can get rid of all catch-all
    /// patterns that are guaranteed to match by turning those into assignments.
    ///
    /// What does this look like in practice?  Let's go over an example.
    /// Let's say we have this case to compile:
    ///
    /// ```gleam
    /// case a {
    ///   Some(1) -> Some(2)
    ///   otherwise -> otherwise
    /// }
    /// ```
    ///
    /// In our internal representation this would become:
    ///
    /// ```text
    /// case {
    ///   a is Some(1) -> Some(2)
    ///   a is otherwise -> otherwise
    ///   ─┬────────────
    ///    ╰── `a` will always match with this "catch all" variable pattern
    /// }
    /// ```
    ///
    /// Focusing on the last branch, we can remove that check that always matches
    /// by keeping track in its body of the correspondence. So it would end up
    /// looking like this:
    ///
    /// ```text
    /// case {
    ///   a is Some(1) -> Some(2)
    ///   ∅ -> {
    ///   ┬
    ///   ╰── This represents the fact that there's no checks left for this branch!
    ///       So we can make another observation: if there's no checks left in a
    ///       branch we know it will always match and we can produce a leaf in the
    ///       decision tree (there's an exception when we have guards, but we'll
    ///       get to it later)!
    ///
    ///     let otherwise = a
    ///     ─┬───────────────
    ///      ╰── And now we can understand what those `bindings` at the start of
    ///          a body are: as we remove variable patterns, we will rewrite those
    ///          as assignments at the top of the body of the corresponding branch.
    ///
    ///     otherwise
    ///   }
    /// }
    /// ```
    ///
    fn move_unconditional_patterns(&mut self, compiler: &mut Compiler<'_>) {
        self.checks.retain_mut(|check| {
            loop {
                match compiler.pattern(check.pattern) {
                    // Variable patterns always match, so we move those to the body
                    // and remove them from the branch's checks.
                    Pattern::Variable { name } => {
                        self.body.assign(name.clone(), check.var.clone());
                        return false;
                    }
                    // A discard pattern always matches, but since the value is not
                    // used we can just remove it without even adding an assignment
                    // to the body!
                    Pattern::Discard => return false,
                    // Assigns are kind of special: they get turned into assignments
                    // (shocking) but then we can't discard the pattern they wrap.
                    // So we replace the assignment pattern with the one it's wrapping
                    // and try again.
                    Pattern::Assign { name, pattern } => {
                        self.body.assign(name.clone(), check.var.clone());
                        check.pattern = *pattern;
                    }
                    // There's a special case of assignments when it comes to string
                    // prefix patterns. We can give a name to a literal prefix like this:
                    // `"0" as digit <> rest`.
                    // We also want to move this special case of an assignment to the
                    // branch body!
                    Pattern::StringPrefix {
                        prefix,
                        prefix_name,
                        rest: _,
                    } => {
                        if let Some(variable) = std::mem::take(prefix_name) {
                            self.body
                                .assign_literal_string(variable.clone(), prefix.clone());
                        }
                        return true;
                    }
                    // There's a special case of assignments when it comes to bit
                    // array patterns. We can give a name to one slice of the array and
                    // bind it to a variable to be used by later steps of the pattern
                    // like this: `<<len, payload:size(len)>>` (here we're binding
                    // two variables! `len` and `payload`).
                    //
                    // This kind of slicing will always match if it's not guarded by
                    // any size test, so if we find a `ReadAction` that is the first
                    // test to perform in a bit array pattern we know it's always
                    // going to match and can be safely moved into the branch's body.
                    Pattern::BitArray { tests } => match tests.front_mut() {
                        Some(BitArrayTest::Match(MatchTest {
                            value: BitArrayMatchedValue::Variable(name),
                            read_action,
                        })) => {
                            let bit_array = check.var.clone();
                            self.body.assign_bit_array_slice(
                                name.clone(),
                                bit_array,
                                read_action.clone(),
                            );
                            let _ = tests.pop_front();
                        }

                        Some(test) => match test {
                            // If we have `_ as a` we treat that as a regular variable
                            // assignment.
                            BitArrayTest::Match(MatchTest {
                                value: BitArrayMatchedValue::Assign { name, value },
                                read_action,
                            }) if value.is_discard() => {
                                *test = BitArrayTest::Match(MatchTest {
                                    value: BitArrayMatchedValue::Variable(name.clone()),
                                    read_action: read_action.clone(),
                                });
                            }

                            // Just like regular assigns, those patterns are unrefutable
                            // and will become assignments in the branch's body.
                            BitArrayTest::Match(MatchTest {
                                value: BitArrayMatchedValue::Assign { name, value },
                                read_action,
                            }) => {
                                self.body
                                    .assign_segment_constant_value(name.clone(), value.as_ref());

                                // We will still need to check the aliased value!
                                *test = BitArrayTest::Match(MatchTest {
                                    value: value.as_ref().clone(),
                                    read_action: read_action.clone(),
                                });
                            }

                            // Discards are removed directly without even binding them
                            // in the branch's body.
                            _ if test.is_discard() => {
                                let _ = tests.pop_front();
                            }

                            // Otherwise there's no unconditional test to pop, we
                            // keep the pattern without changing it.
                            _ => return true,
                        },

                        // If a bit array pattern has no tests then it's always
                        // going to match, no matter what. We just remove it.
                        None => return false,
                    },

                    // All other patterns are not unconditional, so we just keep them.
                    _ => return true,
                }
            }
        });
    }
}

/// The body of a branch. It always starts with a series of variable assignments
/// in the form: `let a = b`. As explained in `move_unconditional_patterns`' doc,
/// each body starts with a series of assignments we keep track of as we're
/// compiling each branch.
///
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Body {
    /// Any variables to bind before running the code.
    ///
    /// The tuples are in the form `(name, value)`, so `(wibble, var)`
    /// corresponds to `let wibble = var`.
    ///
    pub bindings: Vec<(EcoString, BoundValue)>,

    /// The index of the clause in the case expression that should be run.
    ///
    pub clause_index: usize,
}

/// A value that can appear on the right hand side of one of the assignments we
/// find at the top of a body.
///
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum BoundValue {
    /// `let a = variable`
    ///
    Variable(Variable),

    /// `let a = "a literal string"`
    ///
    LiteralString(EcoString),

    /// `let a = 123`
    ///
    LiteralInt(BigInt),

    /// `let a = 12.2`
    ///
    LiteralFloat(EcoString),

    /// `let a = sliceAsInt(bit_array, 0, 16, ...)`
    ///
    BitArraySlice {
        bit_array: Variable,
        read_action: ReadAction,
    },
}

impl Body {
    pub fn new(clause_index: usize) -> Self {
        Self {
            bindings: vec![],
            clause_index,
        }
    }

    /// Adds a new assignment to the body, binding `let variable = value`
    ///
    pub fn assign(&mut self, variable: EcoString, value: Variable) {
        self.bindings.push((variable, BoundValue::Variable(value)));
    }

    fn assign_literal_string(&mut self, variable: EcoString, value: EcoString) {
        self.bindings
            .push((variable, BoundValue::LiteralString(value)));
    }

    fn assign_bit_array_slice(
        &mut self,
        segment_name: EcoString,
        bit_array: Variable,
        value: ReadAction,
    ) {
        self.bindings.push((
            segment_name,
            BoundValue::BitArraySlice {
                bit_array,
                read_action: value,
            },
        ))
    }

    fn assign_segment_constant_value(&mut self, name: EcoString, value: &BitArrayMatchedValue) {
        let value = match value {
            BitArrayMatchedValue::LiteralFloat(value) => BoundValue::LiteralFloat(value.clone()),
            BitArrayMatchedValue::LiteralInt(value) => BoundValue::LiteralInt(value.clone()),
            BitArrayMatchedValue::LiteralString { value, .. } => {
                BoundValue::LiteralString(value.clone())
            }
            BitArrayMatchedValue::Variable(_)
            | BitArrayMatchedValue::Discard(_)
            | BitArrayMatchedValue::Assign { .. } => {
                panic!("aliased non constant value: {value:#?}")
            }
        };

        self.bindings.push((name, value))
    }
}

/// A user defined pattern such as `Some((x, 10))`.
/// This is a bit simpler than the full fledged `TypedPattern` used for code analysis
/// and only focuses on the relevant bits needed to perform exhaustiveness checking
/// and code generation.
///
/// Using this simplified version of a pattern for the case compiler makes it a
/// whole lot simpler and more efficient (patterns will have to be cloned, so
/// we use an arena to allocate those and only store ids to make this operation
/// extra cheap).
///
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Pattern {
    Discard,
    Int {
        value: EcoString,
    },
    Float {
        value: EcoString,
    },
    String {
        value: EcoString,
    },
    StringPrefix {
        prefix: EcoString,
        prefix_name: Option<EcoString>,
        rest: Id<Pattern>,
    },
    Assign {
        name: EcoString,
        pattern: Id<Pattern>,
    },
    Variable {
        name: EcoString,
    },
    Tuple {
        elements: Vec<Id<Pattern>>,
    },
    Variant {
        index: usize,
        name: EcoString,
        module: Option<EcoString>,
        fields: Vec<Id<Pattern>>,
    },
    NonEmptyList {
        first: Id<Pattern>,
        rest: Id<Pattern>,
    },
    EmptyList,
    BitArray {
        tests: VecDeque<BitArrayTest>,
    },
}

impl Pattern {
    /// Each pattern (with a couple exceptions) can be turned into a
    /// simpler `RuntimeCheck`: that is a check that can be performed at runtime
    /// to make sure a `PatternCheck` can succeed on a specific value.
    ///
    fn to_runtime_check_kind(&self) -> Option<RuntimeCheckKind> {
        let kind = match self {
            // These patterns are unconditional: they will always match and be moved
            // out of a branch's checks. So there's no corresponding runtime check
            // we can perform for them.
            Pattern::Discard | Pattern::Variable { .. } | Pattern::Assign { .. } => return None,
            Pattern::Int { value } => RuntimeCheckKind::Int {
                value: value.clone(),
            },
            Pattern::Float { value } => RuntimeCheckKind::Float {
                value: value.clone(),
            },
            Pattern::String { value } => RuntimeCheckKind::String {
                value: value.clone(),
            },
            Pattern::StringPrefix { prefix, .. } => RuntimeCheckKind::StringPrefix {
                prefix: prefix.clone(),
            },
            Pattern::Tuple { elements } => RuntimeCheckKind::Tuple {
                size: elements.len(),
            },
            Pattern::Variant { index, .. } => RuntimeCheckKind::Variant { index: *index },
            Pattern::NonEmptyList { .. } => RuntimeCheckKind::NonEmptyList,
            Pattern::EmptyList => RuntimeCheckKind::EmptyList,
            // Bit arrays have no corresponding kind as they're dealt with in a
            // completely different way.
            Pattern::BitArray { .. } => return None,
        };

        Some(kind)
    }

    fn is_matching_on_unreachable_variant(&self, branch_mode: &BranchMode) -> bool {
        match (self, branch_mode) {
            (
                Self::Variant { index, .. },
                BranchMode::NamedType {
                    inferred_variant: Some(variant),
                    ..
                },
            ) if index != variant => true,
            _ => false,
        }
    }
}

/// A single check making up a branch, checking that a variable matches with a
/// given pattern. For example, the following branch has 2 checks:
///
/// ```text
/// a is Some, b is 1 -> todo
/// ┬    ─┬──
/// │     ╰── This is the pattern being checked
/// ╰── This is the variable being pattern matched on
/// ─┬─────── ─┬────
///  ╰─────────┴── Two `PatternCheck`s
/// ```
///
#[derive(Clone, Eq, PartialEq, Debug)]
struct PatternCheck {
    var: Variable,
    pattern: Id<Pattern>,
}

/// This is one of the checks we can take at runtime to decide how to move
/// forward in the decision tree.
///
/// After performing a successful check on a value we will discover something
/// about its shape: it might be an int, an variant of a custom type, ...
/// Some values (like variants and lists) might hold onto additional data we
/// will have to pattern match on: in order to do that we need a name to refer
/// to those new variables we've discovered after performing a check. That's
/// what `args` is for.
///
/// Let's have a look at an example. Imagine we have a pattern like this one:
/// `a is Wibble(1, _, [])`; after performing a runtime check to make sure `a`
/// is indeed a `Wibble`, we'll need to perform additional checks on it's
/// arguments: that pattern will be replaced by three new ones `a0 is 1`,
/// `a1 is _` and `a2 is []`. Those new variables are the `args`.
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RuntimeCheck {
    Int {
        value: EcoString,
    },
    Float {
        value: EcoString,
    },
    String {
        value: EcoString,
    },
    StringPrefix {
        prefix: EcoString,
        rest: Variable,
    },
    Tuple {
        size: usize,
        elements: Vec<Variable>,
    },
    BitArray {
        test: BitArrayTest,
    },
    Variant {
        match_: VariantMatch,
        index: usize,
        labels: HashMap<usize, EcoString>,
        fields: Vec<Variable>,
    },
    NonEmptyList {
        first: Variable,
        rest: Variable,
    },
    EmptyList,
}

impl RuntimeCheck {
    fn kind(&self) -> Option<RuntimeCheckKind> {
        let kind = match self {
            RuntimeCheck::Int { value } => RuntimeCheckKind::Int {
                value: value.clone(),
            },
            RuntimeCheck::Float { value } => RuntimeCheckKind::Float {
                value: value.clone(),
            },
            RuntimeCheck::String { value } => RuntimeCheckKind::String {
                value: value.clone(),
            },
            RuntimeCheck::StringPrefix { prefix, rest: _ } => RuntimeCheckKind::StringPrefix {
                prefix: prefix.clone(),
            },
            RuntimeCheck::Tuple { size, elements: _ } => RuntimeCheckKind::Tuple { size: *size },
            RuntimeCheck::Variant { index, .. } => RuntimeCheckKind::Variant { index: *index },
            RuntimeCheck::EmptyList => RuntimeCheckKind::EmptyList,
            RuntimeCheck::NonEmptyList { first: _, rest: _ } => RuntimeCheckKind::NonEmptyList,
            RuntimeCheck::BitArray { .. } => return None,
        };
        Some(kind)
    }

    pub(crate) fn is_ignored(&self) -> bool {
        match self {
            RuntimeCheck::Variant {
                match_: VariantMatch::NeverExplicitlyMatchedOn { .. },
                ..
            } => true,
            _ => false,
        }
    }

    /// Returns all the bit array segments referenced in this check.
    /// For each segment it returns its name and the read action used to access
    /// such segment.
    ///
    pub(crate) fn referenced_segment_patterns(&self) -> Vec<(&EcoString, &ReadAction)> {
        match self {
            RuntimeCheck::BitArray { test } => test.referenced_segment_patterns(),
            RuntimeCheck::Int { .. }
            | RuntimeCheck::Float { .. }
            | RuntimeCheck::String { .. }
            | RuntimeCheck::StringPrefix { .. }
            | RuntimeCheck::Tuple { .. }
            | RuntimeCheck::Variant { .. }
            | RuntimeCheck::NonEmptyList { .. }
            | RuntimeCheck::EmptyList => vec![],
        }
    }
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub enum RuntimeCheckKind {
    Int { value: EcoString },
    Float { value: EcoString },
    String { value: EcoString },
    StringPrefix { prefix: EcoString },
    Tuple { size: usize },
    Variant { index: usize },
    EmptyList,
    NonEmptyList,
}

/// All possible variant checks are automatically generated beforehand once we
/// know we are matching on a value with a custom type.
/// Then if the compiled case is explicitly matching on one of those, we update
/// it to store additional information: for example how the variant is used
/// (if qualified or unqualified and if it is aliased).
///
/// This way when we get to code generation we can clump all variants that were
/// never explicitly matched on in a single `else` block without blowing up code
/// size!
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VariantMatch {
    ExplicitlyMatchedOn {
        name: EcoString,
        module: Option<EcoString>,
    },
    NeverExplicitlyMatchedOn {
        name: EcoString,
    },
}

impl VariantMatch {
    pub(crate) fn name(&self) -> EcoString {
        match self {
            VariantMatch::ExplicitlyMatchedOn { name, module: _ } => name.clone(),
            VariantMatch::NeverExplicitlyMatchedOn { name } => name.clone(),
        }
    }

    pub(crate) fn module(&self) -> Option<EcoString> {
        match self {
            VariantMatch::ExplicitlyMatchedOn { name: _, module } => module.clone(),
            VariantMatch::NeverExplicitlyMatchedOn { name: _ } => None,
        }
    }
}

/// A variable that can be matched on in a branch.
///
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Variable {
    pub id: usize,
    pub type_: Arc<Type>,
}

impl Variable {
    fn new(id: usize, type_: Arc<Type>) -> Self {
        Self { id, type_ }
    }

    /// Builds a `PatternCheck` that checks this variable matches the given pattern.
    /// So we can build pattern checks the same way we informally describe them:
    /// ```text
    /// var is pattern
    /// ```
    /// With this builder method would become:
    /// ```rs
    /// var.is(pattern)
    /// ```
    ///
    fn is(&self, pattern: Id<Pattern>) -> PatternCheck {
        PatternCheck {
            var: self.clone(),
            pattern,
        }
    }
}

#[derive(Debug)]
/// Different types need to be handled differently when compiling a case expression
/// into a decision tree. There's some types that have infinite matching patterns
/// (like ints, strings, ...) and thus will always need a fallback option.
///
/// Other types, like custom types, only have a well defined and finite number
/// of patterns that could match: when matching on a `Result` we know that we can
/// only have an `Ok(_)` and an `Error(_)`, anything else would end up being a
/// type error!
///
/// So this enum is used to pick the correct strategy to compile a case that's
/// performing a `PatternCheck` on a variable with a specific type.
///
enum BranchMode {
    /// This covers numbers, functions, variables, strings, and bitarrays.
    Infinite,
    Tuple {
        elements: Vec<Arc<Type>>,
    },
    List {
        inner_type: Arc<Type>,
    },
    NamedType {
        constructors: Vec<TypeValueConstructor>,
        inferred_variant: Option<usize>,
    },
}

impl BranchMode {
    fn is_infinite(&self) -> bool {
        match self {
            BranchMode::Infinite => true,
            BranchMode::Tuple { .. } | BranchMode::List { .. } | BranchMode::NamedType { .. } => {
                false
            }
        }
    }
}

impl Variable {
    fn branch_mode(&self, env: &Environment<'_>) -> BranchMode {
        match collapse_links(self.type_.clone()).as_ref() {
            Type::Fn { .. } | Type::Var { .. } => BranchMode::Infinite,
            Type::Named { module, name, .. }
                if is_prelude_module(module)
                    && (name == "Int"
                        || name == "Float"
                        || name == "BitArray"
                        || name == "String") =>
            {
                BranchMode::Infinite
            }

            Type::Named {
                module,
                name,
                arguments,
                ..
            } if is_prelude_module(module) && name == "List" => BranchMode::List {
                inner_type: arguments.first().expect("list has a type argument").clone(),
            },

            Type::Tuple { elements } => BranchMode::Tuple {
                elements: elements.clone(),
            },

            Type::Named {
                module,
                name,
                arguments,
                inferred_variant,
                ..
            } => {
                let constructors = ConstructorSpecialiser::specialise_constructors(
                    env.get_constructors_for_type(module, name)
                        .expect("Custom type variants must exist"),
                    arguments.as_slice(),
                    &env.current_module,
                    module,
                );

                let inferred_variant = inferred_variant.map(|i| i as usize);
                BranchMode::NamedType {
                    constructors,
                    inferred_variant,
                }
            }
        }
    }
}

/// When compiling a bit array pattern each segment is turned into a series of
/// tests that all need to match in order for the pattern to succeed.
/// Each segment might add some requirements on the total size of a bit array
/// and/or on the value of some of its specific parts.
///
/// Let's look at a simple example to get started:
///
/// ```txt
/// <<0:size(12), 1, rest:bits>>
///   ─┬────────
///    ╰── This first segment requires that the bit array must have at least
///        12 bits and their value must be the Int `0`. So this first
///        segment would be turned into the following series of tests:
///        `Size(>=, 12)` and `Match(0, ReadAction(0, 12, int))`
/// ```
///
/// However, the various sizes and offsets of the different segments might not
/// always be known at compile time and depend on previous sections of the
/// pattern. This is no big deal: as you'll discover in more detail in the
/// `SizeExpression`'s doc we can also represent those variable sizes:
///
/// ```txt
/// <<len, payload:size(len), rest:bits>>
///   ─┬─  ─┬───────────────
///    │    ╰── For this segment to match the bit array must have enough bits
///    │        for the previous segment (8 bits) and for this one (`len` bits),
///    │        so it will turn into the following series of tests:
///    │        `Size(>=, 8 + len)` and `Match(len, ReadAction(8, len, int))`
///    │
///    ╰── This first segment is 8 bits and an int (it's the default Gleam picks
///        if no options are supplied)
/// ```
///
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum BitArrayTest {
    Size(SizeTest),
    Match(MatchTest),

    /// This is a special test to check that the remaining part of a bit array
    /// has a whole number of bytes when using the `:bytes` option.
    ///
    CatchAllIsBytes {
        size_so_far: Offset,
    },

    /// This test is made to ensure a given variable is positive: a segment
    /// pattern where the size is a variable with a negative value will never
    /// match. So we check this to make sure the test will fail.
    ///
    ReadSizeIsNotNegative {
        size: ReadSize,
    },

    /// This test checks that the segment read by the given read action is a
    /// finite Float and not a `NaN` or `Infinity`.
    ///
    /// We need this check as `NaN` and `Infinity` will not match with float
    /// segments (like `<<_:32-float>>`) on the Erlang target and we must
    /// replicate the same behaviours on the JavaScript target as well.
    ///
    SegmentIsFiniteFloat {
        read_action: ReadAction,
    },
}

impl BitArrayTest {
    fn is_discard(&self) -> bool {
        match self {
            BitArrayTest::Match(MatchTest {
                value: BitArrayMatchedValue::Discard(_),
                ..
            }) => true,
            _ => false,
        }
    }

    pub(crate) fn referenced_segment_patterns(&self) -> Vec<(&EcoString, &ReadAction)> {
        match self {
            BitArrayTest::ReadSizeIsNotNegative { size } => {
                let mut references = Vec::new();
                size.referenced_segment_patterns(&mut references);
                references
            }

            BitArrayTest::Size(SizeTest { operator: _, size })
            | BitArrayTest::CatchAllIsBytes { size_so_far: size } => {
                size.referenced_segment_patterns()
            }

            BitArrayTest::SegmentIsFiniteFloat {
                read_action: ReadAction { from, size, .. },
            }
            | BitArrayTest::Match(MatchTest {
                read_action: ReadAction { from, size, .. },
                ..
            }) => {
                let mut references = vec![];
                references.append(&mut from.referenced_segment_patterns());
                size.referenced_segment_patterns(&mut references);

                references
            }
        }
    }
}

/// Test to make sure the bit array has a specific number of bits.
///
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SizeTest {
    pub operator: SizeOperator,
    pub size: Offset,
}

impl SizeTest {
    /// Tells us if this test is guaranteed to succeed given another test that
    /// we know has already succeeded.
    ///
    /// For example, ">= 5" is certainly going to succeed if ">= 6" has already
    /// succeeded.
    ///
    fn succeeds_if_succeeding(&self, succeeding: &SizeTest) -> Confidence {
        match (succeeding.operator, self.operator) {
            (SizeOperator::Equal, SizeOperator::Equal) if succeeding.size == self.size => {
                Confidence::Certain
            }
            (_, SizeOperator::GreaterEqual) => succeeding.size.greater_equal(&self.size),
            _ => Confidence::Uncertain,
        }
    }

    /// Tells us if this test is guaranteed to fail given another test that
    /// we know has already failed.
    ///
    /// For example, ">= 5" is certainly going to fail if ">= 4" has already
    /// failed.
    ///
    fn fails_if_failing(&self, failing: &SizeTest) -> Confidence {
        match (failing.operator, self.operator) {
            (SizeOperator::GreaterEqual, _) => self.size.greater_equal(&failing.size),
            (_, _) if self == failing => Confidence::Certain,
            _ => Confidence::Uncertain,
        }
    }

    /// Tells us if this test is guaranteed to fail given another test that
    /// we know has already succeeded.
    ///
    /// For example, "= 1" is certainly going to fail if "= 2" has already
    /// succeeded.
    ///
    fn fails_if_succeeding(&self, succeeding: &SizeTest) -> Confidence {
        match (succeeding.operator, self.operator) {
            (SizeOperator::GreaterEqual, SizeOperator::Equal) => {
                succeeding.size.greater(&self.size)
            }
            (SizeOperator::Equal, SizeOperator::GreaterEqual) => {
                self.size.greater(&succeeding.size)
            }
            (SizeOperator::Equal, SizeOperator::Equal) => succeeding.size.different(&self.size),
            _ => Confidence::Uncertain,
        }
    }
}

/// Test to make sure the segment read by the specified `read_action` matches
/// a given value.
///
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct MatchTest {
    pub value: BitArrayMatchedValue,
    pub read_action: ReadAction,
}

/// A value that can be matched in a bit array pattern's segment. We do not use
/// a `Pattern` directly since the allowed values are actually a subset of all
/// the possible patterns: it can only contain literal floats, ints, strings,
/// a variable name, and a discard.
///
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum BitArrayMatchedValue {
    LiteralFloat(EcoString),
    LiteralInt(BigInt),
    LiteralString {
        value: EcoString,
        encoding: StringEncoding,
    },
    Variable(EcoString),
    Discard(EcoString),
    Assign {
        name: EcoString,
        value: Box<BitArrayMatchedValue>,
    },
}

impl BitArrayMatchedValue {
    pub(crate) fn is_literal(&self) -> bool {
        match self {
            BitArrayMatchedValue::LiteralFloat(_)
            | BitArrayMatchedValue::LiteralInt(_)
            | BitArrayMatchedValue::LiteralString { .. } => true,
            BitArrayMatchedValue::Variable(..) | BitArrayMatchedValue::Discard(..) => false,
            BitArrayMatchedValue::Assign { value, .. } => value.is_literal(),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum StringEncoding {
    Utf8,
    Utf16,
    Utf32,
}

impl BitArrayMatchedValue {
    pub fn is_discard(&self) -> bool {
        match self {
            BitArrayMatchedValue::Discard(_) => true,
            BitArrayMatchedValue::LiteralFloat(_)
            | BitArrayMatchedValue::LiteralInt(_)
            | BitArrayMatchedValue::LiteralString { .. }
            | BitArrayMatchedValue::Variable(_)
            | BitArrayMatchedValue::Assign { .. } => false,
        }
    }
}

impl BitArrayTest {
    // TODO: for these tests we could also implement a more sophisticated
    // approach for `Match` tests. This is described in the linked paper as
    // read action interference and can help making the tree smaller in some
    // specific cases.
    // This could be an interesting optimisation once we start using the tree
    // for code generation as well.

    /// Tells us if this test is guaranteed to succeed given another test that
    /// we know has already succeeded.
    ///
    /// For example, "size >= 5" is certainly going to succeed if "size >= 6"
    /// has already succeeded.
    ///
    #[must_use]
    fn succeeds_if_succeeding(&self, succeeding: &BitArrayTest) -> Confidence {
        match (succeeding, self) {
            (one, other) if one == other => Confidence::Certain,
            (BitArrayTest::Size(succeeding), BitArrayTest::Size(test)) => {
                test.succeeds_if_succeeding(succeeding)
            }
            // The tests are not comparable, we can't deduce any new information.
            _ => Confidence::Uncertain,
        }
    }

    /// Tells us if this test is guaranteed to fail given another test that
    /// we know has already failed.
    ///
    /// For example, "size >= 5" is certainly going to fail if "size >= 4"
    /// has already failed.
    ///
    #[must_use]
    fn fails_if_failing(&self, failing: &BitArrayTest) -> Confidence {
        match (failing, self) {
            (one, other) if one == other => Confidence::Certain,
            (BitArrayTest::Size(failing), BitArrayTest::Size(test)) => {
                test.fails_if_failing(failing)
            }
            // The tests are not comparable, we can't deduce any new information.
            _ => Confidence::Uncertain,
        }
    }

    /// Tells us if this test is guaranteed to fail given another test that
    /// we know has already succeeded.
    ///
    /// For example, "size = 1" is certainly going to fail if "size = 2" has already
    /// succeeded.
    ///
    #[must_use]
    fn fails_if_succeeding(&self, succeeding: &BitArrayTest) -> Confidence {
        match (succeeding, self) {
            // This is an implementation of Table 6 (a) of the bit array pattern
            // matching paper linked at the top of this module's documentation!
            (BitArrayTest::Size(succeeding), BitArrayTest::Size(test)) => {
                test.fails_if_succeeding(succeeding)
            }
            // The tests are not comparable, we can't deduce any new information.
            _ => Confidence::Uncertain,
        }
    }
}

/// When performing a size test it could require that a size is exactly some
/// specific number of bits (for example if we're matching the last segment
/// of a bit array) or that it has at least some number of bits. For example:
///
/// ```text
/// <<0:size(12), 1:size(8)>>
///   ─┬────────  ─┬───────
///    │           ╰── For this segment to successfully match the bit array must
///    │               have exactly 20 bits: because it will need to read 8 bits
///    │               from bit 13 where the previous one ends: `Size(=, 20)`
///    │
///    ╰── For this segment to successfully match, the bit array must have at
///        least 12 bits: `Size(>, 12)`
/// ```
///
#[derive(Clone, Eq, PartialEq, Debug, Copy)]
pub enum SizeOperator {
    GreaterEqual,
    Equal,
}

/// Represents the action of reading a certain number of bits from a bit array
/// at a given position, returning a value with the given type.
///
/// Notice how the starting position and number of bits to read might not be
/// known at compile time but also include variables! For example here:
///
/// ```txt
// <<len, payload:size(len), rest:bits>>
///  ─┬─  ─┬───────────────
///   │    ╰── Here payload has a variable size, given by the `len` variable
///   │
///   ╰── While this segment has a constant size of 8 bits
/// ```
///
#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct ReadAction {
    /// The offset to start reading the bits from.
    ///
    pub from: Offset,
    /// Number of _bits_ to read.
    ///
    pub size: ReadSize,
    /// The type of the read value.
    ///
    pub type_: ReadType,
    /// Endianness of the read value.
    ///
    pub endianness: Endianness,
    /// Signedness of the read value.
    ///
    pub signed: bool,
}

/// Only a subset of all the possible Gleam types can be used for a pattern
/// segment. We enumerate those out explicitly here.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReadType {
    Float,
    Int,
    String,
    BitArray,
    UtfCodepoint,
}

impl ReadType {
    pub(crate) fn is_int(&self) -> bool {
        match self {
            ReadType::Int => true,
            ReadType::Float | ReadType::String | ReadType::BitArray | ReadType::UtfCodepoint => {
                false
            }
        }
    }

    pub(crate) fn is_float(&self) -> bool {
        match self {
            ReadType::Float => true,
            ReadType::Int | ReadType::String | ReadType::BitArray | ReadType::UtfCodepoint => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Confidence {
    Certain,
    Uncertain,
}

impl Confidence {
    fn is_certain(&self) -> bool {
        match self {
            Confidence::Certain => true,
            Confidence::Uncertain => false,
        }
    }

    fn or_else(&self, fun: impl Fn() -> Confidence) -> Confidence {
        match self {
            Confidence::Certain => Confidence::Certain,
            Confidence::Uncertain => fun(),
        }
    }
}

/// An offset, in bits, into a bit array. An offset contains three parts. For
/// example, in the following pattern:
/// ```txt
/// <<a, b:size(a), c:size(b - 1), payload:size(c)>>
/// ```
///
/// The start of the `payload` segment is the sum of the length of `a` (which we
/// know is 1 byte), plus the size of `b`, which is the value of `a`, plus the
/// size of `c`, which is 1 less than the value of `b`.
///
/// Here, `constant` would be `8`; `variables` would map `a` to `1`, because it
/// is referenced once, in a segment with unit `1`; `calculations` would contain
/// `b - 1`.
///
#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Offset {
    pub constant: BigInt,
    pub variables: im::HashMap<VariableUsage, usize>,
    pub calculations: im::Vector<OffsetCalculation>,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct OffsetCalculation {
    pub left: Offset,
    pub right: Offset,
    pub operator: IntOperator,
}

impl Offset {
    pub fn constant(value: impl Into<BigInt>) -> Self {
        Self {
            constant: value.into(),
            variables: im::HashMap::new(),
            calculations: im::Vector::new(),
        }
    }

    fn from_size(size: &ReadSize) -> Self {
        Self::constant(0).add_size(size)
    }

    fn add_size(mut self, size: &ReadSize) -> Offset {
        match size {
            ReadSize::ConstantBits(value) => Self {
                constant: self.constant + value,
                variables: self.variables,
                calculations: self.calculations,
            },
            ReadSize::VariableBits { variable, unit } => Self {
                constant: self.constant,
                variables: self.variables.alter(
                    |value| match value {
                        Some(value) => Some(value + (*unit as usize)),
                        None => Some(*unit as usize),
                    },
                    variable.as_ref().clone(),
                ),
                calculations: self.calculations,
            },
            ReadSize::RemainingBits | ReadSize::RemainingBytes => self,

            ReadSize::BinaryOperator {
                left,
                right,
                operator,
            } => match operator {
                IntOperator::Add => self.add_size(left).add_size(right),
                _ => {
                    self.calculations.push_back(OffsetCalculation {
                        left: Self::from_size(left),
                        right: Self::from_size(right),
                        operator: *operator,
                    });
                    self
                }
            },
        }
    }

    pub fn add_constant(&self, constant: impl Into<BigInt>) -> Offset {
        Offset {
            constant: self.constant.clone() + constant.into(),
            variables: self.variables.clone(),
            calculations: self.calculations.clone(),
        }
    }

    /// Returns `true` if we can tell for certain this offset is greater than
    /// another one.
    ///
    fn greater(&self, other: &Offset) -> Confidence {
        // We can't easily tell if one calculation is greater than another, so
        // we can only be certain about one being greater if there are no calculations
        if self.constant > other.constant
            && self.calculations.is_empty()
            && superset(&self.variables, &other.variables)
        {
            Confidence::Certain
        } else {
            Confidence::Uncertain
        }
    }

    /// Returns `true` if we can tell for certain this offset is greater or equal
    /// to another one.
    ///
    fn greater_equal(&self, other: &Offset) -> Confidence {
        // Like `greater`, we can only be certain if there are no calculations
        if self == other
            || (self.constant >= other.constant
                && self.calculations.is_empty()
                && superset(&self.variables, &other.variables))
        {
            Confidence::Certain
        } else {
            Confidence::Uncertain
        }
    }

    /// Returns `true` if we can tell for certain this offset is different from
    /// another one.
    ///
    fn different(&self, other: &Offset) -> Confidence {
        self.greater(other).or_else(|| other.greater(self))
    }

    pub(crate) fn is_zero(&self) -> bool {
        self.constant == BigInt::ZERO && self.variables.is_empty() && self.calculations.is_empty()
    }

    /// If this offset has a constant size, returns its size in bits.
    ///
    pub(crate) fn constant_bits(&self) -> Option<BigInt> {
        if self.variables.is_empty() && self.calculations.is_empty() {
            Some(self.constant.clone())
        } else {
            None
        }
    }

    /// If this offset has a constant size that's a whole number of bytes,
    /// returns its size in bytes.
    ///
    pub(crate) fn constant_bytes(&self) -> Option<BigInt> {
        let bits = self.constant_bits()?;
        if bits.clone() % 8 == BigInt::ZERO {
            Some(bits / 8)
        } else {
            None
        }
    }

    fn referenced_segment_patterns(&self) -> Vec<(&EcoString, &ReadAction)> {
        let mut references = Vec::new();

        for variable_usage in self.variables.keys() {
            match variable_usage {
                VariableUsage::PatternSegment(segment_name, read_action) => {
                    references.push((segment_name, read_action));
                }
                VariableUsage::OutsideVariable(..) => {}
            }
        }

        for calculation in self.calculations.iter() {
            references.extend(calculation.left.referenced_segment_patterns());
            references.extend(calculation.right.referenced_segment_patterns());
        }

        references
    }
}

/// The number of bits to read in a read action when reading a segment.
///
#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub enum ReadSize {
    /// Read a constant, compile-time-known number of bits.
    ///
    /// ```txt
    /// <<tag:size(8)>>
    ///   ─┬─────────
    ///    ╰─ Here we know that we have to read exactly 8 bits
    /// ```
    ///
    ConstantBits(BigInt),
    /// Read a variable number of bits.
    ///
    /// ```txt
    /// <<len, payload:size(len)>>
    ///        ─┬───────────────
    ///         ╰─ Here we will know how many bits to read only at runtime since
    ///            the length is a variable
    /// ```
    ///
    VariableBits {
        variable: Box<VariableUsage>,
        unit: u8,
    },

    /// A maths expression calculating the read size from one or more variables.
    ///
    /// ```txt
    /// <<len, payload:size(len * 8)>>
    ///        ─┬───────────────────
    ///         ╰─ Here we have to calculate the length by performing the
    ///            multiplication at runtime
    /// ```
    BinaryOperator {
        left: Box<ReadSize>,
        right: Box<ReadSize>,
        operator: IntOperator,
    },

    /// Read all the remaining bits in the bit array when using a catch all
    /// pattern.
    ///
    /// ```txt
    /// <<_, rest:bits>>
    ///      ─┬───────
    ///       ╰─ We take all the remaining bits from the bit array
    /// ```
    ///
    RemainingBits,
    RemainingBytes,
}

impl ReadSize {
    /// If this is a constant number of bits returns it wrapped in `Some`.
    pub(crate) fn constant_bits(&self) -> Option<BigInt> {
        match self {
            ReadSize::ConstantBits(value) => Some(value.clone()),
            ReadSize::VariableBits { .. }
            | ReadSize::BinaryOperator { .. }
            | ReadSize::RemainingBits
            | ReadSize::RemainingBytes => None,
        }
    }

    /// If this is a constant number of bytes returns it wrapped in `Some`.
    pub(crate) fn constant_bytes(&self) -> Option<BigInt> {
        let bits = self.constant_bits()?;
        if bits.clone() % 8 == BigInt::ZERO {
            Some(bits / 8)
        } else {
            None
        }
    }

    fn referenced_segment_patterns<'a>(
        &'a self,
        references: &mut Vec<(&'a EcoString, &'a ReadAction)>,
    ) {
        match self {
            ReadSize::VariableBits { variable, unit: _ } => match variable.as_ref() {
                VariableUsage::PatternSegment(segment_value, read_action) => {
                    references.push((segment_value, read_action));
                }
                VariableUsage::OutsideVariable(..) => (),
            },

            ReadSize::BinaryOperator { left, right, .. } => {
                left.referenced_segment_patterns(references);
                right.referenced_segment_patterns(references);
            }

            ReadSize::ConstantBits(..) | ReadSize::RemainingBits | ReadSize::RemainingBytes => (),
        };
    }

    fn can_be_negative(&self) -> bool {
        match self {
            ReadSize::ConstantBits(value) => *value < BigInt::ZERO,
            ReadSize::VariableBits { variable, .. } => match variable.as_ref() {
                VariableUsage::PatternSegment(_, read_action) => read_action.signed,
                VariableUsage::OutsideVariable(_) => true,
            },
            ReadSize::BinaryOperator {
                left,
                right,
                operator,
            } => {
                *operator == IntOperator::Subtract
                    || left.can_be_negative()
                    || right.can_be_negative()
            }
            ReadSize::RemainingBits | ReadSize::RemainingBytes => false,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub enum VariableUsage {
    /// A bit array named segment that was brought into scope in the bit array
    /// pattern itself and might be referenced by later segments.
    PatternSegment(EcoString, ReadAction),
    /// A variable defined somewhere else
    OutsideVariable(EcoString),
}

impl VariableUsage {
    pub fn name(&self) -> &EcoString {
        match self {
            VariableUsage::PatternSegment(name, _) | VariableUsage::OutsideVariable(name) => name,
        }
    }
}

/// This is the decision tree that a pattern matching expression gets turned
/// into: it's a tree-like structure where each path to a root node contains a
/// series of checks to perform at runtime to understand if a value matches with
/// a given pattern.
///
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Decision {
    /// This is the final node of the tree, once we get to this one we know we
    /// have a body to run because a given pattern matched.
    ///
    Run { body: Body },

    /// We have to make this decision when we run into a branch that also has a
    /// guard: if it is true we can finally run the body of the branch, stored in
    /// `if_true`.
    /// If it is false we might still have to take other decisions and so we might
    /// have another `DecisionTree` to traverse, stored in `if_false`.
    ///
    Guard {
        guard: usize,
        if_true: Body,
        if_false: Box<Decision>,
    },

    /// When reaching this node we'll have to see if any of the possible checks
    /// in `choices` will succeed on `var`. If it does, we know that's the path
    /// we have to go down to. If none of the checks matches, then we'll have to
    /// go down the `fallback` branch.
    ///
    /// The type system guarantees that all switches will always have at least
    /// one last choice that acts as a fallback to pick if none of the others
    /// matches; no matter the value we're matching on.
    ///
    /// In case we're dealing with exhaustive cases (for example on lists/custom
    /// types) we also keep track of the final check associated with the final
    /// branch: keep in mind, we don't actually have to run that check because
    /// we know that the type system guarantees it will always match; but it
    /// can hold useful informations for type generation so we keep it around.
    /// You can read the doc for `FallbackCheck` to find out a more in depth
    /// explanation and some examples!
    ///
    Switch {
        var: Variable,
        choices: Vec<(RuntimeCheck, Box<Decision>)>,
        fallback: Box<Decision>,
        fallback_check: FallbackCheck,
    },

    /// This is a special node: it represents a missing pattern. If a tree
    /// contains such a node, then we know that the patterns it was compiled
    /// from are not exhaustive and the path leading to this node will describe
    /// what kind of pattern doesn't match!
    ///
    Fail,
}

/// When we have a swith in the decision tree we know there's always going to be
/// at least one choice that comes last and we know is going to match no matter
/// what. This might fall under three different categories.
///
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum FallbackCheck {
    /// This corresponds to the catch all added at the end of a case expression
    /// matching on an infinite type.
    ///
    /// ```txt
    /// case todo {
    ///   1 -> todo
    ///   _ -> todo
    ///   ─┬───────
    //     ╰── when matching on an infinite type there's going to be a catch all
    /// }
    ///
    InfiniteCatchAll,

    /// This happens when we're matching on a variant whose checks are all
    /// explicitly written down:
    ///
    /// ```txt
    /// case todo {
    ///   Ok(_) -> todo
    ///   Error(Nil) -> todo
    ///   ─┬────────────────
    //     ╰── when matching on a custom type (or list!) we'll have to write all
    //         variants down, so there's always going to be a last one.
    /// }
    /// ```
    ///
    /// The type system will make sure that this last check will always match,
    /// no matter what, if none of the other checks matches. We still keep the
    /// corresponding runtime check around because it's useful for code generation!
    ///
    RuntimeCheck { check: RuntimeCheck },

    /// This is a special case for a catch all! It happens when we're matching
    /// on a variant and use a catch all pattern:
    ///
    /// ```txt
    /// case todo {
    ///   Ok(_) -> todo
    ///   _ -> todo
    ///   ─┬───────
    ///    ╰── here we know we're just skipping over the `Error` variant with
    ///       this catch all.
    /// }
    /// ```
    ///
    /// We know exactly which variants we're skipping, so we keep track of
    /// those skipped checks (they will end up being useful for reporting
    /// missing patterns!)
    ///
    CatchAll { ignored_checks: Vec<RuntimeCheck> },
}

impl Decision {
    pub fn run(body: Body) -> Self {
        Decision::Run { body }
    }

    pub fn guard(guard: usize, if_true: Body, if_false: Self) -> Self {
        Decision::Guard {
            guard,
            if_true,
            if_false: Box::new(if_false),
        }
    }
}

/// The `case` compiler itself (shocking, I know).
///
#[derive(Debug)]
struct Compiler<'a> {
    environment: &'a Environment<'a>,
    patterns: Arena<Pattern>,
    variable_id: usize,
    diagnostics: Diagnostics,
}

/// The result of compiling a pattern match expression.
///
pub struct CompileCaseResult {
    pub compiled_case: CompiledCase,
    pub diagnostics: Diagnostics,
}

impl CompileCaseResult {
    pub fn is_reachable(&self, clause: usize, pattern_index: usize) -> Reachability {
        if self
            .diagnostics
            .reachable
            .contains(&(clause, pattern_index))
        {
            Reachability::Reachable
        } else if self
            .diagnostics
            .match_impossible_variants
            .contains(&(clause, pattern_index))
        {
            Reachability::Unreachable(UnreachablePatternReason::ImpossibleVariant)
        } else {
            Reachability::Unreachable(UnreachablePatternReason::DuplicatePattern)
        }
    }

    pub fn missing_patterns(&self, environment: &Environment<'_>) -> Vec<EcoString> {
        missing_patterns::missing_patterns(self, environment)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CompiledCase {
    pub tree: Decision,
    pub subject_variables: Vec<Variable>,
}

impl CompiledCase {
    pub fn failure() -> Self {
        Self {
            tree: Decision::Fail,
            subject_variables: vec![],
        }
    }
}

/// Whether a pattern is reachable, or why it is unreachable.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reachability {
    Reachable,
    Unreachable(UnreachablePatternReason),
}

/// A type for storing diagnostics produced by the decision tree compiler.
///
#[derive(Debug)]
pub struct Diagnostics {
    /// A flag indicating the match is missing one or more pattern.
    pub missing: bool,

    /// The patterns that are reachable. If a pattern isn't in this list it
    /// means it is redundant.
    /// Each entry is a pair of indices: The index of the clause the pattern
    /// belongs to, followed by the index of the pattern itself within the
    /// clause. For example, in this case expression:
    /// ```gleam
    /// case x {
    ///   1 -> todo
    ///   2 | 3 -> todo
    ///   _ -> todo
    /// }
    /// ```
    ///
    /// The `3` pattern would be `(1, 1)`, because it is the second pattern of
    /// the second clause, and both are zero-indexed.
    ///
    pub reachable: HashSet<(usize, usize)>,

    /// Patterns which match on variants of a type which the compiler
    /// can tell will never be present, due to variant inference.
    ///
    /// See `reachable` for an explanation of its structure.
    ///
    pub match_impossible_variants: HashSet<(usize, usize)>,
}

impl<'a> Compiler<'a> {
    fn new(environment: &'a Environment<'a>, variable_id: usize, patterns: Arena<Pattern>) -> Self {
        Self {
            environment,
            patterns,
            variable_id,
            diagnostics: Diagnostics {
                missing: false,
                reachable: HashSet::new(),
                match_impossible_variants: HashSet::new(),
            },
        }
    }

    fn pattern(&mut self, pattern_id: Id<Pattern>) -> &mut Pattern {
        self.patterns
            .get_mut(pattern_id)
            .expect("unknown pattern id")
    }

    /// Returns a new fresh variable (i.e. guaranteed to have a unique `variable_id`)
    /// with the given type.
    ///
    fn fresh_variable(&mut self, type_: Arc<Type>) -> Variable {
        let var = Variable::new(self.variable_id, type_);
        self.variable_id += 1;
        var
    }

    fn mark_as_reached(&mut self, branch: &Branch) {
        let _ = self
            .diagnostics
            .reachable
            .insert((branch.clause_index, branch.alternative_index));
    }

    fn mark_as_matching_impossible_variant(&mut self, branch: &Branch) {
        let _ = self
            .diagnostics
            .reachable
            .remove(&(branch.clause_index, branch.alternative_index));
        let _ = self
            .diagnostics
            .match_impossible_variants
            .insert((branch.clause_index, branch.alternative_index));
    }

    fn compile(&mut self, mut branches: VecDeque<Branch>) -> Decision {
        branches
            .iter_mut()
            .for_each(|branch| branch.move_unconditional_patterns(self));

        let Some(first_branch) = branches.front() else {
            // If there's no branches, that means we have a pattern that is not
            // exhaustive as there's nothing that could match!
            self.diagnostics.missing = true;
            return Decision::Fail;
        };

        self.mark_as_reached(first_branch);

        // In order to compile the branches, we need to pick a `PatternCheck` from
        // the first branch, and use the variable it's pattern matching on to create
        // a new node in the tree. All the branches will be split into different
        // possible paths of this tree.
        match find_pivot_check(first_branch, &branches) {
            Some(PatternCheck { var, .. }) => self.split_and_compile_with_pivot_var(var, branches),

            // If the branch has no remaining checks, it means that we've moved all
            // its variable patterns as assignments into the body and there's no
            // additional checks remaining. So the only thing left that could result
            // in the match failing is the additional guard.
            None => match first_branch.guard {
                // If there's no guard we're in the following situation:
                // `∅ -> body`. It means that this branch will always match no
                // matter what, all the remaining branches are just discarded and
                // we can produce a terminating node to run the body
                // unconditionally.
                None => Decision::run(first_branch.body.clone()),
                // If we have a guard we're in this scenario:
                // `∅ if condition -> body`. We can produce a `Guard` node:
                // if the condition evaluates to `True` we can run its body.
                // Otherwise, we'll have to keep looking at the remaining branches
                // to know what to do if this branch doesn't match.
                Some(guard) => {
                    let if_true = first_branch.body.clone();
                    // All the remaining branches will be compiled and end up
                    // in the path of the tree to choose if the guard is false.
                    let _ = branches.pop_front();
                    let if_false = self.compile(branches);
                    Decision::guard(guard, if_true, if_false)
                }
            },
        }
    }

    fn split_and_compile_with_pivot_var(
        &mut self,
        pivot_var: Variable,
        branches: VecDeque<Branch>,
    ) -> Decision {
        // We first try and come up with a list of all the runtime checks we might
        // have to perform on the variable at runtime. In most cases it's a limited
        // number of checks that we know before hand (for example, when matching
        // on a list, or on a custom type).
        let branch_mode = pivot_var.branch_mode(self.environment);
        let known_checks = match &branch_mode {
            // If the type being matched on is infinite there's no known runtime
            // check we could come up with in advance. So we'll build them as
            // we go.
            BranchMode::Infinite => vec![],

            // If the type is a tuple there's only one runtime check we could
            // perform that actually makes sense.
            BranchMode::Tuple { elements } => vec![self.is_tuple_check(elements)],

            // If the type being matched on is a list we know the resulting
            // decision tree node is only ever going to have two different paths:
            // one to follow if the list is empty, and one to follow if it's not.
            BranchMode::List { inner_type } => {
                vec![
                    RuntimeCheck::EmptyList,
                    self.is_list_check(inner_type.clone()),
                ]
            }

            // If we know that a specific variant is inferred we will require just
            // that one and not all the other ones we know for sure are not going to
            // be there.
            BranchMode::NamedType {
                constructors,
                inferred_variant: Some(index),
            } => {
                let constructor = constructors
                    .get(*index)
                    .expect("wrong index for inferred variant");
                vec![self.is_variant_check(*index, constructor)]
            }

            // Otherwise we know we'll need a check for each of its possible variants.
            BranchMode::NamedType { constructors, .. } => constructors
                .iter()
                .enumerate()
                .map(|(index, constructor)| self.is_variant_check(index, constructor))
                .collect_vec(),
        };

        // We then split all the branches using these checks and compile the
        // choices they've been split up into.
        let mut splitter = BranchSplitter::from_checks(known_checks);
        self.split_branches(&mut splitter, branches, pivot_var.clone(), &branch_mode);
        if branch_mode.is_infinite() {
            // If the branching is infinite, that means we always need to also have
            // a fallback (imagine you're pattern matching on an `Int` and put no
            // `_` at the end of the case expression).
            self.splitter_to_switch(pivot_var, splitter)
        } else if splitter.choices.is_empty() {
            // If the branching doesn't need any fallback but we ended up with no
            // checks it means we're trying to pattern match on an external type
            // but haven't provided a catch-all case.
            // That's never going to match, so we produce a failure node.
            self.diagnostics.missing = true;
            Decision::Fail
        } else {
            // Otherwise we know that one of the possible runtime checks is always
            // going to succeed and there's no need to also have a fallback branch.
            self.splitter_to_exhaustive_switch(pivot_var, splitter)
        }
    }

    fn split_branches(
        &mut self,
        splitter: &mut BranchSplitter,
        branches: VecDeque<Branch>,
        pivot_var: Variable,
        branch_mode: &BranchMode,
    ) {
        for mut branch in branches {
            let Some(pattern_check) = branch.pop_check_on_var(&pivot_var) else {
                // If the branch doesn't perform any check on the pivot variable, it means
                // it could still match no matter what shape `pivot_var` has. So we must
                // add it as a fallback branch, that is a branch that is still relevant
                // for all possible paths in the decision tree.
                splitter.add_fallback_branch(branch);
                continue;
            };

            let checked_pattern = self.pattern(pattern_check.pattern);
            if checked_pattern.is_matching_on_unreachable_variant(branch_mode) {
                self.mark_as_matching_impossible_variant(&branch);
                continue;
            }

            splitter.add_checked_branch(pattern_check, branch, branch_mode, self);
        }
    }

    /// Compiles the branches in a splitter down to a switch matching on a type
    /// with infinite variants (like ints, floats and strings). With a fallaback
    /// branch.
    ///
    fn splitter_to_switch(&mut self, var: Variable, splitter: BranchSplitter) -> Decision {
        let choices = self.compile_all_choices(splitter.choices);
        let last_choice = self.compile(splitter.fallback);
        Decision::Switch {
            var,
            choices,
            fallback: Box::new(last_choice),
            fallback_check: FallbackCheck::InfiniteCatchAll,
        }
    }

    /// Compiles the branches in a splitter down to a switch matching on a type
    /// with a finite known number of variants.
    ///
    fn splitter_to_exhaustive_switch(
        &mut self,
        var: Variable,
        splitter: BranchSplitter,
    ) -> Decision {
        let mut choices = splitter.choices;
        let (choices, fallback, fallback_check) =
            if choices.iter().any(|(check, _)| check.is_ignored()) {
                // If there's any check that is ignored and not explicitly being
                // matched on then we want to ignore all of those and clump them
                // into a single "fallback" branch to avoid bloating the generated
                // tree.
                let mut ignored_checks = vec![];
                let mut remaining_choices = vec![];
                for choice in choices.into_iter() {
                    if choice.0.is_ignored() {
                        ignored_checks.push(choice.0)
                    } else {
                        remaining_choices.push(choice)
                    }
                }

                let fallback_check = FallbackCheck::CatchAll { ignored_checks };
                (remaining_choices, splitter.fallback, fallback_check)
            } else {
                // Otherwise we just use the last check as the final one that
                // can be outright skipped.
                let (last_check, last_choice) = choices.pop().expect("at least one choice");
                let fallback_check = FallbackCheck::RuntimeCheck { check: last_check };
                (choices, last_choice, fallback_check)
            };

        let choices = self.compile_all_choices(choices);
        let fallback = Box::new(self.compile(fallback));
        Decision::Switch {
            var,
            choices,
            fallback,
            fallback_check,
        }
    }

    fn compile_all_choices(
        &mut self,
        choices: Vec<(RuntimeCheck, VecDeque<Branch>)>,
    ) -> Vec<(RuntimeCheck, Box<Decision>)> {
        choices
            .into_iter()
            .map(|(check, branches)| (check, Box::new(self.compile(branches))))
            .collect_vec()
    }

    /// Turns a `RuntimeCheckKind` into a new `RuntimeCheck` by coming up with
    /// the needed new fresh variables.
    /// All the type information needed to create these variables is in the
    /// `branch_mode` arg.
    ///
    fn fresh_runtime_check(
        &mut self,
        kind: RuntimeCheckKind,
        branch_mode: &BranchMode,
    ) -> RuntimeCheck {
        match (kind, branch_mode) {
            (RuntimeCheckKind::Int { value }, _) => RuntimeCheck::Int {
                value: value.clone(),
            },
            (RuntimeCheckKind::Float { value }, _) => RuntimeCheck::Float {
                value: value.clone(),
            },
            (RuntimeCheckKind::String { value }, _) => RuntimeCheck::String {
                value: value.clone(),
            },
            (RuntimeCheckKind::StringPrefix { prefix }, _) => RuntimeCheck::StringPrefix {
                prefix: prefix.clone(),
                rest: self.fresh_variable(string()),
            },
            (RuntimeCheckKind::Tuple { .. }, BranchMode::Tuple { elements }) => {
                self.is_tuple_check(elements)
            }
            (RuntimeCheckKind::Variant { index }, BranchMode::NamedType { constructors, .. }) => {
                self.is_variant_check(
                    index,
                    constructors.get(index).expect("unknown variant index"),
                )
            }
            (RuntimeCheckKind::EmptyList, _) => RuntimeCheck::EmptyList,
            (RuntimeCheckKind::NonEmptyList, BranchMode::List { inner_type }) => {
                self.is_list_check(inner_type.clone())
            }
            (_, _) => unreachable!("type checking should make this impossible"),
        }
    }

    /// Comes up with new pattern cecks that have to match in case a given
    /// runtime check succeeds for the given pattern.
    ///
    /// Let's make an example: when we have a pattern - say `a is Wibble(1, [])` -
    /// we come up with a runtime check to perform on it. For our example the
    /// runtime check is to make sure that `a` is indeed a `Wibble` variant.
    /// However, after successfully performing that check we're left with much to
    /// do! We know that `a` is `Wibble` but now we'll have to make sure that its
    /// inner arguments also match the given patterns. So the new additional checks
    /// we have to add are `a0 is 1, a1 is []` (where `a0` and `a1` are the fresh
    /// variable names we use to refer to the constructor's arguments).
    ///
    fn new_checks(
        &mut self,
        for_pattern: &Pattern,
        after_succeding_check: &RuntimeCheck,
    ) -> Vec<PatternCheck> {
        match (for_pattern, after_succeding_check) {
            // These patterns never result in adding new checks. After a runtime
            // check matches on them there's nothing else to discover.
            (
                Pattern::Discard
                | Pattern::Assign { .. }
                | Pattern::Variable { .. }
                | Pattern::Int { .. }
                | Pattern::Float { .. }
                | Pattern::BitArray { .. }
                | Pattern::EmptyList,
                _,
            )
            | (Pattern::String { .. }, RuntimeCheck::String { .. }) => vec![],

            // After making sure a value is not an empty list we'll have to perform
            // additional checks on its first item and on the tail.
            (
                Pattern::NonEmptyList {
                    first: first_pattern,
                    rest: rest_pattern,
                },
                RuntimeCheck::NonEmptyList {
                    first: first_variable,
                    rest: rest_variable,
                },
            ) => vec![
                first_variable.is(*first_pattern),
                rest_variable.is(*rest_pattern),
            ],

            // After making sure a value is a specific variant we'll have to check each
            // of its arguments respects the given patterns (as shown in the doc example for
            // this function!)
            (
                Pattern::Variant {
                    fields: patterns, ..
                },
                RuntimeCheck::Variant {
                    fields: variables, ..
                },
            ) => (variables.iter().zip(patterns))
                .map(|(field, pattern)| field.is(*pattern))
                .collect_vec(),

            // Tuples are exactly the same as variants: after making sure we're dealing with
            // a tuple, we will have to check that each of its elements matches the given
            // pattern: `a is #(1, _)` will result in the following checks
            // `a0 is 1, a1 is _` (where `a0` and `a1` are fresh variable names we use to
            // refer to each of the tuple's elements).
            (
                Pattern::Tuple { elements: patterns },
                RuntimeCheck::Tuple {
                    elements: variables,
                    ..
                },
            ) => (variables.iter().zip(patterns))
                .map(|(element, pattern)| element.is(*pattern))
                .collect_vec(),

            // Strings are quite fun: if we've checked at runtime a string starts with a given
            // prefix and we want to check that it's some overlapping literal value we'll still
            // have some amount of work to perform.
            //
            // Let's have a look at an example: the pattern we care about is `a is "wibble"`
            // and we've just successfully ran the runtime check for `a is "wib" <> rest`.
            // So we know the string already starts with `"wib"` what we have to check now
            // is that the remaining part `rest` is `"ble"`.
            (Pattern::String { value }, RuntimeCheck::StringPrefix { prefix, rest, .. }) => {
                let remaining = value.strip_prefix(prefix.as_str()).unwrap_or(value);
                vec![rest.is(self.string_pattern(remaining))]
            }

            // String prefixes are similar to strings, but a bit more involved. Let's say we're
            // checking the pattern:
            //
            // ```text
            // "wibblest" <> rest1
            // ─┬────────
            //  ╰── We will refer to this as `prefix1`
            // ```
            //
            // And we know that the following overlapping runtime check has already succeeded:
            //
            // ```text
            // "wibble" <> rest0
            // ─┬──────
            //  ╰── We will refer to this as `prefix0`
            // ```
            //
            // We're lucky because we now know quite a bit about the shape of the string. Since
            // we know it already starts with `"wibble"` we can just check that the remaining
            // part after that starts with the missing part of the prefix:
            // `prefix0 is "st" <> rest1`.
            (
                Pattern::StringPrefix {
                    prefix: prefix1,
                    prefix_name: _,
                    rest: rest1,
                },
                RuntimeCheck::StringPrefix {
                    prefix: prefix0,
                    rest: rest0,
                },
            ) => {
                let remaining = prefix1.strip_prefix(prefix0.as_str()).unwrap_or(prefix1);
                // If the prefixes are exactly the same then the only remaining check
                // is for the two remaining bits to be the same.
                if remaining.is_empty() {
                    vec![rest0.is(*rest1)]
                } else {
                    vec![rest0.is(self.string_prefix_pattern(remaining, *rest1))]
                }
            }

            (_, _) => unreachable!("invalid pattern overlapping"),
        }
    }

    /// Builds an `IsVariant` runtime check, coming up with new fresh variable names
    /// for its arguments.
    ///
    fn is_variant_check(
        &mut self,
        index: usize,
        constructor: &TypeValueConstructor,
    ) -> RuntimeCheck {
        RuntimeCheck::Variant {
            index,
            match_: VariantMatch::NeverExplicitlyMatchedOn {
                name: constructor.name.clone(),
            },
            labels: constructor
                .parameters
                .iter()
                .enumerate()
                .filter_map(|(i, parameter)| Some((i, parameter.label.clone()?)))
                .collect(),
            fields: constructor
                .parameters
                .iter()
                .map(|parameter| parameter.type_.clone())
                .map(|type_| self.fresh_variable(type_))
                .collect_vec(),
        }
    }

    /// Builds an `IsNonEmptyList` runtime check, coming up with fresh variable
    /// names for its arguments.
    ///
    fn is_list_check(&mut self, inner_type: Arc<Type>) -> RuntimeCheck {
        RuntimeCheck::NonEmptyList {
            first: self.fresh_variable(inner_type.clone()),
            rest: self.fresh_variable(Arc::new(Type::list(inner_type))),
        }
    }

    /// Builds an `IsTuple` runtime check, coming up with fresh variable
    /// names for its arguments.
    ///
    fn is_tuple_check(&mut self, elements: &[Arc<Type>]) -> RuntimeCheck {
        RuntimeCheck::Tuple {
            size: elements.len(),
            elements: elements
                .iter()
                .map(|type_| self.fresh_variable(type_.clone()))
                .collect_vec(),
        }
    }

    /// Allocates a new `StringPattern` with the given value.
    ///
    fn string_pattern(&mut self, value: &str) -> Id<Pattern> {
        self.patterns.alloc(Pattern::String {
            value: EcoString::from(value),
        })
    }

    fn bit_array_pattern(&mut self, tests: VecDeque<BitArrayTest>) -> Id<Pattern> {
        self.patterns.alloc(Pattern::BitArray { tests })
    }

    /// Allocates a new `StringPrefix` pattern with the given prefix and pattern
    /// for the rest of the string.
    ///
    fn string_prefix_pattern(&mut self, prefix: &str, rest: Id<Pattern>) -> Id<Pattern> {
        self.patterns.alloc(Pattern::StringPrefix {
            prefix: EcoString::from(prefix),
            prefix_name: None,
            rest,
        })
    }
}

/// Returns a pattern check from `first_branch` to be used as a pivot to split all
/// the `branches`.
///
fn find_pivot_check(first_branch: &Branch, branches: &VecDeque<Branch>) -> Option<PatternCheck> {
    // To try and minimise code duplication, we use the following heuristic: we
    // choose the check matching on the variable that is referenced the most
    // across all checks in all branches.
    let mut var_references = HashMap::new();
    for branch in branches {
        for check in &branch.checks {
            let _ = var_references
                .entry(check.var.id)
                .and_modify(|references| *references += 1)
                .or_insert(0);
        }
    }

    first_branch
        .checks
        .iter()
        .max_by_key(|check| var_references.get(&check.var.id).cloned().unwrap_or(0))
        .cloned()
}

/// A handy data structure we use to split branches in different possible paths
/// based on a check.
///
struct BranchSplitter {
    pub choices: Vec<(RuntimeCheck, VecDeque<Branch>)>,
    pub fallback: VecDeque<Branch>,

    /// This is used to allow quickly looking up a choice in the `choices`
    /// vector, without loosing track of the checks' order.
    indices: HashMap<RuntimeCheckKind, usize>,

    /// This is used to store the indices of just the prefix checks as they have
    /// different rules from all the other `RuntimeCheckKinds` whose indices are
    /// instead stored in the `indices` field.
    ///
    /// We discuss this in more detail in the `index_of_overlapping_runtime_check`
    /// function!
    prefix_indices: Trie<String, usize>,
}

impl BranchSplitter {
    /// Creates a new splitter with the given starting checks.
    ///
    fn from_checks(checks: Vec<RuntimeCheck>) -> Self {
        let mut choices = Vec::with_capacity(checks.len());
        let mut indices = HashMap::new();

        for (index, runtime_check) in checks.into_iter().enumerate() {
            let Some(kind) = runtime_check.kind() else {
                continue;
            };
            let _ = indices.insert(kind, index);
            choices.push((runtime_check, VecDeque::new()));
        }

        Self {
            fallback: VecDeque::new(),
            choices,
            indices,
            prefix_indices: Trie::new(),
        }
    }

    /// Add a fallback branch: this is a branch that is relevant to all possible
    /// paths as it could still run, no matter the result of any of the `Check`s
    /// we've stored!
    ///
    fn add_fallback_branch(&mut self, branch: Branch) {
        self.choices
            .iter_mut()
            .for_each(|(_, branches)| branches.push_back(branch.clone()));
        self.fallback.push_back(branch);
    }

    /// Given a branch and the pattern its using to check on the pivot variable,
    /// adds it to the paths where it's relevant, that is where we know from
    /// previous checks that this pattern has a chance of matching.
    ///
    fn add_checked_branch(
        &mut self,
        pattern_check: PatternCheck,
        mut branch: Branch,
        branch_mode: &BranchMode,
        compiler: &mut Compiler<'_>,
    ) {
        let pattern = compiler.pattern(pattern_check.pattern).clone();

        // Bit array patterns are split in a different way that requires special
        // handling. Instead of reasoning on overlapping checks what we do it we
        // always split the decision tree in two distinct paths based on one of
        // the bit array pattern's tests.
        if let Pattern::BitArray { tests } = pattern {
            self.add_checked_bit_array_branch(pattern_check, tests, branch, compiler);
            return;
        };

        let kind = pattern
            .to_runtime_check_kind()
            .expect("no unconditional patterns left");

        let indices_of_overlapping_checks = self.indices_of_overlapping_checks(&kind);
        if indices_of_overlapping_checks.is_empty() {
            // This is a new choice we haven't yet discovered as it is not overlapping
            // with any of the existing ones. So we add it as a possible new path
            // we might have to go down to in the decision tree.
            self.save_index_of_new_choice(kind.clone());

            let check = compiler.fresh_runtime_check(kind, branch_mode);
            for new_check in compiler.new_checks(&pattern, &check) {
                branch.add_check(new_check);
            }
            let mut branches = self.fallback.clone();
            branches.push_back(branch);

            self.choices.push((check, branches));
        } else {
            // Otherwise, we know that the check for this branch overlaps with
            // (possibly more than one) existing checks and so is relevant only
            // as part of those existing paths.
            // We'll add the branch with its newly discovered checks only to those
            // paths.
            for index in indices_of_overlapping_checks.iter() {
                let (overlapping_check, branches) = self
                    .choices
                    .get_mut(*index)
                    .expect("check to already be a choice");

                let mut branch = branch.clone();
                for new_check in compiler.new_checks(&pattern, overlapping_check) {
                    branch.add_check(new_check);
                }
                branches.push_back(branch);
            }
        }

        // Then we have to update all variant checks with any new name/module
        // we might have discovered from the pattern to make sure we're using
        // the correct qualification to refer to each constructor.
        if let Pattern::Variant { name, module, .. } = pattern {
            for index in indices_of_overlapping_checks {
                let (check, _) = self
                    .choices
                    .get_mut(index)
                    .expect("check to already be a choice");
                if let RuntimeCheck::Variant { match_, .. } = check {
                    *match_ = VariantMatch::ExplicitlyMatchedOn {
                        module: module.clone(),
                        name: name.clone(),
                    }
                }
            }
        }
    }

    /// When we work with bit array patterns the splitter follows a different
    /// strategy to split branches: instead of trying to create a multiway
    /// decision tree with possibly many branches, we create only two branches
    /// based on the first segment test we run into.
    /// One path is going to be for the branches that can match if the test is
    /// successful, and the other one is going to be the usual fallback to go
    /// down to if it's not successful.
    ///
    /// > "And now for the tricky bit..."
    /// > <https://youtu.be/lKXe3HUG2l4?si=i1thYB-kjfMU8NSe&t=645>
    ///
    fn add_checked_bit_array_branch(
        &mut self,
        pattern_check: PatternCheck,
        tests: VecDeque<BitArrayTest>,
        mut branch: Branch,
        compiler: &mut Compiler<'_>,
    ) {
        // If we haven't found a test yet we just use the first one we find
        // as the pivot to split all the branches.
        let pivot_test = match self.choices.as_slice() {
            [] => {
                let test = tests.front().expect("empty bit array test").clone();
                self.choices.push((
                    RuntimeCheck::BitArray { test: test.clone() },
                    VecDeque::new(),
                ));
                test
            }
            [(RuntimeCheck::BitArray { test }, _)] => test.clone(),
            _ => unreachable!("non bit array check when splitting bit array patterns"),
        };

        let pattern_fails_if_test_succeeds = tests
            .iter()
            .any(|test| test.fails_if_succeeding(&pivot_test).is_certain());

        let pattern_fails_if_test_fails = tests
            .iter()
            .any(|test| test.fails_if_failing(&pivot_test).is_certain());

        // The branch is still relevant for the if_true path if it still has a
        // chance of matching knowing that the pivot test has matched.
        // So if we know for certain that the pattern would fail, we don't even
        // bother adding it to the if_true path.
        if !pattern_fails_if_test_succeeds {
            // If the branch is relevant we can further simplify it by removing
            // all those tests that are guaranteed to succeed. For example,
            // if the succeeding pivot test is `size >= 20` there's no point
            // in checking that `size >= 10`, we know that's always true in this
            // path of the decision tree!
            let tests = tests
                .into_iter()
                .filter(|test| test.succeeds_if_succeeding(&pivot_test) == Confidence::Uncertain)
                .collect::<VecDeque<_>>();

            let mut branch = branch.clone();
            let variable = &pattern_check.var;
            branch.add_check(variable.is(compiler.bit_array_pattern(tests)));

            // We know that there's always going to be a single choice for the
            // successful check, so we get that and add the branch to it.
            let (_, if_true_branches) = self
                .choices
                .get_mut(0)
                .expect("bit array compilation with no choice");
            if_true_branches.push_back(branch);
        }

        // Same goes for the if_false branch: knowing the pivot test has failed
        // we only want to keep those branches with a pattern that still have a
        // chance to match.
        if !pattern_fails_if_test_fails {
            // The main difference with the if true case is that we have no way
            // of pruning the number of needed tests, so we add this check back
            // exactly as it is.
            branch.add_check(pattern_check);
            self.fallback.push_back(branch);
        }
    }

    fn save_index_of_new_choice(&mut self, kind: RuntimeCheckKind) {
        let _ = match kind {
            RuntimeCheckKind::Int { .. }
            | RuntimeCheckKind::Float { .. }
            | RuntimeCheckKind::String { .. }
            | RuntimeCheckKind::Tuple { .. }
            | RuntimeCheckKind::Variant { .. }
            | RuntimeCheckKind::EmptyList
            | RuntimeCheckKind::NonEmptyList => self.indices.insert(kind, self.choices.len()),

            RuntimeCheckKind::StringPrefix { prefix } => self
                .prefix_indices
                .insert(prefix.to_string(), self.choices.len()),
        };
    }

    fn indices_of_overlapping_checks(&self, kind: &RuntimeCheckKind) -> Vec<usize> {
        match kind {
            // All these checks will only overlap with a check that is exactly the
            // same, so we just look up their index in the `indices` map using the
            // kind as the lookup.
            RuntimeCheckKind::Int { .. }
            | RuntimeCheckKind::Float { .. }
            | RuntimeCheckKind::Tuple { .. }
            | RuntimeCheckKind::Variant { .. }
            | RuntimeCheckKind::EmptyList
            | RuntimeCheckKind::NonEmptyList => {
                self.indices.get(kind).cloned().into_iter().collect_vec()
            }

            // String patterns are a bit more tricky as they might end up overlapping
            // even if they're not exactly the same kind of check! Let's have a look
            // at an example. Say we're compiling these branches:
            //
            // ```
            // a is "wibble" <> rest -> todo
            // a is "wibbler" <> rest -> todo
            // ```
            //
            // We use the first (and only) check in the first branch as the pivot and
            // now we have to decide where to put the next branch. Is it matching with
            // the first one or completely unrelated?
            // Since `"wibbler"` starts with `"wibble"` we know it's overlapping and
            // it cannot possibly match if the previous one doesn't!
            //
            // So when we find a `String`/`StringPrefix` pattern we look for a prefix
            // among the ones we have discovered so far that could match with it.
            // That is, we look for a prefix of the pattern we're checking in the prefix
            // trie.
            RuntimeCheckKind::StringPrefix { prefix: value } => {
                ancestors_values(&self.prefix_indices, value).collect_vec()
            }

            // Strings are almost exactly the same, except they could also have an exact
            // match with other string patterns. So a string pattern could overlap with
            // another string pattern (if they're matching on the same value), or with
            // one or more string prefix patterns with a matching prefix.
            RuntimeCheckKind::String { value } => {
                let first_index = self.indices.get(kind).cloned();
                first_index
                    .into_iter()
                    .chain(ancestors_values(&self.prefix_indices, value))
                    .collect_vec()
            }
        }
    }
}

fn ancestors_values(trie: &Trie<String, usize>, key: &str) -> impl Iterator<Item = usize> {
    trie.get_ancestor(key)
        .into_iter()
        .flat_map(|ancestor| ancestor.values().copied())
}

pub struct ConstructorSpecialiser {
    specialised_types: HashMap<u64, Arc<Type>>,
}

impl ConstructorSpecialiser {
    fn specialise_constructors(
        constructors: &TypeVariantConstructors,
        type_arguments: &[Arc<Type>],
        current_module: &EcoString,
        type_module: &EcoString,
    ) -> Vec<TypeValueConstructor> {
        match constructors.opaque {
            // If the type is opaque and we are not in the definition module of
            // that type, we don't have access to any constructors so we treat
            // it the same as an external type.
            Opaque::Opaque if current_module != type_module => return Vec::new(),
            Opaque::Opaque | Opaque::NotOpaque => {}
        };

        let specialiser = Self::new(constructors.type_parameters_ids.as_slice(), type_arguments);
        constructors
            .variants
            .iter()
            .map(|v| specialiser.specialise_type_value_constructor(v))
            .collect_vec()
    }

    fn new(parameters: &[u64], type_arguments: &[Arc<Type>]) -> Self {
        let specialised_types = parameters
            .iter()
            .copied()
            .zip(type_arguments.iter().cloned())
            .collect();
        Self { specialised_types }
    }

    fn specialise_type_value_constructor(&self, v: &TypeValueConstructor) -> TypeValueConstructor {
        let TypeValueConstructor {
            name,
            parameters,
            documentation,
        } = v;
        let parameters = parameters
            .iter()
            .map(|p| TypeValueConstructorField {
                type_: self.specialise_type(p.type_.as_ref()),
                label: p.label.clone(),
            })
            .collect_vec();
        TypeValueConstructor {
            name: name.clone(),
            parameters,
            documentation: documentation.clone(),
        }
    }

    fn specialise_type(&self, type_: &Type) -> Arc<Type> {
        Arc::new(match type_ {
            Type::Named {
                publicity,
                package,
                module,
                name,
                arguments,
                inferred_variant,
            } => Type::Named {
                publicity: *publicity,
                package: package.clone(),
                module: module.clone(),
                name: name.clone(),
                arguments: arguments
                    .iter()
                    .map(|argument| self.specialise_type(argument))
                    .collect(),
                inferred_variant: *inferred_variant,
            },

            Type::Fn { arguments, return_ } => Type::Fn {
                arguments: arguments
                    .iter()
                    .map(|argument| self.specialise_type(argument))
                    .collect(),
                return_: return_.clone(),
            },

            Type::Var { type_ } => Type::Var {
                type_: Arc::new(RefCell::new(self.specialise_var(type_))),
            },

            Type::Tuple { elements } => Type::Tuple {
                elements: elements
                    .iter()
                    .map(|element| self.specialise_type(element))
                    .collect(),
            },
        })
    }

    fn specialise_var(&self, type_: &RefCell<TypeVar>) -> TypeVar {
        match &*type_.borrow() {
            TypeVar::Unbound { id } => TypeVar::Unbound { id: *id },

            TypeVar::Link { type_ } => TypeVar::Link {
                type_: self.specialise_type(type_.as_ref()),
            },

            TypeVar::Generic { id } => match self.specialised_types.get(id) {
                Some(type_) => TypeVar::Link {
                    type_: type_.clone(),
                },
                None => TypeVar::Generic { id: *id },
            },
        }
    }
}

/// Intermiate data structure that's used to set up everything that's needed by
/// the pattern matching compiler and get a case expression ready to be compiled,
/// while hiding the intricacies of handling an arena to record different patterns.
///
pub struct CaseToCompile {
    patterns: Arena<Pattern>,
    branches: Vec<Branch>,
    subject_variables: Vec<Variable>,
    /// The number of clauses in this case to compile.
    number_of_clauses: usize,
    variable_id: usize,
}

impl CaseToCompile {
    pub fn new(subject_types: &[Arc<Type>]) -> Self {
        let mut variable_id = 0;
        let subject_variables = subject_types
            .iter()
            .map(|type_| {
                let id = variable_id;
                variable_id += 1;
                Variable::new(id, type_.clone())
            })
            .collect_vec();

        Self {
            patterns: Arena::new(),
            branches: vec![],
            number_of_clauses: 0,
            subject_variables,
            variable_id,
        }
    }

    /// Registers a `TypedClause` as one of the branches to be compiled.
    ///
    /// If you don't have a clause and just have a simple `TypedPattern` you want
    /// to generate a decision tree for you can use `add_pattern`.
    ///
    pub fn add_clause(&mut self, branch: &TypedClause) {
        let all_patterns =
            std::iter::once(&branch.pattern).chain(branch.alternative_patterns.iter());

        for (alternative_index, patterns) in all_patterns.enumerate() {
            let mut checks = Vec::with_capacity(patterns.len());

            // We're doing the zipping ourselves instead of using iters.zip as the
            // borrow checker would complain and the only workaround would be to
            // allocate an entire new vector each time.
            for i in 0..patterns.len() {
                let pattern = self.register(patterns.get(i).expect("pattern index"));
                let var = self
                    .subject_variables
                    .get(i)
                    .expect("wrong number of subjects");
                checks.push(var.is(pattern))
            }

            let has_guard = branch.guard.is_some();
            let branch = Branch::new(self.number_of_clauses, alternative_index, checks, has_guard);
            self.branches.push(branch);
        }

        self.number_of_clauses += 1;
    }

    /// Add a single pattern as a branch to be compiled.
    ///
    /// This is useful in case one wants to check exhaustiveness of a single
    /// pattern without having a fully fledged `TypedClause` to pass to the `add_clause`
    /// method. For example, in `let` destructurings.
    ///
    pub fn add_pattern(&mut self, pattern: &TypedPattern) {
        let pattern = self.register(pattern);
        let var = self
            .subject_variables
            .first()
            .expect("wrong number of subject variables for pattern");
        let branch = Branch::new(self.number_of_clauses, 0, vec![var.is(pattern)], false);
        self.number_of_clauses += 1;
        self.branches.push(branch);
    }

    pub fn compile(self, env: &Environment<'_>) -> CompileCaseResult {
        let mut compiler = Compiler::new(env, self.variable_id, self.patterns);

        let decision = if self.branches.is_empty() {
            let var = self
                .subject_variables
                .first()
                .expect("case with no subjects")
                .clone();

            compiler.split_and_compile_with_pivot_var(var, VecDeque::new())
        } else {
            compiler.compile(self.branches.into())
        };

        CompileCaseResult {
            diagnostics: compiler.diagnostics,
            compiled_case: CompiledCase {
                tree: decision,
                subject_variables: self.subject_variables,
            },
        }
    }

    /// Registers a typed pattern (and all its sub-patterns) into this
    /// `CaseToCompile`'s pattern arena, returning an id to get the pattern back.
    ///
    fn register(&mut self, pattern: &TypedPattern) -> Id<Pattern> {
        match pattern {
            TypedPattern::Invalid { .. } => self.insert(Pattern::Discard),
            TypedPattern::Discard { .. } => self.insert(Pattern::Discard),

            TypedPattern::Int { value, .. } => {
                let value = value.clone();
                self.insert(Pattern::Int { value })
            }

            TypedPattern::Float { value, .. } => {
                let value = value.clone();
                self.insert(Pattern::Float { value })
            }

            TypedPattern::String { value, .. } => {
                let value = value.clone();
                self.insert(Pattern::String { value })
            }

            TypedPattern::Variable { name, .. } => {
                let name = name.clone();
                self.insert(Pattern::Variable { name })
            }

            TypedPattern::Assign { name, pattern, .. } => {
                let name = name.clone();
                let pattern = self.register(pattern);
                self.insert(Pattern::Assign { name, pattern })
            }

            TypedPattern::Tuple { elements, .. } => {
                let elements = elements
                    .iter()
                    .map(|element| self.register(element))
                    .collect_vec();
                self.insert(Pattern::Tuple { elements })
            }

            TypedPattern::List { elements, tail, .. } => {
                let mut list = match tail {
                    Some(tail) => self.register(tail),
                    None => self.insert(Pattern::EmptyList),
                };
                for element in elements.iter().rev() {
                    let first = self.register(element);
                    list = self.insert(Pattern::NonEmptyList { first, rest: list });
                }
                list
            }

            TypedPattern::Constructor {
                arguments,
                constructor,
                name,
                module,
                ..
            } => {
                let index = constructor.expect_ref("must be inferred").constructor_index as usize;
                let module = module.as_ref().map(|(module, _)| module.clone());
                let fields = arguments
                    .iter()
                    .map(|argument| self.register(&argument.value))
                    .collect_vec();
                self.insert(Pattern::Variant {
                    name: name.clone(),
                    module,
                    index,
                    fields,
                })
            }

            TypedPattern::BitArray { segments, .. } => {
                let tests = self.bit_array_to_tests(segments);
                self.insert(Pattern::BitArray { tests })
            }

            TypedPattern::StringPrefix {
                left_side_string,
                left_side_assignment,
                right_side_assignment,
                ..
            } => {
                let prefix = left_side_string.clone();
                let prefix_name = left_side_assignment
                    .as_ref()
                    .map(|(label, _)| label.clone());
                let rest_pattern = match right_side_assignment {
                    AssignName::Variable(name) => Pattern::Variable { name: name.clone() },
                    AssignName::Discard(_) => Pattern::Discard,
                };
                let rest = self.insert(rest_pattern);
                self.insert(Pattern::StringPrefix {
                    prefix,
                    prefix_name,
                    rest,
                })
            }

            TypedPattern::BitArraySize { .. } => {
                unreachable!("Cannot convert BitArraySize to exhaustiveness pattern")
            }
        }
    }

    fn insert(&mut self, pattern: Pattern) -> Id<Pattern> {
        self.patterns.alloc(pattern)
    }

    fn bit_array_to_tests(
        &mut self,
        segments: &[TypedPatternBitArraySegment],
    ) -> VecDeque<BitArrayTest> {
        // If there's no segments then we just add a single check to make sure
        // the bit array is empty.
        if segments.is_empty() {
            let mut test = VecDeque::new();
            test.push_front(BitArrayTest::Size(SizeTest {
                operator: SizeOperator::Equal,
                size: Offset::constant(0),
            }));
            return test;
        }

        let mut previous_end = Offset::constant(0);
        let mut tests = VecDeque::with_capacity(segments.len() * 2);
        let mut pattern_variables = HashMap::new();

        let segments_count = segments.len();
        for (i, segment) in segments.iter().enumerate() {
            let segment_size = segment_size(segment, &pattern_variables, None);

            // If we're reading a variable number of bits we need to make sure
            // that that variable is not negative!
            if segment_size.can_be_negative() {
                tests.push_back(BitArrayTest::ReadSizeIsNotNegative {
                    size: segment_size.clone(),
                });
            }

            // All segments but the last will require the original bit array to
            // have a minimum number of bits for the pattern to succeed. The
            // final segment is special as it could require a specific size, or
            // be a catch all that matches with any number of remaining bits.
            let is_last_segment = i + 1 == segments_count;
            match &segment_size {
                ReadSize::RemainingBits => (),
                ReadSize::RemainingBytes => tests.push_back(BitArrayTest::CatchAllIsBytes {
                    size_so_far: previous_end.clone(),
                }),
                segment_size => {
                    let size = previous_end.clone().add_size(segment_size);
                    let operator = if is_last_segment {
                        SizeOperator::Equal
                    } else {
                        SizeOperator::GreaterEqual
                    };
                    tests.push_back(BitArrayTest::Size(SizeTest { operator, size }));
                }
            };

            // Each segment is also turned into a match test, checking the
            // selected bits match with the pattern's value.
            let value = segment_matched_value(segment, None);

            let type_ = match &segment.type_ {
                type_ if type_.is_int() => ReadType::Int,
                type_ if type_.is_float() => ReadType::Float,
                type_ if type_.is_string() => ReadType::String,
                type_ if type_.is_bit_array() => ReadType::BitArray,
                type_ if type_.is_utf_codepoint() => ReadType::UtfCodepoint,
                x => panic!("invalid segment type in exhaustiveness {x:?}"),
            };

            let read_action = ReadAction {
                size: segment_size.clone(),
                from: previous_end.clone(),
                type_,
                endianness: segment.endianness(),
                signed: segment.signed(),
            };

            // Then if the matched value is a variable that is in scope for the
            // rest of the pattern we keep track of it, so it can be used in the
            // following read actions as a valid size.
            match &value {
                BitArrayMatchedValue::LiteralFloat(_)
                | BitArrayMatchedValue::LiteralInt(_)
                | BitArrayMatchedValue::LiteralString { .. }
                | BitArrayMatchedValue::Discard(_) => {}
                BitArrayMatchedValue::Variable(name)
                | BitArrayMatchedValue::Assign { name, .. } => {
                    let _ = pattern_variables.insert(name.clone(), read_action.clone());
                }
            }

            // If we are matching on a float segment that is not a literal we want
            // to add an additional check to make sure that we won't match with
            // `NaN` and `Infinity`!
            if type_.is_float() && !value.is_literal() {
                tests.push_back(BitArrayTest::SegmentIsFiniteFloat {
                    read_action: read_action.clone(),
                });
            }
            tests.push_back(BitArrayTest::Match(MatchTest { value, read_action }));

            previous_end = previous_end.add_size(&segment_size);
        }
        tests
    }
}

fn segment_matched_value(
    segment: &TypedPatternBitArraySegment,
    // If we are compiling an assignment pattern, we still need access to the
    // `type_` and `options` fields of the `segment`, so we must still pass that
    // in above. However, we need to check the correct sub-pattern of the original
    // pattern, so if they are different we set this argument to `Some`.
    pattern: Option<&TypedPattern>,
) -> BitArrayMatchedValue {
    let pattern = pattern.unwrap_or(&segment.value);
    match pattern {
        ast::Pattern::Int { int_value, .. } => BitArrayMatchedValue::LiteralInt(int_value.clone()),
        ast::Pattern::Float { value, .. } => BitArrayMatchedValue::LiteralFloat(value.clone()),
        ast::Pattern::String { value, .. } if segment.has_utf16_option() => {
            BitArrayMatchedValue::LiteralString {
                value: value.clone(),
                encoding: StringEncoding::Utf16,
            }
        }
        ast::Pattern::String { value, .. } if segment.has_utf32_option() => {
            BitArrayMatchedValue::LiteralString {
                value: value.clone(),
                encoding: StringEncoding::Utf32,
            }
        }
        ast::Pattern::String { value, .. } => BitArrayMatchedValue::LiteralString {
            value: value.clone(),
            encoding: StringEncoding::Utf8,
        },
        ast::Pattern::Variable { name, .. } => BitArrayMatchedValue::Variable(name.clone()),
        ast::Pattern::Discard { name, .. } => BitArrayMatchedValue::Discard(name.clone()),
        ast::Pattern::Assign { name, pattern, .. } => BitArrayMatchedValue::Assign {
            name: name.clone(),
            value: Box::new(segment_matched_value(segment, Some(pattern))),
        },
        x => panic!("unexpected segment value pattern {x:?}"),
    }
}

fn segment_size(
    segment: &TypedPatternBitArraySegment,
    pattern_variables: &HashMap<EcoString, ReadAction>,
    // If we are compiling an assignment pattern, we still need access to the
    // `type_` and `options` fields of the `segment`, so we must still pass that
    // in above. However, we need to check the correct sub-pattern of the original
    // pattern, so if they are different we set this argument to `Some`.
    pattern: Option<&TypedPattern>,
) -> ReadSize {
    let pattern = pattern.unwrap_or(&segment.value);

    match segment.size() {
        // The size of a segment must be a `BitArraySize` pattern.
        Some(ast::Pattern::BitArraySize(size)) => {
            bit_array_size(segment.unit(), pattern_variables, size)
        }
        Some(x) => panic!("invalid pattern size made it to code generation {x:?}"),

        // If a segment has the `bits`/`bytes` option and has no size, that
        // means it's the final catch all segment: we'll have to read any number
        // of bits.
        _ if segment.has_bits_option() => ReadSize::RemainingBits,
        _ if segment.has_bytes_option() => ReadSize::RemainingBytes,

        // If there's no size option we go for a default: 8 bits for int
        // segments, and 64 for anything else.
        None if segment.type_.is_int() => ReadSize::ConstantBits(8.into()),
        None => match pattern {
            ast::Pattern::Assign { pattern, .. } => {
                segment_size(segment, pattern_variables, Some(pattern))
            }

            ast::Pattern::String { value, .. } if segment.has_utf16_option() => {
                ReadSize::ConstantBits(
                    // Each utf16 code unit is 16 bits
                    length_utf16(&convert_string_escape_chars(value)) * BigInt::from(16),
                )
            }
            ast::Pattern::String { value, .. } if segment.has_utf32_option() => {
                // Each utf32 code unit is 32 bits
                ReadSize::ConstantBits(
                    length_utf32(&convert_string_escape_chars(value)) * BigInt::from(32),
                )
            }
            // If the segment is a literal string then it has an automatic size
            // given by its number of bytes.
            ast::Pattern::String { value, .. } => {
                ReadSize::ConstantBits(convert_string_escape_chars(value).len() * BigInt::from(8))
            }
            // In all other cases the segment is considered to be 64 bits.
            _ => ReadSize::ConstantBits(64.into()),
        },
    }
}

fn bit_array_size(
    unit: u8,
    pattern_variables: &HashMap<EcoString, ReadAction>,
    size: &TypedBitArraySize,
) -> ReadSize {
    match size {
        BitArraySize::Int { int_value, .. } => ReadSize::ConstantBits(int_value * unit),
        BitArraySize::Variable { name, .. } => {
            let variable = match pattern_variables.get(name) {
                Some(read_action) => {
                    VariableUsage::PatternSegment(name.clone(), read_action.clone())
                }
                None => VariableUsage::OutsideVariable(name.clone()),
            };
            ReadSize::VariableBits {
                variable: Box::new(variable),
                unit,
            }
        }
        BitArraySize::BinaryOperator {
            operator,
            left,
            right,
            ..
        } => {
            let size = ReadSize::BinaryOperator {
                left: Box::new(bit_array_size(1, pattern_variables, left)),
                right: Box::new(bit_array_size(1, pattern_variables, right)),
                operator: *operator,
            };

            if unit == 1 {
                size
            } else {
                ReadSize::BinaryOperator {
                    left: Box::new(size),
                    right: Box::new(ReadSize::ConstantBits(unit.into())),
                    operator: IntOperator::Multiply,
                }
            }
        }
        BitArraySize::Block { inner, .. } => bit_array_size(unit, pattern_variables, inner),
    }
}

/// Returns `true` if one bag is a superset of the other: that is it contains
/// all the keys of `other` in a quantity that's greater or equal.
///
#[must_use]
fn superset(
    one: &im::HashMap<VariableUsage, usize>,
    other: &im::HashMap<VariableUsage, usize>,
) -> bool {
    other
        .iter()
        .all(|(key, other_occurrences)| match one.get(key) {
            Some(occurrences) => occurrences >= other_occurrences,
            None => false,
        })
}
