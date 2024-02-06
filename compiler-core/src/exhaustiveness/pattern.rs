use crate::{
    ast::{AssignName, TypedPattern},
    type_::Type,
};
use ecow::EcoString;
use id_arena::{Arena, Id};
use itertools::Itertools;
use std::sync::Arc;

pub type PatternId = Id<Pattern>;

/// A user defined pattern such as `Some((x, 10))`.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Pattern {
    Discard,
    // TODO: support nested or patterns in the parser + codegen
    #[allow(unused)]
    Or {
        left: PatternId,
        right: PatternId,
    },
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
        rest: AssignName,
    },
    Assign {
        name: EcoString,
        pattern: PatternId,
    },
    Variable {
        name: EcoString,
    },
    Tuple {
        elements: Vec<PatternId>,
    },
    Constructor {
        constructor: Constructor,
        arguments: Vec<PatternId>,
    },
    List {
        first: PatternId,
        rest: PatternId,
    },
    EmptyList,
    // TODO: Compile the matching within the bit strings
    BitArray {
        value: EcoString,
    },
}

/// A type constructor.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constructor {
    Int(EcoString),
    Float(EcoString),
    Tuple(Vec<Arc<Type>>),
    String(EcoString),
    Variant { type_: Arc<Type>, index: u16 },
    // TODO: Generate a decision tree for this
    BitArray,
    // TODO: Generate a decision tree for this
    StringPrefix,
}

impl Constructor {
    /// Returns the index of this constructor relative to its type.
    pub fn index(&self) -> u16 {
        match self {
            Constructor::Int(_)
            | Constructor::Float(_)
            | Constructor::Tuple(_)
            | Constructor::String(_)
            | Constructor::BitArray
            | Constructor::StringPrefix => 0,

            Constructor::Variant { index, .. } => *index,
        }
    }
}

#[derive(Debug, Default)]
pub struct PatternArena {
    arena: Arena<Pattern>,
}

impl PatternArena {
    pub fn new() -> Self {
        Self::default()
    }

    #[cfg(test)]
    pub fn get(&self, id: PatternId) -> Option<&Pattern> {
        self.arena.get(id)
    }

    pub fn register(&mut self, pattern: &TypedPattern) -> PatternId {
        match pattern {
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

            TypedPattern::Tuple { elems, .. } => {
                let elements = elems.iter().map(|elem| self.register(elem)).collect_vec();
                self.insert(Pattern::Tuple { elements })
            }

            TypedPattern::List { elements, tail, .. } => {
                let mut list = match tail {
                    Some(tail) => self.register(tail),
                    None => self.insert(Pattern::EmptyList),
                };
                for element in elements.iter().rev() {
                    let first = self.register(element);
                    list = self.insert(Pattern::List { first, rest: list });
                }
                list
            }

            TypedPattern::Constructor {
                arguments,
                constructor,
                type_,
                ..
            } => {
                let constructor = Constructor::Variant {
                    type_: type_.clone(),
                    index: constructor.expect_ref("must be inferred").constructor_index,
                };
                let arguments = arguments
                    .iter()
                    .map(|argument| self.register(&argument.value))
                    .collect_vec();
                self.insert(Pattern::Constructor {
                    constructor,
                    arguments,
                })
            }

            TypedPattern::BitArray { location, .. } => {
                // TODO: in future support bit strings fully and check the
                // exhaustiveness of their segment patterns.
                // For now we use the location to give each bit string a pattern
                // a unique value.
                self.insert(Pattern::BitArray {
                    value: format!("{}:{}", location.start, location.end).into(),
                })
            }

            TypedPattern::StringPrefix {
                left_side_string,
                right_side_assignment,
                ..
            } => {
                let prefix = left_side_string.clone();
                let rest = right_side_assignment.clone();
                self.insert(Pattern::StringPrefix { prefix, rest })
            }

            TypedPattern::VarUsage { .. } => {
                unreachable!("Cannot convert VarUsage to exhaustiveness pattern")
            }
        }
    }

    pub fn insert(&mut self, pattern: Pattern) -> PatternId {
        self.arena.alloc(pattern)
    }

    pub fn into_inner(self) -> Arena<Pattern> {
        self.arena
    }
}
