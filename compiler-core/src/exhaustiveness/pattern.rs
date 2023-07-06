use crate::{
    ast::{AssignName, TypedPattern},
    type_::Type,
};
use id_arena::{Arena, Id};
use itertools::Itertools;
use smol_str::SmolStr;
use std::sync::Arc;

pub type PatternId = Id<Pattern>;

/// A user defined pattern such as `Some((x, 10))`.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Pattern {
    Discard,
    Or {
        left: PatternId,
        right: PatternId,
    },
    Int {
        value: SmolStr,
    },
    Float {
        value: SmolStr,
    },
    String {
        value: SmolStr,
    },
    StringPrefix {
        prefix: SmolStr,
        rest: AssignName,
    },
    Assign {
        name: SmolStr,
        pattern: PatternId,
    },
    Variable {
        name: SmolStr,
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
    BitString {
        value: SmolStr,
    },
}

/// A type constructor.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constructor {
    Int(SmolStr),
    Float(SmolStr),
    Tuple(Vec<Arc<Type>>),
    String(SmolStr),
    Variant(Arc<Type>, usize),
    // TODO: Generate a decision tree for this
    BitString,
    // TODO: Generate a decision tree for this
    StringPrefix,
}

impl Constructor {
    /// Returns the index of this constructor relative to its type.
    pub fn index(&self) -> usize {
        match self {
            Constructor::Int(_)
            | Constructor::Float(_)
            | Constructor::Tuple(_)
            | Constructor::String(_)
            | Constructor::BitString
            | Constructor::StringPrefix => 0,

            Constructor::Variant(_, index) => *index,
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

    pub fn get(&self, id: PatternId) -> Option<&Pattern> {
        self.arena.get(id)
    }

    pub fn get_mut(&mut self, id: PatternId) -> Option<&mut Pattern> {
        self.arena.get_mut(id)
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

            // TODO: test
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

            // TODO: test
            TypedPattern::Constructor {
                name,
                arguments,
                module,
                constructor,
                with_spread,
                type_,
                ..
            } => {
                // TODO: This is using the arguments to get the arity. This may
                // not be correct as it could be using a spread. Will need to
                // get it either from the field map or the type.
                let constructor = Constructor::Variant(type_.clone(), arguments.len());
                // TODO: The arguments may not be given in the same order as the
                // definition if labels are used. We need to check if that is
                // the case (or if it has been expanded during earlier type
                // checking) and then reorder the arguments if needed.
                let arguments = arguments
                    .iter()
                    .map(|argument| self.register(&argument.value))
                    .collect_vec();
                self.insert(Pattern::Constructor {
                    constructor,
                    arguments,
                })
            }

            // TODO: test
            TypedPattern::BitString { location, .. } => {
                // TODO: in future support bit strings fully and check the
                // exhaustiveness of their segment patterns.
                // For now we use the location to give each bit string a pattern
                // a unique value.
                self.insert(Pattern::BitString {
                    value: format!("{}:{}", location.start, location.end).into(),
                })
            }

            // TODO: test
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
