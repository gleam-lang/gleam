#[cfg(test)]
use crate::ast::Meta;

use crate::ast::{Pattern, TypedClause};
use crate::typ::Type;

pub fn check(typ: &Type, clauses: &Vec<TypedClause>) -> Result<(), Left> {
    clauses
        .iter()
        .fold(cases_left(typ), |case, clause| apply(case, &clause.pattern))
        .to_result()
}

#[derive(Debug, PartialEq)]
pub enum Left {
    None,
    Int,
    Float,
    String,
}

impl Left {
    pub fn to_result(self) -> Result<(), Self> {
        match self {
            Left::None => Ok(()),
            other => Err(other),
        }
    }
}

fn cases_left(typ: &Type) -> Left {
    match typ {
        Type::Fn { .. } => Left::None, // TODO

        Type::Var { .. } => Left::None, // TODO

        Type::Map { .. } => Left::None, // TODO

        Type::Tuple { .. } => Left::None, // TODO

        Type::Module { .. } => Left::None, // TODO

        Type::RowNil { .. } => Left::None, // TODO

        Type::RowCons { .. } => Left::None, // TODO

        Type::App { module, name, .. } if module == "" => match &**name {
            "Int" => Left::Int,
            "Float" => Left::Float,
            "String" => Left::String,
            _ => Left::None, // TODO
        },

        Type::App { .. } => Left::None, // TODO
    }
}

fn apply(_left: Left, pattern: &Pattern) -> Left {
    match pattern {
        Pattern::Discard { .. } => Left::None,

        Pattern::Var { .. } => Left::None,

        Pattern::Int { .. } => Left::Int,

        Pattern::Float { .. } => Left::Float,

        Pattern::String { .. } => Left::String,

        Pattern::Nil { .. } => Left::None, // TODO

        Pattern::Cons { .. } => Left::None, // TODO

        Pattern::Tuple { .. } => Left::None, // TODO

        Pattern::Constructor { .. } => Left::None, // TODO
    }
}

#[test]
fn apply_test() {
    // Discard pattern
    assert_eq!(Left::None, apply(Left::None, &discard()));
    assert_eq!(Left::None, apply(Left::Int, &discard()));

    // Var pattern
    assert_eq!(Left::None, apply(Left::None, &var()));
    assert_eq!(Left::None, apply(Left::Int, &var()));

    // Int pattern
    assert_eq!(Left::Int, apply(Left::Int, &int(0)));
    assert_eq!(Left::Int, apply(Left::Int, &int(1)));
    assert_eq!(Left::Int, apply(Left::Int, &int(-1)));

    // Float pattern
    assert_eq!(Left::Float, apply(Left::Float, &float(0.0)));
    assert_eq!(Left::Float, apply(Left::Float, &float(0.1)));
    assert_eq!(Left::Float, apply(Left::Float, &float(-0.1)));

    // String pattern
    assert_eq!(Left::String, apply(Left::String, &string("")));
    assert_eq!(Left::String, apply(Left::String, &string("ok")));
}

#[cfg(test)]
fn discard() -> Pattern {
    Pattern::Discard {
        meta: Meta::default(),
    }
}

#[cfg(test)]
fn var() -> Pattern {
    Pattern::Var {
        meta: Meta::default(),
        name: "".to_string(),
    }
}

#[cfg(test)]
fn int(value: i64) -> Pattern {
    Pattern::Int {
        meta: Meta::default(),
        value,
    }
}

#[cfg(test)]
fn float(value: f64) -> Pattern {
    Pattern::Float {
        meta: Meta::default(),
        value,
    }
}

#[cfg(test)]
fn string(s: &'static str) -> Pattern {
    Pattern::String {
        meta: Meta::default(),
        value: s.to_string(),
    }
}
