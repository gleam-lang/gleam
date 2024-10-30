use ecow::EcoString;
use num::{BigInt, BigRational};

use crate::{
    ast::{BinOp, Constant, TypedConstant},
    type_::ValueConstructorVariant,
};

/// Represents a temporary value for computing constant operations, as most values
/// can be represented more simply than a TypedConstant
#[derive(Debug, Clone, PartialEq)]
pub enum FoldedConstant<'a> {
    Int(BigInt),
    Float(BigRational),
    String(EcoString),
    Bool(bool),
    /// For more complex constants, such as tuples, bit arrays and records
    Complex(&'a TypedConstant),
}

pub fn fold_constant_bin_op<'a>(
    left: &'a TypedConstant,
    right: &'a TypedConstant,
    name: &BinOp,
) -> FoldedConstant<'a> {
    let left = fold_single_constant(left);
    let right = fold_single_constant(right);

    match (name, left, right) {
        (BinOp::And, FoldedConstant::Bool(left), FoldedConstant::Bool(right)) => {
            FoldedConstant::Bool(left && right)
        }
        (BinOp::Or, FoldedConstant::Bool(left), FoldedConstant::Bool(right)) => {
            FoldedConstant::Bool(left || right)
        }
        (BinOp::Eq, left, right) => FoldedConstant::Bool(left == right),
        (BinOp::NotEq, left, right) => FoldedConstant::Bool(left != right),
        (BinOp::LtInt, FoldedConstant::Int(left), FoldedConstant::Int(right)) => {
            FoldedConstant::Bool(left < right)
        }
        (BinOp::LtEqInt, FoldedConstant::Int(left), FoldedConstant::Int(right)) => {
            FoldedConstant::Bool(left <= right)
        }
        (BinOp::LtFloat, FoldedConstant::Float(left), FoldedConstant::Float(right)) => {
            FoldedConstant::Bool(left < right)
        }
        (BinOp::LtEqFloat, FoldedConstant::Float(left), FoldedConstant::Float(right)) => {
            FoldedConstant::Bool(left <= right)
        }
        (BinOp::GtEqInt, FoldedConstant::Int(left), FoldedConstant::Int(right)) => {
            FoldedConstant::Bool(left >= right)
        }
        (BinOp::GtInt, FoldedConstant::Int(left), FoldedConstant::Int(right)) => {
            FoldedConstant::Bool(left > right)
        }
        (BinOp::GtEqFloat, FoldedConstant::Float(left), FoldedConstant::Float(right)) => {
            FoldedConstant::Bool(left >= right)
        }
        (BinOp::GtFloat, FoldedConstant::Float(left), FoldedConstant::Float(right)) => {
            FoldedConstant::Bool(left > right)
        }
        (BinOp::AddInt, FoldedConstant::Int(left), FoldedConstant::Int(right)) => {
            FoldedConstant::Int(left + right)
        }
        (BinOp::AddFloat, FoldedConstant::Float(left), FoldedConstant::Float(right)) => {
            FoldedConstant::Float(left + right)
        }
        (BinOp::SubInt, FoldedConstant::Int(left), FoldedConstant::Int(right)) => {
            FoldedConstant::Int(left - right)
        }
        (BinOp::SubFloat, FoldedConstant::Float(left), FoldedConstant::Float(right)) => {
            FoldedConstant::Float(left - right)
        }
        (BinOp::MultInt, FoldedConstant::Int(left), FoldedConstant::Int(right)) => {
            FoldedConstant::Int(left * right)
        }
        (BinOp::MultFloat, FoldedConstant::Float(left), FoldedConstant::Float(right)) => {
            FoldedConstant::Float(left * right)
        }
        (BinOp::DivInt, FoldedConstant::Int(left), FoldedConstant::Int(right)) => {
            FoldedConstant::Int(left / right)
        }
        (BinOp::RemainderInt, FoldedConstant::Int(left), FoldedConstant::Int(right)) => {
            FoldedConstant::Int(left % right)
        }
        (BinOp::DivFloat, FoldedConstant::Float(left), FoldedConstant::Float(right)) => {
            FoldedConstant::Float(left / right)
        }
        (BinOp::Concatenate, FoldedConstant::String(mut left), FoldedConstant::String(right)) => {
            left.push_str(&right);
            FoldedConstant::String(left)
        }
        _ => panic!("Types have already been checked"),
    }
}

fn fold_single_constant(value: &TypedConstant) -> FoldedConstant<'_> {
    match value {
        Constant::Var {
            constructor: Some(constructor),
            ..
        } => match &constructor.variant {
            ValueConstructorVariant::ModuleConstant { literal, .. } => {
                fold_single_constant(literal)
            }
            _ => FoldedConstant::Complex(value),
        },
        Constant::BinaryOperation {
            left, right, name, ..
        } => fold_constant_bin_op(left, right, name),
        Constant::Int { value, .. } => {
            FoldedConstant::Int(value.parse().expect("Syntax should be valid"))
        }
        Constant::Float { value, .. } => {
            FoldedConstant::Float(value.parse().expect("Syntax should be valid"))
        }
        Constant::String { value, .. } => FoldedConstant::String(value.clone()),
        Constant::Record { type_, name, .. } if type_.is_bool() && name == "True" => {
            FoldedConstant::Bool(true)
        }
        Constant::Record { type_, name, .. } if type_.is_bool() && name == "False" => {
            FoldedConstant::Bool(false)
        }
        _ => FoldedConstant::Complex(value),
    }
}
