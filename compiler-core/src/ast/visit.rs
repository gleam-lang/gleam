#![allow(unused_variables)]

use crate::{
    ast::TypedAssignment,
    type_::{ModuleValueConstructor, TypedCallArg, ValueConstructor},
};
use std::sync::Arc;

use ecow::EcoString;

use crate::type_::Type;

use super::{
    BinOp, SrcSpan, TypeAst, TypedArg, TypedClause, TypedExpr, TypedExprBitArraySegment,
    TypedRecordUpdateArg, TypedStatement,
};

pub trait Visit<'a> {
    fn visit_typed_expr(&self, node: &'a TypedExpr) {
        visit_typed_expr(self, node);
    }

    fn visit_typed_expr_int(
        &self,
        location: &'a SrcSpan,
        typ: &'a Arc<Type>,
        value: &'a EcoString,
    ) {
        visit_typed_expr_int(self, location, typ, value);
    }

    fn visit_typed_expr_float(
        &self,
        location: &'a SrcSpan,
        typ: &'a Arc<Type>,
        value: &'a EcoString,
    ) {
        visit_typed_expr_float(self, location, typ, value);
    }

    fn visit_typed_expr_string(
        &self,
        location: &'a SrcSpan,
        typ: &'a Arc<Type>,
        value: &'a EcoString,
    ) {
        visit_typed_expr_string(self, location, typ, value);
    }

    fn visit_typed_expr_block(&self, location: &'a SrcSpan, statements: &'a [TypedStatement]) {
        visit_typed_expr_block(self, location, statements);
    }

    fn visit_typed_expr_pipeline(
        &self,
        location: &'a SrcSpan,
        assignments: &'a [TypedAssignment],
        finally: &'a TypedExpr,
    ) {
        visit_typed_expr_pipeline(self, location, assignments, finally);
    }

    fn visit_typed_expr_var(
        &self,
        location: &'a SrcSpan,
        constructor: &'a ValueConstructor,
        name: &'a EcoString,
    ) {
        visit_typed_expr_var(self, location, constructor, name);
    }

    fn visit_typed_expr_fn(
        &self,
        location: &'a SrcSpan,
        typ: &'a Arc<Type>,
        is_capture: &'a bool,
        args: &'a [TypedArg],
        body: &'a [TypedStatement],
        return_annotation: &'a Option<TypeAst>,
    ) {
        visit_typed_expr_fn(
            self,
            location,
            typ,
            is_capture,
            args,
            body,
            return_annotation,
        );
    }

    fn visit_typed_expr_list(
        &self,
        location: &'a SrcSpan,
        typ: &'a Arc<Type>,
        elements: &'a [TypedExpr],
        tail: &'a Option<Box<TypedExpr>>,
    ) {
        visit_typed_expr_list(self, location, typ, elements, tail);
    }

    fn visit_typed_expr_call(
        &self,
        location: &'a SrcSpan,
        typ: &'a Arc<Type>,
        fun: &'a TypedExpr,
        args: &'a [TypedCallArg],
    ) {
        visit_typed_expr_call(self, location, typ, fun, args);
    }

    fn visit_typed_expr_bin_op(
        &self,
        location: &'a SrcSpan,
        typ: &'a Arc<Type>,
        name: &'a BinOp,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
    ) {
        visit_typed_expr_bin_op(self, location, typ, name, left, right);
    }

    fn visit_typed_expr_case(
        &self,
        location: &'a SrcSpan,
        typ: &'a Arc<Type>,
        subjects: &'a [TypedExpr],
        clauses: &'a [TypedClause],
    ) {
        visit_typed_expr_case(self, location, typ, subjects, clauses);
    }

    fn visit_typed_expr_record_access(
        &self,
        location: &'a SrcSpan,
        typ: &'a Arc<Type>,
        label: &'a EcoString,
        index: &'a u64,
        record: &'a TypedExpr,
    ) {
        visit_typed_expr_record_access(self, location, typ, label, index, record);
    }

    fn visit_typed_expr_module_select(
        &self,
        location: &'a SrcSpan,
        typ: &'a Arc<Type>,
        label: &'a EcoString,
        module_name: &'a EcoString,
        module_alias: &'a EcoString,
        constructor: &'a ModuleValueConstructor,
    ) {
        visit_typed_expr_module_select(
            self,
            location,
            typ,
            label,
            module_name,
            module_alias,
            constructor,
        );
    }

    fn visit_typed_expr_tuple(
        &self,
        location: &'a SrcSpan,
        typ: &'a Arc<Type>,
        elems: &'a [TypedExpr],
    ) {
        visit_typed_expr_tuple(self, location, typ, elems);
    }

    fn visit_typed_expr_tuple_index(
        &self,
        location: &'a SrcSpan,
        typ: &'a Arc<Type>,
        index: &'a u64,
        tuple: &'a TypedExpr,
    ) {
        visit_typed_expr_tuple_index(self, location, typ, index, tuple);
    }

    fn visit_typed_expr_todo(
        &self,
        location: &'a SrcSpan,
        message: &'a Option<Box<TypedExpr>>,
        type_: &'a Arc<Type>,
    ) {
        visit_typed_expr_todo(self, location, message, type_);
    }

    fn visit_typed_expr_panic(
        &self,
        location: &'a SrcSpan,
        message: &'a Option<Box<TypedExpr>>,
        type_: &'a Arc<Type>,
    ) {
        visit_typed_expr_panic(self, location, message, type_);
    }

    fn visit_typed_expr_bit_array(
        &self,
        location: &'a SrcSpan,
        typ: &'a Arc<Type>,
        segments: &'a [TypedExprBitArraySegment],
    ) {
        visit_typed_expr_bit_array(self, location, typ, segments);
    }

    fn visit_typed_expr_record_update(
        &self,
        location: &'a SrcSpan,
        typ: &'a Arc<Type>,
        spread: &'a TypedExpr,
        args: &'a [TypedRecordUpdateArg],
    ) {
        visit_typed_expr_record_update(self, location, typ, spread, args);
    }

    fn visit_typed_expr_negate_bool(&self, location: &'a SrcSpan, value: &'a TypedExpr) {
        visit_typed_expr_negate_bool(self, location, value);
    }

    fn visit_typed_expr_negate_int(&self, location: &'a SrcSpan, value: &'a TypedExpr) {
        visit_typed_expr_negate_int(self, location, value)
    }
}

pub fn visit_typed_expr<'a, V>(v: &V, node: &'a TypedExpr)
where
    V: Visit<'a> + ?Sized,
{
    match node {
        TypedExpr::Int {
            location,
            typ,
            value,
        } => v.visit_typed_expr_int(location, typ, value),
        TypedExpr::Float {
            location,
            typ,
            value,
        } => v.visit_typed_expr_float(location, typ, value),
        TypedExpr::String {
            location,
            typ,
            value,
        } => v.visit_typed_expr_string(location, typ, value),
        TypedExpr::Block {
            location,
            statements,
        } => v.visit_typed_expr_block(location, statements),
        TypedExpr::Pipeline {
            location,
            assignments,
            finally,
        } => v.visit_typed_expr_pipeline(location, assignments, finally),
        TypedExpr::Var {
            location,
            constructor,
            name,
        } => v.visit_typed_expr_var(location, constructor, name),
        TypedExpr::Fn {
            location,
            typ,
            is_capture,
            args,
            body,
            return_annotation,
        } => v.visit_typed_expr_fn(location, typ, is_capture, args, body, return_annotation),
        TypedExpr::List {
            location,
            typ,
            elements,
            tail,
        } => v.visit_typed_expr_list(location, typ, elements, tail),
        TypedExpr::Call {
            location,
            typ,
            fun,
            args,
        } => v.visit_typed_expr_call(location, typ, fun, args),
        TypedExpr::BinOp {
            location,
            typ,
            name,
            left,
            right,
        } => v.visit_typed_expr_bin_op(location, typ, name, left, right),
        TypedExpr::Case {
            location,
            typ,
            subjects,
            clauses,
        } => v.visit_typed_expr_case(location, typ, subjects, clauses),
        TypedExpr::RecordAccess {
            location,
            typ,
            label,
            index,
            record,
        } => v.visit_typed_expr_record_access(location, typ, label, index, record),
        TypedExpr::ModuleSelect {
            location,
            typ,
            label,
            module_name,
            module_alias,
            constructor,
        } => v.visit_typed_expr_module_select(
            location,
            typ,
            label,
            module_name,
            module_alias,
            constructor,
        ),
        TypedExpr::Tuple {
            location,
            typ,
            elems,
        } => v.visit_typed_expr_tuple(location, typ, elems),
        TypedExpr::TupleIndex {
            location,
            typ,
            index,
            tuple,
        } => v.visit_typed_expr_tuple_index(location, typ, index, tuple),
        TypedExpr::Todo {
            location,
            message,
            type_,
        } => v.visit_typed_expr_todo(location, message, type_),
        TypedExpr::Panic {
            location,
            message,
            type_,
        } => v.visit_typed_expr_panic(location, message, type_),
        TypedExpr::BitArray {
            location,
            typ,
            segments,
        } => v.visit_typed_expr_bit_array(location, typ, segments),
        TypedExpr::RecordUpdate {
            location,
            typ,
            spread,
            args,
        } => v.visit_typed_expr_record_update(location, typ, spread, args),
        TypedExpr::NegateBool { location, value } => {
            v.visit_typed_expr_negate_bool(location, value)
        }
        TypedExpr::NegateInt { location, value } => v.visit_typed_expr_negate_int(location, value),
    }
}

pub fn visit_typed_expr_int<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    typ: &'a Arc<Type>,
    value: &'a EcoString,
) where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_expr_float<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    typ: &'a Arc<Type>,
    value: &'a EcoString,
) where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_expr_string<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    typ: &'a Arc<Type>,
    value: &'a EcoString,
) where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_expr_block<'a, V>(v: &V, location: &'a SrcSpan, statements: &'a [TypedStatement])
where
    V: Visit<'a> + ?Sized,
{
    for stmt in statements {
        todo!()
    }
}

pub fn visit_typed_expr_pipeline<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    assignments: &'a [TypedAssignment],
    finally: &'a TypedExpr,
) where
    V: Visit<'a> + ?Sized,
{
    for assignment in assignments {
        todo!();
    }

    v.visit_typed_expr(finally);
}

pub fn visit_typed_expr_var<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    constructor: &'a ValueConstructor,
    name: &'a EcoString,
) where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_expr_fn<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    typ: &'a Arc<Type>,
    is_capture: &'a bool,
    args: &'a [TypedArg],
    body: &'a [TypedStatement],
    return_annotation: &'a Option<TypeAst>,
) where
    V: Visit<'a> + ?Sized,
{
    for stmt in body {
        todo!()
    }
}

pub fn visit_typed_expr_list<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    typ: &'a Arc<Type>,
    elements: &'a [TypedExpr],
    tail: &'a Option<Box<TypedExpr>>,
) where
    V: Visit<'a> + ?Sized,
{
    for element in elements {
        v.visit_typed_expr(element);
    }

    if let Some(tail) = tail {
        v.visit_typed_expr(tail);
    }
}

pub fn visit_typed_expr_call<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    typ: &'a Arc<Type>,
    fun: &'a TypedExpr,
    args: &'a [TypedCallArg],
) where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(fun);
    for arg in args {
        todo!()
    }
}

pub fn visit_typed_expr_bin_op<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    typ: &'a Arc<Type>,
    name: &'a BinOp,
    left: &'a TypedExpr,
    right: &'a TypedExpr,
) where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(left);
    v.visit_typed_expr(right);
}

pub fn visit_typed_expr_case<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    typ: &'a Arc<Type>,
    subjects: &'a [TypedExpr],
    clauses: &'a [TypedClause],
) where
    V: Visit<'a> + ?Sized,
{
    for subject in subjects {
        v.visit_typed_expr(subject);
    }

    for clause in clauses {
        todo!()
    }
}

pub fn visit_typed_expr_record_access<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    typ: &'a Arc<Type>,
    label: &'a EcoString,
    index: &'a u64,
    record: &'a TypedExpr,
) where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(record);
}

pub fn visit_typed_expr_module_select<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    typ: &'a Arc<Type>,
    label: &'a EcoString,
    module_name: &'a EcoString,
    module_alias: &'a EcoString,
    constructor: &'a ModuleValueConstructor,
) where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_expr_tuple<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    typ: &'a Arc<Type>,
    elems: &'a [TypedExpr],
) where
    V: Visit<'a> + ?Sized,
{
    for elem in elems {
        v.visit_typed_expr(elem);
    }
}

pub fn visit_typed_expr_tuple_index<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    typ: &'a Arc<Type>,
    index: &'a u64,
    tuple: &'a TypedExpr,
) where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(tuple);
}

pub fn visit_typed_expr_todo<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    message: &'a Option<Box<TypedExpr>>,
    type_: &'a Arc<Type>,
) where
    V: Visit<'a> + ?Sized,
{
    if let Some(message) = message {
        v.visit_typed_expr(message);
    }
}

pub fn visit_typed_expr_panic<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    message: &'a Option<Box<TypedExpr>>,
    type_: &'a Arc<Type>,
) where
    V: Visit<'a> + ?Sized,
{
    if let Some(message) = message {
        v.visit_typed_expr(message);
    }
}

pub fn visit_typed_expr_bit_array<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    typ: &'a Arc<Type>,
    segments: &'a [TypedExprBitArraySegment],
) where
    V: Visit<'a> + ?Sized,
{
    for segment in segments {
        todo!()
    }
}

pub fn visit_typed_expr_record_update<'a, V>(
    v: &V,
    location: &'a SrcSpan,
    typ: &'a Arc<Type>,
    spread: &'a TypedExpr,
    args: &'a [TypedRecordUpdateArg],
) where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(spread);
    for arg in args {
        todo!()
    }
}

pub fn visit_typed_expr_negate_bool<'a, V>(v: &V, location: &'a SrcSpan, value: &'a TypedExpr)
where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(value);
}

pub fn visit_typed_expr_negate_int<'a, V>(v: &V, location: &'a SrcSpan, value: &'a TypedExpr)
where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(value);
}
