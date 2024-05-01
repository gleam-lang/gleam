//! AST traversal routines, referenced from [`syn::visit`](https://docs.rs/syn/latest/syn/visit/index.html)
//!
//! Each method of the [`Visit`] trait can be overriden to customize the
//! behaviour when visiting the corresponding type of AST node. By default,
//! every method recursively visits the substructure of the node by using the
//! right visitor method.
//!
//! # Example
//!
//! Suppose we would like to collect all function names in a module,
//! we can do the following:
//!
//! ```no_run
//! use gleam_core::ast::{TypedFunction, visit::{self, Visit}};
//!
//! struct FnCollector<'ast> {
//!     functions: Vec<&'ast TypedFunction>
//! }
//!
//! impl<'ast> Visit<'ast> for FnCollector<'ast> {
//!     fn visit_typed_function(&mut self, fun: &'ast TypedFunction) {
//!         self.functions.push(fun);
//!
//!         // Use the default behaviour to visit any nested functions
//!         visit::visit_typed_function(self, fun);
//!     }
//! }
//!
//! fn print_all_module_functions() {
//!     let module = todo!("module");
//!     let mut fn_collector = FnCollector { functions: vec![] };
//!
//!     // This will walk the AST and collect all functions
//!     fn_collector.visit_typed_module(module);
//!
//!     // Print the collected functions
//!     println!("{:#?}", fn_collector.functions);
//! }
//! ```

use crate::type_::{ModuleValueConstructor, TypedCallArg, ValueConstructor};
use std::sync::Arc;

use ecow::EcoString;

use crate::type_::Type;

use super::{
    BinOp, BitArrayOption, Definition, SrcSpan, Statement, TypeAst, TypedArg, TypedAssignment,
    TypedClause, TypedDefinition, TypedExpr, TypedExprBitArraySegment, TypedFunction, TypedModule,
    TypedRecordUpdateArg, TypedStatement, Use,
};

pub trait Visit<'ast> {
    fn visit_typed_module(&mut self, module: &'ast TypedModule) {
        visit_typed_module(self, module);
    }

    fn visit_typed_definition(&mut self, def: &'ast TypedDefinition) {
        visit_typed_definition(self, def);
    }

    fn visit_typed_function(&mut self, fun: &'ast TypedFunction) {
        visit_typed_function(self, fun);
    }

    fn visit_typed_expr(&mut self, expr: &'ast TypedExpr) {
        visit_typed_expr(self, expr);
    }

    fn visit_typed_expr_int(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        value: &'ast EcoString,
    ) {
        visit_typed_expr_int(self, location, typ, value);
    }

    fn visit_typed_expr_float(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        value: &'ast EcoString,
    ) {
        visit_typed_expr_float(self, location, typ, value);
    }

    fn visit_typed_expr_string(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        value: &'ast EcoString,
    ) {
        visit_typed_expr_string(self, location, typ, value);
    }

    fn visit_typed_expr_block(
        &mut self,
        location: &'ast SrcSpan,
        statements: &'ast [TypedStatement],
    ) {
        visit_typed_expr_block(self, location, statements);
    }

    fn visit_typed_expr_pipeline(
        &mut self,
        location: &'ast SrcSpan,
        assignments: &'ast [TypedAssignment],
        finally: &'ast TypedExpr,
    ) {
        visit_typed_expr_pipeline(self, location, assignments, finally);
    }

    fn visit_typed_expr_var(
        &mut self,
        location: &'ast SrcSpan,
        constructor: &'ast ValueConstructor,
        name: &'ast EcoString,
    ) {
        visit_typed_expr_var(self, location, constructor, name);
    }

    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        is_capture: &'ast bool,
        args: &'ast [TypedArg],
        body: &'ast [TypedStatement],
        return_annotation: &'ast Option<TypeAst>,
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
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        elements: &'ast [TypedExpr],
        tail: &'ast Option<Box<TypedExpr>>,
    ) {
        visit_typed_expr_list(self, location, typ, elements, tail);
    }

    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        args: &'ast [TypedCallArg],
    ) {
        visit_typed_expr_call(self, location, typ, fun, args);
    }

    fn visit_typed_expr_bin_op(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        name: &'ast BinOp,
        left: &'ast TypedExpr,
        right: &'ast TypedExpr,
    ) {
        visit_typed_expr_bin_op(self, location, typ, name, left, right);
    }

    fn visit_typed_expr_case(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        subjects: &'ast [TypedExpr],
        clauses: &'ast [TypedClause],
    ) {
        visit_typed_expr_case(self, location, typ, subjects, clauses);
    }

    fn visit_typed_expr_record_access(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        label: &'ast EcoString,
        index: &'ast u64,
        record: &'ast TypedExpr,
    ) {
        visit_typed_expr_record_access(self, location, typ, label, index, record);
    }

    fn visit_typed_expr_module_select(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        label: &'ast EcoString,
        module_name: &'ast EcoString,
        module_alias: &'ast EcoString,
        constructor: &'ast ModuleValueConstructor,
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
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        elems: &'ast [TypedExpr],
    ) {
        visit_typed_expr_tuple(self, location, typ, elems);
    }

    fn visit_typed_expr_tuple_index(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        index: &'ast u64,
        tuple: &'ast TypedExpr,
    ) {
        visit_typed_expr_tuple_index(self, location, typ, index, tuple);
    }

    fn visit_typed_expr_todo(
        &mut self,
        location: &'ast SrcSpan,
        message: &'ast Option<Box<TypedExpr>>,
        type_: &'ast Arc<Type>,
    ) {
        visit_typed_expr_todo(self, location, message, type_);
    }

    fn visit_typed_expr_panic(
        &mut self,
        location: &'ast SrcSpan,
        message: &'ast Option<Box<TypedExpr>>,
        type_: &'ast Arc<Type>,
    ) {
        visit_typed_expr_panic(self, location, message, type_);
    }

    fn visit_typed_expr_bit_array(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        segments: &'ast [TypedExprBitArraySegment],
    ) {
        visit_typed_expr_bit_array(self, location, typ, segments);
    }

    fn visit_typed_expr_record_update(
        &mut self,
        location: &'ast SrcSpan,
        typ: &'ast Arc<Type>,
        spread: &'ast TypedExpr,
        args: &'ast [TypedRecordUpdateArg],
    ) {
        visit_typed_expr_record_update(self, location, typ, spread, args);
    }

    fn visit_typed_expr_negate_bool(&mut self, location: &'ast SrcSpan, value: &'ast TypedExpr) {
        visit_typed_expr_negate_bool(self, location, value);
    }

    fn visit_typed_expr_negate_int(&mut self, location: &'ast SrcSpan, value: &'ast TypedExpr) {
        visit_typed_expr_negate_int(self, location, value)
    }

    fn visit_typed_statement(&mut self, stmt: &'ast TypedStatement) {
        visit_typed_statement(self, stmt);
    }

    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        visit_typed_assignment(self, assignment);
    }

    fn visit_use(&mut self, use_: &'ast Use) {
        visit_use(self, use_);
    }

    fn visit_typed_call_arg(&mut self, arg: &'ast TypedCallArg) {
        visit_typed_call_arg(self, arg);
    }

    fn visit_typed_clause(&mut self, clause: &'ast TypedClause) {
        visit_typed_clause(self, clause);
    }

    fn visit_typed_expr_bit_array_segment(&mut self, segment: &'ast TypedExprBitArraySegment) {
        visit_typed_expr_bit_array_segment(self, segment);
    }

    fn visit_typed_record_update_arg(&mut self, arg: &'ast TypedRecordUpdateArg) {
        visit_typed_record_update_arg(self, arg);
    }

    fn visit_typed_bit_array_option(&mut self, option: &'ast BitArrayOption<TypedExpr>) {
        visit_typed_bit_array_option(self, option);
    }
}

pub fn visit_typed_module<'a, V>(v: &mut V, module: &'a TypedModule)
where
    V: Visit<'a> + ?Sized,
{
    for def in &module.definitions {
        v.visit_typed_definition(def);
    }
}

pub fn visit_typed_definition<'a, V>(v: &mut V, def: &'a TypedDefinition)
where
    V: Visit<'a> + ?Sized,
{
    match def {
        Definition::Function(fun) => v.visit_typed_function(fun),
        Definition::TypeAlias(_type_alias) => { /* TODO */ }
        Definition::CustomType(_custom_type) => { /* TODO */ }
        Definition::Import(_import) => { /* TODO */ }
        Definition::ModuleConstant(_module_constant) => { /* TODO */ }
    }
}
pub fn visit_typed_function<'a, V>(v: &mut V, fun: &'a TypedFunction)
where
    V: Visit<'a> + ?Sized,
{
    for stmt in &fun.body {
        v.visit_typed_statement(stmt);
    }
}

pub fn visit_typed_expr<'a, V>(v: &mut V, node: &'a TypedExpr)
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
    _v: &mut V,
    _location: &'a SrcSpan,
    _typ: &'a Arc<Type>,
    _value: &'a EcoString,
) where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_expr_float<'a, V>(
    _v: &mut V,
    _location: &'a SrcSpan,
    _typ: &'a Arc<Type>,
    _value: &'a EcoString,
) where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_expr_string<'a, V>(
    _v: &mut V,
    _location: &'a SrcSpan,
    _typ: &'a Arc<Type>,
    _value: &'a EcoString,
) where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_expr_block<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    statements: &'a [TypedStatement],
) where
    V: Visit<'a> + ?Sized,
{
    for stmt in statements {
        v.visit_typed_statement(stmt);
    }
}

pub fn visit_typed_expr_pipeline<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    assignments: &'a [TypedAssignment],
    finally: &'a TypedExpr,
) where
    V: Visit<'a> + ?Sized,
{
    for assignment in assignments {
        v.visit_typed_assignment(assignment);
    }

    v.visit_typed_expr(finally);
}

pub fn visit_typed_expr_var<'a, V>(
    _v: &mut V,
    _location: &'a SrcSpan,
    _constructor: &'a ValueConstructor,
    _name: &'a EcoString,
) where
    V: Visit<'a> + ?Sized,
{
    /* TODO */
}

pub fn visit_typed_expr_fn<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    _typ: &'a Arc<Type>,
    _is_capture: &'a bool,
    _args: &'a [TypedArg],
    body: &'a [TypedStatement],
    _return_annotation: &'a Option<TypeAst>,
) where
    V: Visit<'a> + ?Sized,
{
    for stmt in body {
        v.visit_typed_statement(stmt);
    }
}

pub fn visit_typed_expr_list<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    _typ: &'a Arc<Type>,
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
    v: &mut V,
    _location: &'a SrcSpan,
    _typ: &'a Arc<Type>,
    fun: &'a TypedExpr,
    args: &'a [TypedCallArg],
) where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(fun);
    for arg in args {
        v.visit_typed_call_arg(arg);
    }
}

pub fn visit_typed_expr_bin_op<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    _typ: &'a Arc<Type>,
    _name: &'a BinOp,
    left: &'a TypedExpr,
    right: &'a TypedExpr,
) where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(left);
    v.visit_typed_expr(right);
}

pub fn visit_typed_expr_case<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    _typ: &'a Arc<Type>,
    subjects: &'a [TypedExpr],
    clauses: &'a [TypedClause],
) where
    V: Visit<'a> + ?Sized,
{
    for subject in subjects {
        v.visit_typed_expr(subject);
    }

    for clause in clauses {
        v.visit_typed_clause(clause);
    }
}

pub fn visit_typed_expr_record_access<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    _typ: &'a Arc<Type>,
    _label: &'a EcoString,
    _index: &'a u64,
    record: &'a TypedExpr,
) where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(record);
}

pub fn visit_typed_expr_module_select<'a, V>(
    _v: &mut V,
    _location: &'a SrcSpan,
    _typ: &'a Arc<Type>,
    _label: &'a EcoString,
    _module_name: &'a EcoString,
    _module_alias: &'a EcoString,
    _constructor: &'a ModuleValueConstructor,
) where
    V: Visit<'a> + ?Sized,
{
    /* TODO */
}

pub fn visit_typed_expr_tuple<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    _typ: &'a Arc<Type>,
    elems: &'a [TypedExpr],
) where
    V: Visit<'a> + ?Sized,
{
    for elem in elems {
        v.visit_typed_expr(elem);
    }
}

pub fn visit_typed_expr_tuple_index<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    _typ: &'a Arc<Type>,
    _index: &'a u64,
    tuple: &'a TypedExpr,
) where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(tuple);
}

pub fn visit_typed_expr_todo<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    message: &'a Option<Box<TypedExpr>>,
    _type_: &'a Arc<Type>,
) where
    V: Visit<'a> + ?Sized,
{
    if let Some(message) = message {
        v.visit_typed_expr(message);
    }
}

pub fn visit_typed_expr_panic<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    message: &'a Option<Box<TypedExpr>>,
    _type_: &'a Arc<Type>,
) where
    V: Visit<'a> + ?Sized,
{
    if let Some(message) = message {
        v.visit_typed_expr(message);
    }
}

pub fn visit_typed_expr_bit_array<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    _typ: &'a Arc<Type>,
    segments: &'a [TypedExprBitArraySegment],
) where
    V: Visit<'a> + ?Sized,
{
    for segment in segments {
        v.visit_typed_expr_bit_array_segment(segment);
    }
}

pub fn visit_typed_expr_record_update<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    _typ: &'a Arc<Type>,
    spread: &'a TypedExpr,
    args: &'a [TypedRecordUpdateArg],
) where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(spread);
    for arg in args {
        v.visit_typed_record_update_arg(arg);
    }
}

pub fn visit_typed_expr_negate_bool<'a, V>(v: &mut V, _location: &'a SrcSpan, value: &'a TypedExpr)
where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(value);
}

pub fn visit_typed_expr_negate_int<'a, V>(v: &mut V, _location: &'a SrcSpan, value: &'a TypedExpr)
where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(value);
}

pub fn visit_typed_statement<'a, V>(v: &mut V, stmt: &'a TypedStatement)
where
    V: Visit<'a> + ?Sized,
{
    match stmt {
        Statement::Expression(expr) => v.visit_typed_expr(expr),
        Statement::Assignment(assignment) => v.visit_typed_assignment(assignment),
        Statement::Use(use_) => v.visit_use(use_),
    }
}

pub fn visit_typed_assignment<'a, V>(v: &mut V, assignment: &'a TypedAssignment)
where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(&assignment.value);
}

pub fn visit_use<'a, V>(_v: &mut V, _use_: &'a Use)
where
    V: Visit<'a> + ?Sized,
{
    /* TODO */
}

pub fn visit_typed_call_arg<'a, V>(v: &mut V, arg: &'a TypedCallArg)
where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(&arg.value);
}

pub fn visit_typed_clause<'a, V>(v: &mut V, clause: &'a TypedClause)
where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(&clause.then);
}

pub fn visit_typed_expr_bit_array_segment<'a, V>(v: &mut V, segment: &'a TypedExprBitArraySegment)
where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(&segment.value);
    for option in &segment.options {
        v.visit_typed_bit_array_option(option);
    }
}

pub fn visit_typed_record_update_arg<'a, V>(v: &mut V, arg: &'a TypedRecordUpdateArg)
where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(&arg.value);
}

pub fn visit_typed_bit_array_option<'a, V>(_v: &mut V, option: &'a BitArrayOption<TypedExpr>)
where
    V: Visit<'a> + ?Sized,
{
    match option {
        BitArrayOption::Bytes { location: _ } => { /* TODO */ }
        BitArrayOption::Int { location: _ } => { /* TODO */ }
        BitArrayOption::Float { location: _ } => { /* TODO */ }
        BitArrayOption::Bits { location: _ } => { /* TODO */ }
        BitArrayOption::Utf8 { location: _ } => { /* TODO */ }
        BitArrayOption::Utf16 { location: _ } => { /* TODO */ }
        BitArrayOption::Utf32 { location: _ } => { /* TODO */ }
        BitArrayOption::Utf8Codepoint { location: _ } => { /* TODO */ }
        BitArrayOption::Utf16Codepoint { location: _ } => { /* TODO */ }
        BitArrayOption::Utf32Codepoint { location: _ } => { /* TODO */ }
        BitArrayOption::Signed { location: _ } => { /* TODO */ }
        BitArrayOption::Unsigned { location: _ } => { /* TODO */ }
        BitArrayOption::Big { location: _ } => { /* TODO */ }
        BitArrayOption::Little { location: _ } => { /* TODO */ }
        BitArrayOption::Native { location: _ } => { /* TODO */ }
        BitArrayOption::Size {
            location: _,
            value: _,
            short_form: _,
        } => { /* TODO */ }
        BitArrayOption::Unit {
            location: _,
            value: _,
        } => { /* TODO */ }
    }
}
