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

use crate::{
    analyse::Inferred,
    type_::{ModuleValueConstructor, PatternConstructor, TypedCallArg, ValueConstructor},
};
use std::sync::Arc;

use ecow::EcoString;

use crate::type_::Type;

use super::{
    untyped::FunctionLiteralKind, AssignName, BinOp, BitArrayOption, CallArg, Definition, Pattern,
    SrcSpan, Statement, TodoKind, TypeAst, TypedArg, TypedAssignment, TypedClause, TypedDefinition,
    TypedExpr, TypedExprBitArraySegment, TypedFunction, TypedModule, TypedModuleConstant,
    TypedPattern, TypedPatternBitArraySegment, TypedStatement, Use,
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

    fn visit_typed_module_constant(&mut self, constant: &'ast TypedModuleConstant) {
        visit_typed_module_constant(self, constant);
    }

    fn visit_typed_expr(&mut self, expr: &'ast TypedExpr) {
        visit_typed_expr(self, expr);
    }

    fn visit_typed_expr_int(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        value: &'ast EcoString,
    ) {
        visit_typed_expr_int(self, location, type_, value);
    }

    fn visit_typed_expr_float(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        value: &'ast EcoString,
    ) {
        visit_typed_expr_float(self, location, type_, value);
    }

    fn visit_typed_expr_string(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        value: &'ast EcoString,
    ) {
        visit_typed_expr_string(self, location, type_, value);
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
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        args: &'ast [TypedArg],
        body: &'ast [TypedStatement],
        return_annotation: &'ast Option<TypeAst>,
    ) {
        visit_typed_expr_fn(self, location, type_, kind, args, body, return_annotation);
    }

    fn visit_typed_expr_list(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        elements: &'ast [TypedExpr],
        tail: &'ast Option<Box<TypedExpr>>,
    ) {
        visit_typed_expr_list(self, location, type_, elements, tail);
    }

    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        args: &'ast [TypedCallArg],
    ) {
        visit_typed_expr_call(self, location, type_, fun, args);
    }

    fn visit_typed_expr_bin_op(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        name: &'ast BinOp,
        left: &'ast TypedExpr,
        right: &'ast TypedExpr,
    ) {
        visit_typed_expr_bin_op(self, location, type_, name, left, right);
    }

    fn visit_typed_expr_case(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        subjects: &'ast [TypedExpr],
        clauses: &'ast [TypedClause],
    ) {
        visit_typed_expr_case(self, location, type_, subjects, clauses);
    }

    fn visit_typed_expr_record_access(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        label: &'ast EcoString,
        index: &'ast u64,
        record: &'ast TypedExpr,
    ) {
        visit_typed_expr_record_access(self, location, type_, label, index, record);
    }

    fn visit_typed_expr_module_select(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        label: &'ast EcoString,
        module_name: &'ast EcoString,
        module_alias: &'ast EcoString,
        constructor: &'ast ModuleValueConstructor,
    ) {
        visit_typed_expr_module_select(
            self,
            location,
            type_,
            label,
            module_name,
            module_alias,
            constructor,
        );
    }

    fn visit_typed_expr_tuple(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        elems: &'ast [TypedExpr],
    ) {
        visit_typed_expr_tuple(self, location, type_, elems);
    }

    fn visit_typed_expr_tuple_index(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        index: &'ast u64,
        tuple: &'ast TypedExpr,
    ) {
        visit_typed_expr_tuple_index(self, location, type_, index, tuple);
    }

    fn visit_typed_expr_todo(
        &mut self,
        location: &'ast SrcSpan,
        message: &'ast Option<Box<TypedExpr>>,
        kind: &'ast TodoKind,
        type_: &'ast Arc<Type>,
    ) {
        visit_typed_expr_todo(self, location, message, kind, type_);
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
        type_: &'ast Arc<Type>,
        segments: &'ast [TypedExprBitArraySegment],
    ) {
        visit_typed_expr_bit_array(self, location, type_, segments);
    }

    fn visit_typed_expr_record_update(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        record: &'ast TypedAssignment,
        constructor: &'ast TypedExpr,
        args: &'ast [TypedCallArg],
    ) {
        visit_typed_expr_record_update(self, location, type_, record, constructor, args);
    }

    fn visit_typed_expr_negate_bool(&mut self, location: &'ast SrcSpan, value: &'ast TypedExpr) {
        visit_typed_expr_negate_bool(self, location, value);
    }

    fn visit_typed_expr_negate_int(&mut self, location: &'ast SrcSpan, value: &'ast TypedExpr) {
        visit_typed_expr_negate_int(self, location, value)
    }

    fn visit_typed_expr_invalid(&mut self, location: &'ast SrcSpan, type_: &'ast Arc<Type>) {
        visit_typed_expr_invalid(self, location, type_);
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

    fn visit_typed_bit_array_option(&mut self, option: &'ast BitArrayOption<TypedExpr>) {
        visit_typed_bit_array_option(self, option);
    }

    fn visit_typed_pattern(&mut self, pattern: &'ast TypedPattern) {
        visit_typed_pattern(self, pattern);
    }

    fn visit_typed_pattern_int(&mut self, location: &'ast SrcSpan, value: &'ast EcoString) {
        visit_typed_pattern_int(self, location, value);
    }

    fn visit_typed_pattern_float(&mut self, location: &'ast SrcSpan, value: &'ast EcoString) {
        visit_typed_pattern_float(self, location, value);
    }

    fn visit_typed_pattern_string(&mut self, location: &'ast SrcSpan, value: &'ast EcoString) {
        visit_typed_pattern_string(self, location, value);
    }

    fn visit_typed_pattern_variable(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        type_: &'ast Arc<Type>,
    ) {
        visit_typed_pattern_variable(self, location, name, type_);
    }

    fn visit_typed_pattern_var_usage(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        constructor: &'ast Option<ValueConstructor>,
        type_: &'ast Arc<Type>,
    ) {
        visit_typed_pattern_var_usage(self, location, name, constructor, type_);
    }

    fn visit_typed_pattern_assign(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        pattern: &'ast TypedPattern,
    ) {
        visit_typed_pattern_assign(self, location, name, pattern);
    }

    fn visit_typed_pattern_discard(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        type_: &'ast Arc<Type>,
    ) {
        visit_typed_pattern_discard(self, location, name, type_)
    }

    fn visit_typed_pattern_list(
        &mut self,
        location: &'ast SrcSpan,
        elements: &'ast Vec<TypedPattern>,
        tail: &'ast Option<Box<TypedPattern>>,
        type_: &'ast Arc<Type>,
    ) {
        visit_typed_pattern_list(self, location, elements, tail, type_);
    }

    #[allow(clippy::too_many_arguments)]
    fn visit_typed_pattern_constructor(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        arguments: &'ast Vec<CallArg<TypedPattern>>,
        module: &'ast Option<(EcoString, SrcSpan)>,
        constructor: &'ast Inferred<PatternConstructor>,
        spread: &'ast Option<SrcSpan>,
        type_: &'ast Arc<Type>,
    ) {
        visit_typed_pattern_constructor(
            self,
            location,
            name,
            arguments,
            module,
            constructor,
            spread,
            type_,
        );
    }

    fn visit_typed_pattern_call_arg(&mut self, arg: &'ast CallArg<TypedPattern>) {
        visit_typed_pattern_call_arg(self, arg);
    }

    fn visit_typed_pattern_tuple(
        &mut self,
        location: &'ast SrcSpan,
        elems: &'ast Vec<TypedPattern>,
    ) {
        visit_typed_pattern_tuple(self, location, elems);
    }

    fn visit_typed_pattern_bit_array(
        &mut self,
        location: &'ast SrcSpan,
        segments: &'ast Vec<TypedPatternBitArraySegment>,
    ) {
        visit_typed_pattern_bit_array(self, location, segments);
    }

    fn visit_typed_pattern_string_prefix(
        &mut self,
        location: &'ast SrcSpan,
        left_location: &'ast SrcSpan,
        left_side_assignment: &'ast Option<(EcoString, SrcSpan)>,
        right_location: &'ast SrcSpan,
        left_side_string: &'ast EcoString,
        right_side_assignment: &'ast AssignName,
    ) {
        visit_typed_pattern_string_prefix(
            self,
            location,
            left_location,
            left_side_assignment,
            right_location,
            left_side_string,
            right_side_assignment,
        );
    }

    fn visit_type_ast(&mut self, node: &'ast TypeAst) {
        visit_type_ast(self, node);
    }

    fn visit_type_ast_constructor(
        &mut self,
        location: &'ast SrcSpan,
        module: &'ast Option<(EcoString, SrcSpan)>,
        name: &'ast EcoString,
        arguments: &'ast Vec<TypeAst>,
    ) {
        visit_type_ast_constructor(self, location, module, name, arguments);
    }

    fn visit_type_ast_fn(
        &mut self,
        location: &'ast SrcSpan,
        arguments: &'ast Vec<TypeAst>,
        return_: &'ast TypeAst,
    ) {
        visit_type_ast_fn(self, location, arguments, return_);
    }

    fn visit_type_ast_var(&mut self, location: &'ast SrcSpan, name: &'ast EcoString) {
        visit_type_ast_var(self, location, name);
    }

    fn visit_type_ast_tuple(&mut self, location: &'ast SrcSpan, elems: &'ast Vec<TypeAst>) {
        visit_type_ast_tuple(self, location, elems);
    }

    fn visit_type_ast_hole(&mut self, location: &'ast SrcSpan, name: &'ast EcoString) {
        visit_type_ast_hole(self, location, name);
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
        Definition::TypeAlias(_typealias) => { /* TODO */ }
        Definition::CustomType(_custom_type) => { /* TODO */ }
        Definition::Import(_import) => { /* TODO */ }
        Definition::ModuleConstant(constant) => v.visit_typed_module_constant(constant),
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

pub fn visit_type_ast<'a, V>(v: &mut V, node: &'a TypeAst)
where
    V: Visit<'a> + ?Sized,
{
    match node {
        TypeAst::Constructor(super::TypeAstConstructor {
            location,
            arguments,
            module,
            name,
        }) => {
            v.visit_type_ast_constructor(location, module, name, arguments);
        }
        TypeAst::Fn(super::TypeAstFn {
            location,
            arguments,
            return_,
        }) => {
            v.visit_type_ast_fn(location, arguments, return_);
        }
        TypeAst::Var(super::TypeAstVar { location, name }) => {
            v.visit_type_ast_var(location, name);
        }
        TypeAst::Tuple(super::TypeAstTuple { location, elems }) => {
            v.visit_type_ast_tuple(location, elems);
        }
        TypeAst::Hole(super::TypeAstHole { location, name }) => {
            v.visit_type_ast_hole(location, name);
        }
    }
}

pub fn visit_type_ast_constructor<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    _module: &'a Option<(EcoString, SrcSpan)>,
    _name: &'a EcoString,
    arguments: &'a Vec<TypeAst>,
) where
    V: Visit<'a> + ?Sized,
{
    for argument in arguments {
        v.visit_type_ast(argument);
    }
}

pub fn visit_type_ast_fn<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    arguments: &'a Vec<TypeAst>,
    return_: &'a TypeAst,
) where
    V: Visit<'a> + ?Sized,
{
    for argument in arguments {
        v.visit_type_ast(argument);
    }
    v.visit_type_ast(return_);
}

pub fn visit_type_ast_var<'a, V>(_v: &mut V, _location: &'a SrcSpan, _name: &'a EcoString)
where
    V: Visit<'a> + ?Sized,
{
    // No further traversal needed for variables
}

pub fn visit_type_ast_tuple<'a, V>(v: &mut V, _location: &'a SrcSpan, elems: &'a Vec<TypeAst>)
where
    V: Visit<'a> + ?Sized,
{
    for elem in elems {
        v.visit_type_ast(elem);
    }
}

pub fn visit_type_ast_hole<'a, V>(_v: &mut V, _location: &'a SrcSpan, _name: &'a EcoString)
where
    V: Visit<'a> + ?Sized,
{
    // No further traversal needed for holes
}

pub fn visit_typed_module_constant<'a, V>(_v: &mut V, _constant: &'a TypedModuleConstant)
where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_expr<'a, V>(v: &mut V, node: &'a TypedExpr)
where
    V: Visit<'a> + ?Sized,
{
    match node {
        TypedExpr::Int {
            location,
            type_,
            value,
            int_value: _,
        } => v.visit_typed_expr_int(location, type_, value),
        TypedExpr::Float {
            location,
            type_,
            value,
        } => v.visit_typed_expr_float(location, type_, value),
        TypedExpr::String {
            location,
            type_,
            value,
        } => v.visit_typed_expr_string(location, type_, value),
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
            type_,
            kind,
            args,
            body,
            return_annotation,
        } => v.visit_typed_expr_fn(location, type_, kind, args, body, return_annotation),
        TypedExpr::List {
            location,
            type_,
            elements,
            tail,
        } => v.visit_typed_expr_list(location, type_, elements, tail),
        TypedExpr::Call {
            location,
            type_,
            fun,
            args,
        } => v.visit_typed_expr_call(location, type_, fun, args),
        TypedExpr::BinOp {
            location,
            type_,
            name,
            left,
            right,
        } => v.visit_typed_expr_bin_op(location, type_, name, left, right),
        TypedExpr::Case {
            location,
            type_,
            subjects,
            clauses,
        } => v.visit_typed_expr_case(location, type_, subjects, clauses),
        TypedExpr::RecordAccess {
            location,
            type_,
            label,
            index,
            record,
        } => v.visit_typed_expr_record_access(location, type_, label, index, record),
        TypedExpr::ModuleSelect {
            location,
            type_,
            label,
            module_name,
            module_alias,
            constructor,
        } => v.visit_typed_expr_module_select(
            location,
            type_,
            label,
            module_name,
            module_alias,
            constructor,
        ),
        TypedExpr::Tuple {
            location,
            type_,
            elems,
        } => v.visit_typed_expr_tuple(location, type_, elems),
        TypedExpr::TupleIndex {
            location,
            type_,
            index,
            tuple,
        } => v.visit_typed_expr_tuple_index(location, type_, index, tuple),
        TypedExpr::Todo {
            location,
            message,
            kind,
            type_,
        } => v.visit_typed_expr_todo(location, message, kind, type_),
        TypedExpr::Panic {
            location,
            message,
            type_,
        } => v.visit_typed_expr_panic(location, message, type_),
        TypedExpr::BitArray {
            location,
            type_,
            segments,
        } => v.visit_typed_expr_bit_array(location, type_, segments),
        TypedExpr::RecordUpdate {
            location,
            type_,
            record,
            constructor,
            args,
        } => v.visit_typed_expr_record_update(location, type_, record, constructor, args),
        TypedExpr::NegateBool { location, value } => {
            v.visit_typed_expr_negate_bool(location, value)
        }
        TypedExpr::NegateInt { location, value } => v.visit_typed_expr_negate_int(location, value),
        TypedExpr::Invalid { location, type_ } => v.visit_typed_expr_invalid(location, type_),
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
    _kind: &'a FunctionLiteralKind,
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
    _kind: &'a TodoKind,
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
    _type: &'a Arc<Type>,
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
    _type: &'a Arc<Type>,
    record: &'a TypedAssignment,
    constructor: &'a TypedExpr,
    args: &'a [TypedCallArg],
) where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_expr(constructor);
    v.visit_typed_assignment(record);
    for arg in args {
        v.visit_typed_call_arg(arg);
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
    v.visit_typed_pattern(&assignment.pattern);
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
    for pattern in clause.pattern.iter() {
        v.visit_typed_pattern(pattern);
    }
    for patterns in clause.alternative_patterns.iter() {
        for pattern in patterns {
            v.visit_typed_pattern(pattern);
        }
    }
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

pub fn visit_typed_bit_array_option<'a, V>(v: &mut V, option: &'a BitArrayOption<TypedExpr>)
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
            value,
            short_form: _,
        } => {
            v.visit_typed_expr(value);
        }
        BitArrayOption::Unit {
            location: _,
            value: _,
        } => { /* TODO */ }
    }
}

pub fn visit_typed_pattern<'a, V>(v: &mut V, pattern: &'a TypedPattern)
where
    V: Visit<'a> + ?Sized,
{
    match pattern {
        Pattern::Int {
            location,
            value,
            int_value: _,
        } => v.visit_typed_pattern_int(location, value),
        Pattern::Float { location, value } => v.visit_typed_pattern_float(location, value),
        Pattern::String { location, value } => v.visit_typed_pattern_string(location, value),
        Pattern::Variable {
            location,
            name,
            type_,
        } => v.visit_typed_pattern_variable(location, name, type_),
        Pattern::VarUsage {
            location,
            name,
            constructor,
            type_,
        } => v.visit_typed_pattern_var_usage(location, name, constructor, type_),
        Pattern::Assign {
            location,
            name,
            pattern,
        } => v.visit_typed_pattern_assign(location, name, pattern),
        Pattern::Discard {
            location,
            name,
            type_,
        } => v.visit_typed_pattern_discard(location, name, type_),
        Pattern::List {
            location,
            elements,
            tail,
            type_,
        } => v.visit_typed_pattern_list(location, elements, tail, type_),
        Pattern::Constructor {
            location,
            name,
            arguments,
            module,
            constructor,
            spread,
            type_,
        } => v.visit_typed_pattern_constructor(
            location,
            name,
            arguments,
            module,
            constructor,
            spread,
            type_,
        ),
        Pattern::Tuple { location, elems } => v.visit_typed_pattern_tuple(location, elems),
        Pattern::BitArray { location, segments } => {
            v.visit_typed_pattern_bit_array(location, segments)
        }
        Pattern::StringPrefix {
            location,
            left_location,
            left_side_assignment,
            right_location,
            left_side_string,
            right_side_assignment,
        } => v.visit_typed_pattern_string_prefix(
            location,
            left_location,
            left_side_assignment,
            right_location,
            left_side_string,
            right_side_assignment,
        ),
        Pattern::Invalid { location, type_ } => v.visit_typed_expr_invalid(location, type_),
    }
}

fn visit_typed_pattern_int<'a, V>(_v: &mut V, _location: &'a SrcSpan, _value: &'a EcoString)
where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_pattern_float<'a, V>(_v: &mut V, _location: &'a SrcSpan, _value: &'a EcoString)
where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_pattern_string<'a, V>(_v: &mut V, _location: &'a SrcSpan, _value: &'a EcoString)
where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_pattern_variable<'a, V>(
    _v: &mut V,
    _location: &'a SrcSpan,
    _name: &'a EcoString,
    _type: &'a Arc<Type>,
) where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_pattern_var_usage<'a, V>(
    _v: &mut V,
    _location: &'a SrcSpan,
    _name: &'a EcoString,
    _constructor: &'a Option<ValueConstructor>,
    _type: &'a Arc<Type>,
) where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_pattern_assign<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    _name: &'a EcoString,
    pattern: &'a TypedPattern,
) where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_pattern(pattern);
}

pub fn visit_typed_pattern_discard<'a, V>(
    _v: &mut V,
    _location: &'a SrcSpan,
    _name: &'a EcoString,
    _type: &'a Arc<Type>,
) where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_pattern_list<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    elements: &'a Vec<TypedPattern>,
    tail: &'a Option<Box<TypedPattern>>,
    _type: &'a Arc<Type>,
) where
    V: Visit<'a> + ?Sized,
{
    for element in elements {
        v.visit_typed_pattern(element);
    }
    if let Some(tail) = tail {
        v.visit_typed_pattern(tail);
    }
}

#[allow(clippy::too_many_arguments)]
pub fn visit_typed_pattern_constructor<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    _name: &'a EcoString,
    arguments: &'a Vec<CallArg<TypedPattern>>,
    _module: &'a Option<(EcoString, SrcSpan)>,
    _constructor: &'a Inferred<PatternConstructor>,
    _spread: &'a Option<SrcSpan>,
    _type: &'a Arc<Type>,
) where
    V: Visit<'a> + ?Sized,
{
    for argument in arguments {
        v.visit_typed_pattern_call_arg(argument);
    }
}

pub fn visit_typed_pattern_call_arg<'a, V>(v: &mut V, argument: &'a CallArg<TypedPattern>)
where
    V: Visit<'a> + ?Sized,
{
    v.visit_typed_pattern(&argument.value)
}

pub fn visit_typed_pattern_tuple<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    elems: &'a Vec<TypedPattern>,
) where
    V: Visit<'a> + ?Sized,
{
    for elem in elems {
        v.visit_typed_pattern(elem);
    }
}

pub fn visit_typed_pattern_bit_array<'a, V>(
    v: &mut V,
    _location: &'a SrcSpan,
    segments: &'a Vec<TypedPatternBitArraySegment>,
) where
    V: Visit<'a> + ?Sized,
{
    for segment in segments {
        v.visit_typed_pattern(&segment.value);
    }
}

pub fn visit_typed_pattern_string_prefix<'a, V>(
    _v: &mut V,
    _location: &'a SrcSpan,
    _left_location: &'a SrcSpan,
    _left_side_assignment: &'a Option<(EcoString, SrcSpan)>,
    _right_location: &'a SrcSpan,
    _left_side_string: &'a EcoString,
    _right_side_assignment: &'a AssignName,
) where
    V: Visit<'a> + ?Sized,
{
}

pub fn visit_typed_expr_invalid<'a, V>(_v: &mut V, _location: &'a SrcSpan, _typ: &'a Arc<Type>)
where
    V: Visit<'a> + ?Sized,
{
}
