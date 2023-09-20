use smol_str::SmolStr;
use vec1::Vec1;

use crate::ast::{
    Assignment, BinOp, CallArg, RecordUpdateSpread, SrcSpan, Statement, TodoKind, TypeAst,
    TypeAstConstructor, TypeAstFn, TypeAstHole, TypeAstTuple, TypeAstVar, UntypedArg,
    UntypedAssignment, UntypedClause, UntypedExpr, UntypedExprBitArraySegment,
    UntypedRecordUpdateArg, UntypedStatement, Use, UseAssignment,
};

pub trait TypeAstFolder {
    /// Visit a node and potentially replace it with another node using the
    /// `fold_*` methods. Afterwards, the `walk` method is called on the new
    /// node to continue traversing.
    ///
    /// You probably don't want to override this method.
    fn fold_type(&mut self, t: TypeAst) -> TypeAst {
        let t = self.update_type(t);
        self.walk_type(t)
    }

    /// You probably don't want to override this method.
    fn update_type(&mut self, t: TypeAst) -> TypeAst {
        match t {
            TypeAst::Constructor(c) => self.fold_type_constructor(c),
            TypeAst::Fn(f) => self.fold_type_fn(f),
            TypeAst::Var(v) => self.fold_type_var(v),
            TypeAst::Tuple(t) => self.fold_type_tuple(t),
            TypeAst::Hole(h) => self.fold_type_hole(h),
        }
    }

    /// You probably don't want to override this method.
    fn walk_type(&mut self, t: TypeAst) -> TypeAst {
        match t {
            TypeAst::Constructor(mut c) => {
                c.arguments = self.fold_all_types(c.arguments);
                TypeAst::Constructor(c)
            }

            TypeAst::Fn(mut f) => {
                f.arguments = self.fold_all_types(f.arguments);
                f.return_ = Box::new(self.fold_type(*f.return_));
                TypeAst::Fn(f)
            }

            TypeAst::Tuple(mut t) => {
                t.elems = self.fold_all_types(t.elems);
                TypeAst::Tuple(t)
            }

            TypeAst::Var(_) | TypeAst::Hole(_) => t,
        }
    }

    /// You probably don't want to override this method.
    fn fold_all_types(&mut self, ts: Vec<TypeAst>) -> Vec<TypeAst> {
        ts.into_iter().map(|t| self.fold_type(t)).collect()
    }

    fn fold_type_constructor(&mut self, constructor: TypeAstConstructor) -> TypeAst {
        TypeAst::Constructor(constructor)
    }

    fn fold_type_fn(&mut self, function: TypeAstFn) -> TypeAst {
        TypeAst::Fn(function)
    }

    fn fold_type_tuple(&mut self, tuple: TypeAstTuple) -> TypeAst {
        TypeAst::Tuple(tuple)
    }

    fn fold_type_var(&mut self, var: TypeAstVar) -> TypeAst {
        TypeAst::Var(var)
    }

    fn fold_type_hole(&mut self, hole: TypeAstHole) -> TypeAst {
        TypeAst::Hole(hole)
    }
}

pub trait UntypedExprFolder: TypeAstFolder {
    /// Visit a node and potentially replace it with another node using the
    /// `fold_*` methods. Afterwards, the `walk` method is called on the new
    /// node to continue traversing.
    ///
    /// You probably don't want to override this method.
    fn fold_expr(&mut self, t: UntypedExpr) -> UntypedExpr {
        let t = self.update_expr(t);
        self.walk_expr(t)
    }

    /// You probably don't want to override this method.
    fn update_expr(&mut self, e: UntypedExpr) -> UntypedExpr {
        match e {
            UntypedExpr::Var { location, name } => self.fold_var(location, name),
            UntypedExpr::Int { location, value } => self.fold_int(location, value),
            UntypedExpr::Float { location, value } => self.fold_float(location, value),
            UntypedExpr::String { location, value } => self.fold_string(location, value),

            UntypedExpr::Block {
                location,
                statements,
            } => self.fold_block(location, statements),

            UntypedExpr::Fn {
                location,
                is_capture,
                arguments,
                body,
                return_annotation,
            } => self.fold_fn(location, is_capture, arguments, body, return_annotation),

            UntypedExpr::List {
                location,
                elements,
                tail,
            } => self.fold_list(location, elements, tail),

            UntypedExpr::Call {
                location,
                fun,
                arguments,
            } => self.fold_call(location, fun, arguments),

            UntypedExpr::BinOp {
                location,
                name,
                left,
                right,
            } => self.fold_bin_op(location, name, left, right),

            UntypedExpr::PipeLine { expressions } => self.fold_pipe_line(expressions),

            UntypedExpr::Case {
                location,
                subjects,
                clauses,
            } => self.fold_case(location, subjects, clauses),

            UntypedExpr::FieldAccess {
                location,
                label,
                container,
            } => self.fold_field_access(location, label, container),

            UntypedExpr::Tuple { location, elems } => self.fold_tuple(location, elems),

            UntypedExpr::TupleIndex {
                location,
                index,
                tuple,
            } => self.fold_tuple_index(location, index, tuple),

            UntypedExpr::Todo {
                kind,
                location,
                message,
            } => self.fold_todo(kind, location, message),

            UntypedExpr::Panic { location, message } => self.fold_panic(location, message),

            UntypedExpr::BitArray { location, segments } => self.fold_bit_array(location, segments),

            UntypedExpr::RecordUpdate {
                location,
                constructor,
                spread,
                arguments,
            } => self.fold_record_update(location, constructor, spread, arguments),

            UntypedExpr::NegateBool { location, value } => self.fold_negate_bool(location, value),

            UntypedExpr::NegateInt { location, value } => self.fold_negate_int(location, value),

            UntypedExpr::Placeholder { location } => self.fold_placeholder(location),
        }
    }

    /// You probably don't want to override this method.
    fn walk_expr(&mut self, e: UntypedExpr) -> UntypedExpr {
        match e {
            UntypedExpr::Int { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::Todo { .. }
            | UntypedExpr::Panic { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::NegateInt { .. }
            | UntypedExpr::NegateBool { .. }
            | UntypedExpr::Placeholder { .. } => e,

            UntypedExpr::Block {
                location,
                statements,
            } => {
                let statements = statements.mapped(|s| self.fold_statement(s));
                UntypedExpr::Block {
                    location,
                    statements,
                }
            }

            UntypedExpr::Fn {
                location,
                is_capture,
                arguments,
                body,
                return_annotation,
            } => {
                let arguments = arguments.into_iter().map(|a| self.fold_arg(a)).collect();
                let return_annotation = return_annotation.map(|t| self.fold_type(t));
                let body = body.mapped(|s| self.fold_statement(s));
                UntypedExpr::Fn {
                    location,
                    is_capture,
                    arguments,
                    body,
                    return_annotation,
                }
            }

            UntypedExpr::List {
                location,
                elements,
                tail,
            } => {
                let elements = elements.into_iter().map(|e| self.fold_expr(e)).collect();
                let tail = tail.map(|e| Box::new(self.fold_expr(*e)));
                UntypedExpr::List {
                    location,
                    elements,
                    tail,
                }
            }

            UntypedExpr::Call {
                location,
                fun,
                arguments,
            } => todo!(),

            UntypedExpr::BinOp {
                location,
                name,
                left,
                right,
            } => {
                let left = Box::new(self.fold_expr(*left));
                let right = Box::new(self.fold_expr(*right));
                UntypedExpr::BinOp {
                    location,
                    name,
                    left,
                    right,
                }
            }

            UntypedExpr::PipeLine { expressions } => {
                let expressions = expressions.mapped(|e| self.fold_expr(e));
                UntypedExpr::PipeLine { expressions }
            }

            UntypedExpr::Case {
                location,
                subjects,
                clauses,
            } => todo!(),

            UntypedExpr::FieldAccess {
                location,
                label,
                container,
            } => {
                let container = Box::new(self.fold_expr(*container));
                UntypedExpr::FieldAccess {
                    location,
                    label,
                    container,
                }
            }

            UntypedExpr::Tuple { location, elems } => {
                let elems = elems.into_iter().map(|e| self.fold_expr(e)).collect();
                UntypedExpr::Tuple { location, elems }
            }

            UntypedExpr::TupleIndex {
                location,
                index,
                tuple,
            } => {
                let tuple = Box::new(self.fold_expr(*tuple));
                UntypedExpr::TupleIndex {
                    location,
                    index,
                    tuple,
                }
            }

            UntypedExpr::BitArray { location, segments } => todo!(),

            UntypedExpr::RecordUpdate {
                location,
                constructor,
                spread,
                arguments,
            } => todo!(),
        }
    }

    /// You probably don't want to override this method.
    fn fold_arg(&mut self, arg: UntypedArg) -> UntypedArg {
        let UntypedArg {
            location,
            names,
            annotation,
            type_,
        } = arg;
        let annotation = annotation.map(|t| self.fold_type(t));
        UntypedArg {
            location,
            names,
            annotation,
            type_,
        }
    }

    /// You probably don't want to override this method.
    fn fold_statement(&mut self, s: UntypedStatement) -> UntypedStatement {
        let s = self.update_statement(s);
        self.walk_statement(s)
    }

    /// You probably don't want to override this method.
    fn update_statement(&mut self, s: UntypedStatement) -> UntypedStatement {
        match s {
            Statement::Expression(e) => Statement::Expression(e),
            Statement::Assignment(a) => Statement::Assignment(self.fold_assignment(a)),
            Statement::Use(u) => Statement::Use(self.fold_use(u)),
        }
    }

    /// You probably don't want to override this method.
    fn walk_statement(&mut self, s: UntypedStatement) -> UntypedStatement {
        match s {
            Statement::Expression(e) => Statement::Expression(self.fold_expr(e)),

            Statement::Assignment(Assignment {
                location,
                value,
                pattern,
                kind,
                annotation,
            }) => {
                let annotation = annotation.map(|t| self.fold_type(t));
                let value = Box::new(self.fold_expr(*value));
                Statement::Assignment(Assignment {
                    location,
                    value,
                    pattern,
                    kind,
                    annotation,
                })
            }

            Statement::Use(Use {
                location,
                call,
                assignments,
            }) => {
                let assignments = assignments
                    .into_iter()
                    .map(|a| self.fold_use_assignment(a))
                    .collect();
                let call = Box::new(self.fold_expr(*call));
                Statement::Use(Use {
                    location,
                    call,
                    assignments,
                })
            }
        }
    }

    /// You probably don't want to override this method.
    fn fold_use_assignment(&mut self, use_: UseAssignment) -> UseAssignment {
        let UseAssignment {
            location,
            pattern,
            annotation,
        } = use_;
        let annotation = annotation.map(|t| self.fold_type(t));
        UseAssignment {
            location,
            pattern,
            annotation,
        }
    }

    fn fold_int(&mut self, location: SrcSpan, value: SmolStr) -> UntypedExpr {
        UntypedExpr::Int { location, value }
    }

    fn fold_float(&mut self, location: SrcSpan, value: SmolStr) -> UntypedExpr {
        UntypedExpr::Float { location, value }
    }

    fn fold_string(&mut self, location: SrcSpan, value: SmolStr) -> UntypedExpr {
        UntypedExpr::String { location, value }
    }

    fn fold_block(&mut self, location: SrcSpan, statements: Vec1<UntypedStatement>) -> UntypedExpr {
        UntypedExpr::Block {
            location,
            statements,
        }
    }

    fn fold_var(&mut self, location: SrcSpan, name: SmolStr) -> UntypedExpr {
        UntypedExpr::Var { location, name }
    }

    fn fold_fn(
        &mut self,
        location: SrcSpan,
        is_capture: bool,
        arguments: Vec<UntypedArg>,
        body: Vec1<UntypedStatement>,
        return_annotation: Option<TypeAst>,
    ) -> UntypedExpr {
        UntypedExpr::Fn {
            location,
            is_capture,
            arguments,
            body,
            return_annotation,
        }
    }

    fn fold_list(
        &mut self,
        location: SrcSpan,
        elements: Vec<UntypedExpr>,
        tail: Option<Box<UntypedExpr>>,
    ) -> UntypedExpr {
        UntypedExpr::List {
            location,
            elements,
            tail,
        }
    }

    fn fold_call(
        &mut self,
        location: SrcSpan,
        fun: Box<UntypedExpr>,
        arguments: Vec<CallArg<UntypedExpr>>,
    ) -> UntypedExpr {
        UntypedExpr::Call {
            location,
            fun,
            arguments,
        }
    }

    fn fold_bin_op(
        &mut self,
        location: SrcSpan,
        name: BinOp,
        left: Box<UntypedExpr>,
        right: Box<UntypedExpr>,
    ) -> UntypedExpr {
        UntypedExpr::BinOp {
            location,
            name,
            left,
            right,
        }
    }

    fn fold_pipe_line(&mut self, expressions: Vec1<UntypedExpr>) -> UntypedExpr {
        UntypedExpr::PipeLine { expressions }
    }

    fn fold_case(
        &mut self,
        location: SrcSpan,
        subjects: Vec<UntypedExpr>,
        clauses: Vec<UntypedClause>,
    ) -> UntypedExpr {
        UntypedExpr::Case {
            location,
            subjects,
            clauses,
        }
    }

    fn fold_field_access(
        &mut self,
        location: SrcSpan,
        label: SmolStr,
        container: Box<UntypedExpr>,
    ) -> UntypedExpr {
        UntypedExpr::FieldAccess {
            location,
            label,
            container,
        }
    }

    fn fold_tuple(&mut self, location: SrcSpan, elems: Vec<UntypedExpr>) -> UntypedExpr {
        UntypedExpr::Tuple { location, elems }
    }

    fn fold_tuple_index(
        &mut self,
        location: SrcSpan,
        index: u64,
        tuple: Box<UntypedExpr>,
    ) -> UntypedExpr {
        UntypedExpr::TupleIndex {
            location,
            index,
            tuple,
        }
    }

    fn fold_todo(
        &mut self,
        kind: TodoKind,
        location: SrcSpan,
        message: Option<SmolStr>,
    ) -> UntypedExpr {
        UntypedExpr::Todo {
            kind,
            location,
            message,
        }
    }

    fn fold_panic(&mut self, location: SrcSpan, message: Option<SmolStr>) -> UntypedExpr {
        UntypedExpr::Panic { location, message }
    }

    fn fold_bit_array(
        &mut self,
        location: SrcSpan,
        segments: Vec<UntypedExprBitArraySegment>,
    ) -> UntypedExpr {
        UntypedExpr::BitArray { location, segments }
    }

    fn fold_record_update(
        &mut self,
        location: SrcSpan,
        constructor: Box<UntypedExpr>,
        spread: RecordUpdateSpread,
        arguments: Vec<UntypedRecordUpdateArg>,
    ) -> UntypedExpr {
        UntypedExpr::RecordUpdate {
            location,
            constructor,
            spread,
            arguments,
        }
    }

    fn fold_negate_bool(&mut self, location: SrcSpan, value: Box<UntypedExpr>) -> UntypedExpr {
        UntypedExpr::NegateBool { location, value }
    }

    fn fold_negate_int(&mut self, location: SrcSpan, value: Box<UntypedExpr>) -> UntypedExpr {
        UntypedExpr::NegateInt { location, value }
    }

    fn fold_placeholder(&mut self, location: SrcSpan) -> UntypedExpr {
        UntypedExpr::Placeholder { location }
    }

    fn fold_assignment(&mut self, assignment: UntypedAssignment) -> UntypedAssignment {
        assignment
    }

    fn fold_use(&mut self, use_: Use) -> Use {
        use_
    }
}
