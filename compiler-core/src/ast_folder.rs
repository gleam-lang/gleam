use ecow::EcoString;
use vec1::Vec1;

use crate::{
    analyse::Inferred,
    ast::{
        AssignName, Assignment, BinOp, CallArg, Constant, Definition, Pattern, RecordUpdateSpread,
        SrcSpan, Statement, TargetedDefinition, TodoKind, TypeAst, TypeAstConstructor, TypeAstFn,
        TypeAstHole, TypeAstTuple, TypeAstVar, UntypedArg, UntypedAssignment, UntypedClause,
        UntypedConstant, UntypedConstantBitArraySegment, UntypedCustomType, UntypedDefinition,
        UntypedExpr, UntypedExprBitArraySegment, UntypedFunction, UntypedImport, UntypedModule,
        UntypedModuleConstant, UntypedPattern, UntypedPatternBitArraySegment,
        UntypedRecordUpdateArg, UntypedStatement, UntypedTypeAlias, Use, UseAssignment,
    },
    build::Target,
};

#[allow(dead_code)]
pub trait UntypedModuleFolder: TypeAstFolder + UntypedExprFolder {
    /// You probably don't want to override this method.
    fn fold_module(&mut self, mut m: UntypedModule) -> UntypedModule {
        m.definitions = m
            .definitions
            .into_iter()
            .map(|d| {
                let TargetedDefinition { definition, target } = d;
                match definition {
                    Definition::Function(f) => {
                        let f = self.fold_function_definition(f, target);
                        let definition = self.walk_function_definition(f);
                        TargetedDefinition { definition, target }
                    }

                    Definition::TypeAlias(a) => {
                        let a = self.fold_type_alias(a, target);
                        let definition = self.walk_type_alias(a);
                        TargetedDefinition { definition, target }
                    }

                    Definition::CustomType(t) => {
                        let t = self.fold_custom_type(t, target);
                        let definition = self.walk_custom_type(t);
                        TargetedDefinition { definition, target }
                    }

                    Definition::Import(i) => {
                        let i = self.fold_import(i, target);
                        let definition = self.walk_import(i);
                        TargetedDefinition { definition, target }
                    }

                    Definition::ModuleConstant(c) => {
                        let c = self.fold_module_constant(c, target);
                        let definition = self.walk_module_constant(c);
                        TargetedDefinition { definition, target }
                    }
                }
            })
            .collect();
        m
    }

    /// You probably don't want to override this method.
    fn walk_function_definition(&mut self, mut f: UntypedFunction) -> UntypedDefinition {
        f.body = f.body.mapped(|s| self.fold_statement(s));
        f.return_annotation = f.return_annotation.map(|t| self.fold_type(t));
        f.arguments = f
            .arguments
            .into_iter()
            .map(|mut a| {
                a.annotation = a.annotation.map(|t| self.fold_type(t));
                a
            })
            .collect();
        Definition::Function(f)
    }

    /// You probably don't want to override this method.
    fn walk_type_alias(&mut self, mut f: UntypedTypeAlias) -> UntypedDefinition {
        f.type_ast = self.fold_type(f.type_ast);
        Definition::TypeAlias(f)
    }

    /// You probably don't want to override this method.
    fn walk_custom_type(&mut self, mut c: UntypedCustomType) -> UntypedDefinition {
        c.constructors = c
            .constructors
            .into_iter()
            .map(|mut c| {
                c.arguments = c
                    .arguments
                    .into_iter()
                    .map(|mut a| {
                        a.ast = self.fold_type(a.ast);
                        a
                    })
                    .collect();
                c
            })
            .collect();

        Definition::CustomType(c)
    }

    /// You probably don't want to override this method.
    fn walk_import(&mut self, i: UntypedImport) -> UntypedDefinition {
        Definition::Import(i)
    }

    /// You probably don't want to override this method.
    fn walk_module_constant(&mut self, mut c: UntypedModuleConstant) -> UntypedDefinition {
        c.annotation = c.annotation.map(|t| self.fold_type(t));
        c.value = Box::new(self.fold_constant(*c.value));
        Definition::ModuleConstant(c)
    }

    fn fold_function_definition(
        &mut self,
        f: UntypedFunction,
        _target: Option<Target>,
    ) -> UntypedFunction {
        f
    }

    fn fold_type_alias(
        &mut self,
        f: UntypedTypeAlias,
        _target: Option<Target>,
    ) -> UntypedTypeAlias {
        f
    }

    fn fold_custom_type(
        &mut self,
        c: UntypedCustomType,
        _target: Option<Target>,
    ) -> UntypedCustomType {
        c
    }

    fn fold_import(&mut self, i: UntypedImport, _target: Option<Target>) -> UntypedImport {
        i
    }

    fn fold_module_constant(
        &mut self,
        c: UntypedModuleConstant,
        _target: Option<Target>,
    ) -> UntypedModuleConstant {
        c
    }
}

#[allow(dead_code)]
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

#[allow(dead_code)]
pub trait UntypedExprFolder: TypeAstFolder + UntypedConstantFolder + PatternFolder {
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
                label_location,
                label,
                container,
            } => self.fold_field_access(location, label_location, label, container),

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
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::NegateInt { .. }
            | UntypedExpr::NegateBool { .. }
            | UntypedExpr::Placeholder { .. } => e,

            UntypedExpr::Todo {
                kind,
                location,
                message,
            } => UntypedExpr::Todo {
                kind,
                location,
                message: message.map(|msg_expr| Box::new(self.fold_expr(*msg_expr))),
            },

            UntypedExpr::Panic { location, message } => UntypedExpr::Panic {
                location,
                message: message.map(|msg_expr| Box::new(self.fold_expr(*msg_expr))),
            },

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
            } => {
                let fun = Box::new(self.fold_expr(*fun));
                let arguments = arguments
                    .into_iter()
                    .map(|mut a| {
                        a.value = self.fold_expr(a.value);
                        a
                    })
                    .collect();
                UntypedExpr::Call {
                    location,
                    fun,
                    arguments,
                }
            }

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
            } => {
                let subjects = subjects.into_iter().map(|e| self.fold_expr(e)).collect();
                let clauses = clauses
                    .into_iter()
                    .map(|mut c| {
                        c.pattern = c
                            .pattern
                            .into_iter()
                            .map(|p| self.fold_pattern(p))
                            .collect();
                        c.alternative_patterns = c
                            .alternative_patterns
                            .into_iter()
                            .map(|p| p.into_iter().map(|p| self.fold_pattern(p)).collect())
                            .collect();
                        c.then = self.fold_expr(c.then);
                        c
                    })
                    .collect();
                UntypedExpr::Case {
                    location,
                    subjects,
                    clauses,
                }
            }

            UntypedExpr::FieldAccess {
                location,
                label_location,
                label,
                container,
            } => {
                let container = Box::new(self.fold_expr(*container));
                UntypedExpr::FieldAccess {
                    location,
                    label_location,
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

            UntypedExpr::BitArray { location, segments } => {
                let segments = segments
                    .into_iter()
                    .map(|mut s| {
                        s.value = Box::new(self.fold_expr(*s.value));
                        s
                    })
                    .collect();
                UntypedExpr::BitArray { location, segments }
            }

            UntypedExpr::RecordUpdate {
                location,
                constructor,
                spread,
                arguments,
            } => {
                let constructor = Box::new(self.fold_expr(*constructor));
                let arguments = arguments
                    .into_iter()
                    .map(|mut a| {
                        a.value = self.fold_expr(a.value);
                        a
                    })
                    .collect();
                UntypedExpr::RecordUpdate {
                    location,
                    constructor,
                    spread,
                    arguments,
                }
            }
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
                let pattern = self.fold_pattern(pattern);
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
                assignments_location,
                call,
                assignments,
            }) => {
                let assignments = assignments
                    .into_iter()
                    .map(|a| {
                        let mut use_ = self.fold_use_assignment(a);
                        use_.pattern = self.fold_pattern(use_.pattern);
                        use_
                    })
                    .collect();
                let call = Box::new(self.fold_expr(*call));
                Statement::Use(Use {
                    location,
                    assignments_location,
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

    fn fold_int(&mut self, location: SrcSpan, value: EcoString) -> UntypedExpr {
        UntypedExpr::Int { location, value }
    }

    fn fold_float(&mut self, location: SrcSpan, value: EcoString) -> UntypedExpr {
        UntypedExpr::Float { location, value }
    }

    fn fold_string(&mut self, location: SrcSpan, value: EcoString) -> UntypedExpr {
        UntypedExpr::String { location, value }
    }

    fn fold_block(&mut self, location: SrcSpan, statements: Vec1<UntypedStatement>) -> UntypedExpr {
        UntypedExpr::Block {
            location,
            statements,
        }
    }

    fn fold_var(&mut self, location: SrcSpan, name: EcoString) -> UntypedExpr {
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
        label_location: SrcSpan,
        label: EcoString,
        container: Box<UntypedExpr>,
    ) -> UntypedExpr {
        UntypedExpr::FieldAccess {
            location,
            label_location,
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
        message: Option<Box<UntypedExpr>>,
    ) -> UntypedExpr {
        UntypedExpr::Todo {
            kind,
            location,
            message,
        }
    }

    fn fold_panic(&mut self, location: SrcSpan, message: Option<Box<UntypedExpr>>) -> UntypedExpr {
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

#[allow(dead_code)]
pub trait UntypedConstantFolder {
    /// You probably don't want to override this method.
    fn fold_constant(&mut self, m: UntypedConstant) -> UntypedConstant {
        let m = self.update_constant(m);
        self.walk_constant(m)
    }

    /// You probably don't want to override this method.
    fn update_constant(&mut self, m: UntypedConstant) -> UntypedConstant {
        match m {
            Constant::Int { location, value } => self.fold_constant_int(location, value),

            Constant::Float { location, value } => self.fold_constant_float(location, value),

            Constant::String { location, value } => self.fold_constant_string(location, value),

            Constant::Tuple { location, elements } => self.fold_constant_tuple(location, elements),

            Constant::List {
                location,
                elements,
                typ: (),
            } => self.fold_constant_list(location, elements),

            Constant::Record {
                location,
                module,
                name,
                args,
                tag: (),
                typ: (),
                field_map: _,
            } => self.fold_constant_record(location, module, name, args),

            Constant::BitArray { location, segments } => {
                self.fold_constant_bit_array(location, segments)
            }

            Constant::Var {
                location,
                module,
                name,
                constructor: _,
                typ: (),
            } => self.fold_constant_var(location, module, name),

            Constant::Invalid { location, typ: () } => self.fold_constant_invalid(location),
        }
    }

    fn fold_constant_int(&mut self, location: SrcSpan, value: EcoString) -> UntypedConstant {
        Constant::Int { location, value }
    }

    fn fold_constant_float(&mut self, location: SrcSpan, value: EcoString) -> UntypedConstant {
        Constant::Float { location, value }
    }

    fn fold_constant_string(&mut self, location: SrcSpan, value: EcoString) -> UntypedConstant {
        Constant::String { location, value }
    }

    fn fold_constant_tuple(
        &mut self,
        location: SrcSpan,
        elements: Vec<UntypedConstant>,
    ) -> UntypedConstant {
        Constant::Tuple { location, elements }
    }

    fn fold_constant_list(
        &mut self,
        location: SrcSpan,
        elements: Vec<UntypedConstant>,
    ) -> UntypedConstant {
        Constant::List {
            location,
            elements,
            typ: (),
        }
    }

    fn fold_constant_record(
        &mut self,
        location: SrcSpan,
        module: Option<EcoString>,
        name: EcoString,
        args: Vec<CallArg<UntypedConstant>>,
    ) -> UntypedConstant {
        Constant::Record {
            location,
            module,
            name,
            args,
            tag: (),
            typ: (),
            field_map: None,
        }
    }

    fn fold_constant_bit_array(
        &mut self,
        location: SrcSpan,
        segments: Vec<UntypedConstantBitArraySegment>,
    ) -> UntypedConstant {
        Constant::BitArray { location, segments }
    }

    fn fold_constant_var(
        &mut self,
        location: SrcSpan,
        module: Option<EcoString>,
        name: EcoString,
    ) -> UntypedConstant {
        Constant::Var {
            location,
            module,
            name,
            constructor: None,
            typ: (),
        }
    }

    fn fold_constant_invalid(&mut self, location: SrcSpan) -> UntypedConstant {
        Constant::Invalid { location, typ: () }
    }

    /// You probably don't want to override this method.
    fn walk_constant(&mut self, m: UntypedConstant) -> UntypedConstant {
        match m {
            Constant::Var { .. }
            | Constant::Int { .. }
            | Constant::Float { .. }
            | Constant::String { .. }
            | Constant::Tuple { .. }
            | Constant::Invalid { .. } => m,

            Constant::List {
                location,
                elements,
                typ,
            } => {
                let elements = elements
                    .into_iter()
                    .map(|e| self.fold_constant(e))
                    .collect();
                Constant::List {
                    location,
                    elements,
                    typ,
                }
            }

            Constant::Record {
                location,
                module,
                name,
                args,
                tag,
                typ,
                field_map,
            } => {
                let args = args
                    .into_iter()
                    .map(|mut a| {
                        a.value = self.fold_constant(a.value);
                        a
                    })
                    .collect();
                Constant::Record {
                    location,
                    module,
                    name,
                    args,
                    tag,
                    typ,
                    field_map,
                }
            }

            Constant::BitArray { location, segments } => {
                let segments = segments
                    .into_iter()
                    .map(|mut s| {
                        s.value = Box::new(self.fold_constant(*s.value));
                        s
                    })
                    .collect();
                Constant::BitArray { location, segments }
            }
        }
    }
}

#[allow(dead_code)]
pub trait PatternFolder {
    /// You probably don't want to override this method.
    fn fold_pattern(&mut self, m: UntypedPattern) -> UntypedPattern {
        let m = self.update_pattern(m);
        self.walk_pattern(m)
    }

    /// You probably don't want to override this method.
    fn update_pattern(&mut self, m: UntypedPattern) -> UntypedPattern {
        match m {
            Pattern::Int { location, value } => self.fold_pattern_int(location, value),

            Pattern::Float { location, value } => self.fold_pattern_float(location, value),

            Pattern::String { location, value } => self.fold_pattern_string(location, value),

            Pattern::Variable {
                location,
                name,
                type_: (),
            } => self.fold_pattern_var(location, name),

            Pattern::VarUsage {
                location,
                name,
                constructor: _,
                type_: (),
            } => self.fold_pattern_var_usage(location, name),

            Pattern::Assign {
                name,
                location,
                pattern,
            } => self.fold_pattern_assign(name, location, pattern),

            Pattern::Discard {
                name,
                location,
                type_: (),
            } => self.fold_pattern_discard(name, location),

            Pattern::List {
                location,
                elements,
                tail,
                type_: (),
            } => self.fold_pattern_list(location, elements, tail),

            Pattern::Constructor {
                location,
                name,
                arguments,
                module,
                with_spread,
                constructor: _,
                type_: (),
            } => self.fold_pattern_constructor(location, name, arguments, module, with_spread),

            Pattern::Tuple { location, elems } => self.fold_pattern_tuple(location, elems),

            Pattern::BitArray { location, segments } => {
                self.fold_pattern_bit_array(location, segments)
            }

            Pattern::StringPrefix {
                location,
                left_location,
                left_side_assignment,
                right_location,
                left_side_string,
                right_side_assignment,
            } => self.fold_pattern_string_prefix(
                location,
                left_location,
                left_side_assignment,
                right_location,
                left_side_string,
                right_side_assignment,
            ),
        }
    }

    fn fold_pattern_int(&mut self, location: SrcSpan, value: EcoString) -> UntypedPattern {
        Pattern::Int { location, value }
    }

    fn fold_pattern_float(&mut self, location: SrcSpan, value: EcoString) -> UntypedPattern {
        Pattern::Float { location, value }
    }

    fn fold_pattern_string(&mut self, location: SrcSpan, value: EcoString) -> UntypedPattern {
        Pattern::String { location, value }
    }

    fn fold_pattern_var(&mut self, location: SrcSpan, name: EcoString) -> UntypedPattern {
        Pattern::Variable {
            location,
            name,
            type_: (),
        }
    }

    fn fold_pattern_var_usage(&mut self, location: SrcSpan, name: EcoString) -> UntypedPattern {
        Pattern::VarUsage {
            location,
            name,
            constructor: None,
            type_: (),
        }
    }

    fn fold_pattern_assign(
        &mut self,
        name: EcoString,
        location: SrcSpan,
        pattern: Box<UntypedPattern>,
    ) -> UntypedPattern {
        Pattern::Assign {
            name,
            location,
            pattern,
        }
    }

    fn fold_pattern_discard(&mut self, name: EcoString, location: SrcSpan) -> UntypedPattern {
        Pattern::Discard {
            name,
            location,
            type_: (),
        }
    }

    fn fold_pattern_list(
        &mut self,
        location: SrcSpan,
        elements: Vec<UntypedPattern>,
        tail: Option<Box<UntypedPattern>>,
    ) -> UntypedPattern {
        Pattern::List {
            location,
            elements,
            tail,
            type_: (),
        }
    }

    fn fold_pattern_constructor(
        &mut self,
        location: SrcSpan,
        name: EcoString,
        arguments: Vec<CallArg<UntypedPattern>>,
        module: Option<EcoString>,
        with_spread: bool,
    ) -> UntypedPattern {
        Pattern::Constructor {
            location,
            name,
            arguments,
            module,
            constructor: Inferred::Unknown,
            with_spread,
            type_: (),
        }
    }

    fn fold_pattern_tuple(
        &mut self,
        location: SrcSpan,
        elems: Vec<UntypedPattern>,
    ) -> UntypedPattern {
        Pattern::Tuple { location, elems }
    }

    fn fold_pattern_bit_array(
        &mut self,
        location: SrcSpan,
        segments: Vec<UntypedPatternBitArraySegment>,
    ) -> UntypedPattern {
        Pattern::BitArray { location, segments }
    }

    fn fold_pattern_string_prefix(
        &mut self,
        location: SrcSpan,
        left_location: SrcSpan,
        left_side_assignment: Option<(EcoString, SrcSpan)>,
        right_location: SrcSpan,
        left_side_string: EcoString,
        right_side_assignment: AssignName,
    ) -> UntypedPattern {
        Pattern::StringPrefix {
            location,
            left_location,
            left_side_assignment,
            right_location,
            left_side_string,
            right_side_assignment,
        }
    }

    /// You probably don't want to override this method.
    fn walk_pattern(&mut self, m: UntypedPattern) -> UntypedPattern {
        match m {
            Pattern::Int { .. }
            | Pattern::Variable { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::Discard { .. }
            | Pattern::VarUsage { .. }
            | Pattern::StringPrefix { .. } => m,

            Pattern::Assign {
                name,
                location,
                pattern,
            } => {
                let pattern = Box::new(self.fold_pattern(*pattern));
                Pattern::Assign {
                    name,
                    location,
                    pattern,
                }
            }

            Pattern::List {
                location,
                elements,
                tail,
                type_,
            } => {
                let elements = elements.into_iter().map(|p| self.fold_pattern(p)).collect();
                let tail = tail.map(|p| Box::new(self.fold_pattern(*p)));
                Pattern::List {
                    location,
                    elements,
                    tail,
                    type_,
                }
            }

            Pattern::Constructor {
                location,
                name,
                arguments,
                module,
                constructor,
                with_spread,
                type_,
            } => {
                let arguments = arguments
                    .into_iter()
                    .map(|mut a| {
                        a.value = self.fold_pattern(a.value);
                        a
                    })
                    .collect();
                Pattern::Constructor {
                    location,
                    name,
                    arguments,
                    module,
                    constructor,
                    with_spread,
                    type_,
                }
            }

            Pattern::Tuple { location, elems } => {
                let elems = elems.into_iter().map(|p| self.fold_pattern(p)).collect();
                Pattern::Tuple { location, elems }
            }

            Pattern::BitArray { location, segments } => {
                let segments = segments
                    .into_iter()
                    .map(|mut s| {
                        s.value = Box::new(self.fold_pattern(*s.value));
                        s
                    })
                    .collect();
                Pattern::BitArray { location, segments }
            }
        }
    }
}
