use ecow::EcoString;
use num_bigint::BigInt;
use vec1::Vec1;

use crate::{
    analyse::Inferred,
    ast::{
        Assert, AssignName, Assignment, BinOp, CallArg, Constant, Definition, FunctionLiteralKind,
        Pattern, RecordBeingUpdated, SrcSpan, Statement, TargetedDefinition, TodoKind, TypeAst,
        TypeAstConstructor, TypeAstFn, TypeAstHole, TypeAstTuple, TypeAstVar, UntypedArg,
        UntypedAssert, UntypedAssignment, UntypedClause, UntypedConstant,
        UntypedConstantBitArraySegment, UntypedCustomType, UntypedDefinition, UntypedExpr,
        UntypedExprBitArraySegment, UntypedFunction, UntypedImport, UntypedModule,
        UntypedModuleConstant, UntypedPattern, UntypedPatternBitArraySegment,
        UntypedRecordUpdateArg, UntypedStatement, UntypedTypeAlias, UntypedUse,
        UntypedUseAssignment, Use, UseAssignment,
    },
    build::Target,
    type_::error::VariableOrigin,
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
    fn walk_function_definition(&mut self, mut function: UntypedFunction) -> UntypedDefinition {
        function.body = function.body.mapped(|s| self.fold_statement(s));
        function.return_annotation = function
            .return_annotation
            .map(|type_| self.fold_type(type_));
        function.arguments = function
            .arguments
            .into_iter()
            .map(|mut argument| {
                argument.annotation = argument.annotation.map(|type_| self.fold_type(type_));
                argument
            })
            .collect();
        Definition::Function(function)
    }

    /// You probably don't want to override this method.
    fn walk_type_alias(&mut self, mut type_alias: UntypedTypeAlias) -> UntypedDefinition {
        type_alias.type_ast = self.fold_type(type_alias.type_ast);
        Definition::TypeAlias(type_alias)
    }

    /// You probably don't want to override this method.
    fn walk_custom_type(&mut self, mut custom_type: UntypedCustomType) -> UntypedDefinition {
        custom_type.constructors = custom_type
            .constructors
            .into_iter()
            .map(|mut constructor| {
                constructor.arguments = constructor
                    .arguments
                    .into_iter()
                    .map(|mut argument| {
                        argument.ast = self.fold_type(argument.ast);
                        argument
                    })
                    .collect();
                constructor
            })
            .collect();

        Definition::CustomType(custom_type)
    }

    /// You probably don't want to override this method.
    fn walk_import(&mut self, i: UntypedImport) -> UntypedDefinition {
        Definition::Import(i)
    }

    /// You probably don't want to override this method.
    fn walk_module_constant(&mut self, mut constant: UntypedModuleConstant) -> UntypedDefinition {
        constant.annotation = constant.annotation.map(|type_| self.fold_type(type_));
        constant.value = Box::new(self.fold_constant(*constant.value));
        Definition::ModuleConstant(constant)
    }

    fn fold_function_definition(
        &mut self,
        function: UntypedFunction,
        _target: Option<Target>,
    ) -> UntypedFunction {
        function
    }

    fn fold_type_alias(
        &mut self,
        function: UntypedTypeAlias,
        _target: Option<Target>,
    ) -> UntypedTypeAlias {
        function
    }

    fn fold_custom_type(
        &mut self,
        custom_type: UntypedCustomType,
        _target: Option<Target>,
    ) -> UntypedCustomType {
        custom_type
    }

    fn fold_import(&mut self, import: UntypedImport, _target: Option<Target>) -> UntypedImport {
        import
    }

    fn fold_module_constant(
        &mut self,
        constant: UntypedModuleConstant,
        _target: Option<Target>,
    ) -> UntypedModuleConstant {
        constant
    }
}

#[allow(dead_code)]
pub trait TypeAstFolder {
    /// Visit a node and potentially replace it with another node using the
    /// `fold_*` methods. Afterwards, the `walk` method is called on the new
    /// node to continue traversing.
    ///
    /// You probably don't want to override this method.
    fn fold_type(&mut self, type_: TypeAst) -> TypeAst {
        let type_ = self.update_type(type_);
        self.walk_type(type_)
    }

    /// You probably don't want to override this method.
    fn update_type(&mut self, type_: TypeAst) -> TypeAst {
        match type_ {
            TypeAst::Constructor(constructor) => self.fold_type_constructor(constructor),
            TypeAst::Fn(fn_) => self.fold_type_fn(fn_),
            TypeAst::Var(var) => self.fold_type_var(var),
            TypeAst::Tuple(tuple) => self.fold_type_tuple(tuple),
            TypeAst::Hole(hole) => self.fold_type_hole(hole),
        }
    }

    /// You probably don't want to override this method.
    fn walk_type(&mut self, type_: TypeAst) -> TypeAst {
        match type_ {
            TypeAst::Constructor(mut constructor) => {
                constructor.arguments = self.fold_all_types(constructor.arguments);
                TypeAst::Constructor(constructor)
            }

            TypeAst::Fn(mut fn_) => {
                fn_.arguments = self.fold_all_types(fn_.arguments);
                fn_.return_ = Box::new(self.fold_type(*fn_.return_));
                TypeAst::Fn(fn_)
            }

            TypeAst::Tuple(mut tuple) => {
                tuple.elements = self.fold_all_types(tuple.elements);
                TypeAst::Tuple(tuple)
            }

            TypeAst::Var(_) | TypeAst::Hole(_) => type_,
        }
    }

    /// You probably don't want to override this method.
    fn fold_all_types(&mut self, types: Vec<TypeAst>) -> Vec<TypeAst> {
        types
            .into_iter()
            .map(|type_| self.fold_type(type_))
            .collect()
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
    fn fold_expr(&mut self, expression: UntypedExpr) -> UntypedExpr {
        let expression = self.update_expr(expression);
        self.walk_expr(expression)
    }

    /// You probably don't want to override this method.
    fn update_expr(&mut self, expression: UntypedExpr) -> UntypedExpr {
        match expression {
            UntypedExpr::Var { location, name } => self.fold_var(location, name),
            UntypedExpr::Int {
                location,
                value,
                int_value,
            } => self.fold_int(location, value, int_value),
            UntypedExpr::Float { location, value } => self.fold_float(location, value),
            UntypedExpr::String { location, value } => self.fold_string(location, value),

            UntypedExpr::Block {
                location,
                statements,
            } => self.fold_block(location, statements),

            UntypedExpr::Fn {
                location,
                end_of_head_byte_index,
                kind,
                arguments,
                body,
                return_annotation,
            } => self.fold_fn(
                location,
                end_of_head_byte_index,
                kind,
                arguments,
                body,
                return_annotation,
            ),

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
                name_location,
                left,
                right,
            } => self.fold_bin_op(location, name, name_location, left, right),

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

            UntypedExpr::Tuple { location, elements } => self.fold_tuple(location, elements),

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

            UntypedExpr::Echo {
                location,
                expression,
            } => self.fold_echo(location, expression),

            UntypedExpr::Panic { location, message } => self.fold_panic(location, message),

            UntypedExpr::BitArray { location, segments } => self.fold_bit_array(location, segments),

            UntypedExpr::RecordUpdate {
                location,
                constructor,
                record,
                arguments,
            } => self.fold_record_update(location, constructor, record, arguments),

            UntypedExpr::NegateBool { location, value } => self.fold_negate_bool(location, value),

            UntypedExpr::NegateInt { location, value } => self.fold_negate_int(location, value),

            UntypedExpr::Placeholder { location } => self.fold_placeholder(location),
        }
    }

    /// You probably don't want to override this method.
    fn walk_expr(&mut self, expression: UntypedExpr) -> UntypedExpr {
        match expression {
            UntypedExpr::Int { .. }
            | UntypedExpr::Var { .. }
            | UntypedExpr::Float { .. }
            | UntypedExpr::String { .. }
            | UntypedExpr::NegateInt { .. }
            | UntypedExpr::NegateBool { .. }
            | UntypedExpr::Placeholder { .. } => expression,

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

            UntypedExpr::Echo {
                location,
                expression,
            } => UntypedExpr::Echo {
                location,
                expression: expression.map(|expression| Box::new(self.fold_expr(*expression))),
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
                kind,
                end_of_head_byte_index,
                arguments,
                body,
                return_annotation,
            } => {
                let arguments = arguments.into_iter().map(|a| self.fold_arg(a)).collect();
                let return_annotation = return_annotation.map(|type_| self.fold_type(type_));
                let body = body.mapped(|s| self.fold_statement(s));
                UntypedExpr::Fn {
                    location,
                    end_of_head_byte_index,
                    kind,
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
                let elements = elements
                    .into_iter()
                    .map(|element| self.fold_expr(element))
                    .collect();
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
                name_location,
                left,
                right,
            } => {
                let left = Box::new(self.fold_expr(*left));
                let right = Box::new(self.fold_expr(*right));
                UntypedExpr::BinOp {
                    location,
                    name,
                    name_location,
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
                let clauses = clauses.map(|clauses| {
                    clauses
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
                        .collect()
                });
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

            UntypedExpr::Tuple { location, elements } => {
                let elements = elements
                    .into_iter()
                    .map(|element| self.fold_expr(element))
                    .collect();
                UntypedExpr::Tuple { location, elements }
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
                record,
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
                    record,
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
        let annotation = annotation.map(|type_| self.fold_type(type_));
        UntypedArg {
            location,
            names,
            annotation,
            type_,
        }
    }

    /// You probably don't want to override this method.
    fn fold_statement(&mut self, statement: UntypedStatement) -> UntypedStatement {
        let statement = self.update_statement(statement);
        self.walk_statement(statement)
    }

    /// You probably don't want to override this method.
    fn update_statement(&mut self, statement: UntypedStatement) -> UntypedStatement {
        match statement {
            Statement::Expression(expression) => Statement::Expression(expression),
            Statement::Assignment(assignment) => {
                Statement::Assignment(self.fold_assignment(assignment))
            }
            Statement::Use(use_) => Statement::Use(self.fold_use(use_)),
            Statement::Assert(assert) => Statement::Assert(self.fold_assert(assert)),
        }
    }

    /// You probably don't want to override this method.
    fn walk_statement(&mut self, statement: UntypedStatement) -> UntypedStatement {
        match statement {
            Statement::Expression(expression) => Statement::Expression(self.fold_expr(expression)),

            Statement::Assignment(Assignment {
                location,
                value,
                pattern,
                kind,
                annotation,
            }) => {
                let pattern = self.fold_pattern(pattern);
                let annotation = annotation.map(|type_| self.fold_type(type_));
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
                right_hand_side_location,
                assignments_location,
                call,
                assignments,
            }) => {
                let assignments = assignments
                    .into_iter()
                    .map(|assignment| {
                        let mut use_ = self.fold_use_assignment(assignment);
                        use_.pattern = self.fold_pattern(use_.pattern);
                        use_
                    })
                    .collect();
                let call = Box::new(self.fold_expr(*call));
                Statement::Use(Use {
                    location,
                    right_hand_side_location,
                    assignments_location,
                    call,
                    assignments,
                })
            }

            Statement::Assert(Assert { location, value }) => {
                let value = self.fold_expr(value);
                Statement::Assert(Assert { location, value })
            }
        }
    }

    /// You probably don't want to override this method.
    fn fold_use_assignment(&mut self, use_: UntypedUseAssignment) -> UntypedUseAssignment {
        let UseAssignment {
            location,
            pattern,
            annotation,
        } = use_;
        let annotation = annotation.map(|type_| self.fold_type(type_));
        UseAssignment {
            location,
            pattern,
            annotation,
        }
    }

    fn fold_int(&mut self, location: SrcSpan, value: EcoString, int_value: BigInt) -> UntypedExpr {
        UntypedExpr::Int {
            location,
            value,
            int_value,
        }
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
        end_of_head_byte_index: u32,
        kind: FunctionLiteralKind,
        arguments: Vec<UntypedArg>,
        body: Vec1<UntypedStatement>,
        return_annotation: Option<TypeAst>,
    ) -> UntypedExpr {
        UntypedExpr::Fn {
            location,
            end_of_head_byte_index,
            kind,
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
        name_location: SrcSpan,
        left: Box<UntypedExpr>,
        right: Box<UntypedExpr>,
    ) -> UntypedExpr {
        UntypedExpr::BinOp {
            location,
            name,
            name_location,
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
        clauses: Option<Vec<UntypedClause>>,
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

    fn fold_tuple(&mut self, location: SrcSpan, elements: Vec<UntypedExpr>) -> UntypedExpr {
        UntypedExpr::Tuple { location, elements }
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

    fn fold_echo(
        &mut self,
        location: SrcSpan,
        expression: Option<Box<UntypedExpr>>,
    ) -> UntypedExpr {
        UntypedExpr::Echo {
            location,
            expression,
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
        record: RecordBeingUpdated,
        arguments: Vec<UntypedRecordUpdateArg>,
    ) -> UntypedExpr {
        UntypedExpr::RecordUpdate {
            location,
            constructor,
            record,
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

    fn fold_use(&mut self, use_: UntypedUse) -> UntypedUse {
        use_
    }

    fn fold_assert(&mut self, assert: UntypedAssert) -> UntypedAssert {
        assert
    }
}

#[allow(dead_code)]
pub trait UntypedConstantFolder {
    /// You probably don't want to override this method.
    fn fold_constant(&mut self, constant: UntypedConstant) -> UntypedConstant {
        let constant = self.update_constant(constant);
        self.walk_constant(constant)
    }

    /// You probably don't want to override this method.
    fn update_constant(&mut self, constant: UntypedConstant) -> UntypedConstant {
        match constant {
            Constant::Int {
                location,
                value,
                int_value,
            } => self.fold_constant_int(location, value, int_value),

            Constant::Float { location, value } => self.fold_constant_float(location, value),

            Constant::String { location, value } => self.fold_constant_string(location, value),

            Constant::Tuple { location, elements } => self.fold_constant_tuple(location, elements),

            Constant::List {
                location,
                elements,
                type_: (),
            } => self.fold_constant_list(location, elements),

            Constant::Record {
                location,
                module,
                name,
                args,
                tag: (),
                type_: (),
                field_map: _,
                record_constructor: _,
            } => self.fold_constant_record(location, module, name, args),

            Constant::BitArray { location, segments } => {
                self.fold_constant_bit_array(location, segments)
            }

            Constant::Var {
                location,
                module,
                name,
                constructor: _,
                type_: (),
            } => self.fold_constant_var(location, module, name),

            Constant::StringConcatenation {
                location,
                left,
                right,
            } => self.fold_constant_string_concatenation(location, left, right),

            Constant::Invalid {
                location,
                type_: (),
            } => self.fold_constant_invalid(location),
        }
    }

    fn fold_constant_int(
        &mut self,
        location: SrcSpan,
        value: EcoString,
        int_value: BigInt,
    ) -> UntypedConstant {
        Constant::Int {
            location,
            value,
            int_value,
        }
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
            type_: (),
        }
    }

    fn fold_constant_record(
        &mut self,
        location: SrcSpan,
        module: Option<(EcoString, SrcSpan)>,
        name: EcoString,
        args: Vec<CallArg<UntypedConstant>>,
    ) -> UntypedConstant {
        Constant::Record {
            location,
            module,
            name,
            args,
            tag: (),
            type_: (),
            field_map: None,
            record_constructor: None,
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
        module: Option<(EcoString, SrcSpan)>,
        name: EcoString,
    ) -> UntypedConstant {
        Constant::Var {
            location,
            module,
            name,
            constructor: None,
            type_: (),
        }
    }

    fn fold_constant_string_concatenation(
        &mut self,
        location: SrcSpan,
        left: Box<UntypedConstant>,
        right: Box<UntypedConstant>,
    ) -> UntypedConstant {
        Constant::StringConcatenation {
            location,
            left,
            right,
        }
    }

    fn fold_constant_invalid(&mut self, location: SrcSpan) -> UntypedConstant {
        Constant::Invalid {
            location,
            type_: (),
        }
    }

    /// You probably don't want to override this method.
    fn walk_constant(&mut self, constant: UntypedConstant) -> UntypedConstant {
        match constant {
            Constant::Var { .. }
            | Constant::Int { .. }
            | Constant::Float { .. }
            | Constant::String { .. }
            | Constant::Tuple { .. }
            | Constant::Invalid { .. } => constant,

            Constant::List {
                location,
                elements,
                type_,
            } => {
                let elements = elements
                    .into_iter()
                    .map(|element| self.fold_constant(element))
                    .collect();
                Constant::List {
                    location,
                    elements,
                    type_,
                }
            }

            Constant::Record {
                location,
                module,
                name,
                args,
                tag,
                type_,
                field_map,
                record_constructor,
            } => {
                let args = args
                    .into_iter()
                    .map(|mut arg| {
                        arg.value = self.fold_constant(arg.value);
                        arg
                    })
                    .collect();
                Constant::Record {
                    location,
                    module,
                    name,
                    args,
                    tag,
                    type_,
                    field_map,
                    record_constructor,
                }
            }

            Constant::BitArray { location, segments } => {
                let segments = segments
                    .into_iter()
                    .map(|mut segment| {
                        segment.value = Box::new(self.fold_constant(*segment.value));
                        segment
                    })
                    .collect();
                Constant::BitArray { location, segments }
            }

            Constant::StringConcatenation {
                location,
                left,
                right,
            } => {
                let left = Box::new(self.fold_constant(*left));
                let right = Box::new(self.fold_constant(*right));
                Constant::StringConcatenation {
                    location,
                    left,
                    right,
                }
            }
        }
    }
}

#[allow(dead_code)]
pub trait PatternFolder {
    /// You probably don't want to override this method.
    fn fold_pattern(&mut self, pattern: UntypedPattern) -> UntypedPattern {
        let pattern = self.update_pattern(pattern);
        self.walk_pattern(pattern)
    }

    /// You probably don't want to override this method.
    fn update_pattern(&mut self, pattern: UntypedPattern) -> UntypedPattern {
        match pattern {
            Pattern::Int {
                location,
                value,
                int_value,
            } => self.fold_pattern_int(location, value, int_value),

            Pattern::Float { location, value } => self.fold_pattern_float(location, value),

            Pattern::String { location, value } => self.fold_pattern_string(location, value),

            Pattern::Variable {
                location,
                name,
                type_: (),
                origin,
            } => self.fold_pattern_var(location, name, origin),

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
                name_location,
                name,
                arguments,
                module,
                spread,
                constructor: _,
                type_: (),
            } => self.fold_pattern_constructor(
                location,
                name_location,
                name,
                arguments,
                module,
                spread,
            ),

            Pattern::Tuple { location, elements } => self.fold_pattern_tuple(location, elements),

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

            Pattern::Invalid { location, .. } => self.fold_pattern_invalid(location),
        }
    }

    fn fold_pattern_int(
        &mut self,
        location: SrcSpan,
        value: EcoString,
        int_value: BigInt,
    ) -> UntypedPattern {
        Pattern::Int {
            location,
            value,
            int_value,
        }
    }

    fn fold_pattern_float(&mut self, location: SrcSpan, value: EcoString) -> UntypedPattern {
        Pattern::Float { location, value }
    }

    fn fold_pattern_string(&mut self, location: SrcSpan, value: EcoString) -> UntypedPattern {
        Pattern::String { location, value }
    }

    fn fold_pattern_var(
        &mut self,
        location: SrcSpan,
        name: EcoString,
        origin: VariableOrigin,
    ) -> UntypedPattern {
        Pattern::Variable {
            location,
            name,
            type_: (),
            origin,
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
        name_location: SrcSpan,
        name: EcoString,
        arguments: Vec<CallArg<UntypedPattern>>,
        module: Option<(EcoString, SrcSpan)>,
        spread: Option<SrcSpan>,
    ) -> UntypedPattern {
        Pattern::Constructor {
            location,
            name_location,
            name,
            arguments,
            module,
            constructor: Inferred::Unknown,
            spread,
            type_: (),
        }
    }

    fn fold_pattern_tuple(
        &mut self,
        location: SrcSpan,
        elements: Vec<UntypedPattern>,
    ) -> UntypedPattern {
        Pattern::Tuple { location, elements }
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

    fn fold_pattern_invalid(&mut self, location: SrcSpan) -> UntypedPattern {
        Pattern::Invalid {
            location,
            type_: (),
        }
    }

    /// You probably don't want to override this method.
    fn walk_pattern(&mut self, pattern: UntypedPattern) -> UntypedPattern {
        match pattern {
            Pattern::Int { .. }
            | Pattern::Variable { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::Discard { .. }
            | Pattern::VarUsage { .. }
            | Pattern::StringPrefix { .. }
            | Pattern::Invalid { .. } => pattern,

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
                let elements = elements
                    .into_iter()
                    .map(|pattern| self.fold_pattern(pattern))
                    .collect();
                let tail = tail.map(|pattern| Box::new(self.fold_pattern(*pattern)));
                Pattern::List {
                    location,
                    elements,
                    tail,
                    type_,
                }
            }

            Pattern::Constructor {
                location,
                name_location,
                name,
                arguments,
                module,
                constructor,
                spread,
                type_,
            } => {
                let arguments = arguments
                    .into_iter()
                    .map(|mut argument| {
                        argument.value = self.fold_pattern(argument.value);
                        argument
                    })
                    .collect();
                Pattern::Constructor {
                    location,
                    name_location,
                    name,
                    arguments,
                    module,
                    constructor,
                    spread,
                    type_,
                }
            }

            Pattern::Tuple { location, elements } => {
                let elements = elements
                    .into_iter()
                    .map(|pattern| self.fold_pattern(pattern))
                    .collect();
                Pattern::Tuple { location, elements }
            }

            Pattern::BitArray { location, segments } => {
                let segments = segments
                    .into_iter()
                    .map(|mut segment| {
                        segment.value = Box::new(self.fold_pattern(*segment.value));
                        segment
                    })
                    .collect();
                Pattern::BitArray { location, segments }
            }
        }
    }
}
