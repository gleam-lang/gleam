use std::sync::Arc;

use crate::{
    ast::{
        Assert, Assignment, AssignmentKind, BitArrayOption, BitArraySegment, CallArg, Clause,
        Definition, PipelineAssignmentKind, SrcSpan, Statement, TypedAssert, TypedAssignment,
        TypedClause, TypedDefinition, TypedExpr, TypedExprBitArraySegment, TypedFunction,
        TypedModule, TypedPipelineAssignment, TypedStatement, TypedUse,
    },
    exhaustiveness::CompiledCase,
    type_::Type,
};

pub fn module(mut module: TypedModule) -> TypedModule {
    module.definitions = module.definitions.into_iter().map(definition).collect();
    module
}

fn definition(definition: TypedDefinition) -> TypedDefinition {
    match definition {
        Definition::Function(function_ast) => Definition::Function(function(function_ast)),
        Definition::TypeAlias(_)
        | Definition::CustomType(_)
        | Definition::Import(_)
        | Definition::ModuleConstant(_) => definition,
    }
}

fn function(mut function: TypedFunction) -> TypedFunction {
    function.body = function.body.mapped(statement);
    function
}

fn statement(statement: TypedStatement) -> TypedStatement {
    match statement {
        Statement::Expression(expression_ast) => Statement::Expression(expression(expression_ast)),
        Statement::Assignment(assignment_ast) => Statement::Assignment(assignment(assignment_ast)),
        Statement::Use(use_ast) => Statement::Use(use_(use_ast)),
        Statement::Assert(assert_ast) => Statement::Assert(assert(assert_ast)),
    }
}

fn assert(assert: TypedAssert) -> TypedAssert {
    let Assert {
        location,
        value,
        message,
    } = assert;

    Assert {
        location,
        value: expression(value),
        message: message.map(expression),
    }
}

fn use_(mut use_: TypedUse) -> TypedUse {
    use_.call = boxed_expression(use_.call);
    use_
}

fn assignment(assignment: TypedAssignment) -> TypedAssignment {
    let Assignment {
        location,
        value,
        pattern,
        kind,
        annotation,
        compiled_case,
    } = assignment;

    Assignment {
        location,
        value: expression(value),
        pattern,
        kind: assignment_kind(kind),
        annotation,
        compiled_case,
    }
}

fn assignment_kind(kind: AssignmentKind<TypedExpr>) -> AssignmentKind<TypedExpr> {
    match kind {
        AssignmentKind::Let | AssignmentKind::Generated => kind,
        AssignmentKind::Assert { location, message } => AssignmentKind::Assert {
            location,
            message: message.map(expression),
        },
    }
}

fn boxed_expression(boxed: Box<TypedExpr>) -> Box<TypedExpr> {
    Box::new(expression(*boxed))
}

fn expressions(expressions: Vec<TypedExpr>) -> Vec<TypedExpr> {
    expressions.into_iter().map(expression).collect()
}

fn expression(expression_ast: TypedExpr) -> TypedExpr {
    match expression_ast {
        TypedExpr::Int { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::String { .. }
        | TypedExpr::Fn { .. }
        | TypedExpr::Var { .. }
        | TypedExpr::ModuleSelect { .. }
        | TypedExpr::Invalid { .. } => expression_ast,

        TypedExpr::Block {
            location,
            statements,
        } => TypedExpr::Block {
            location,
            statements: statements.mapped(statement),
        },

        TypedExpr::NegateBool { location, value } => TypedExpr::NegateBool {
            location,
            value: boxed_expression(value),
        },

        TypedExpr::NegateInt { location, value } => TypedExpr::NegateInt {
            location,
            value: boxed_expression(value),
        },

        TypedExpr::Pipeline {
            location,
            first_value,
            assignments,
            finally,
            finally_kind,
        } => pipeline(location, first_value, assignments, finally, finally_kind),

        TypedExpr::List {
            location,
            type_,
            elements,
            tail,
        } => TypedExpr::List {
            location,
            type_,
            elements: expressions(elements),
            tail: tail.map(boxed_expression),
        },

        TypedExpr::Call {
            location,
            type_,
            fun,
            args,
        } => call(location, type_, fun, args),

        TypedExpr::BinOp {
            location,
            type_,
            name,
            name_location,
            left,
            right,
        } => TypedExpr::BinOp {
            location,
            type_,
            name,
            name_location,
            left: boxed_expression(left),
            right: boxed_expression(right),
        },

        TypedExpr::Case {
            location,
            type_,
            subjects,
            clauses,
            compiled_case,
        } => case(location, type_, subjects, clauses, compiled_case),

        TypedExpr::RecordAccess {
            location,
            field_start,
            type_,
            label,
            index,
            record,
        } => TypedExpr::RecordAccess {
            location,
            field_start,
            type_,
            label,
            index,
            record: boxed_expression(record),
        },

        TypedExpr::Tuple {
            location,
            type_,
            elements,
        } => TypedExpr::Tuple {
            location,
            type_,
            elements: expressions(elements),
        },

        TypedExpr::TupleIndex {
            location,
            type_,
            index,
            tuple,
        } => TypedExpr::TupleIndex {
            location,
            type_,
            index,
            tuple: boxed_expression(tuple),
        },

        TypedExpr::Todo {
            location,
            message,
            kind,
            type_,
        } => TypedExpr::Todo {
            location,
            message: message.map(boxed_expression),
            kind,
            type_,
        },

        TypedExpr::Panic {
            location,
            message,
            type_,
        } => TypedExpr::Panic {
            location,
            message: message.map(boxed_expression),
            type_,
        },

        TypedExpr::Echo {
            location,
            type_,
            expression,
        } => TypedExpr::Echo {
            location,
            expression: expression.map(boxed_expression),
            type_,
        },

        TypedExpr::BitArray {
            location,
            type_,
            segments,
        } => bit_array(location, type_, segments),

        TypedExpr::RecordUpdate {
            location,
            type_,
            record,
            constructor,
            args,
        } => TypedExpr::RecordUpdate {
            location,
            type_,
            record: Box::new(assignment(*record)),
            constructor: boxed_expression(constructor),
            args,
        },
    }
}

fn call(
    location: SrcSpan,
    type_: Arc<Type>,
    function: Box<TypedExpr>,
    arguments: Vec<CallArg<TypedExpr>>,
) -> TypedExpr {
    let function = boxed_expression(function);
    let arguments = arguments
        .into_iter()
        .map(
            |CallArg {
                 label,
                 location,
                 value,
                 implicit,
             }| CallArg {
                label,
                location,
                value: expression(value),
                implicit,
            },
        )
        .collect();

    TypedExpr::Call {
        location,
        type_,
        fun: function,
        args: arguments,
    }
}

fn pipeline(
    location: SrcSpan,
    first_value: TypedPipelineAssignment,
    assignments: Vec<(TypedPipelineAssignment, PipelineAssignmentKind)>,
    finally: Box<TypedExpr>,
    finally_kind: PipelineAssignmentKind,
) -> TypedExpr {
    let first_value = pipeline_assignment(first_value);
    let assignments = assignments
        .into_iter()
        .map(|(assignment, kind)| (pipeline_assignment(assignment), kind))
        .collect();
    let finally = boxed_expression(finally);

    TypedExpr::Pipeline {
        location,
        first_value,
        assignments,
        finally,
        finally_kind,
    }
}

fn pipeline_assignment(assignment: TypedPipelineAssignment) -> TypedPipelineAssignment {
    let TypedPipelineAssignment {
        location,
        name,
        value,
    } = assignment;

    TypedPipelineAssignment {
        location,
        name,
        value: boxed_expression(value),
    }
}

fn bit_array(
    location: SrcSpan,
    type_: Arc<Type>,
    segments: Vec<TypedExprBitArraySegment>,
) -> TypedExpr {
    let segments = segments
        .into_iter()
        .map(
            |BitArraySegment {
                 location,
                 value,
                 options,
                 type_,
             }| BitArraySegment {
                location,
                value: boxed_expression(value),
                options: options.into_iter().map(bit_array_option).collect(),
                type_,
            },
        )
        .collect();

    TypedExpr::BitArray {
        location,
        type_,
        segments,
    }
}

fn bit_array_option(option: BitArrayOption<TypedExpr>) -> BitArrayOption<TypedExpr> {
    match option {
        BitArrayOption::Bytes { .. }
        | BitArrayOption::Int { .. }
        | BitArrayOption::Float { .. }
        | BitArrayOption::Bits { .. }
        | BitArrayOption::Utf8 { .. }
        | BitArrayOption::Utf16 { .. }
        | BitArrayOption::Utf32 { .. }
        | BitArrayOption::Utf8Codepoint { .. }
        | BitArrayOption::Utf16Codepoint { .. }
        | BitArrayOption::Utf32Codepoint { .. }
        | BitArrayOption::Signed { .. }
        | BitArrayOption::Unsigned { .. }
        | BitArrayOption::Big { .. }
        | BitArrayOption::Little { .. }
        | BitArrayOption::Native { .. }
        | BitArrayOption::Unit { .. } => option,
        BitArrayOption::Size {
            location,
            value,
            short_form,
        } => BitArrayOption::Size {
            location,
            value: boxed_expression(value),
            short_form,
        },
    }
}

fn case(
    location: SrcSpan,
    type_: Arc<Type>,
    subjects: Vec<TypedExpr>,
    clauses: Vec<TypedClause>,
    compiled_case: CompiledCase,
) -> TypedExpr {
    let subjects = expressions(subjects);
    let clauses = clauses
        .into_iter()
        .map(
            |Clause {
                 location,
                 pattern,
                 alternative_patterns,
                 guard,
                 then,
             }| Clause {
                location,
                pattern,
                alternative_patterns,
                guard,
                then: expression(then),
            },
        )
        .collect();

    TypedExpr::Case {
        location,
        type_,
        subjects,
        clauses,
        compiled_case,
    }
}
