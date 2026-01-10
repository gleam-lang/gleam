// Gleam Parser
//
// Terminology:
//   Expression Unit:
//     Essentially a thing that goes between operators.
//     Int, Bool, function call, "{" expression-sequence "}", case x {}, ..etc
//
//   Expression:
//     One or more Expression Units separated by an operator
//
//   Binding:
//     (let|let assert|use) name (:TypeAnnotation)? = Expression
//
//   Expression Sequence:
//     * One or more Expressions
//     * A Binding followed by at least one more Expression Sequences
//
// Naming Conventions:
//   parse_x
//      Parse a specific part of the grammar, not erroring if it cannot.
//      Generally returns `Result<Option<A>, ParseError>`, note the inner Option
//
//   expect_x
//      Parse a generic or specific part of the grammar, erroring if it cannot.
//      Generally returns `Result<A, ParseError>`, note no inner Option
//
//   maybe_x
//      Parse a generic part of the grammar. Returning `None` if it cannot.
//      Returns `Some(x)` and advances the token stream if it can.
//
// Operator Precedence Parsing:
//   Needs to take place in expressions and in clause guards.
//   It is accomplished using the Simple Precedence Parser algorithm.
//   See: https://en.wikipedia.org/wiki/Simple_precedence_parser
//
//   It relies or the operator grammar being in the general form:
//   e ::= expr op expr | expr
//   Which just means that exprs and operators always alternate, starting with an expr
//
//   The gist of the algorithm is:
//   Create 2 stacks, one to hold expressions, and one to hold un-reduced operators.
//   While consuming the input stream, if an expression is encountered add it to the top
//   of the expression stack. If an operator is encountered, compare its precedence to the
//   top of the operator stack and perform the appropriate action, which is either using an
//   operator to reduce 2 expressions on the top of the expression stack or put it on the top
//   of the operator stack. When the end of the input is reached, attempt to reduce all of the
//   expressions down to a single expression(or no expression) using the remaining operators
//   on the operator stack. If there are any operators left, or more than 1 expression left
//   this is a syntax error. But the implementation here shouldn't need to handle that case
//   as the outer parser ensures the correct structure.
//
pub mod error;
pub mod extra;
pub mod lexer;
mod token;

use crate::Warning;
use crate::analyse::Inferred;
use crate::ast::{
    Arg, ArgNames, Assert, AssignName, Assignment, AssignmentKind, BinOp, BitArrayOption,
    BitArraySegment, BitArraySize, CAPTURE_VARIABLE, CallArg, Clause, ClauseGuard, Constant,
    CustomType, Definition, Function, FunctionLiteralKind, HasLocation, Import, IntOperator,
    Module, ModuleConstant, Pattern, Publicity, RecordBeingUpdated, RecordConstructor,
    RecordConstructorArg, RecordUpdateArg, SrcSpan, Statement, TailPattern, TargetedDefinition,
    TodoKind, TypeAlias, TypeAst, TypeAstConstructor, TypeAstFn, TypeAstHole, TypeAstTuple,
    TypeAstVar, UnqualifiedImport, UntypedArg, UntypedClause, UntypedClauseGuard, UntypedConstant,
    UntypedDefinition, UntypedExpr, UntypedModule, UntypedPattern, UntypedRecordUpdateArg,
    UntypedStatement, UntypedUseAssignment, Use, UseAssignment,
};
use crate::build::Target;
use crate::error::wrap;
use crate::exhaustiveness::CompiledCase;
use crate::parse::extra::ModuleExtra;
use crate::type_::Deprecation;
use crate::type_::error::{VariableDeclaration, VariableOrigin, VariableSyntax};
use crate::type_::expression::{Implementations, Purity};
use crate::warning::{DeprecatedSyntaxWarning, WarningEmitter};
use camino::Utf8PathBuf;
use ecow::EcoString;
use error::{LexicalError, ParseError, ParseErrorType};
use lexer::{LexResult, Spanned};
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::hash::{Hash, Hasher};
use std::str::FromStr;
pub use token::Token;
use vec1::{Vec1, vec1};

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub struct Parsed {
    pub module: UntypedModule,
    pub extra: ModuleExtra,
}

/// We use this to keep track of the `@internal` annotation for top level
/// definitions. Instead of using just a boolean we want to keep track of the
/// source position of the annotation in case it is present. This way we can
/// report a better error message highlighting the annotation in case it is
/// used on a private definition (it doesn't make sense to mark something
/// private as internal):
///
/// ```txt
/// @internal
/// ^^^^^^^^^ we first get to the annotation
/// fn wibble() {}
/// ^^ and only later discover it's applied on a private definition
///    so we have to keep track of the attribute's position to highlight it
///    in the resulting error message.
/// ```
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum InternalAttribute {
    #[default]
    Missing,
    Present(SrcSpan),
}

#[derive(Debug, Default)]
struct Attributes {
    target: Option<Target>,
    deprecated: Deprecation,
    external_erlang: Option<(EcoString, EcoString, SrcSpan)>,
    external_javascript: Option<(EcoString, EcoString, SrcSpan)>,
    internal: InternalAttribute,
}

impl Attributes {
    fn has_function_only(&self) -> bool {
        self.external_erlang.is_some() || self.external_javascript.is_some()
    }

    fn has_external_for(&self, target: Target) -> bool {
        match target {
            Target::Erlang => self.external_erlang.is_some(),
            Target::JavaScript => self.external_javascript.is_some(),
        }
    }

    fn set_external_for(&mut self, target: Target, ext: Option<(EcoString, EcoString, SrcSpan)>) {
        match target {
            Target::Erlang => self.external_erlang = ext,
            Target::JavaScript => self.external_javascript = ext,
        }
    }
}

//
// Public Interface
//

pub type SpannedString = (SrcSpan, EcoString);

pub fn parse_module(
    path: Utf8PathBuf,
    src: &str,
    warnings: &WarningEmitter,
) -> Result<Parsed, ParseError> {
    let lex = lexer::make_tokenizer(src);
    let mut parser = Parser::new(lex);
    let mut parsed = parser.parse_module()?;
    parsed.extra = parser.extra;

    let src = EcoString::from(src);
    for warning in parser.warnings {
        warnings.emit(Warning::DeprecatedSyntax {
            path: path.clone(),
            src: src.clone(),
            warning,
        });
    }

    for detached in parser.detached_doc_comments {
        warnings.emit(Warning::DetachedDocComment {
            path: path.clone(),
            src: src.clone(),
            location: detached,
        });
    }

    Ok(parsed)
}

//
// Test Interface
//
#[cfg(test)]
pub fn parse_statement_sequence(src: &str) -> Result<Vec1<UntypedStatement>, ParseError> {
    let lex = lexer::make_tokenizer(src);
    let mut parser = Parser::new(lex);
    let expr = parser.parse_statement_seq();
    let expr = parser.ensure_no_errors_or_remaining_input(expr)?;
    match expr {
        Some((e, _)) => Ok(e),
        _ => parse_error(ParseErrorType::ExpectedExpr, SrcSpan { start: 0, end: 0 }),
    }
}

//
// Test Interface
//
#[cfg(test)]
pub fn parse_const_value(src: &str) -> Result<Constant<(), ()>, ParseError> {
    let lex = lexer::make_tokenizer(src);
    let mut parser = Parser::new(lex);
    let expr = parser.parse_const_value();
    let expr = parser.ensure_no_errors_or_remaining_input(expr)?;
    match expr {
        Some(e) => Ok(e),
        _ => parse_error(ParseErrorType::ExpectedExpr, SrcSpan { start: 0, end: 0 }),
    }
}

//
// Parser
//
#[derive(Debug)]
pub struct Parser<T: Iterator<Item = LexResult>> {
    tokens: T,
    lex_errors: Vec<LexicalError>,
    warnings: Vec<DeprecatedSyntaxWarning>,
    tok0: Option<Spanned>,
    tok1: Option<Spanned>,
    extra: ModuleExtra,
    doc_comments: VecDeque<(u32, EcoString)>,
    detached_doc_comments: Vec<SrcSpan>,
}
impl<T> Parser<T>
where
    T: Iterator<Item = LexResult>,
{
    pub fn new(input: T) -> Self {
        let mut parser = Parser {
            tokens: input,
            lex_errors: vec![],
            warnings: vec![],
            tok0: None,
            tok1: None,
            extra: ModuleExtra::new(),
            doc_comments: VecDeque::new(),
            detached_doc_comments: Vec::new(),
        };
        parser.advance();
        parser.advance();
        parser
    }

    fn parse_module(&mut self) -> Result<Parsed, ParseError> {
        let definitions = Parser::series_of(self, &Parser::parse_definition, None);
        let definitions = self.ensure_no_errors_or_remaining_input(definitions)?;
        let module = Module {
            name: "".into(),
            documentation: vec![],
            type_info: (),
            definitions,
            names: Default::default(),
            unused_definition_positions: Default::default(),
        };
        Ok(Parsed {
            module,
            extra: Default::default(),
        })
    }

    // The way the parser is currently implemented, it cannot exit immediately while advancing
    // the token stream upon seeing a LexError. That is to avoid having to put `?` all over the
    // place and instead we collect LexErrors in `self.lex_errors` and attempt to continue parsing.
    // Once parsing has returned we want to surface an error in the order:
    // 1) LexError, 2) ParseError, 3) More Tokens Left
    fn ensure_no_errors_or_remaining_input<A>(
        &mut self,
        parse_result: Result<A, ParseError>,
    ) -> Result<A, ParseError> {
        let parse_result = self.ensure_no_errors(parse_result)?;
        if let Some((start, token, end)) = self.next_tok() {
            // there are still more tokens
            let expected = vec!["An import, const, type, or function.".into()];
            return parse_error(
                ParseErrorType::UnexpectedToken {
                    token,
                    expected,
                    hint: None,
                },
                SrcSpan { start, end },
            );
        }
        // no errors
        Ok(parse_result)
    }

    // The way the parser is currently implemented, it cannot exit immediately
    // while advancing the token stream upon seeing a LexError. That is to avoid
    // having to put `?` all over the place and instead we collect LexErrors in
    // `self.lex_errors` and attempt to continue parsing.
    // Once parsing has returned we want to surface an error in the order:
    // 1) LexError, 2) ParseError
    fn ensure_no_errors<A>(
        &mut self,
        parse_result: Result<A, ParseError>,
    ) -> Result<A, ParseError> {
        if let Some(error) = self.lex_errors.first() {
            // Lex errors first
            let location = error.location;
            let error = *error;
            parse_error(ParseErrorType::LexError { error }, location)
        } else {
            // Return any existing parse error
            parse_result
        }
    }

    fn parse_definition(&mut self) -> Result<Option<TargetedDefinition>, ParseError> {
        let mut attributes = Attributes::default();
        let location = self.parse_attributes(&mut attributes)?;

        let def = match (self.tok0.take(), self.tok1.as_ref()) {
            // Imports
            (Some((start, Token::Import, _)), _) => {
                self.advance();
                self.parse_import(start)
            }
            // Module Constants
            (Some((start, Token::Const, _)), _) => {
                self.advance();
                self.parse_module_const(start, false, &attributes)
            }
            (Some((start, Token::Pub, _)), Some((_, Token::Const, _))) => {
                self.advance();
                self.advance();
                self.parse_module_const(start, true, &attributes)
            }

            // Function
            (Some((start, Token::Fn, _)), _) => {
                self.advance();
                self.parse_function(start, false, false, &mut attributes)
            }
            (Some((start, Token::Pub, _)), Some((_, Token::Fn, _))) => {
                self.advance();
                self.advance();
                self.parse_function(start, true, false, &mut attributes)
            }

            // Custom Types, and Type Aliases
            (Some((start, Token::Type, _)), _) => {
                self.advance();
                self.parse_custom_type(start, false, false, &mut attributes)
            }
            (Some((start, Token::Pub, _)), Some((_, Token::Opaque, _))) => {
                self.advance();
                self.advance();
                let _ = self.expect_one(&Token::Type)?;
                self.parse_custom_type(start, true, true, &mut attributes)
            }
            (Some((start, Token::Pub, _)), Some((_, Token::Type, _))) => {
                self.advance();
                self.advance();
                self.parse_custom_type(start, true, false, &mut attributes)
            }
            (Some((start, Token::Opaque, _)), Some((_, Token::Type, _))) => {
                // A private opaque type makes no sense! We still want to parse it
                // and return an error later during the analysis phase.
                self.advance();
                self.advance();
                self.parse_custom_type(start, false, true, &mut attributes)
            }

            (t0, _) => {
                self.tok0 = t0;
                Ok(None)
            }
        }?;

        match (def, location) {
            (Some(definition), _) if definition.is_function() || definition.is_custom_type() => {
                Ok(Some(TargetedDefinition {
                    definition,
                    target: attributes.target,
                }))
            }

            (Some(definition), None) => Ok(Some(TargetedDefinition {
                definition,
                target: attributes.target,
            })),

            (_, Some(location)) if attributes.has_function_only() => {
                parse_error(ParseErrorType::ExpectedFunctionDefinition, location)
            }

            (Some(definition), _) => Ok(Some(TargetedDefinition {
                definition,
                target: attributes.target,
            })),

            (_, Some(location)) => parse_error(ParseErrorType::ExpectedDefinition, location),

            (None, None) => Ok(None),
        }
    }

    //
    // Parse Expressions
    //

    // examples:
    //   unit
    //   unit op unit
    //   unit op unit pipe unit(call)
    //   unit op unit pipe unit(call) pipe unit(call)
    fn parse_expression(&mut self) -> Result<Option<UntypedExpr>, ParseError> {
        self.parse_expression_inner(false)
    }

    fn parse_expression_inner(
        &mut self,
        is_let_binding: bool,
    ) -> Result<Option<UntypedExpr>, ParseError> {
        // uses the simple operator parser algorithm
        let mut opstack = vec![];
        let mut estack = vec![];
        let mut last_op_start = 0;
        let mut last_op_end = 0;

        // This is used to keep track if we've just ran into a `|>` operator in
        // order to properly parse an echo based on its position: if it is in a
        // pipeline then it isn't expected to be followed by an expression.
        // Otherwise, it's expected to be followed by an expression.
        let mut expression_unit_context = ExpressionUnitContext::Other;

        loop {
            match self.parse_expression_unit(expression_unit_context)? {
                Some(unit) => {
                    self.post_process_expression_unit(&unit, is_let_binding)?;
                    estack.push(unit)
                }
                _ if estack.is_empty() => return Ok(None),
                _ => {
                    return parse_error(
                        ParseErrorType::OpNakedRight,
                        SrcSpan {
                            start: last_op_start,
                            end: last_op_end,
                        },
                    );
                }
            }

            let Some((op_s, t, op_e)) = self.tok0.take() else {
                break;
            };

            let Some(p) = precedence(&t) else {
                self.tok0 = Some((op_s, t, op_e));
                break;
            };

            expression_unit_context = if t == Token::Pipe {
                ExpressionUnitContext::FollowingPipe
            } else {
                ExpressionUnitContext::Other
            };

            // Is Op
            self.advance();
            last_op_start = op_s;
            last_op_end = op_e;
            let _ = handle_op(
                Some(((op_s, t, op_e), p)),
                &mut opstack,
                &mut estack,
                &do_reduce_expression,
            );
        }

        Ok(handle_op(
            None,
            &mut opstack,
            &mut estack,
            &do_reduce_expression,
        ))
    }

    fn post_process_expression_unit(
        &mut self,
        unit: &UntypedExpr,
        is_let_binding: bool,
    ) -> Result<(), ParseError> {
        // Produce better error message for `[x] = [1]` outside
        // of `let` statement.
        if !is_let_binding
            && let UntypedExpr::List { .. } = unit
            && let Some((start, Token::Equal, end)) = self.tok0
        {
            return parse_error(ParseErrorType::NoLetBinding, SrcSpan { start, end });
        }
        Ok(())
    }

    // examples:
    //   1
    //   "one"
    //   True
    //   fn() { "hi" }
    //   unit().unit().unit()
    //   A(a.., label: tuple(1))
    //   { expression_sequence }
    fn parse_expression_unit(
        &mut self,
        context: ExpressionUnitContext,
    ) -> Result<Option<UntypedExpr>, ParseError> {
        let mut expr = match self.tok0.take() {
            Some((start, Token::String { value }, end)) => {
                self.advance();
                UntypedExpr::String {
                    location: SrcSpan { start, end },
                    value,
                }
            }
            Some((start, Token::Int { value, int_value }, end)) => {
                self.advance();
                UntypedExpr::Int {
                    location: SrcSpan { start, end },
                    value,
                    int_value,
                }
            }

            Some((start, Token::Float { value, float_value }, end)) => {
                self.advance();
                UntypedExpr::Float {
                    location: SrcSpan { start, end },
                    value,
                    float_value,
                }
            }

            // var lower_name and UpName
            Some((start, Token::Name { name } | Token::UpName { name }, end)) => {
                self.advance();
                UntypedExpr::Var {
                    location: SrcSpan { start, end },
                    name,
                }
            }

            Some((start, Token::Todo, end)) => {
                self.advance();
                let message = self.maybe_parse_as_message()?;
                let end = message.as_ref().map_or(end, |m| m.location().end);
                UntypedExpr::Todo {
                    location: SrcSpan { start, end },
                    kind: TodoKind::Keyword,
                    message,
                }
            }

            Some((start, Token::Panic, end)) => {
                self.advance();
                let message = self.maybe_parse_as_message()?;
                let end = message.as_ref().map_or(end, |m| m.location().end);
                UntypedExpr::Panic {
                    location: SrcSpan { start, end },
                    message,
                }
            }

            Some((start, Token::Echo, echo_end)) => {
                self.advance();
                if context == ExpressionUnitContext::FollowingPipe {
                    // If an echo is used as a step in a pipeline (`|> echo`)
                    // then it cannot be followed by an expression.
                    let message = self.maybe_parse_as_message()?;
                    let end = message.as_ref().map_or(echo_end, |m| m.location().end);
                    UntypedExpr::Echo {
                        location: SrcSpan { start, end },
                        keyword_end: echo_end,
                        expression: None,
                        message,
                    }
                } else {
                    // Otherwise it must be followed by an expression.
                    // However, you might have noticed we're not erroring if the
                    // expression is not there. Instead we move this error to
                    // the analysis phase so that a wrong usage of echo won't
                    // stop analysis from happening everywhere and be fault
                    // tolerant like everything else.
                    let expression = self.parse_expression()?;
                    let end = expression.as_ref().map_or(echo_end, |e| e.location().end);

                    let message = self.maybe_parse_as_message()?;
                    let end = message.as_ref().map_or(end, |m| m.location().end);

                    UntypedExpr::Echo {
                        location: SrcSpan { start, end },
                        keyword_end: echo_end,
                        expression: expression.map(Box::new),
                        message,
                    }
                }
            }

            Some((start, Token::Hash, _)) => {
                self.advance();
                let _ = self
                    .expect_one(&Token::LeftParen)
                    .map_err(|e| self.add_comment_style_hint(e))?;
                let elements =
                    Parser::series_of(self, &Parser::parse_expression, Some(&Token::Comma))?;
                let (_, end) =
                    self.expect_one_following_series(&Token::RightParen, "an expression")?;
                UntypedExpr::Tuple {
                    location: SrcSpan { start, end },
                    elements,
                }
            }

            // list
            Some((start, Token::LeftSquare, _)) => {
                self.advance();
                let (elements, elements_end_with_comma) = self.series_of_has_trailing_separator(
                    &Parser::parse_expression,
                    Some(&Token::Comma),
                )?;

                // Parse an optional tail
                let mut tail = None;
                let mut elements_after_tail = None;
                let mut dot_dot_location = None;

                if let Some((start, end)) = self.maybe_one(&Token::DotDot) {
                    dot_dot_location = Some((start, end));
                    tail = self.parse_expression()?.map(Box::new);
                    if self.maybe_one(&Token::Comma).is_some() {
                        // See if there's a list of items after the tail,
                        // like `[..wibble, wobble, wabble]`
                        let elements =
                            self.series_of(&Parser::parse_expression, Some(&Token::Comma));
                        match elements {
                            Err(_) => {}
                            Ok(elements) => {
                                elements_after_tail = Some(elements);
                            }
                        };
                    };

                    if tail.is_some() {
                        if !elements_end_with_comma {
                            self.warnings
                                .push(DeprecatedSyntaxWarning::DeprecatedListPrepend {
                                    location: SrcSpan { start, end },
                                });
                        }

                        // Give a better error when there is two consecutive spreads
                        // like `[..wibble, ..wabble, woo]`. However, if there's other
                        // elements after the tail of the list
                        if let Some((second_start, second_end)) = self.maybe_one(&Token::DotDot) {
                            let _second_tail = self.parse_expression();

                            if elements_after_tail.is_none()
                                || elements_after_tail
                                    .as_ref()
                                    .is_some_and(|vec| vec.is_empty())
                            {
                                return parse_error(
                                    ParseErrorType::ListSpreadWithAnotherSpread {
                                        first_spread_location: SrcSpan { start, end },
                                    },
                                    SrcSpan {
                                        start: second_start,
                                        end: second_end,
                                    },
                                );
                            }
                        }
                    }
                }

                let (_, end) = self.expect_one(&Token::RightSquare)?;

                // Return errors for malformed lists
                match dot_dot_location {
                    Some((start, end)) if tail.is_none() => {
                        return parse_error(
                            ParseErrorType::ListSpreadWithoutTail,
                            SrcSpan { start, end },
                        );
                    }
                    _ => {}
                }
                if tail.is_some()
                    && elements.is_empty()
                    && elements_after_tail.as_ref().is_none_or(|e| e.is_empty())
                {
                    return parse_error(
                        ParseErrorType::ListSpreadWithoutElements,
                        SrcSpan { start, end },
                    );
                }

                match elements_after_tail {
                    Some(elements) if !elements.is_empty() => {
                        let (start, end) = match (dot_dot_location, tail) {
                            (Some((start, _)), Some(tail)) => (start, tail.location().end),
                            (_, _) => (start, end),
                        };
                        return parse_error(
                            ParseErrorType::ListSpreadFollowedByElements,
                            SrcSpan { start, end },
                        );
                    }
                    _ => {}
                }

                UntypedExpr::List {
                    location: SrcSpan { start, end },
                    elements,
                    tail,
                }
            }

            // BitArray
            Some((start, Token::LtLt, _)) => {
                self.advance();
                let segments = Parser::series_of(
                    self,
                    &|s| {
                        Parser::parse_bit_array_segment(
                            s,
                            &(|this| this.parse_expression_unit(ExpressionUnitContext::Other)),
                            &Parser::expect_expression,
                            &bit_array_expr_int,
                        )
                    },
                    Some(&Token::Comma),
                )?;
                let (_, end) =
                    self.expect_one_following_series(&Token::GtGt, "a bit array segment")?;
                UntypedExpr::BitArray {
                    location: SrcSpan { start, end },
                    segments,
                }
            }
            Some((start, Token::Fn, _)) => {
                self.advance();
                let mut attributes = Attributes::default();
                match self.parse_function(start, false, true, &mut attributes)? {
                    Some(Definition::Function(Function {
                        location,
                        arguments,
                        body,
                        return_annotation,
                        end_position,
                        ..
                    })) => {
                        let Ok(body) = Vec1::try_from_vec(body) else {
                            return parse_error(ParseErrorType::ExpectedFunctionBody, location);
                        };

                        UntypedExpr::Fn {
                            location: SrcSpan::new(location.start, end_position),
                            end_of_head_byte_index: location.end,
                            kind: FunctionLiteralKind::Anonymous { head: location },
                            arguments,
                            body,
                            return_annotation,
                        }
                    }

                    _ => {
                        // this isn't just none, it could also be Some(UntypedExpr::..)
                        return self.next_tok_unexpected(vec!["An opening parenthesis.".into()]);
                    }
                }
            }

            // expression block  "{" "}"
            Some((start, Token::LeftBrace, _)) => {
                self.advance();
                self.parse_block(start)?
            }

            // case
            Some((start, Token::Case, case_e)) => {
                self.advance();
                let subjects =
                    Parser::series_of(self, &Parser::parse_expression, Some(&Token::Comma))?;
                if self.maybe_one(&Token::LeftBrace).is_some() {
                    let clauses = Parser::series_of(self, &Parser::parse_case_clause, None)?;
                    let (_, end) =
                        self.expect_one_following_series(&Token::RightBrace, "a case clause")?;
                    if subjects.is_empty() {
                        return parse_error(
                            ParseErrorType::ExpectedExpr,
                            SrcSpan { start, end: case_e },
                        );
                    } else {
                        UntypedExpr::Case {
                            location: SrcSpan { start, end },
                            subjects,
                            clauses: Some(clauses),
                        }
                    }
                } else {
                    UntypedExpr::Case {
                        location: SrcSpan::new(
                            start,
                            subjects
                                .last()
                                .map(|subject| subject.location().end)
                                .unwrap_or(case_e),
                        ),
                        subjects,
                        clauses: None,
                    }
                }
            }

            // Helpful error if trying to write an if expression instead of a
            // case.
            Some((start, Token::If, end)) => {
                return parse_error(ParseErrorType::IfExpression, SrcSpan { start, end });
            }

            // Helpful error on possibly trying to group with "(".
            Some((start, Token::LeftParen, _)) => {
                return parse_error(ParseErrorType::ExprLparStart, SrcSpan { start, end: start });
            }

            // Boolean negation
            Some((start, Token::Bang, _end)) => {
                self.advance();
                match self.parse_expression_unit(ExpressionUnitContext::Other)? {
                    Some(value) => UntypedExpr::NegateBool {
                        location: SrcSpan {
                            start,
                            end: value.location().end,
                        },
                        value: Box::from(value),
                    },
                    None => {
                        return parse_error(
                            ParseErrorType::ExpectedExpr,
                            SrcSpan { start, end: start },
                        );
                    }
                }
            }

            // Int negation
            Some((start, Token::Minus, _end)) => {
                self.advance();
                match self.parse_expression_unit(ExpressionUnitContext::Other)? {
                    Some(value) => UntypedExpr::NegateInt {
                        location: SrcSpan {
                            start,
                            end: value.location().end,
                        },
                        value: Box::from(value),
                    },
                    None => {
                        return parse_error(
                            ParseErrorType::ExpectedExpr,
                            SrcSpan { start, end: start },
                        );
                    }
                }
            }

            t0 => {
                self.tok0 = t0;
                return Ok(None);
            }
        };

        // field access and call can stack up
        loop {
            match self.maybe_one(&Token::Dot) {
                Some((dot_start, _)) => {
                    let start = expr.location().start;
                    // field access
                    match self.tok0.take() {
                        // tuple access
                        Some((
                            _,
                            Token::Int {
                                value,
                                int_value: _,
                            },
                            end,
                        )) => {
                            self.advance();
                            let v = value.replace("_", "");
                            match u64::from_str(&v) {
                                Ok(index) => {
                                    expr = UntypedExpr::TupleIndex {
                                        location: SrcSpan { start, end },
                                        index,
                                        tuple: Box::new(expr),
                                    }
                                }
                                _ => {
                                    return parse_error(
                                        ParseErrorType::InvalidTupleAccess,
                                        SrcSpan { start, end },
                                    );
                                }
                            }
                        }

                        Some((label_start, Token::Name { name: label }, end)) => {
                            self.advance();
                            expr = UntypedExpr::FieldAccess {
                                location: SrcSpan { start, end },
                                label_location: SrcSpan {
                                    start: label_start,
                                    end,
                                },
                                label,
                                container: Box::new(expr),
                            }
                        }

                        Some((label_start, Token::UpName { name: label }, end)) => {
                            self.advance();
                            expr = UntypedExpr::FieldAccess {
                                location: SrcSpan { start, end },
                                label_location: SrcSpan {
                                    start: label_start,
                                    end,
                                },
                                label,
                                container: Box::new(expr),
                            }
                        }

                        t0 => {
                            // parse a field access with no label
                            self.tok0 = t0;
                            let end = dot_start + 1;
                            expr = UntypedExpr::FieldAccess {
                                location: SrcSpan { start, end },
                                label_location: SrcSpan {
                                    start: dot_start,
                                    end,
                                },
                                label: "".into(),
                                container: Box::new(expr),
                            };
                            return Ok(Some(expr));
                        }
                    }
                }
                _ => {
                    if self.maybe_one(&Token::LeftParen).is_some() {
                        let start = expr.location().start;
                        match self.maybe_one(&Token::DotDot) {
                            Some((dot_s, _)) => {
                                // Record update
                                let base = self.expect_expression()?;
                                let base_e = base.location().end;
                                let record = RecordBeingUpdated {
                                    base: Box::new(base),
                                    location: SrcSpan {
                                        start: dot_s,
                                        end: base_e,
                                    },
                                };
                                let mut arguments = vec![];
                                if self.maybe_one(&Token::Comma).is_some() {
                                    arguments = Parser::series_of(
                                        self,
                                        &Parser::parse_record_update_arg,
                                        Some(&Token::Comma),
                                    )?;
                                }
                                let (_, end) = self.expect_one(&Token::RightParen)?;

                                expr = UntypedExpr::RecordUpdate {
                                    location: SrcSpan { start, end },
                                    constructor: Box::new(expr),
                                    record,
                                    arguments,
                                };
                            }
                            _ => {
                                // Call
                                let arguments = self.parse_fn_arguments()?;
                                let (_, end) = self.expect_one(&Token::RightParen)?;
                                expr = make_call(expr, arguments, start, end)?;
                            }
                        }
                    } else {
                        // done
                        break;
                    }
                }
            }
        }

        Ok(Some(expr))
    }

    fn add_comment_style_hint(&self, mut err: ParseError) -> ParseError {
        if let ParseErrorType::UnexpectedToken { ref mut hint, .. } = err.error {
            let text =
                "Maybe you meant to create a comment?\nComments in Gleam start with `//`, not `#`";
            *hint = Some(text.into());
        }
        err
    }

    // A `use` expression
    // use <- function
    // use <- function()
    // use <- function(a, b)
    // use <- module.function(a, b)
    // use a, b, c <- function(a, b)
    // use a, b, c, <- function(a, b)
    fn parse_use(&mut self, start: u32, end: u32) -> Result<UntypedStatement, ParseError> {
        let assignments = match self.tok0 {
            Some((_, Token::LArrow, _)) => {
                vec![]
            }
            _ => Parser::series_of(self, &Parser::parse_use_assignment, Some(&Token::Comma))?,
        };

        _ = self.expect_one_following_series(&Token::LArrow, "a use variable assignment")?;
        let call = self.expect_expression()?;

        let assignments_location = match (assignments.first(), assignments.last()) {
            (Some(first), Some(last)) => SrcSpan {
                start: first.location.start,
                end: last.location.end,
            },
            (_, _) => SrcSpan { start, end },
        };

        Ok(Statement::Use(Use {
            location: SrcSpan::new(start, call.location().end),
            assignments_location,
            right_hand_side_location: call.location(),
            assignments,
            call: Box::new(call),
        }))
    }

    fn parse_use_assignment(&mut self) -> Result<Option<UntypedUseAssignment>, ParseError> {
        let start = self.tok0.as_ref().map(|t| t.0).unwrap_or(0);

        let pattern = self
            .parse_pattern(PatternPosition::UsePattern)?
            .ok_or_else(|| ParseError {
                error: ParseErrorType::ExpectedPattern,
                location: SrcSpan { start, end: start },
            })?;

        let annotation = self.parse_type_annotation(&Token::Colon)?;
        let end = match annotation {
            Some(ref a) => a.location().end,
            None => pattern.location().end,
        };

        Ok(Some(UseAssignment {
            location: SrcSpan { start, end },
            pattern,
            annotation,
        }))
    }

    fn maybe_parse_as_message(&mut self) -> Result<Option<Box<UntypedExpr>>, ParseError> {
        let message = if self.maybe_one(&Token::As).is_some() {
            let expression = self.expect_expression_unit(ExpressionUnitContext::Other)?;
            Some(Box::new(expression))
        } else {
            None
        };

        Ok(message)
    }

    // An assignment, with `Let` already consumed
    fn parse_assignment(&mut self, start: u32) -> Result<UntypedStatement, ParseError> {
        let mut kind = match self.tok0 {
            Some((assert_keyword_start, Token::Assert, assert_end)) => {
                _ = self.next_tok();
                AssignmentKind::Assert {
                    location: SrcSpan::new(start, assert_end),
                    assert_keyword_start,
                    message: None,
                }
            }
            _ => AssignmentKind::Let,
        };
        let pattern = match self.parse_pattern(PatternPosition::LetAssignment)? {
            Some(p) => p,
            _ => {
                // DUPE: 62884
                return self.next_tok_unexpected(vec!["A pattern".into()])?;
            }
        };
        let annotation = self.parse_type_annotation(&Token::Colon)?;
        let (eq_s, eq_e) = self.maybe_one(&Token::Equal).ok_or(ParseError {
            error: ParseErrorType::ExpectedEqual,
            location: SrcSpan {
                start: pattern.location().start,
                end: pattern.location().end,
            },
        })?;
        let value = self.parse_expression_inner(true)?.ok_or(match self.tok0 {
            Some((start, Token::DiscardName { .. }, end)) => ParseError {
                error: ParseErrorType::IncorrectName,
                location: SrcSpan { start, end },
            },

            _ => ParseError {
                error: ParseErrorType::ExpectedValue,
                location: SrcSpan {
                    start: eq_s,
                    end: eq_e,
                },
            },
        })?;

        let mut end = value.location().end;

        match &mut kind {
            AssignmentKind::Let | AssignmentKind::Generated => {}
            AssignmentKind::Assert { message, .. } => {
                if self.maybe_one(&Token::As).is_some() {
                    let message_expression =
                        self.expect_expression_unit(ExpressionUnitContext::Other)?;
                    end = message_expression.location().end;
                    *message = Some(message_expression);
                }
            }
        }

        Ok(Statement::Assignment(Box::new(Assignment {
            location: SrcSpan { start, end },
            value,
            compiled_case: CompiledCase::failure(),
            pattern,
            annotation,
            kind,
        })))
    }

    // An assert statement, with `Assert` already consumed
    fn parse_assert(&mut self, start: u32) -> Result<UntypedStatement, ParseError> {
        let value = self.expect_expression()?;
        let mut end = value.location().end;

        let message = if self.maybe_one(&Token::As).is_some() {
            let message_expression = self.expect_expression_unit(ExpressionUnitContext::Other)?;
            end = message_expression.location().end;
            Some(message_expression)
        } else {
            None
        };

        Ok(Statement::Assert(Assert {
            location: SrcSpan { start, end },
            value,
            message,
        }))
    }

    // examples:
    //   expr
    //   expr expr..
    //   expr assignment..
    //   assignment
    //   assignment expr..
    //   assignment assignment..
    fn parse_statement_seq(&mut self) -> Result<Option<(Vec1<UntypedStatement>, u32)>, ParseError> {
        let mut statements = vec![];
        let mut start = None;
        let mut end = 0;

        // Try and parse as many expressions as possible
        while let Some(statement) = self.parse_statement()? {
            if start.is_none() {
                start = Some(statement.location().start);
            }
            end = statement.location().end;
            statements.push(statement);
        }

        match Vec1::try_from_vec(statements) {
            Ok(statements) => Ok(Some((statements, end))),
            Err(_) => Ok(None),
        }
    }

    fn parse_statement(&mut self) -> Result<Option<UntypedStatement>, ParseError> {
        match self.tok0.take() {
            Some((start, Token::Use, end)) => {
                self.advance();
                Ok(Some(self.parse_use(start, end)?))
            }

            Some((start, Token::Let, _)) => {
                self.advance();
                Ok(Some(self.parse_assignment(start)?))
            }

            Some((start, Token::Assert, _)) => {
                self.advance();
                Ok(Some(self.parse_assert(start)?))
            }

            // Helpful error when trying to define a constant inside a function.
            Some((start, Token::Const, end)) => parse_error(
                ParseErrorType::ConstantInsideFunction,
                SrcSpan { start, end },
            ),

            token => {
                self.tok0 = token;
                self.parse_statement_errors()?;
                let expression = self.parse_expression()?.map(Statement::Expression);
                Ok(expression)
            }
        }
    }

    fn parse_statement_errors(&mut self) -> Result<(), ParseError> {
        // Better error: name definitions must start with `let`
        if let Some((_, Token::Name { .. }, _)) = self.tok0.as_ref()
            && let Some((start, Token::Equal | Token::Colon, end)) = self.tok1
        {
            return parse_error(ParseErrorType::NoLetBinding, SrcSpan { start, end });
        }
        Ok(())
    }

    fn parse_block(&mut self, start: u32) -> Result<UntypedExpr, ParseError> {
        let body = self.parse_statement_seq()?;
        let (_, end) = self.expect_one(&Token::RightBrace)?;
        let location = SrcSpan { start, end };
        let statements = match body {
            Some((statements, _)) => statements,
            None => vec1![Statement::Expression(UntypedExpr::Todo {
                kind: TodoKind::EmptyBlock,
                location,
                message: None
            })],
        };

        Ok(UntypedExpr::Block {
            location,
            statements,
        })
    }

    // The left side of an "=" or a "->"
    fn parse_pattern(
        &mut self,
        position: PatternPosition,
    ) -> Result<Option<UntypedPattern>, ParseError> {
        let pattern = match self.tok0.take() {
            // Pattern::Var or Pattern::Constructor start
            Some((start, Token::Name { name }, end)) => {
                self.advance();

                // A variable is not permitted on the left hand side of a `<>`
                if let Some((_, Token::LtGt, _)) = self.tok0.as_ref() {
                    return concat_pattern_variable_left_hand_side_error(start, end);
                }

                if self.maybe_one(&Token::Dot).is_some() {
                    // We're doing this to get a better error message instead of a generic
                    // `I was expecting a type`, you can have a look at this issue to get
                    // a better idea: https://github.com/gleam-lang/gleam/issues/2841.
                    match self.expect_constructor_pattern(Some((start, name, end)), position) {
                        Ok(result) => result,
                        Err(ParseError {
                            location: SrcSpan { end, .. },
                            ..
                        }) => {
                            return parse_error(
                                ParseErrorType::InvalidModuleTypePattern,
                                SrcSpan { start, end },
                            );
                        }
                    }
                } else {
                    Pattern::Variable {
                        origin: VariableOrigin {
                            syntax: VariableSyntax::Variable(name.clone()),
                            declaration: position.to_declaration(),
                        },
                        location: SrcSpan { start, end },
                        name,
                        type_: (),
                    }
                }
            }
            // Constructor
            Some((start, tok @ Token::UpName { .. }, end)) => {
                self.tok0 = Some((start, tok, end));
                self.expect_constructor_pattern(None, position)?
            }

            Some((start, Token::DiscardName { name }, end)) => {
                self.advance();

                // A discard is not permitted on the left hand side of a `<>`
                if let Some((_, Token::LtGt, _)) = self.tok0.as_ref() {
                    return concat_pattern_variable_left_hand_side_error(start, end);
                }

                Pattern::Discard {
                    location: SrcSpan { start, end },
                    name,
                    type_: (),
                }
            }

            Some((start, Token::String { value }, end)) => {
                self.advance();

                match self.tok0 {
                    // String matching with assignment, it could either be a
                    // String prefix matching: "Hello, " as greeting <> name -> ...
                    // or a full string matching: "Hello, World!" as greeting -> ...
                    Some((_, Token::As, _)) => {
                        self.advance();
                        let (name_start, name, name_end) = self.expect_name()?;
                        let name_span = SrcSpan {
                            start: name_start,
                            end: name_end,
                        };

                        match self.tok0 {
                            // String prefix matching with assignment
                            // "Hello, " as greeting <> name -> ...
                            Some((_, Token::LtGt, _)) => {
                                self.advance();
                                let (r_start, right, r_end) = self.expect_assign_name()?;
                                Pattern::StringPrefix {
                                    location: SrcSpan { start, end: r_end },
                                    left_location: SrcSpan {
                                        start,
                                        end: name_end,
                                    },
                                    right_location: SrcSpan {
                                        start: r_start,
                                        end: r_end,
                                    },
                                    left_side_string: value,
                                    left_side_assignment: Some((name, name_span)),
                                    right_side_assignment: right,
                                }
                            }
                            // Full string matching with assignment
                            _ => {
                                return Ok(Some(Pattern::Assign {
                                    name,
                                    location: name_span,
                                    pattern: Box::new(Pattern::String {
                                        location: SrcSpan { start, end },
                                        value,
                                    }),
                                }));
                            }
                        }
                    }

                    // String prefix matching with no left side assignment
                    // "Hello, " <> name -> ...
                    Some((_, Token::LtGt, _)) => {
                        self.advance();
                        let (r_start, right, r_end) = self.expect_assign_name()?;
                        Pattern::StringPrefix {
                            location: SrcSpan { start, end: r_end },
                            left_location: SrcSpan { start, end },
                            right_location: SrcSpan {
                                start: r_start,
                                end: r_end,
                            },
                            left_side_string: value,
                            left_side_assignment: None,
                            right_side_assignment: right,
                        }
                    }

                    // Full string matching
                    // "Hello, World!" -> ...
                    _ => Pattern::String {
                        location: SrcSpan { start, end },
                        value,
                    },
                }
            }
            Some((start, Token::Int { value, int_value }, end)) => {
                self.advance();
                Pattern::Int {
                    location: SrcSpan { start, end },
                    value,
                    int_value,
                }
            }
            Some((start, Token::Float { value, float_value }, end)) => {
                self.advance();
                Pattern::Float {
                    location: SrcSpan { start, end },
                    value,
                    float_value,
                }
            }
            Some((start, Token::Hash, _)) => {
                self.advance();
                let _ = self.expect_one(&Token::LeftParen)?;
                let elements = Parser::series_of(
                    self,
                    &|this| this.parse_pattern(position),
                    Some(&Token::Comma),
                )?;
                let (_, end) = self.expect_one_following_series(&Token::RightParen, "a pattern")?;
                Pattern::Tuple {
                    location: SrcSpan { start, end },
                    elements,
                }
            }
            // BitArray
            Some((start, Token::LtLt, _)) => {
                self.advance();
                let segments = Parser::series_of(
                    self,
                    &|s| {
                        Parser::parse_bit_array_segment(
                            s,
                            &|s| match s.parse_pattern(position) {
                                Ok(Some(Pattern::BitArray { location, .. })) => {
                                    parse_error(ParseErrorType::NestedBitArrayPattern, location)
                                }
                                x => x,
                            },
                            &Parser::expect_bit_array_pattern_segment_arg,
                            &bit_array_size_int,
                        )
                    },
                    Some(&Token::Comma),
                )?;
                let (_, end) =
                    self.expect_one_following_series(&Token::GtGt, "a bit array segment pattern")?;
                Pattern::BitArray {
                    location: SrcSpan { start, end },
                    segments,
                }
            }

            // List
            Some((start, Token::LeftSquare, _)) => {
                self.advance();
                let (elements, elements_end_with_comma) = self.series_of_has_trailing_separator(
                    &|this| this.parse_pattern(position),
                    Some(&Token::Comma),
                )?;

                let mut elements_after_tail = None;
                let mut dot_dot_location = None;
                let tail = match self.tok0 {
                    Some((dot_dot_start, Token::DotDot, dot_dot_end)) => {
                        dot_dot_location = Some((dot_dot_start, dot_dot_end));
                        if !elements.is_empty() && !elements_end_with_comma {
                            self.warnings
                                .push(DeprecatedSyntaxWarning::DeprecatedListPattern {
                                    location: SrcSpan {
                                        start: dot_dot_start,
                                        end: dot_dot_end,
                                    },
                                });
                        }

                        self.advance();
                        let tail = self.parse_pattern(position)?;
                        if self.maybe_one(&Token::Comma).is_some() {
                            // See if there's a list of items after the tail,
                            // like `[..wibble, wobble, wabble]`
                            let elements = Parser::series_of(
                                self,
                                &|this| this.parse_pattern(position),
                                Some(&Token::Comma),
                            );
                            match elements {
                                Err(_) => {}
                                Ok(elements) => {
                                    elements_after_tail = Some(elements);
                                }
                            };
                        };
                        Some(tail)
                    }
                    _ => None,
                };

                let (end, closing_square_bracket_end) =
                    self.expect_one_following_series(&Token::RightSquare, "a pattern")?;

                // If there are elements after the tail, return an error
                match elements_after_tail {
                    Some(elements) if !elements.is_empty() => {
                        let (start, end) = match (dot_dot_location, tail) {
                            (Some((start, _)), Some(Some(tail))) => (start, tail.location().end),
                            (Some((start, end)), Some(None)) => (start, end),
                            (_, _) => (start, end),
                        };
                        return parse_error(
                            ParseErrorType::ListPatternSpreadFollowedByElements,
                            SrcSpan { start, end },
                        );
                    }
                    _ => {}
                }

                let tail = match tail {
                    // There is a tail and it has a Pattern::Var or Pattern::Discard
                    Some(Some(pattern @ (Pattern::Variable { .. } | Pattern::Discard { .. }))) => {
                        Some(pattern)
                    }
                    // There is a tail and but it has no content, implicit discard
                    Some(Some(pattern)) => {
                        return parse_error(ParseErrorType::InvalidTailPattern, pattern.location());
                    }
                    Some(None) => Some(Pattern::Discard {
                        location: SrcSpan {
                            start: closing_square_bracket_end - 1,
                            end: closing_square_bracket_end,
                        },
                        name: "_".into(),
                        type_: (),
                    }),
                    // No tail specified
                    None => None,
                };

                if elements.is_empty() && tail.as_ref().is_some_and(|pattern| pattern.is_discard())
                {
                    self.warnings
                        .push(DeprecatedSyntaxWarning::DeprecatedListCatchAllPattern {
                            location: SrcSpan {
                                start,
                                end: closing_square_bracket_end,
                            },
                        })
                }

                Pattern::List {
                    location: SrcSpan {
                        start,
                        end: closing_square_bracket_end,
                    },
                    elements,
                    tail: tail.map(|tail_pattern| {
                        let dot_dot_start = dot_dot_location
                            .expect("parsed tail with no preceding `..`")
                            .0;

                        Box::new(TailPattern {
                            location: SrcSpan::new(dot_dot_start, tail_pattern.location().end),
                            pattern: tail_pattern,
                        })
                    }),
                    type_: (),
                }
            }

            // No pattern
            t0 => {
                self.tok0 = t0;
                return Ok(None);
            }
        };

        match self.tok0 {
            Some((_, Token::As, _)) => {
                self.advance();
                let (start, name, end) = self.expect_name()?;
                Ok(Some(Pattern::Assign {
                    name,
                    location: SrcSpan { start, end },
                    pattern: Box::new(pattern),
                }))
            }
            _ => Ok(Some(pattern)),
        }
    }

    fn add_multi_line_clause_hint(&self, mut err: ParseError) -> ParseError {
        if let ParseErrorType::UnexpectedToken { ref mut hint, .. } = err.error {
            *hint = Some("Did you mean to wrap a multi line clause in curly braces?".into());
        }
        err
    }

    // examples:
    //   pattern -> expr
    //   pattern, pattern if -> expr
    //   pattern, pattern | pattern, pattern if -> expr
    fn parse_case_clause(&mut self) -> Result<Option<UntypedClause>, ParseError> {
        let patterns = self.parse_patterns(PatternPosition::CaseClause)?;
        match &patterns.first() {
            Some(lead) => {
                let mut alternative_patterns = vec![];
                while let Some((vbar_start, vbar_end)) = self.maybe_one(&Token::Vbar) {
                    let patterns = self.parse_patterns(PatternPosition::CaseClause)?;
                    if patterns.is_empty() {
                        return parse_error(
                            ParseErrorType::ExpectedPattern,
                            SrcSpan {
                                start: vbar_start,
                                end: vbar_end,
                            },
                        );
                    }
                    alternative_patterns.push(patterns);
                }
                let guard = self.parse_case_clause_guard()?;
                let (arr_s, arr_e) = self
                    .expect_one(&Token::RArrow)
                    .map_err(|e| self.add_multi_line_clause_hint(e))?;
                let then = self.parse_expression()?;
                match then {
                    Some(then) => Ok(Some(Clause {
                        location: SrcSpan {
                            start: lead.location().start,
                            end: then.location().end,
                        },
                        pattern: patterns,
                        alternative_patterns,
                        guard,
                        then,
                    })),
                    _ => match self.tok0 {
                        Some((start, Token::DiscardName { .. }, end)) => {
                            parse_error(ParseErrorType::IncorrectName, SrcSpan { start, end })
                        }
                        _ => parse_error(
                            ParseErrorType::ExpectedExpr,
                            SrcSpan {
                                start: arr_s,
                                end: arr_e,
                            },
                        ),
                    },
                }
            }
            _ => Ok(None),
        }
    }
    fn parse_patterns(
        &mut self,
        position: PatternPosition,
    ) -> Result<Vec<UntypedPattern>, ParseError> {
        Parser::series_of(
            self,
            &|this| this.parse_pattern(position),
            Some(&Token::Comma),
        )
    }

    // examples:
    //   if a
    //   if a < b
    //   if a < b || b < c
    fn parse_case_clause_guard(&mut self) -> Result<Option<UntypedClauseGuard>, ParseError> {
        let Some((start, end)) = self.maybe_one(&Token::If) else {
            return Ok(None);
        };
        let clause_guard_result = self.parse_clause_guard_inner();
        // If inner clause is none, a warning should be shown to let the user
        // know that empty clauses in guards are deprecated.
        if let Ok(None) = clause_guard_result {
            self.warnings
                .push(DeprecatedSyntaxWarning::DeprecatedEmptyClauseGuard {
                    location: SrcSpan { start, end },
                });
        }
        clause_guard_result
    }

    fn parse_clause_guard_inner(&mut self) -> Result<Option<UntypedClauseGuard>, ParseError> {
        let mut opstack = vec![];
        let mut estack = vec![];
        let mut last_op_start = 0;
        let mut last_op_end = 0;
        loop {
            match self.parse_case_clause_guard_unit()? {
                Some(unit) => estack.push(unit),
                _ => {
                    if estack.is_empty() {
                        return Ok(None);
                    } else {
                        return parse_error(
                            ParseErrorType::OpNakedRight,
                            SrcSpan {
                                start: last_op_start,
                                end: last_op_end,
                            },
                        );
                    }
                }
            }

            let Some((op_s, t, op_e)) = self.tok0.take() else {
                break;
            };

            let Some(precedence) = t.guard_precedence() else {
                // Is not Op
                self.tok0 = Some((op_s, t, op_e));
                break;
            };

            // Is Op
            self.advance();
            last_op_start = op_s;
            last_op_end = op_e;
            let _ = handle_op(
                Some(((op_s, t, op_e), precedence)),
                &mut opstack,
                &mut estack,
                &do_reduce_clause_guard,
            );
        }

        Ok(handle_op(
            None,
            &mut opstack,
            &mut estack,
            &do_reduce_clause_guard,
        ))
    }

    /// Checks if we have an unexpected left parenthesis and returns appropriate
    /// error if it is a function call.
    fn parse_function_call_in_clause_guard(&mut self, start: u32) -> Result<(), ParseError> {
        if let Some((l_paren_start, l_paren_end)) = self.maybe_one(&Token::LeftParen) {
            if let Ok((_, end)) = self
                .parse_fn_arguments()
                .and(self.expect_one(&Token::RightParen))
            {
                return parse_error(ParseErrorType::CallInClauseGuard, SrcSpan { start, end });
            }

            return parse_error(
                ParseErrorType::UnexpectedToken {
                    token: Token::LeftParen,
                    expected: vec![Token::RArrow.to_string().into()],
                    hint: None,
                },
                SrcSpan {
                    start: l_paren_start,
                    end: l_paren_end,
                },
            )
            .map_err(|e| self.add_multi_line_clause_hint(e));
        }

        Ok(())
    }

    // examples
    // a
    // 1
    // a.1
    // { a }
    // a || b
    // a < b || b < c
    fn parse_case_clause_guard_unit(&mut self) -> Result<Option<UntypedClauseGuard>, ParseError> {
        match self.tok0.take() {
            Some((start, Token::Bang, _)) => {
                self.advance();
                match self.parse_case_clause_guard_unit()? {
                    Some(unit) => Ok(Some(ClauseGuard::Not {
                        location: SrcSpan {
                            start,
                            end: unit.location().end,
                        },
                        expression: Box::new(unit),
                    })),
                    None => {
                        parse_error(ParseErrorType::ExpectedValue, SrcSpan { start, end: start })
                    }
                }
            }

            Some((start, Token::Name { name }, end)) => {
                self.advance();

                self.parse_function_call_in_clause_guard(start)?;

                let mut unit =
                    match self.parse_record_in_clause_guard(&name, SrcSpan { start, end })? {
                        Some(record) => record,
                        _ => ClauseGuard::Var {
                            location: SrcSpan { start, end },
                            type_: (),
                            name,
                            definition_location: SrcSpan::default(),
                        },
                    };

                loop {
                    let dot_s = match self.maybe_one(&Token::Dot) {
                        Some((dot_s, _)) => dot_s,
                        None => return Ok(Some(unit)),
                    };

                    match self.next_tok() {
                        Some((
                            _,
                            Token::Int {
                                value,
                                int_value: _,
                            },
                            int_e,
                        )) => {
                            let v = value.replace("_", "");
                            match u64::from_str(&v) {
                                Ok(index) => {
                                    unit = ClauseGuard::TupleIndex {
                                        location: SrcSpan {
                                            start: dot_s,
                                            end: int_e,
                                        },
                                        index,
                                        type_: (),
                                        tuple: Box::new(unit),
                                    };
                                }
                                _ => {
                                    return parse_error(
                                        ParseErrorType::InvalidTupleAccess,
                                        SrcSpan { start, end },
                                    );
                                }
                            }
                        }

                        Some((name_start, Token::Name { name: label }, name_end)) => {
                            self.parse_function_call_in_clause_guard(start)?;

                            unit = ClauseGuard::FieldAccess {
                                label_location: SrcSpan {
                                    start: name_start,
                                    end: name_end,
                                },
                                index: None,
                                label,
                                type_: (),
                                container: Box::new(unit),
                            };
                        }

                        Some((start, _, end)) => {
                            return parse_error(
                                ParseErrorType::IncorrectName,
                                SrcSpan { start, end },
                            );
                        }

                        _ => return self.next_tok_unexpected(vec!["A positive integer".into()]),
                    }
                }
            }

            Some((start, Token::LeftBrace, _)) => {
                self.advance();
                Ok(Some(self.parse_case_clause_guard_block(start)?))
            }

            t0 => {
                self.tok0 = t0;
                match self.parse_const_value()? {
                    Some(const_val) => {
                        // Constant
                        Ok(Some(ClauseGuard::Constant(const_val)))
                    }
                    _ => Ok(None),
                }
            }
        }
    }

    fn parse_case_clause_guard_block(
        &mut self,
        start: u32,
    ) -> Result<UntypedClauseGuard, ParseError> {
        let body = self.parse_clause_guard_inner()?;

        let Some(body) = body else {
            let location = match self.next_tok() {
                Some((_, Token::RightBrace, end)) => SrcSpan { start, end },
                Some((_, _, _)) | None => SrcSpan {
                    start,
                    end: start + 1,
                },
            };

            return parse_error(ParseErrorType::EmptyGuardBlock, location);
        };

        let (_, end) = self.expect_one(&Token::RightBrace)?;
        Ok(ClauseGuard::Block {
            location: SrcSpan { start, end },
            value: Box::new(body),
        })
    }

    fn parse_record_in_clause_guard(
        &mut self,
        module: &EcoString,
        module_location: SrcSpan,
    ) -> Result<Option<UntypedClauseGuard>, ParseError> {
        let (name, end) = match (self.tok0.take(), self.peek_tok1()) {
            (Some((_, Token::Dot, _)), Some(Token::UpName { .. })) => {
                self.advance(); // dot
                let Some((_, Token::UpName { name }, end)) = self.next_tok() else {
                    return Ok(None);
                };
                (name, end)
            }
            (tok0, _) => {
                self.tok0 = tok0;
                return Ok(None);
            }
        };

        match self.parse_const_record_finish(
            module_location.start,
            Some((module.clone(), module_location)),
            name,
            end,
        )? {
            Some(record) => Ok(Some(ClauseGuard::Constant(record))),
            _ => Ok(None),
        }
    }

    // examples:
    //   UpName( args )
    fn expect_constructor_pattern(
        &mut self,
        module: Option<(u32, EcoString, u32)>,
        position: PatternPosition,
    ) -> Result<UntypedPattern, ParseError> {
        let (name_start, name, name_end) = self.expect_upname()?;
        let mut start = name_start;
        let (arguments, spread, end) =
            self.parse_constructor_pattern_arguments(name_end, position)?;
        if let Some((s, _, _)) = module {
            start = s;
        }
        Ok(Pattern::Constructor {
            location: SrcSpan { start, end },
            name_location: SrcSpan::new(name_start, name_end),
            arguments,
            module: module.map(|(start, n, end)| (n, SrcSpan { start, end })),
            name,
            spread,
            constructor: Inferred::Unknown,
            type_: (),
        })
    }

    // examples:
    //   ( args )
    #[allow(clippy::type_complexity)]
    fn parse_constructor_pattern_arguments(
        &mut self,
        upname_end: u32,
        position: PatternPosition,
    ) -> Result<(Vec<CallArg<UntypedPattern>>, Option<SrcSpan>, u32), ParseError> {
        if self.maybe_one(&Token::LeftParen).is_some() {
            let (arguments, arguments_end_with_comma) = self.series_of_has_trailing_separator(
                &|this| this.parse_constructor_pattern_arg(position),
                Some(&Token::Comma),
            )?;

            let spread = self
                .maybe_one(&Token::DotDot)
                .map(|(start, end)| SrcSpan { start, end });

            if let Some(spread_location) = spread {
                let _ = self.maybe_one(&Token::Comma);
                if !arguments.is_empty() && !arguments_end_with_comma {
                    self.warnings
                        .push(DeprecatedSyntaxWarning::DeprecatedRecordSpreadPattern {
                            location: spread_location,
                        })
                }
            }
            let (_, end) = self.expect_one(&Token::RightParen)?;
            Ok((arguments, spread, end))
        } else {
            Ok((vec![], None, upname_end))
        }
    }

    // examples:
    //   a: <pattern>
    //   a:
    //   <pattern>
    fn parse_constructor_pattern_arg(
        &mut self,
        position: PatternPosition,
    ) -> Result<Option<CallArg<UntypedPattern>>, ParseError> {
        match (self.tok0.take(), self.tok1.take()) {
            // named arg
            (Some((start, Token::Name { name }, _)), Some((_, Token::Colon, end))) => {
                self.advance();
                self.advance();
                match self.parse_pattern(position)? {
                    Some(value) => Ok(Some(CallArg {
                        implicit: None,
                        location: SrcSpan {
                            start,
                            end: value.location().end,
                        },
                        label: Some(name),
                        value,
                    })),
                    _ => {
                        // Argument supplied with a label shorthand.
                        Ok(Some(CallArg {
                            implicit: None,
                            location: SrcSpan { start, end },
                            label: Some(name.clone()),
                            value: UntypedPattern::Variable {
                                origin: VariableOrigin {
                                    syntax: VariableSyntax::LabelShorthand(name.clone()),
                                    declaration: position.to_declaration(),
                                },
                                name,
                                location: SrcSpan { start, end },
                                type_: (),
                            },
                        }))
                    }
                }
            }
            // unnamed arg
            (t0, t1) => {
                self.tok0 = t0;
                self.tok1 = t1;
                match self.parse_pattern(position)? {
                    Some(value) => Ok(Some(CallArg {
                        implicit: None,
                        location: value.location(),
                        label: None,
                        value,
                    })),
                    _ => Ok(None),
                }
            }
        }
    }

    // examples:
    //   a: expr
    //   a:
    fn parse_record_update_arg(&mut self) -> Result<Option<UntypedRecordUpdateArg>, ParseError> {
        match self.maybe_name() {
            Some((start, label, _)) => {
                let (_, end) = self.expect_one(&Token::Colon)?;
                let value = self.parse_expression()?;
                match value {
                    Some(value) => Ok(Some(UntypedRecordUpdateArg {
                        label,
                        location: SrcSpan {
                            start,
                            end: value.location().end,
                        },
                        value,
                    })),
                    _ => {
                        // Argument supplied with a label shorthand.
                        Ok(Some(UntypedRecordUpdateArg {
                            label: label.clone(),
                            location: SrcSpan { start, end },
                            value: UntypedExpr::Var {
                                name: label,
                                location: SrcSpan { start, end },
                            },
                        }))
                    }
                }
            }
            _ => Ok(None),
        }
    }

    //
    // Parse Functions
    //

    // Starts after "fn"
    //
    // examples:
    //   fn a(name: String) -> String { .. }
    //   pub fn a(name name: String) -> String { .. }
    fn parse_function(
        &mut self,
        start: u32,
        public: bool,
        is_anon: bool,
        attributes: &mut Attributes,
    ) -> Result<Option<UntypedDefinition>, ParseError> {
        let documentation = if is_anon {
            None
        } else {
            self.take_documentation(start)
        };
        let mut name = None;
        if !is_anon {
            let (name_start, n, name_end) = self.expect_name()?;
            name = Some((
                SrcSpan {
                    start: name_start,
                    end: name_end,
                },
                n,
            ));
        }
        if let Some((less_start, less_end)) = self.maybe_one(&Token::Less) {
            return Err(ParseError {
                error: ParseErrorType::FunctionDefinitionAngleGenerics,
                location: SrcSpan {
                    start: less_start,
                    end: less_end,
                },
            });
        }
        let _ = self
            .expect_one(&Token::LeftParen)
            .map_err(|e| self.add_anon_function_hint(e))?;
        let arguments = Parser::series_of(
            self,
            &|parser| Parser::parse_fn_param(parser, is_anon),
            Some(&Token::Comma),
        )?;
        let (_, rpar_e) =
            self.expect_one_following_series(&Token::RightParen, "a function parameter")?;

        // Check for TypeScript-style return type annotation (:) instead of arrow (->)
        if let Some((colon_start, colon_end)) = self.maybe_one(&Token::Colon) {
            return Err(ParseError {
                error: ParseErrorType::UnexpectedToken {
                    token: Token::Colon,
                    expected: vec!["`->`".into()],
                    hint: Some("Return type annotations are written using `->`, not `:`".into()),
                },
                location: SrcSpan {
                    start: colon_start,
                    end: colon_end,
                },
            });
        };

        let return_annotation = self.parse_type_annotation(&Token::RArrow)?;

        let (body_start, body, end, end_position) = match self.maybe_one(&Token::LeftBrace) {
            Some((left_brace_start, _)) => {
                let some_body = self.parse_statement_seq()?;
                let (_, right_brace_end) = self.expect_one(&Token::RightBrace)?;
                let end = return_annotation
                    .as_ref()
                    .map(|l| l.location().end)
                    .unwrap_or(rpar_e);
                let body = match some_body {
                    None => vec![Statement::Expression(UntypedExpr::Todo {
                        kind: TodoKind::EmptyFunction {
                            function_location: SrcSpan { start, end },
                        },
                        location: SrcSpan {
                            start: left_brace_start + 1,
                            end: right_brace_end,
                        },
                        message: None,
                    })],
                    Some((body, _)) => body.to_vec(),
                };

                (Some(left_brace_start), body, end, right_brace_end)
            }

            None => (None, vec![], rpar_e, rpar_e),
        };

        Ok(Some(Definition::Function(Function {
            documentation,
            location: SrcSpan { start, end },
            end_position,
            body_start,
            publicity: self.publicity(public, attributes.internal)?,
            name,
            arguments,
            body,
            return_type: (),
            return_annotation,
            deprecation: std::mem::take(&mut attributes.deprecated),
            external_erlang: attributes.external_erlang.take(),
            external_javascript: attributes.external_javascript.take(),
            implementations: Implementations {
                gleam: true,
                can_run_on_erlang: true,
                can_run_on_javascript: true,
                uses_erlang_externals: false,
                uses_javascript_externals: false,
            },
            purity: Purity::Pure,
        })))
    }

    fn add_anon_function_hint(&self, mut err: ParseError) -> ParseError {
        if let ParseErrorType::UnexpectedToken {
            ref mut hint,
            token: Token::Name { .. },
            ..
        } = err.error
        {
            *hint = Some("Only module-level functions can be named.".into());
        }
        err
    }

    fn publicity(
        &self,
        public: bool,
        internal: InternalAttribute,
    ) -> Result<Publicity, ParseError> {
        match (internal, public) {
            (InternalAttribute::Missing, true) => Ok(Publicity::Public),
            (InternalAttribute::Missing, false) => Ok(Publicity::Private),
            (InternalAttribute::Present(location), true) => Ok(Publicity::Internal {
                attribute_location: Some(location),
            }),
            (InternalAttribute::Present(location), false) => Err(ParseError {
                error: ParseErrorType::RedundantInternalAttribute,
                location,
            }),
        }
    }

    // Parse a single function definition param
    //
    // examples:
    //   _
    //   a
    //   a a
    //   a _
    //   a _:A
    //   a a:A
    fn parse_fn_param(&mut self, is_anon: bool) -> Result<Option<UntypedArg>, ParseError> {
        let (start, names, mut end) = match (self.tok0.take(), self.tok1.take()) {
            // labeled discard
            (
                Some((start, Token::Name { name: label }, tok0_end)),
                Some((name_start, Token::DiscardName { name }, end)),
            ) => {
                if is_anon {
                    return parse_error(
                        ParseErrorType::UnexpectedLabel,
                        SrcSpan {
                            start,
                            end: tok0_end,
                        },
                    );
                }

                self.advance();
                self.advance();
                (
                    start,
                    ArgNames::LabelledDiscard {
                        name,
                        name_location: SrcSpan::new(name_start, end),
                        label,
                        label_location: SrcSpan::new(start, tok0_end),
                    },
                    end,
                )
            }
            // discard
            (Some((start, Token::DiscardName { name }, end)), t1) => {
                self.tok1 = t1;
                self.advance();
                (
                    start,
                    ArgNames::Discard {
                        name,
                        location: SrcSpan { start, end },
                    },
                    end,
                )
            }
            // labeled name
            (
                Some((start, Token::Name { name: label }, tok0_end)),
                Some((name_start, Token::Name { name }, end)),
            ) => {
                if is_anon {
                    return parse_error(
                        ParseErrorType::UnexpectedLabel,
                        SrcSpan {
                            start,
                            end: tok0_end,
                        },
                    );
                }

                self.advance();
                self.advance();
                (
                    start,
                    ArgNames::NamedLabelled {
                        name,
                        name_location: SrcSpan::new(name_start, end),
                        label,
                        label_location: SrcSpan::new(start, tok0_end),
                    },
                    end,
                )
            }
            // name
            (Some((start, Token::Name { name }, end)), t1) => {
                self.tok1 = t1;
                self.advance();
                (
                    start,
                    ArgNames::Named {
                        name,
                        location: SrcSpan { start, end },
                    },
                    end,
                )
            }
            (t0, t1) => {
                self.tok0 = t0;
                self.tok1 = t1;
                return Ok(None);
            }
        };
        let annotation = match self.parse_type_annotation(&Token::Colon)? {
            Some(a) => {
                end = a.location().end;
                Some(a)
            }
            _ => None,
        };
        Ok(Some(Arg {
            location: SrcSpan { start, end },
            type_: (),
            names,
            annotation,
        }))
    }

    // Parse function call arguments, no parens
    //
    // examples:
    //   _
    //   expr, expr
    //   a: _, expr
    //   a: expr, _, b: _
    fn parse_fn_arguments(&mut self) -> Result<Vec<ParserArg>, ParseError> {
        let arguments = Parser::series_of(self, &Parser::parse_fn_argument, Some(&Token::Comma))?;
        Ok(arguments)
    }

    // Parse a single function call arg
    //
    // examples:
    //   _
    //   expr
    //   a: _
    //   a: expr
    fn parse_fn_argument(&mut self) -> Result<Option<ParserArg>, ParseError> {
        let label = match (self.tok0.take(), self.tok1.take()) {
            (Some((start, Token::Name { name }, _)), Some((_, Token::Colon, end))) => {
                self.advance();
                self.advance();
                Some((start, name, end))
            }
            (t0, t1) => {
                self.tok0 = t0;
                self.tok1 = t1;
                None
            }
        };

        match self.parse_expression()? {
            Some(value) => {
                let arg = match label {
                    Some((start, label, _)) => CallArg {
                        implicit: None,
                        label: Some(label),
                        location: SrcSpan {
                            start,
                            end: value.location().end,
                        },
                        value,
                    },
                    _ => CallArg {
                        implicit: None,
                        label: None,
                        location: value.location(),
                        value,
                    },
                };
                Ok(Some(ParserArg::Arg(Box::new(arg))))
            }
            _ => {
                match self.maybe_discard_name() {
                    Some((name_start, name, name_end)) => {
                        let arg = match label {
                            Some((label_start, label, _)) => ParserArg::Hole {
                                label: Some(label),
                                arg_location: SrcSpan {
                                    start: label_start,
                                    end: name_end,
                                },
                                discard_location: SrcSpan {
                                    start: name_start,
                                    end: name_end,
                                },
                                name,
                            },
                            _ => ParserArg::Hole {
                                label: None,
                                arg_location: SrcSpan {
                                    start: name_start,
                                    end: name_end,
                                },
                                discard_location: SrcSpan {
                                    start: name_start,
                                    end: name_end,
                                },
                                name,
                            },
                        };

                        Ok(Some(arg))
                    }
                    _ => {
                        match label {
                            Some((start, label, end)) => {
                                // Argument supplied with a label shorthand.
                                Ok(Some(ParserArg::Arg(Box::new(CallArg {
                                    implicit: None,
                                    label: Some(label.clone()),
                                    location: SrcSpan { start, end },
                                    value: UntypedExpr::Var {
                                        name: label,
                                        location: SrcSpan { start, end },
                                    },
                                }))))
                            }
                            _ => Ok(None),
                        }
                    }
                }
            }
        }
    }

    //
    // Parse Custom Types
    //

    // examples:
    //   type A { A }
    //   type A { A(String) }
    //   type Box(inner_type) { Box(inner: inner_type) }
    //   type NamedBox(inner_type) { Box(String, inner: inner_type) }
    fn parse_custom_type(
        &mut self,
        start: u32,
        public: bool,
        opaque: bool,
        attributes: &mut Attributes,
    ) -> Result<Option<UntypedDefinition>, ParseError> {
        let documentation = self.take_documentation(start);
        let (name_start, name, parameters, end, name_end) = self.expect_type_name()?;
        let name_location = SrcSpan::new(name_start, name_end);
        let (constructors, end_position) = if self.maybe_one(&Token::LeftBrace).is_some() {
            // Custom Type
            let constructors = Parser::series_of(
                self,
                &|p| {
                    // The only attribute supported on constructors is @deprecated
                    let mut attributes = Attributes::default();
                    let attr_loc = Parser::parse_attributes(p, &mut attributes)?;

                    if let Some(attr_span) = attr_loc {
                        // Expecting all but the deprecated atterbutes to be default
                        if attributes.external_erlang.is_some()
                            || attributes.external_javascript.is_some()
                            || attributes.target.is_some()
                            || attributes.internal != InternalAttribute::Missing
                        {
                            return parse_error(
                                ParseErrorType::UnknownAttributeRecordVariant,
                                attr_span,
                            );
                        }
                    }

                    match Parser::maybe_upname(p) {
                        Some((c_s, c_n, c_e)) => {
                            let documentation = p.take_documentation(c_s);
                            let (arguments, arguments_e) =
                                Parser::parse_type_constructor_arguments(p)?;
                            let end = arguments_e.max(c_e);
                            Ok(Some(RecordConstructor {
                                location: SrcSpan { start: c_s, end },
                                name_location: SrcSpan {
                                    start: c_s,
                                    end: c_e,
                                },
                                name: c_n,
                                arguments,
                                documentation,
                                deprecation: attributes.deprecated,
                            }))
                        }
                        _ => Ok(None),
                    }
                },
                // No separator
                None,
            )?;
            let (_, close_end) = self.expect_custom_type_close(&name, public, opaque)?;
            (constructors, close_end)
        } else {
            match self.maybe_one(&Token::Equal) {
                Some((eq_s, eq_e)) => {
                    // Type Alias
                    if opaque {
                        return parse_error(
                            ParseErrorType::OpaqueTypeAlias,
                            SrcSpan { start, end },
                        );
                    }

                    match self.parse_type()? {
                        Some(t) => {
                            let type_end = t.location().end;
                            return Ok(Some(Definition::TypeAlias(TypeAlias {
                                documentation,
                                location: SrcSpan::new(start, type_end),
                                publicity: self.publicity(public, attributes.internal)?,
                                alias: name,
                                name_location,
                                parameters,
                                type_ast: t,
                                type_: (),
                                deprecation: std::mem::take(&mut attributes.deprecated),
                            })));
                        }
                        _ => {
                            return parse_error(
                                ParseErrorType::ExpectedType,
                                SrcSpan::new(eq_s, eq_e),
                            );
                        }
                    }
                }
                _ => (vec![], end),
            }
        };

        Ok(Some(Definition::CustomType(CustomType {
            documentation,
            location: SrcSpan { start, end },
            end_position,
            publicity: self.publicity(public, attributes.internal)?,
            opaque,
            name,
            name_location,
            parameters,
            constructors,
            typed_parameters: vec![],
            deprecation: std::mem::take(&mut attributes.deprecated),
            external_erlang: std::mem::take(&mut attributes.external_erlang),
            external_javascript: std::mem::take(&mut attributes.external_javascript),
        })))
    }

    // examples:
    //   A
    //   A(one, two)
    fn expect_type_name(
        &mut self,
    ) -> Result<(u32, EcoString, Vec<SpannedString>, u32, u32), ParseError> {
        let (start, upname, end) = self.expect_upname()?;
        if let Some((par_s, _)) = self.maybe_one(&Token::LeftParen) {
            let arguments =
                Parser::series_of(self, &|p| Ok(Parser::maybe_name(p)), Some(&Token::Comma))?;
            let (_, par_e) = self.expect_one_following_series(&Token::RightParen, "a name")?;
            if arguments.is_empty() {
                return parse_error(
                    ParseErrorType::TypeDefinitionNoArguments,
                    SrcSpan::new(par_s, par_e),
                );
            }
            let arguments2 = arguments
                .into_iter()
                .map(|(start, name, end)| (SrcSpan { start, end }, name))
                .collect();
            Ok((start, upname, arguments2, par_e, end))
        } else if let Some((less_start, less_end)) = self.maybe_one(&Token::Less) {
            let mut arguments = Parser::series_of(
                self,
                &|p|
                        // Permit either names (`a`) or upnames (`A`) in this error-handling mode,
                        // as upnames are common in other languages. Convert to lowercase so the
                        // example is correct whichever was used.
                        Ok(Parser::maybe_name(p)
                            .or_else(|| Parser::maybe_upname(p))
                            .map(|(_, name, _)| name.to_lowercase())),
                Some(&Token::Comma),
            )?;

            // If no type arguments were parsed, fall back to a dummy type argument as an example,
            // because `Type()` would be invalid
            if arguments.is_empty() {
                arguments = vec!["value".into()];
            }

            Err(ParseError {
                error: ParseErrorType::TypeDefinitionAngleGenerics {
                    name: upname,
                    arguments,
                },
                location: SrcSpan {
                    start: less_start,
                    end: less_end,
                },
            })
        } else {
            Ok((start, upname, vec![], end, end))
        }
    }

    // examples:
    //   *no args*
    //   ()
    //   (a, b)
    fn parse_type_constructor_arguments(
        &mut self,
    ) -> Result<(Vec<RecordConstructorArg<()>>, u32), ParseError> {
        if self.maybe_one(&Token::LeftParen).is_some() {
            let arguments = Parser::series_of(
                self,
                &|p| match (p.tok0.take(), p.tok1.take()) {
                    (
                        Some((start, Token::Name { name }, name_end)),
                        Some((_, Token::Colon, end)),
                    ) => {
                        let _ = Parser::next_tok(p);
                        let _ = Parser::next_tok(p);
                        let doc = p.take_documentation(start);
                        match Parser::parse_type(p)? {
                            Some(type_ast) => {
                                let end = type_ast.location().end;
                                Ok(Some(RecordConstructorArg {
                                    label: Some((SrcSpan::new(start, name_end), name)),
                                    ast: type_ast,
                                    location: SrcSpan { start, end },
                                    type_: (),
                                    doc,
                                }))
                            }
                            None => {
                                parse_error(ParseErrorType::ExpectedType, SrcSpan { start, end })
                            }
                        }
                    }
                    (t0, t1) => {
                        p.tok0 = t0;
                        p.tok1 = t1;
                        match Parser::parse_type(p)? {
                            Some(type_ast) => {
                                let doc = match &p.tok0 {
                                    Some((start, _, _)) => p.take_documentation(*start),
                                    None => None,
                                };
                                let type_location = type_ast.location();
                                Ok(Some(RecordConstructorArg {
                                    label: None,
                                    ast: type_ast,
                                    location: type_location,
                                    type_: (),
                                    doc,
                                }))
                            }
                            None => Ok(None),
                        }
                    }
                },
                Some(&Token::Comma),
            )?;
            let (_, end) = self
                .expect_one_following_series(&Token::RightParen, "a constructor argument name")?;
            Ok((arguments, end))
        } else {
            Ok((vec![], 0))
        }
    }

    //
    // Parse Type Annotations
    //

    // examples:
    //   :a
    //   :Int
    //   :Result(a, _)
    //   :Result(Result(a, e), #(_, String))
    fn parse_type_annotation(&mut self, start_tok: &Token) -> Result<Option<TypeAst>, ParseError> {
        if let Some((start, end)) = self.maybe_one(start_tok) {
            match self.parse_type() {
                Ok(None) => parse_error(ParseErrorType::ExpectedType, SrcSpan { start, end }),
                other => other,
            }
        } else {
            Ok(None)
        }
    }

    // Parse the type part of a type annotation, same as `parse_type_annotation` minus the ":"
    fn parse_type(&mut self) -> Result<Option<TypeAst>, ParseError> {
        match self.tok0.take() {
            // Type hole
            Some((start, Token::DiscardName { name }, end)) => {
                self.advance();
                Ok(Some(TypeAst::Hole(TypeAstHole {
                    location: SrcSpan { start, end },
                    name,
                })))
            }

            // Tuple
            Some((start, Token::Hash, _)) => {
                self.advance();
                let _ = self.expect_one(&Token::LeftParen)?;
                let elements = self.parse_types()?;
                let (_, end) = self.expect_one(&Token::RightParen)?;
                Ok(Some(TypeAst::Tuple(TypeAstTuple {
                    location: SrcSpan { start, end },
                    elements,
                })))
            }

            // Function
            Some((start, Token::Fn, _)) => {
                self.advance();
                let _ = self.expect_one(&Token::LeftParen)?;
                let arguments =
                    Parser::series_of(self, &|x| Parser::parse_type(x), Some(&Token::Comma))?;
                let _ = self.expect_one_following_series(&Token::RightParen, "a type")?;
                let (arr_s, arr_e) = self.expect_one(&Token::RArrow)?;
                let return_ = self.parse_type()?;
                match return_ {
                    Some(return_) => Ok(Some(TypeAst::Fn(TypeAstFn {
                        location: SrcSpan {
                            start,
                            end: return_.location().end,
                        },
                        return_: Box::new(return_),
                        arguments,
                    }))),
                    _ => parse_error(
                        ParseErrorType::ExpectedType,
                        SrcSpan {
                            start: arr_s,
                            end: arr_e,
                        },
                    ),
                }
            }

            // Constructor function
            Some((start, Token::UpName { name }, end)) => {
                self.advance();
                self.parse_type_name_finish(start, start, None, name, end)
            }

            // Constructor Module or type Variable
            Some((start, Token::Name { name: mod_name }, end)) => {
                self.advance();
                if self.maybe_one(&Token::Dot).is_some() {
                    let (name_start, upname, upname_e) = self.expect_upname()?;
                    self.parse_type_name_finish(
                        start,
                        name_start,
                        Some((mod_name, SrcSpan { start, end })),
                        upname,
                        upname_e,
                    )
                } else {
                    Ok(Some(TypeAst::Var(TypeAstVar {
                        location: SrcSpan { start, end },
                        name: mod_name,
                    })))
                }
            }

            t0 => {
                self.tok0 = t0;
                Ok(None)
            }
        }
    }

    // Parse the '( ... )' of a type name
    fn parse_type_name_finish(
        &mut self,
        start: u32,
        name_start: u32,
        module: Option<(EcoString, SrcSpan)>,
        name: EcoString,
        end: u32,
    ) -> Result<Option<TypeAst>, ParseError> {
        if let Some((par_s, _)) = self.maybe_one(&Token::LeftParen) {
            let arguments = self.parse_types()?;
            let (_, par_e) = self.expect_one(&Token::RightParen)?;
            Ok(Some(TypeAst::Constructor(TypeAstConstructor {
                location: SrcSpan { start, end: par_e },
                name_location: SrcSpan {
                    start: name_start,
                    end,
                },
                module,
                name,
                arguments,
                start_parentheses: Some(par_s),
            })))
        } else if let Some((less_start, less_end)) = self.maybe_one(&Token::Less) {
            let arguments = self.parse_types()?;
            Err(ParseError {
                error: ParseErrorType::TypeUsageAngleGenerics {
                    name,
                    module: module.map(|(module_name, _)| module_name),
                    arguments,
                },
                location: SrcSpan {
                    start: less_start,
                    end: less_end,
                },
            })
        } else {
            Ok(Some(TypeAst::Constructor(TypeAstConstructor {
                location: SrcSpan { start, end },
                name_location: SrcSpan {
                    start: name_start,
                    end,
                },
                module,
                name,
                arguments: vec![],
                start_parentheses: None,
            })))
        }
    }

    // For parsing a comma separated "list" of types, for tuple, constructor, and function
    fn parse_types(&mut self) -> Result<Vec<TypeAst>, ParseError> {
        let elements = Parser::series_of(self, &|p| Parser::parse_type(p), Some(&Token::Comma))?;
        Ok(elements)
    }

    //
    // Parse Imports
    //

    // examples:
    //   import a
    //   import a/b
    //   import a/b.{c}
    //   import a/b.{c as d} as e
    fn parse_import(&mut self, import_start: u32) -> Result<Option<UntypedDefinition>, ParseError> {
        let mut start = 0;
        let mut end;
        let mut module = EcoString::new();
        let mut last_segment_start;
        let mut last_segment_end;

        // Gather module names
        loop {
            let (s, name, e) = self.expect_name()?;
            if module.is_empty() {
                start = s;
            } else {
                module.push('/');
            }
            module.push_str(&name);
            end = e;
            last_segment_start = s;
            last_segment_end = e;

            // Useful error for : import a/.{b}
            if let Some((s, _)) = self.maybe_one(&Token::SlashDot) {
                return parse_error(
                    ParseErrorType::ExpectedName,
                    SrcSpan {
                        start: s + 1,
                        end: s + 1,
                    },
                );
            }

            // break if there's no trailing slash
            if self.maybe_one(&Token::Slash).is_none() {
                break;
            }
        }

        let (_, documentation) = self.take_documentation(start).unzip();

        // Gather imports
        let mut unqualified_values = vec![];
        let mut unqualified_types = vec![];

        if let Some((dot_start, dot_end)) = self.maybe_one(&Token::Dot) {
            if let Err(e) = self.expect_one(&Token::LeftBrace) {
                // If the module does contain a '/', then it's unlikely that the user
                // intended for the import to be pythonic, so skip this.
                if module.contains('/') {
                    return Err(e);
                }

                // Catch `import gleam.io` and provide a more helpful error...
                let ParseErrorType::UnexpectedToken {
                    token: Token::Name { name } | Token::UpName { name },
                    ..
                } = &e.error
                else {
                    return Err(e);
                };

                return Err(ParseError {
                    error: ParseErrorType::IncorrectImportModuleSeparator {
                        module,
                        item: name.clone(),
                    },
                    location: SrcSpan::new(dot_start, dot_end),
                });
            };

            let parsed = self.parse_unqualified_imports()?;
            unqualified_types = parsed.types;
            unqualified_values = parsed.values;
            let (_, e) = self.expect_one(&Token::RightBrace)?;
            end = e;
        }

        // Parse as_name
        let mut as_name = None;
        if let Some((as_start, _)) = self.maybe_one(&Token::As) {
            let (_, name, e) = self.expect_assign_name()?;

            end = e;
            as_name = Some((
                name,
                SrcSpan {
                    start: as_start,
                    end,
                },
            ));
        }

        Ok(Some(Definition::Import(Import {
            documentation,
            location: SrcSpan {
                start: import_start,
                end,
            },
            module_location: SrcSpan {
                start: last_segment_start,
                end: last_segment_end,
            },
            unqualified_values,
            unqualified_types,
            module,
            as_name,
            package: (),
        })))
    }

    // [Name (as Name)? | UpName (as Name)? ](, [Name (as Name)? | UpName (as Name)?])*,?
    fn parse_unqualified_imports(&mut self) -> Result<ParsedUnqualifiedImports, ParseError> {
        let mut imports = ParsedUnqualifiedImports::default();
        loop {
            // parse imports
            match self.tok0.take() {
                Some((start, Token::Name { name }, end)) => {
                    self.advance();
                    let location = SrcSpan { start, end };
                    let mut import = UnqualifiedImport {
                        name,
                        location,
                        imported_name_location: location,
                        as_name: None,
                    };
                    if self.maybe_one(&Token::As).is_some() {
                        let (_, as_name, end) = self.expect_name()?;
                        import.as_name = Some(as_name);
                        import.location.end = end;
                    }
                    imports.values.push(import)
                }

                Some((start, Token::UpName { name }, end)) => {
                    self.advance();
                    let location = SrcSpan { start, end };
                    let mut import = UnqualifiedImport {
                        name,
                        location,
                        imported_name_location: location,
                        as_name: None,
                    };
                    if self.maybe_one(&Token::As).is_some() {
                        let (_, as_name, end) = self.expect_upname()?;
                        import.as_name = Some(as_name);
                        import.location.end = end;
                    }
                    imports.values.push(import)
                }

                Some((start, Token::Type, _)) => {
                    self.advance();
                    let (name_start, name, end) = self.expect_upname()?;
                    let location = SrcSpan { start, end };
                    let mut import = UnqualifiedImport {
                        name,
                        location,
                        imported_name_location: SrcSpan::new(name_start, end),
                        as_name: None,
                    };
                    if self.maybe_one(&Token::As).is_some() {
                        let (_, as_name, end) = self.expect_upname()?;
                        import.as_name = Some(as_name);
                        import.location.end = end;
                    }
                    imports.types.push(import)
                }

                t0 => {
                    self.tok0 = t0;
                    break;
                }
            }
            // parse comma
            match self.tok0 {
                Some((_, Token::Comma, _)) => {
                    self.advance();
                }
                _ => break,
            }
        }
        Ok(imports)
    }

    //
    // Parse Constants
    //

    // examples:
    //   const a = 1
    //   const a:Int = 1
    //   pub const a:Int = 1
    fn parse_module_const(
        &mut self,
        start: u32,
        public: bool,
        attributes: &Attributes,
    ) -> Result<Option<UntypedDefinition>, ParseError> {
        let (name_start, name, name_end) = self.expect_name()?;
        let documentation = self.take_documentation(name_start);

        let annotation = self.parse_type_annotation(&Token::Colon)?;

        let (eq_s, eq_e) = self.expect_one(&Token::Equal)?;
        match self.parse_const_value()? {
            Some(value) => {
                Ok(Some(Definition::ModuleConstant(ModuleConstant {
                    documentation,
                    location: SrcSpan {
                        start,

                        // End after the type annotation if it's there, otherwise after the name
                        end: annotation
                            .as_ref()
                            .map(|annotation| annotation.location().end)
                            .unwrap_or(0)
                            .max(name_end),
                    },
                    publicity: self.publicity(public, attributes.internal)?,
                    name,
                    name_location: SrcSpan::new(name_start, name_end),
                    annotation,
                    value: Box::new(value),
                    type_: (),
                    deprecation: attributes.deprecated.clone(),
                    implementations: Implementations {
                        gleam: true,
                        can_run_on_erlang: true,
                        can_run_on_javascript: true,
                        uses_erlang_externals: false,
                        uses_javascript_externals: false,
                    },
                })))
            }
            _ => parse_error(
                ParseErrorType::NoValueAfterEqual,
                SrcSpan {
                    start: eq_s,
                    end: eq_e,
                },
            ),
        }
    }

    // examples:
    //   1
    //   "hi"
    //   True
    //   [1,2,3]
    //   wibble <> "wobble"
    fn parse_const_value(&mut self) -> Result<Option<UntypedConstant>, ParseError> {
        let constant_result = self.parse_const_value_unit();
        match constant_result {
            Ok(Some(constant)) => self.parse_const_maybe_concatenation(constant),
            _ => constant_result,
        }
    }

    fn parse_const_value_unit(&mut self) -> Result<Option<UntypedConstant>, ParseError> {
        match self.tok0.take() {
            Some((start, Token::String { value }, end)) => {
                self.advance();
                Ok(Some(Constant::String {
                    value,
                    location: SrcSpan { start, end },
                }))
            }

            Some((start, Token::Float { value, float_value }, end)) => {
                self.advance();
                Ok(Some(Constant::Float {
                    value,
                    location: SrcSpan { start, end },
                    float_value,
                }))
            }

            Some((start, Token::Int { value, int_value }, end)) => {
                self.advance();
                Ok(Some(Constant::Int {
                    value,
                    int_value,
                    location: SrcSpan { start, end },
                }))
            }

            Some((start, Token::Hash, _)) => {
                self.advance();
                let _ = self.expect_one(&Token::LeftParen)?;
                let elements =
                    Parser::series_of(self, &Parser::parse_const_value, Some(&Token::Comma))?;
                let (_, end) =
                    self.expect_one_following_series(&Token::RightParen, "a constant value")?;
                Ok(Some(Constant::Tuple {
                    elements,
                    location: SrcSpan { start, end },
                    type_: (),
                }))
            }

            Some((start, Token::LeftSquare, _)) => {
                self.advance();
                let elements =
                    Parser::series_of(self, &Parser::parse_const_value, Some(&Token::Comma))?;
                let (_, end) =
                    self.expect_one_following_series(&Token::RightSquare, "a constant value")?;
                Ok(Some(Constant::List {
                    elements,
                    location: SrcSpan { start, end },
                    type_: (),
                }))
            }
            // BitArray
            Some((start, Token::LtLt, _)) => {
                self.advance();
                let segments = Parser::series_of(
                    self,
                    &|s| {
                        Parser::parse_bit_array_segment(
                            s,
                            &Parser::parse_const_value,
                            &Parser::expect_const_int,
                            &bit_array_const_int,
                        )
                    },
                    Some(&Token::Comma),
                )?;
                let (_, end) =
                    self.expect_one_following_series(&Token::GtGt, "a bit array segment")?;
                Ok(Some(Constant::BitArray {
                    location: SrcSpan { start, end },
                    segments,
                }))
            }

            Some((start, Token::UpName { name }, end)) => {
                self.advance();
                self.parse_const_record_finish(start, None, name, end)
            }

            Some((start, Token::Name { name }, module_end))
                if self.peek_tok1() == Some(&Token::Dot) =>
            {
                self.advance(); // name
                self.advance(); // dot

                match self.tok0.take() {
                    Some((_, Token::UpName { name: upname }, end)) => {
                        self.advance(); // upname
                        self.parse_const_record_finish(
                            start,
                            Some((name, SrcSpan::new(start, module_end))),
                            upname,
                            end,
                        )
                    }
                    Some((_, Token::Name { name: end_name }, end)) => {
                        self.advance(); // name

                        match self.tok0 {
                            Some((_, Token::LeftParen, _)) => parse_error(
                                ParseErrorType::UnexpectedFunction,
                                SrcSpan {
                                    start,
                                    end: end + 1,
                                },
                            ),
                            _ => Ok(Some(Constant::Var {
                                location: SrcSpan { start, end },
                                module: Some((name, SrcSpan::new(start, module_end))),
                                name: end_name,
                                constructor: None,
                                type_: (),
                            })),
                        }
                    }
                    Some((start, token, end)) => parse_error(
                        ParseErrorType::UnexpectedToken {
                            token,
                            expected: vec!["UpName".into(), "Name".into()],
                            hint: None,
                        },
                        SrcSpan { start, end },
                    ),
                    None => {
                        parse_error(ParseErrorType::UnexpectedEof, SrcSpan { start: 0, end: 0 })
                    }
                }
            }

            Some((start, Token::Name { name }, end)) => {
                self.advance(); // name

                match self.tok0 {
                    Some((_, Token::LeftParen, _)) => parse_error(
                        ParseErrorType::UnexpectedFunction,
                        SrcSpan {
                            start,
                            end: end + 1,
                        },
                    ),
                    _ => Ok(Some(Constant::Var {
                        location: SrcSpan { start, end },
                        module: None,
                        name,
                        constructor: None,
                        type_: (),
                    })),
                }
            }

            // Helpful error for fn
            Some((start, Token::Fn, end)) => {
                parse_error(ParseErrorType::NotConstType, SrcSpan { start, end })
            }

            t0 => {
                self.tok0 = t0;
                Ok(None)
            }
        }
    }

    fn parse_const_maybe_concatenation(
        &mut self,
        left: UntypedConstant,
    ) -> Result<Option<UntypedConstant>, ParseError> {
        match self.tok0.take() {
            Some((op_start, Token::LtGt, op_end)) => {
                self.advance();

                match self.parse_const_value() {
                    Ok(Some(right_constant_value)) => Ok(Some(Constant::StringConcatenation {
                        location: SrcSpan {
                            start: left.location().start,
                            end: right_constant_value.location().end,
                        },
                        left: Box::new(left),
                        right: Box::new(right_constant_value),
                    })),
                    _ => parse_error(
                        ParseErrorType::OpNakedRight,
                        SrcSpan {
                            start: op_start,
                            end: op_end,
                        },
                    ),
                }
            }
            t0 => {
                self.tok0 = t0;
                Ok(Some(left))
            }
        }
    }

    // Parse the '( .. )' of a const type constructor
    fn parse_const_record_finish(
        &mut self,
        start: u32,
        module: Option<(EcoString, SrcSpan)>,
        name: EcoString,
        end: u32,
    ) -> Result<Option<UntypedConstant>, ParseError> {
        match self.maybe_one(&Token::LeftParen) {
            Some((par_s, _)) => {
                if self.maybe_one(&Token::DotDot).is_some() {
                    let record = match self.parse_const_value()? {
                        Some(value) => RecordBeingUpdated {
                            location: value.location(),
                            base: Box::new(value),
                        },
                        None => {
                            return parse_error(
                                ParseErrorType::UnexpectedEof,
                                SrcSpan::new(par_s, par_s + 2),
                            );
                        }
                    };

                    let mut update_arguments = vec![];
                    if self.maybe_one(&Token::Comma).is_some() {
                        update_arguments = Parser::series_of(
                            self,
                            &Parser::parse_const_record_update_arg,
                            Some(&Token::Comma),
                        )?;
                    }

                    let (_, par_e) = self.expect_one_following_series(
                        &Token::RightParen,
                        "a constant record update argument",
                    )?;

                    let constructor_location = SrcSpan { start, end };

                    Ok(Some(Constant::RecordUpdate {
                        location: SrcSpan { start, end: par_e },
                        constructor_location,
                        module,
                        name,
                        record,
                        arguments: update_arguments,
                        tag: (),
                        type_: (),
                        field_map: Inferred::Unknown,
                    }))
                } else {
                    let arguments = Parser::series_of(
                        self,
                        &Parser::parse_const_record_arg,
                        Some(&Token::Comma),
                    )?;

                    let (_, par_e) = self.expect_one_following_series(
                        &Token::RightParen,
                        "a constant record argument",
                    )?;

                    if arguments.is_empty() {
                        return parse_error(
                            ParseErrorType::ConstantRecordConstructorNoArguments,
                            SrcSpan::new(par_s, par_e),
                        );
                    }

                    Ok(Some(Constant::Record {
                        location: SrcSpan { start, end: par_e },
                        module,
                        name,
                        arguments,
                        tag: (),
                        type_: (),
                        field_map: Inferred::Unknown,
                        record_constructor: None,
                    }))
                }
            }
            _ => Ok(Some(Constant::Record {
                location: SrcSpan { start, end },
                module,
                name,
                arguments: vec![],
                tag: (),
                type_: (),
                field_map: Inferred::Unknown,
                record_constructor: None,
            })),
        }
    }

    // examples:
    //  name: const
    //  const
    //  name:
    fn parse_const_record_arg(&mut self) -> Result<Option<CallArg<UntypedConstant>>, ParseError> {
        let label = match (self.tok0.take(), self.tok1.take()) {
            // Named arg
            (Some((start, Token::Name { name }, _)), Some((_, Token::Colon, end))) => {
                self.advance();
                self.advance();
                Some((start, name, end))
            }

            // Unnamed arg
            (t0, t1) => {
                self.tok0 = t0;
                self.tok1 = t1;
                None
            }
        };

        match self.parse_const_value()? {
            Some(value) => match label {
                Some((start, label, _)) => Ok(Some(CallArg {
                    implicit: None,
                    location: SrcSpan {
                        start,
                        end: value.location().end,
                    },
                    value,
                    label: Some(label),
                })),
                _ => Ok(Some(CallArg {
                    implicit: None,
                    location: value.location(),
                    value,
                    label: None,
                })),
            },
            _ => {
                match label {
                    Some((start, label, end)) => {
                        // Argument supplied with a label shorthand.
                        Ok(Some(CallArg {
                            implicit: None,
                            location: SrcSpan { start, end },
                            label: Some(label.clone()),
                            value: UntypedConstant::Var {
                                location: SrcSpan { start, end },
                                constructor: None,
                                module: None,
                                name: label,
                                type_: (),
                            },
                        }))
                    }
                    _ => Ok(None),
                }
            }
        }
    }

    fn parse_const_record_update_arg(
        &mut self,
    ) -> Result<Option<RecordUpdateArg<UntypedConstant>>, ParseError> {
        let (start, label, label_end) = match (self.tok0.take(), self.tok1.take()) {
            // Named arg - required for record updates
            (Some((start, Token::Name { name }, _)), Some((_, Token::Colon, end))) => {
                self.advance();
                self.advance();
                (start, name, end)
            }

            // Unnamed arg or other - return error since record updates require labels
            (Some((start, Token::Name { name }, end)), t1) => {
                self.tok0 = Some((start, Token::Name { name: name.clone() }, end));
                self.tok1 = t1;

                // Check if this is label shorthand (name without colon)
                // In this case, use the name as both label and value
                match self.parse_const_value()? {
                    Some(value) if value.location() == SrcSpan { start, end } => {
                        return Ok(Some(RecordUpdateArg {
                            label: name.clone(),
                            location: SrcSpan { start, end },
                            value,
                        }));
                    }
                    _ => {
                        self.tok0 = Some((start, Token::Name { name }, end));
                        return parse_error(ParseErrorType::ExpectedName, SrcSpan { start, end });
                    }
                }
            }

            (t0, t1) => {
                self.tok0 = t0;
                self.tok1 = t1;
                return Ok(None);
            }
        };

        match self.parse_const_value()? {
            Some(value) => Ok(Some(RecordUpdateArg {
                label,
                location: SrcSpan {
                    start,
                    end: value.location().end,
                },
                value,
            })),
            _ => {
                // Label shorthand: field without value means field: field
                Ok(Some(RecordUpdateArg {
                    label: label.clone(),
                    location: SrcSpan {
                        start,
                        end: label_end,
                    },
                    value: UntypedConstant::Var {
                        location: SrcSpan {
                            start,
                            end: label_end,
                        },
                        constructor: None,
                        module: None,
                        name: label,
                        type_: (),
                    },
                }))
            }
        }
    }

    //
    // Bit String parsing
    //

    // The structure is roughly the same for pattern, const, and expr
    // that's why these functions take functions
    //
    // pattern (: option)?
    fn parse_bit_array_segment<A>(
        &mut self,
        value_parser: &impl Fn(&mut Self) -> Result<Option<A>, ParseError>,
        arg_parser: &impl Fn(&mut Self) -> Result<A, ParseError>,
        to_int_segment: &impl Fn(EcoString, BigInt, u32, u32) -> A,
    ) -> Result<Option<BitArraySegment<A, ()>>, ParseError>
    where
        A: HasLocation + std::fmt::Debug,
    {
        match value_parser(self)? {
            Some(value) => {
                let options = if self.maybe_one(&Token::Colon).is_some() {
                    Parser::series_of(
                        self,
                        &|s| Parser::parse_bit_array_option(s, &arg_parser, &to_int_segment),
                        Some(&Token::Minus),
                    )?
                } else {
                    vec![]
                };
                let end = options
                    .last()
                    .map(|o| o.location().end)
                    .unwrap_or_else(|| value.location().end);
                Ok(Some(BitArraySegment {
                    location: SrcSpan {
                        start: value.location().start,
                        end,
                    },
                    value: Box::new(value),
                    type_: (),
                    options,
                }))
            }
            _ => Ok(None),
        }
    }

    // examples:
    //   1
    //   size(1)
    //   size(five)
    //   utf8
    fn parse_bit_array_option<A: std::fmt::Debug>(
        &mut self,
        arg_parser: &impl Fn(&mut Self) -> Result<A, ParseError>,
        to_int_segment: &impl Fn(EcoString, BigInt, u32, u32) -> A,
    ) -> Result<Option<BitArrayOption<A>>, ParseError> {
        match self.next_tok() {
            // named segment
            Some((start, Token::Name { name }, end)) => {
                if self.maybe_one(&Token::LeftParen).is_some() {
                    // named function segment
                    match name.as_str() {
                        "unit" => match self.next_tok() {
                            Some((int_s, Token::Int { value, .. }, int_e)) => {
                                let (_, end) = self.expect_one(&Token::RightParen)?;
                                let v = value.replace("_", "");
                                match u8::from_str(&v) {
                                    Ok(units) if units > 0 => Ok(Some(BitArrayOption::Unit {
                                        location: SrcSpan { start, end },
                                        value: units,
                                    })),

                                    _ => Err(ParseError {
                                        error: ParseErrorType::InvalidBitArrayUnit,
                                        location: SrcSpan {
                                            start: int_s,
                                            end: int_e,
                                        },
                                    }),
                                }
                            }
                            _ => self.next_tok_unexpected(vec!["positive integer".into()]),
                        },

                        "size" => {
                            let value = arg_parser(self)?;
                            let (_, end) = self.expect_one(&Token::RightParen)?;
                            Ok(Some(BitArrayOption::Size {
                                location: SrcSpan { start, end },
                                value: Box::new(value),
                                short_form: false,
                            }))
                        }
                        _ => parse_error(
                            ParseErrorType::InvalidBitArraySegment,
                            SrcSpan { start, end },
                        ),
                    }
                } else {
                    str_to_bit_array_option(&name, SrcSpan { start, end })
                        .ok_or(ParseError {
                            error: ParseErrorType::InvalidBitArraySegment,
                            location: SrcSpan { start, end },
                        })
                        .map(Some)
                }
            }
            // int segment
            Some((start, Token::Int { value, int_value }, end)) => Ok(Some(BitArrayOption::Size {
                location: SrcSpan { start, end },
                value: Box::new(to_int_segment(value, int_value, start, end)),
                short_form: true,
            })),
            // invalid
            _ => self.next_tok_unexpected(vec![
                "A valid bit array segment type".into(),
                "See: https://tour.gleam.run/data-types/bit-arrays/".into(),
            ]),
        }
    }

    fn expect_bit_array_pattern_segment_arg(&mut self) -> Result<UntypedPattern, ParseError> {
        Ok(Pattern::BitArraySize(self.expect_bit_array_size()?))
    }

    fn expect_bit_array_size(&mut self) -> Result<BitArraySize<()>, ParseError> {
        let left = match self.next_tok() {
            Some((start, Token::Name { name }, end)) => BitArraySize::Variable {
                location: SrcSpan { start, end },
                name,
                constructor: None,
                type_: (),
            },
            Some((start, Token::Int { value, int_value }, end)) => BitArraySize::Int {
                location: SrcSpan { start, end },
                value,
                int_value,
            },
            Some((start, Token::LeftBrace, _)) => {
                let inner = self.expect_bit_array_size()?;
                let (_, end) = self.expect_one(&Token::RightBrace)?;

                BitArraySize::Block {
                    location: SrcSpan { start, end },
                    inner: Box::new(inner),
                }
            }
            _ => return self.next_tok_unexpected(vec!["A variable name or an integer".into()]),
        };

        let Some((start, token, end)) = self.tok0.take() else {
            return Ok(left);
        };

        match token {
            Token::Plus => {
                _ = self.next_tok();
                let right = self.expect_bit_array_size()?;

                Ok(self.bit_array_size_binary_operator(left, right, IntOperator::Add))
            }
            Token::Minus => {
                _ = self.next_tok();
                let right = self.expect_bit_array_size()?;

                Ok(self.bit_array_size_binary_operator(left, right, IntOperator::Subtract))
            }
            Token::Star => {
                _ = self.next_tok();
                let right = self.expect_bit_array_size()?;

                Ok(
                    self.bit_array_size_high_precedence_operator(
                        left,
                        right,
                        IntOperator::Multiply,
                    ),
                )
            }
            Token::Slash => {
                _ = self.next_tok();
                let right = self.expect_bit_array_size()?;

                Ok(self.bit_array_size_high_precedence_operator(left, right, IntOperator::Divide))
            }
            Token::Percent => {
                _ = self.next_tok();
                let right = self.expect_bit_array_size()?;

                Ok(self.bit_array_size_high_precedence_operator(
                    left,
                    right,
                    IntOperator::Remainder,
                ))
            }
            Token::Name { .. }
            | Token::UpName { .. }
            | Token::DiscardName { .. }
            | Token::Int { .. }
            | Token::Float { .. }
            | Token::String { .. }
            | Token::CommentDoc { .. }
            | Token::LeftParen
            | Token::RightParen
            | Token::LeftSquare
            | Token::RightSquare
            | Token::LeftBrace
            | Token::RightBrace
            | Token::Less
            | Token::Greater
            | Token::LessEqual
            | Token::GreaterEqual
            | Token::PlusDot
            | Token::MinusDot
            | Token::StarDot
            | Token::SlashDot
            | Token::LessDot
            | Token::GreaterDot
            | Token::LessEqualDot
            | Token::GreaterEqualDot
            | Token::LtGt
            | Token::Colon
            | Token::Comma
            | Token::Hash
            | Token::Bang
            | Token::Equal
            | Token::EqualEqual
            | Token::NotEqual
            | Token::Vbar
            | Token::VbarVbar
            | Token::AmperAmper
            | Token::LtLt
            | Token::GtGt
            | Token::Pipe
            | Token::Dot
            | Token::RArrow
            | Token::LArrow
            | Token::DotDot
            | Token::At
            | Token::EndOfFile
            | Token::CommentNormal
            | Token::CommentModule
            | Token::NewLine
            | Token::As
            | Token::Assert
            | Token::Auto
            | Token::Case
            | Token::Const
            | Token::Delegate
            | Token::Derive
            | Token::Echo
            | Token::Else
            | Token::Fn
            | Token::If
            | Token::Implement
            | Token::Import
            | Token::Let
            | Token::Macro
            | Token::Opaque
            | Token::Panic
            | Token::Pub
            | Token::Test
            | Token::Todo
            | Token::Type
            | Token::Use => {
                self.tok0 = Some((start, token, end));
                Ok(left)
            }
        }
    }

    fn bit_array_size_binary_operator(
        &self,
        left: BitArraySize<()>,
        right: BitArraySize<()>,
        operator: IntOperator,
    ) -> BitArraySize<()> {
        let start = left.location().start;
        let end = right.location().end;

        BitArraySize::BinaryOperator {
            left: Box::new(left),
            right: Box::new(right),
            operator,
            location: SrcSpan { start, end },
        }
    }

    fn bit_array_size_high_precedence_operator(
        &self,
        left: BitArraySize<()>,
        right: BitArraySize<()>,
        operator: IntOperator,
    ) -> BitArraySize<()> {
        match right {
            BitArraySize::Int { .. }
            | BitArraySize::Variable { .. }
            | BitArraySize::Block { .. } => {
                self.bit_array_size_binary_operator(left, right, operator)
            }
            // This operator has the highest precedence of the possible operators
            // for bit array size patterns. So, `a * b + c` should be parsed as
            // `(a * b) + c`. If the right-hand side of this operator is another
            // operator, we rearrange it to ensure correct precedence.
            BitArraySize::BinaryOperator {
                operator: right_operator,
                left: middle,
                right,
                ..
            } => self.bit_array_size_binary_operator(
                self.bit_array_size_binary_operator(left, *middle, operator),
                *right,
                right_operator,
            ),
        }
    }

    fn expect_const_int(&mut self) -> Result<UntypedConstant, ParseError> {
        match self.next_tok() {
            Some((start, Token::Int { value, int_value }, end)) => Ok(Constant::Int {
                location: SrcSpan { start, end },
                value,
                int_value,
            }),
            _ => self.next_tok_unexpected(vec!["A variable name or an integer".into()]),
        }
    }

    fn expect_expression(&mut self) -> Result<UntypedExpr, ParseError> {
        match self.parse_expression()? {
            Some(e) => Ok(e),
            _ => self.next_tok_unexpected(vec!["An expression".into()]),
        }
    }

    fn expect_expression_unit(
        &mut self,
        context: ExpressionUnitContext,
    ) -> Result<UntypedExpr, ParseError> {
        if let Some(e) = self.parse_expression_unit(context)? {
            Ok(e)
        } else {
            self.next_tok_unexpected(vec!["An expression".into()])
        }
    }

    //
    // Parse Helpers
    //

    /// Expect a particular token, advances the token stream
    fn expect_one(&mut self, wanted: &Token) -> Result<(u32, u32), ParseError> {
        match self.maybe_one(wanted) {
            Some((start, end)) => Ok((start, end)),
            None => self.next_tok_unexpected(vec![wanted.to_string().into()]),
        }
    }

    // Expect a particular token after having parsed a series, advances the token stream
    // Used for giving a clearer error message in cases where the series item is what failed to parse
    fn expect_one_following_series(
        &mut self,
        wanted: &Token,
        series: &'static str,
    ) -> Result<(u32, u32), ParseError> {
        match self.maybe_one(wanted) {
            Some((start, end)) => Ok((start, end)),
            None => self.next_tok_unexpected(vec![wanted.to_string().into(), series.into()]),
        }
    }

    /// Expect the end to a custom type definiton or handle an incorrect
    /// record constructor definition.
    ///
    /// Used for mapping to a more specific error type and message.
    fn expect_custom_type_close(
        &mut self,
        name: &EcoString,
        public: bool,
        opaque: bool,
    ) -> Result<(u32, u32), ParseError> {
        match self.maybe_one(&Token::RightBrace) {
            Some((start, end)) => Ok((start, end)),
            None => match self.next_tok() {
                None => parse_error(ParseErrorType::UnexpectedEof, SrcSpan { start: 0, end: 0 }),
                Some((start, token, end)) => {
                    // If provided a Name, map to a more detailed error
                    // message to nudge the user.
                    // Else, handle as an unexpected token.
                    let field = if let Token::Name { name } = token {
                        name
                    } else {
                        let hint = match (&token, self.tok0.take()) {
                            (&Token::Fn, _) | (&Token::Pub, Some((_, Token::Fn, _))) => {
                                let text = "Gleam is not an object oriented programming language so
functions are declared separately from types.";
                                Some(wrap(text).into())
                            }
                            (_, _) => None,
                        };

                        return parse_error(
                            ParseErrorType::UnexpectedToken {
                                token,
                                expected: vec![
                                    Token::RightBrace.to_string().into(),
                                    "a record constructor".into(),
                                ],
                                hint,
                            },
                            SrcSpan { start, end },
                        );
                    };
                    let field_type = match self.parse_type_annotation(&Token::Colon) {
                        Ok(Some(annotation)) => Some(Box::new(annotation)),
                        _ => None,
                    };
                    parse_error(
                        ParseErrorType::ExpectedRecordConstructor {
                            name: name.clone(),
                            public,
                            opaque,
                            field,
                            field_type,
                        },
                        SrcSpan { start, end },
                    )
                }
            },
        }
    }

    // Expect a Name else a token dependent helpful error
    fn expect_name(&mut self) -> Result<(u32, EcoString, u32), ParseError> {
        let (start, token, end) = self.expect_assign_name()?;
        match token {
            AssignName::Variable(name) => Ok((start, name, end)),
            AssignName::Discard(_) => {
                parse_error(ParseErrorType::IncorrectName, SrcSpan { start, end })
            }
        }
    }

    fn expect_assign_name(&mut self) -> Result<(u32, AssignName, u32), ParseError> {
        let t = self.next_tok();
        match t {
            Some((start, tok, end)) => match tok {
                Token::Name { name } => Ok((start, AssignName::Variable(name), end)),
                Token::DiscardName { name, .. } => Ok((start, AssignName::Discard(name), end)),
                Token::UpName { .. } => {
                    parse_error(ParseErrorType::IncorrectName, SrcSpan { start, end })
                }
                _ if tok.is_reserved_word() => parse_error(
                    ParseErrorType::UnexpectedReservedWord,
                    SrcSpan { start, end },
                ),
                Token::Int { .. }
                | Token::Float { .. }
                | Token::String { .. }
                | Token::CommentDoc { .. }
                | Token::LeftParen
                | Token::RightParen
                | Token::LeftSquare
                | Token::RightSquare
                | Token::LeftBrace
                | Token::RightBrace
                | Token::Plus
                | Token::Minus
                | Token::Star
                | Token::Slash
                | Token::Less
                | Token::Greater
                | Token::LessEqual
                | Token::GreaterEqual
                | Token::Percent
                | Token::PlusDot
                | Token::MinusDot
                | Token::StarDot
                | Token::SlashDot
                | Token::LessDot
                | Token::GreaterDot
                | Token::LessEqualDot
                | Token::GreaterEqualDot
                | Token::LtGt
                | Token::Colon
                | Token::Comma
                | Token::Hash
                | Token::Bang
                | Token::Equal
                | Token::EqualEqual
                | Token::NotEqual
                | Token::Vbar
                | Token::VbarVbar
                | Token::AmperAmper
                | Token::LtLt
                | Token::GtGt
                | Token::Pipe
                | Token::Dot
                | Token::RArrow
                | Token::LArrow
                | Token::DotDot
                | Token::At
                | Token::EndOfFile
                | Token::CommentNormal
                | Token::CommentModule
                | Token::NewLine
                | Token::As
                | Token::Assert
                | Token::Auto
                | Token::Case
                | Token::Const
                | Token::Delegate
                | Token::Derive
                | Token::Echo
                | Token::Else
                | Token::Fn
                | Token::If
                | Token::Implement
                | Token::Import
                | Token::Let
                | Token::Macro
                | Token::Opaque
                | Token::Panic
                | Token::Pub
                | Token::Test
                | Token::Todo
                | Token::Type
                | Token::Use => parse_error(ParseErrorType::ExpectedName, SrcSpan { start, end }),
            },
            None => parse_error(ParseErrorType::UnexpectedEof, SrcSpan { start: 0, end: 0 }),
        }
    }

    // Expect an UpName else a token dependent helpful error
    fn expect_upname(&mut self) -> Result<(u32, EcoString, u32), ParseError> {
        let t = self.next_tok();
        match t {
            Some((start, tok, end)) => match tok {
                Token::Name { .. } | Token::DiscardName { .. } => {
                    parse_error(ParseErrorType::IncorrectUpName, SrcSpan { start, end })
                }
                Token::UpName { name } => Ok((start, name, end)),
                Token::Int { .. }
                | Token::Float { .. }
                | Token::String { .. }
                | Token::CommentDoc { .. }
                | Token::LeftParen
                | Token::RightParen
                | Token::LeftSquare
                | Token::RightSquare
                | Token::LeftBrace
                | Token::RightBrace
                | Token::Plus
                | Token::Minus
                | Token::Star
                | Token::Slash
                | Token::Less
                | Token::Greater
                | Token::LessEqual
                | Token::GreaterEqual
                | Token::Percent
                | Token::PlusDot
                | Token::MinusDot
                | Token::StarDot
                | Token::SlashDot
                | Token::LessDot
                | Token::GreaterDot
                | Token::LessEqualDot
                | Token::GreaterEqualDot
                | Token::LtGt
                | Token::Colon
                | Token::Comma
                | Token::Hash
                | Token::Bang
                | Token::Equal
                | Token::EqualEqual
                | Token::NotEqual
                | Token::Vbar
                | Token::VbarVbar
                | Token::AmperAmper
                | Token::LtLt
                | Token::GtGt
                | Token::Pipe
                | Token::Dot
                | Token::RArrow
                | Token::LArrow
                | Token::DotDot
                | Token::At
                | Token::EndOfFile
                | Token::CommentNormal
                | Token::CommentModule
                | Token::NewLine
                | Token::As
                | Token::Assert
                | Token::Auto
                | Token::Case
                | Token::Const
                | Token::Delegate
                | Token::Derive
                | Token::Echo
                | Token::Else
                | Token::Fn
                | Token::If
                | Token::Implement
                | Token::Import
                | Token::Let
                | Token::Macro
                | Token::Opaque
                | Token::Panic
                | Token::Pub
                | Token::Test
                | Token::Todo
                | Token::Type
                | Token::Use => parse_error(ParseErrorType::ExpectedUpName, SrcSpan { start, end }),
            },
            None => parse_error(ParseErrorType::UnexpectedEof, SrcSpan { start: 0, end: 0 }),
        }
    }

    // Expect a target name. e.g. `javascript` or `erlang`.
    // The location of the preceding left parenthesis is required
    // to give the correct error span in case the target name is missing.
    fn expect_target(&mut self, paren_location: SrcSpan) -> Result<Target, ParseError> {
        let (start, t, end) = match self.next_tok() {
            Some(t) => t,
            None => {
                return parse_error(ParseErrorType::UnexpectedEof, SrcSpan { start: 0, end: 0 });
            }
        };
        if let Token::Name { name } = t {
            match name.as_str() {
                "javascript" => Ok(Target::JavaScript),
                "erlang" => Ok(Target::Erlang),
                "js" => {
                    self.warnings
                        .push(DeprecatedSyntaxWarning::DeprecatedTargetShorthand {
                            location: SrcSpan::new(start, end),
                            target: Target::JavaScript,
                        });
                    Ok(Target::JavaScript)
                }
                "erl" => {
                    self.warnings
                        .push(DeprecatedSyntaxWarning::DeprecatedTargetShorthand {
                            location: SrcSpan::new(start, end),
                            target: Target::Erlang,
                        });
                    Ok(Target::Erlang)
                }
                _ => parse_error(ParseErrorType::UnknownTarget, SrcSpan::new(start, end)),
            }
        } else {
            parse_error(ParseErrorType::ExpectedTargetName, paren_location)
        }
    }

    // Expect a String else error
    fn expect_string(&mut self) -> Result<(u32, EcoString, u32), ParseError> {
        match self.next_tok() {
            Some((start, Token::String { value }, end)) => Ok((start, value, end)),
            _ => self.next_tok_unexpected(vec!["a string".into()]),
        }
    }

    fn peek_tok1(&mut self) -> Option<&Token> {
        self.tok1.as_ref().map(|(_, token, _)| token)
    }

    // If the next token matches the requested, consume it and return (start, end)
    fn maybe_one(&mut self, tok: &Token) -> Option<(u32, u32)> {
        match self.tok0.take() {
            Some((s, t, e)) if t == *tok => {
                self.advance();
                Some((s, e))
            }

            t0 => {
                self.tok0 = t0;
                None
            }
        }
    }

    // Parse a series by repeating a parser, and possibly a separator
    fn series_of<A>(
        &mut self,
        parser: &impl Fn(&mut Self) -> Result<Option<A>, ParseError>,
        sep: Option<&Token>,
    ) -> Result<Vec<A>, ParseError> {
        let (res, _) = self.series_of_has_trailing_separator(parser, sep)?;
        Ok(res)
    }

    /// Parse a series by repeating a parser, and a separator. Returns true if
    /// the series ends with the trailing separator.
    fn series_of_has_trailing_separator<A>(
        &mut self,
        parser: &impl Fn(&mut Self) -> Result<Option<A>, ParseError>,
        sep: Option<&Token>,
    ) -> Result<(Vec<A>, bool), ParseError> {
        let mut results = vec![];
        let mut final_separator = None;
        while let Some(result) = parser(self)? {
            results.push(result);
            if let Some(sep) = sep {
                if let Some(separator) = self.maybe_one(sep) {
                    final_separator = Some(separator);
                } else {
                    final_separator = None;
                    break;
                }

                // Helpful error if extra separator
                if let Some((start, end)) = self.maybe_one(sep) {
                    return parse_error(ParseErrorType::ExtraSeparator, SrcSpan { start, end });
                }
            }
        }

        // If the sequence ends with a trailing comma we want to keep track of
        // its position.
        if let (Some(Token::Comma), Some((_, end))) = (sep, final_separator) {
            self.extra.trailing_commas.push(end)
        };

        Ok((results, final_separator.is_some()))
    }

    // If next token is a Name, consume it and return relevant info, otherwise, return none
    fn maybe_name(&mut self) -> Option<(u32, EcoString, u32)> {
        match self.tok0.take() {
            Some((s, Token::Name { name }, e)) => {
                self.advance();
                Some((s, name, e))
            }
            t0 => {
                self.tok0 = t0;
                None
            }
        }
    }

    // if next token is an UpName, consume it and return relevant info, otherwise, return none
    fn maybe_upname(&mut self) -> Option<(u32, EcoString, u32)> {
        match self.tok0.take() {
            Some((s, Token::UpName { name }, e)) => {
                self.advance();
                Some((s, name, e))
            }
            t0 => {
                self.tok0 = t0;
                None
            }
        }
    }

    // if next token is a DiscardName, consume it and return relevant info, otherwise, return none
    fn maybe_discard_name(&mut self) -> Option<(u32, EcoString, u32)> {
        match self.tok0.take() {
            Some((s, Token::DiscardName { name }, e)) => {
                self.advance();
                Some((s, name, e))
            }
            t0 => {
                self.tok0 = t0;
                None
            }
        }
    }

    // Unexpected token error on the next token or EOF
    fn next_tok_unexpected<A>(&mut self, expected: Vec<EcoString>) -> Result<A, ParseError> {
        match self.next_tok() {
            None => parse_error(ParseErrorType::UnexpectedEof, SrcSpan { start: 0, end: 0 }),
            Some((start, token, end)) => parse_error(
                ParseErrorType::UnexpectedToken {
                    token,
                    expected,
                    hint: None,
                },
                SrcSpan { start, end },
            ),
        }
    }

    // Moves the token stream forward
    fn advance(&mut self) {
        let _ = self.next_tok();
    }

    // Moving the token stream forward
    // returns old tok0
    fn next_tok(&mut self) -> Option<Spanned> {
        let t = self.tok0.take();
        let mut previous_newline = None;
        let mut nxt;
        loop {
            match self.tokens.next() {
                // gather and skip extra
                Some(Ok((start, Token::CommentNormal, end))) => {
                    self.extra.comments.push(SrcSpan { start, end });
                    previous_newline = None;
                }
                Some(Ok((start, Token::CommentDoc { content }, end))) => {
                    self.extra.doc_comments.push(SrcSpan::new(start, end));
                    self.doc_comments.push_back((start, content));
                    previous_newline = None;
                }
                Some(Ok((start, Token::CommentModule, end))) => {
                    self.extra.module_comments.push(SrcSpan { start, end });
                    previous_newline = None;
                }
                Some(Ok((start, Token::NewLine, _))) => {
                    self.extra.new_lines.push(start);
                    // If the previous token is a newline as well that means we
                    // have run into an empty line.
                    if let Some(start) = previous_newline {
                        // We increase the byte position so that newline's start
                        // doesn't overlap with the previous token's end.
                        self.extra.empty_lines.push(start + 1);
                    }
                    previous_newline = Some(start);
                }

                // die on lex error
                Some(Err(err)) => {
                    nxt = None;
                    self.lex_errors.push(err);
                    break;
                }

                Some(Ok(tok)) => {
                    nxt = Some(tok);
                    break;
                }
                None => {
                    nxt = None;
                    break;
                }
            }
        }
        self.tok0 = self.tok1.take();
        self.tok1 = nxt.take();
        t
    }

    fn take_documentation(&mut self, until: u32) -> Option<(u32, EcoString)> {
        let mut content = String::new();
        let mut doc_start = u32::MAX;
        while let Some((start, line)) = self.doc_comments.front() {
            if *start < doc_start {
                doc_start = *start;
            }
            if *start >= until {
                break;
            }

            if self.extra.has_comment_between(*start, until) {
                // We ignore doc comments that come before a regular comment.
                let location = SrcSpan::new(*start, start + line.len() as u32);
                _ = self.doc_comments.pop_front();
                self.detached_doc_comments.push(location);
                continue;
            }

            content.push_str(line);
            content.push('\n');
            _ = self.doc_comments.pop_front();
        }
        if content.is_empty() {
            None
        } else {
            Some((doc_start, content.into()))
        }
    }

    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes,
    ) -> Result<Option<SrcSpan>, ParseError> {
        let mut attributes_span = None;

        while let Some((start, end)) = self.maybe_one(&Token::At) {
            if attributes_span.is_none() {
                attributes_span = Some(SrcSpan { start, end });
            }

            let end = self.parse_attribute(start, attributes)?;
            attributes_span = attributes_span.map(|span| SrcSpan {
                start: span.start,
                end,
            });
        }

        Ok(attributes_span)
    }

    fn parse_attribute(
        &mut self,
        start: u32,
        attributes: &mut Attributes,
    ) -> Result<u32, ParseError> {
        // Parse the name of the attribute.

        let (_, name, end) = self.expect_name()?;

        let end = match name.as_str() {
            "external" => {
                let _ = self.expect_one(&Token::LeftParen)?;
                self.parse_external_attribute(start, end, attributes)
            }
            "target" => self.parse_target_attribute(start, end, attributes),
            "deprecated" => {
                let _ = self.expect_one(&Token::LeftParen)?;
                self.parse_deprecated_attribute(start, end, attributes)
            }
            "internal" => self.parse_internal_attribute(start, end, attributes),
            _ => parse_error(ParseErrorType::UnknownAttribute, SrcSpan { start, end }),
        }?;

        Ok(end)
    }

    fn parse_target_attribute(
        &mut self,
        start: u32,
        end: u32,
        attributes: &mut Attributes,
    ) -> Result<u32, ParseError> {
        let (paren_start, paren_end) = self.expect_one(&Token::LeftParen)?;
        let target = self.expect_target(SrcSpan::new(paren_start, paren_end))?;
        if attributes.target.is_some() {
            return parse_error(ParseErrorType::DuplicateAttribute, SrcSpan { start, end });
        }
        let (_, end) = self.expect_one(&Token::RightParen)?;
        if attributes.target.is_some() {
            return parse_error(ParseErrorType::DuplicateAttribute, SrcSpan { start, end });
        }
        attributes.target = Some(target);
        Ok(end)
    }

    fn parse_external_attribute(
        &mut self,
        start: u32,
        end: u32,
        attributes: &mut Attributes,
    ) -> Result<u32, ParseError> {
        let (_, name, _) = self.expect_name()?;

        let target = match name.as_str() {
            "erlang" => Target::Erlang,
            "javascript" => Target::JavaScript,
            _ => return parse_error(ParseErrorType::UnknownTarget, SrcSpan::new(start, end)),
        };

        let _ = self.expect_one(&Token::Comma)?;
        let (_, module, _) = self.expect_string()?;
        let _ = self.expect_one(&Token::Comma)?;
        let (_, function, _) = self.expect_string()?;
        let _ = self.maybe_one(&Token::Comma);
        let (_, end) = self.expect_one(&Token::RightParen)?;

        if attributes.has_external_for(target) {
            return parse_error(ParseErrorType::DuplicateAttribute, SrcSpan { start, end });
        }

        attributes.set_external_for(target, Some((module, function, SrcSpan { start, end })));
        Ok(end)
    }

    fn parse_deprecated_attribute(
        &mut self,
        start: u32,
        end: u32,
        attributes: &mut Attributes,
    ) -> Result<u32, ParseError> {
        if attributes.deprecated.is_deprecated() {
            return parse_error(ParseErrorType::DuplicateAttribute, SrcSpan::new(start, end));
        }
        let (_, message, _) = self.expect_string().map_err(|_| ParseError {
            error: ParseErrorType::ExpectedDeprecationMessage,
            location: SrcSpan { start, end },
        })?;
        let (_, end) = self.expect_one(&Token::RightParen)?;
        attributes.deprecated = Deprecation::Deprecated { message };
        Ok(end)
    }

    fn parse_internal_attribute(
        &mut self,
        start: u32,
        end: u32,
        attributes: &mut Attributes,
    ) -> Result<u32, ParseError> {
        match attributes.internal {
            // If `internal` is present that means that we have already run into
            // another `@internal` annotation, so it results in a `DuplicateAttribute`
            // error.
            InternalAttribute::Present(_) => {
                parse_error(ParseErrorType::DuplicateAttribute, SrcSpan::new(start, end))
            }
            InternalAttribute::Missing => {
                attributes.internal = InternalAttribute::Present(SrcSpan::new(start, end));
                Ok(end)
            }
        }
    }
}

fn concat_pattern_variable_left_hand_side_error<T>(start: u32, end: u32) -> Result<T, ParseError> {
    Err(ParseError {
        error: ParseErrorType::ConcatPatternVariableLeftHandSide,
        location: SrcSpan::new(start, end),
    })
}

// Operator Precedence Parsing
//
// Higher number means higher precedence.
// All operators are left associative.

/// Simple-Precedence-Parser, handle seeing an operator or end
fn handle_op<A>(
    next_op: Option<(Spanned, u8)>,
    opstack: &mut Vec<(Spanned, u8)>,
    estack: &mut Vec<A>,
    do_reduce: &impl Fn(Spanned, &mut Vec<A>),
) -> Option<A> {
    let mut next_op = next_op;
    loop {
        match (opstack.pop(), next_op.take()) {
            (None, None) => match estack.pop() {
                Some(fin) => {
                    if estack.is_empty() {
                        return Some(fin);
                    } else {
                        panic!("Expression not fully reduced.")
                    }
                }
                _ => {
                    return None;
                }
            },

            (None, Some(op)) => {
                opstack.push(op);
                break;
            }

            (Some((op, _)), None) => do_reduce(op, estack),

            (Some((opl, pl)), Some((opr, pr))) => {
                match pl.cmp(&pr) {
                    // all ops are left associative
                    Ordering::Greater | Ordering::Equal => {
                        do_reduce(opl, estack);
                        next_op = Some((opr, pr));
                    }
                    Ordering::Less => {
                        opstack.push((opl, pl));
                        opstack.push((opr, pr));
                        break;
                    }
                }
            }
        }
    }
    None
}

fn precedence(t: &Token) -> Option<u8> {
    if t == &Token::Pipe {
        return Some(6);
    };
    tok_to_binop(t).map(|op| op.precedence())
}

fn tok_to_binop(t: &Token) -> Option<BinOp> {
    match t {
        Token::VbarVbar => Some(BinOp::Or),
        Token::AmperAmper => Some(BinOp::And),
        Token::EqualEqual => Some(BinOp::Eq),
        Token::NotEqual => Some(BinOp::NotEq),
        Token::Less => Some(BinOp::LtInt),
        Token::LessEqual => Some(BinOp::LtEqInt),
        Token::Greater => Some(BinOp::GtInt),
        Token::GreaterEqual => Some(BinOp::GtEqInt),
        Token::LessDot => Some(BinOp::LtFloat),
        Token::LessEqualDot => Some(BinOp::LtEqFloat),
        Token::GreaterDot => Some(BinOp::GtFloat),
        Token::GreaterEqualDot => Some(BinOp::GtEqFloat),
        Token::Plus => Some(BinOp::AddInt),
        Token::Minus => Some(BinOp::SubInt),
        Token::PlusDot => Some(BinOp::AddFloat),
        Token::MinusDot => Some(BinOp::SubFloat),
        Token::Percent => Some(BinOp::RemainderInt),
        Token::Star => Some(BinOp::MultInt),
        Token::StarDot => Some(BinOp::MultFloat),
        Token::Slash => Some(BinOp::DivInt),
        Token::SlashDot => Some(BinOp::DivFloat),
        Token::LtGt => Some(BinOp::Concatenate),
        Token::Name { .. }
        | Token::UpName { .. }
        | Token::DiscardName { .. }
        | Token::Int { .. }
        | Token::Float { .. }
        | Token::String { .. }
        | Token::CommentDoc { .. }
        | Token::LeftParen
        | Token::RightParen
        | Token::LeftSquare
        | Token::RightSquare
        | Token::LeftBrace
        | Token::RightBrace
        | Token::Colon
        | Token::Comma
        | Token::Hash
        | Token::Bang
        | Token::Equal
        | Token::Vbar
        | Token::LtLt
        | Token::GtGt
        | Token::Pipe
        | Token::Dot
        | Token::RArrow
        | Token::LArrow
        | Token::DotDot
        | Token::At
        | Token::EndOfFile
        | Token::CommentNormal
        | Token::CommentModule
        | Token::NewLine
        | Token::As
        | Token::Assert
        | Token::Auto
        | Token::Case
        | Token::Const
        | Token::Delegate
        | Token::Derive
        | Token::Echo
        | Token::Else
        | Token::Fn
        | Token::If
        | Token::Implement
        | Token::Import
        | Token::Let
        | Token::Macro
        | Token::Opaque
        | Token::Panic
        | Token::Pub
        | Token::Test
        | Token::Todo
        | Token::Type
        | Token::Use => None,
    }
}
/// Simple-Precedence-Parser, perform reduction for expression
fn do_reduce_expression(op: Spanned, estack: &mut Vec<UntypedExpr>) {
    match (estack.pop(), estack.pop()) {
        (Some(er), Some(el)) => {
            let new_e = expr_op_reduction(op, el, er);
            estack.push(new_e);
        }
        _ => panic!("Tried to reduce without 2 expressions"),
    }
}

/// Simple-Precedence-Parser, perform reduction for clause guard
fn do_reduce_clause_guard(op: Spanned, estack: &mut Vec<UntypedClauseGuard>) {
    match (estack.pop(), estack.pop()) {
        (Some(er), Some(el)) => {
            let new_e = clause_guard_reduction(op, el, er);
            estack.push(new_e);
        }
        _ => panic!("Tried to reduce without 2 guards"),
    }
}

fn expr_op_reduction(
    (token_start, token, token_end): Spanned,
    l: UntypedExpr,
    r: UntypedExpr,
) -> UntypedExpr {
    if token == Token::Pipe {
        let expressions = if let UntypedExpr::PipeLine { mut expressions } = l {
            expressions.push(r);
            expressions
        } else {
            vec1![l, r]
        };
        UntypedExpr::PipeLine { expressions }
    } else {
        match tok_to_binop(&token) {
            Some(bin_op) => UntypedExpr::BinOp {
                location: SrcSpan {
                    start: l.location().start,
                    end: r.location().end,
                },
                name: bin_op,
                name_location: SrcSpan {
                    start: token_start,
                    end: token_end,
                },
                left: Box::new(l),
                right: Box::new(r),
            },
            _ => {
                panic!("Token could not be converted to binop.")
            }
        }
    }
}

fn clause_guard_reduction(
    (_, token, _): Spanned,
    l: UntypedClauseGuard,
    r: UntypedClauseGuard,
) -> UntypedClauseGuard {
    let location = SrcSpan {
        start: l.location().start,
        end: r.location().end,
    };
    let left = Box::new(l);
    let right = Box::new(r);
    match token {
        Token::VbarVbar => ClauseGuard::Or {
            location,
            left,
            right,
        },

        Token::AmperAmper => ClauseGuard::And {
            location,
            left,
            right,
        },

        Token::EqualEqual => ClauseGuard::Equals {
            location,
            left,
            right,
        },

        Token::NotEqual => ClauseGuard::NotEquals {
            location,
            left,
            right,
        },

        Token::Greater => ClauseGuard::GtInt {
            location,
            left,
            right,
        },

        Token::GreaterEqual => ClauseGuard::GtEqInt {
            location,
            left,
            right,
        },

        Token::Less => ClauseGuard::LtInt {
            location,
            left,
            right,
        },

        Token::LessEqual => ClauseGuard::LtEqInt {
            location,
            left,
            right,
        },

        Token::GreaterDot => ClauseGuard::GtFloat {
            location,
            left,
            right,
        },

        Token::GreaterEqualDot => ClauseGuard::GtEqFloat {
            location,
            left,
            right,
        },

        Token::LessDot => ClauseGuard::LtFloat {
            location,
            left,
            right,
        },

        Token::LessEqualDot => ClauseGuard::LtEqFloat {
            location,
            left,
            right,
        },

        Token::Plus => ClauseGuard::AddInt {
            location,
            left,
            right,
        },

        Token::PlusDot => ClauseGuard::AddFloat {
            location,
            left,
            right,
        },

        Token::Minus => ClauseGuard::SubInt {
            location,
            left,
            right,
        },

        Token::MinusDot => ClauseGuard::SubFloat {
            location,
            left,
            right,
        },

        Token::Star => ClauseGuard::MultInt {
            location,
            left,
            right,
        },

        Token::StarDot => ClauseGuard::MultFloat {
            location,
            left,
            right,
        },

        Token::Slash => ClauseGuard::DivInt {
            location,
            left,
            right,
        },

        Token::SlashDot => ClauseGuard::DivFloat {
            location,
            left,
            right,
        },

        Token::Percent => ClauseGuard::RemainderInt {
            location,
            left,
            right,
        },

        Token::Name { .. }
        | Token::UpName { .. }
        | Token::DiscardName { .. }
        | Token::Int { .. }
        | Token::Float { .. }
        | Token::String { .. }
        | Token::CommentDoc { .. }
        | Token::LeftParen
        | Token::RightParen
        | Token::LeftSquare
        | Token::RightSquare
        | Token::LeftBrace
        | Token::RightBrace
        | Token::LtGt
        | Token::Colon
        | Token::Comma
        | Token::Hash
        | Token::Bang
        | Token::Equal
        | Token::Vbar
        | Token::LtLt
        | Token::GtGt
        | Token::Pipe
        | Token::Dot
        | Token::RArrow
        | Token::LArrow
        | Token::DotDot
        | Token::At
        | Token::EndOfFile
        | Token::CommentNormal
        | Token::CommentModule
        | Token::NewLine
        | Token::As
        | Token::Assert
        | Token::Auto
        | Token::Case
        | Token::Const
        | Token::Delegate
        | Token::Derive
        | Token::Echo
        | Token::Else
        | Token::Fn
        | Token::If
        | Token::Implement
        | Token::Import
        | Token::Let
        | Token::Macro
        | Token::Opaque
        | Token::Panic
        | Token::Pub
        | Token::Test
        | Token::Todo
        | Token::Type
        | Token::Use => panic!("Token could not be converted to Guard Op."),
    }
}

// BitArray Parse Helpers
//
// BitArrays in patterns, guards, and expressions have a very similar structure
// but need specific types. These are helpers for that. There is probably a
// rustier way to do this :)
fn bit_array_size_int(value: EcoString, int_value: BigInt, start: u32, end: u32) -> UntypedPattern {
    Pattern::BitArraySize(BitArraySize::Int {
        location: SrcSpan { start, end },
        value,
        int_value,
    })
}

fn bit_array_expr_int(value: EcoString, int_value: BigInt, start: u32, end: u32) -> UntypedExpr {
    UntypedExpr::Int {
        location: SrcSpan { start, end },
        value,
        int_value,
    }
}

fn bit_array_const_int(
    value: EcoString,
    int_value: BigInt,
    start: u32,
    end: u32,
) -> UntypedConstant {
    Constant::Int {
        location: SrcSpan { start, end },
        value,
        int_value,
    }
}

fn str_to_bit_array_option<A>(lit: &str, location: SrcSpan) -> Option<BitArrayOption<A>> {
    match lit {
        "bytes" => Some(BitArrayOption::Bytes { location }),
        "int" => Some(BitArrayOption::Int { location }),
        "float" => Some(BitArrayOption::Float { location }),
        "bits" => Some(BitArrayOption::Bits { location }),
        "utf8" => Some(BitArrayOption::Utf8 { location }),
        "utf16" => Some(BitArrayOption::Utf16 { location }),
        "utf32" => Some(BitArrayOption::Utf32 { location }),
        "utf8_codepoint" => Some(BitArrayOption::Utf8Codepoint { location }),
        "utf16_codepoint" => Some(BitArrayOption::Utf16Codepoint { location }),
        "utf32_codepoint" => Some(BitArrayOption::Utf32Codepoint { location }),
        "signed" => Some(BitArrayOption::Signed { location }),
        "unsigned" => Some(BitArrayOption::Unsigned { location }),
        "big" => Some(BitArrayOption::Big { location }),
        "little" => Some(BitArrayOption::Little { location }),
        "native" => Some(BitArrayOption::Native { location }),
        _ => None,
    }
}

//
// Error Helpers
//
fn parse_error<T>(error: ParseErrorType, location: SrcSpan) -> Result<T, ParseError> {
    Err(ParseError { error, location })
}

//
// Misc Helpers
//

// Parsing a function call into the appropriate structure
#[derive(Debug)]
pub enum ParserArg {
    Arg(Box<CallArg<UntypedExpr>>),
    Hole {
        name: EcoString,
        /// The whole span of the argument.
        arg_location: SrcSpan,
        /// Just the span of the ignore name.
        discard_location: SrcSpan,
        label: Option<EcoString>,
    },
}

pub fn make_call(
    fun: UntypedExpr,
    arguments: Vec<ParserArg>,
    start: u32,
    end: u32,
) -> Result<UntypedExpr, ParseError> {
    let mut hole_location = None;

    let arguments = arguments
        .into_iter()
        .map(|argument| match argument {
            ParserArg::Arg(arg) => Ok(*arg),
            ParserArg::Hole {
                arg_location,
                discard_location,
                name,
                label,
            } => {
                if hole_location.is_some() {
                    return parse_error(ParseErrorType::TooManyArgHoles, SrcSpan { start, end });
                }

                hole_location = Some(discard_location);
                if name != "_" {
                    return parse_error(
                        ParseErrorType::UnexpectedToken {
                            token: Token::Name { name },
                            expected: vec!["An expression".into(), "An underscore".into()],
                            hint: None,
                        },
                        arg_location,
                    );
                }

                Ok(CallArg {
                    implicit: None,
                    label,
                    location: arg_location,
                    value: UntypedExpr::Var {
                        location: discard_location,
                        name: CAPTURE_VARIABLE.into(),
                    },
                })
            }
        })
        .collect::<Result<_, _>>()?;

    let call = UntypedExpr::Call {
        location: SrcSpan { start, end },
        fun: Box::new(fun),
        arguments,
    };

    match hole_location {
        // A normal call
        None => Ok(call),

        // An anon function using the capture syntax run(_, 1, 2)
        Some(hole_location) => Ok(UntypedExpr::Fn {
            location: call.location(),
            end_of_head_byte_index: call.location().end,
            kind: FunctionLiteralKind::Capture {
                hole: hole_location,
            },
            arguments: vec![Arg {
                location: hole_location,
                annotation: None,
                names: ArgNames::Named {
                    name: CAPTURE_VARIABLE.into(),
                    location: hole_location,
                },
                type_: (),
            }],
            body: vec1![Statement::Expression(call)],
            return_annotation: None,
        }),
    }
}

#[derive(Debug, Default)]
struct ParsedUnqualifiedImports {
    types: Vec<UnqualifiedImport>,
    values: Vec<UnqualifiedImport>,
}

/// Parses an Int value to a bigint.
///
pub fn parse_int_value(value: &str) -> Option<BigInt> {
    let (radix, value) = if let Some(value) = value.strip_prefix("0x") {
        (16, value)
    } else if let Some(value) = value.strip_prefix("0o") {
        (8, value)
    } else if let Some(value) = value.strip_prefix("0b") {
        (2, value)
    } else {
        (10, value)
    };

    let value = value.trim_start_matches('_');

    BigInt::parse_bytes(value.as_bytes(), radix)
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum ExpressionUnitContext {
    FollowingPipe,
    Other,
}

#[derive(Debug, Clone, Copy)]
pub enum PatternPosition {
    LetAssignment,
    CaseClause,
    UsePattern,
}

impl PatternPosition {
    pub fn to_declaration(&self) -> VariableDeclaration {
        match self {
            PatternPosition::LetAssignment => VariableDeclaration::LetPattern,
            PatternPosition::CaseClause => VariableDeclaration::ClausePattern,
            PatternPosition::UsePattern => VariableDeclaration::UsePattern,
        }
    }
}

/// A thin f64 wrapper that does not permit NaN.
/// This allows us to implement `Eq`, which require reflexivity.
///
/// Used for gleam float literals, which cannot be NaN.
///
/// While there is no syntax for "infinity", float literals might be too big and
/// overflow into infinity. This is still allowed so we can parse big literal
/// numbers and the error will be raised during the analysis phase.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct LiteralFloatValue(f64);

impl LiteralFloatValue {
    pub const ONE: Self = LiteralFloatValue(1.0);
    pub const ZERO: Self = LiteralFloatValue(0.0);

    /// Parse from a string, returning `None` if the string
    /// is not a valid f64 or the float is `NaN``
    pub fn parse(value: &str) -> Option<Self> {
        value
            .replace("_", "")
            .parse::<f64>()
            .ok()
            .filter(|float| !float.is_nan())
            .map(LiteralFloatValue)
    }

    pub fn value(&self) -> f64 {
        self.0
    }
}

impl Eq for LiteralFloatValue {}

impl Ord for LiteralFloatValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0
            .partial_cmp(&other.0)
            .expect("Only NaN comparisons should fail")
    }
}

impl PartialOrd for LiteralFloatValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Hash for LiteralFloatValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state)
    }
}

impl Serialize for LiteralFloatValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_f64(self.0)
    }
}

impl<'de> Deserialize<'de> for LiteralFloatValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = f64::deserialize(deserializer)?;
        if value.is_nan() {
            Err(serde::de::Error::custom("NaN is not allowed"))
        } else {
            Ok(LiteralFloatValue(value))
        }
    }
}
