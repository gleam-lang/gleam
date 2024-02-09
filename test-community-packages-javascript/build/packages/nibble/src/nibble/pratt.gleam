// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/function
import nibble.{ Parser }

// TYPES -----------------------------------------------------------------------

pub opaque type Config(a, ctx) {
    Config(
        one_of: List(fn (Config(a, ctx)) -> Parser(a, ctx)),
        and_then_one_of: List(Operator(a, ctx)),
        spaces: Parser(Nil, ctx)
    )
}

pub opaque type Operator(a, ctx) {
    Operator(
        fn(Config(a, ctx)) -> #(Int, fn (a) -> Parser(a, ctx))
    )
}

//

pub fn expression(
    one_of first: List(fn (Config(a, ctx)) -> Parser(a, ctx)),
    and_then_one_of then: List(Operator(a, ctx)),
    dropping spaces: Parser(Nil, ctx)
) -> Parser(a, ctx) {
    let config = Config(first, then, spaces)
    sub_expression(config, 0)
}

pub fn sub_expression (config: Config(a, ctx), precedence: Int) -> Parser(a, ctx) {
    let expr = nibble.lazy(fn () {
        config.one_of 
            |> list.map(fn (p) { p(config) })
            |> nibble.one_of
    })

    let go = fn (expr) {
        nibble.succeed(function.identity)
            |> nibble.drop(config.spaces)
            |> nibble.keep(
                nibble.one_of([
                    nibble.succeed(expr)
                        |> nibble.then(operation(_, config, precedence))
                        |> nibble.map(nibble.Continue),
                    nibble.succeed(expr)
                        |> nibble.map(nibble.Break)
                ])
            )
    }

    nibble.succeed(function.identity)
        |> nibble.drop(config.spaces)
        |> nibble.keep(expr)
        |> nibble.then(nibble.loop(_, go))
}

fn operation (expr: a, config: Config(a, ctx), current_precedence: Int) -> Parser(a, ctx) {
    config.and_then_one_of
        |> list.filter_map(fn (operator) {
            let Operator(op) = operator
            case op(config) {
                #(precedence, parser) if precedence > current_precedence ->
                    Ok(parser(expr))

                _ ->
                    Error(Nil)
            }

        })
        |> nibble.one_of()
}

//

pub fn prefix (precedence: Int, operator: Parser(Nil, ctx), apply: fn (a) -> a) -> fn (Config(a, ctx)) -> Parser(a, ctx) {
    fn (config) {
        nibble.succeed(apply)
            |> nibble.drop(operator)
            |> nibble.keep(sub_expression(config, precedence))
    }
}

pub fn infix_left (precedence: Int, operator: Parser(Nil, ctx), apply: fn (a, a) -> a) -> Operator(a, ctx) {
    make_infix(#(precedence, precedence), operator, apply)
}

pub fn infix_right (precedence: Int, operator: Parser(Nil, ctx), apply: fn (a, a) -> a) -> Operator(a, ctx) {
    make_infix(#(precedence, precedence - 1), operator, apply)
}

pub fn postfix (precedence: Int, operator: Parser(Nil, ctx), apply: fn (a) -> a) -> Operator(a, ctx) {
    Operator(fn (_) {
        #(precedence, fn (lhs) {
            nibble.succeed(apply(lhs))
                |> nibble.drop(operator)
        })
    })
}

fn make_infix(precedence: #(Int, Int), operator: Parser(Nil, ctx), apply: fn (a, a) -> a) -> Operator(a, ctx) {
    let #(left_precedence, right_precedence) = precedence
    Operator(fn (config) {
        #(left_precedence, fn (lhs) {
            nibble.succeed(apply(lhs, _))
                |> nibble.drop(operator)
                |> nibble.keep(sub_expression(config, right_precedence))
        })
    })
}
