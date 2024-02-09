////

// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/float
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/map.{Map}
import gleam/option.{Option}
import gleam/string
import nibble/predicates

// TYPES -----------------------------------------------------------------------

///
pub opaque type Parser(a, ctx) {
  Parser(fn(State(ctx)) -> Step(a, ctx))
}

type State(ctx) {
  State(
    // The Gleam stdlib doesn't seem to have an `Array` type, so we'll just
    // use a `Map` instead. We only need something for indexed access, to it's
    // not a huge deal.
    //
    // TODO: Louis says making an `Array` backed by tuples in Erlang will
    // be way better for performance. In JavaScript we could just use normal
    // arrays - someone should look into this. 
    src: Map(Int, String),
    offset: Int,
    context: List(Located(ctx)),
    row: Int,
    col: Int,
  )
}

type Step(a, ctx) {
  Cont(Backtrackable, a, State(ctx))
  Fail(Backtrackable, Bag(ctx))
}

///
pub type Located(ctx) {
  Located(row: Int, col: Int, context: ctx)
}

type Backtrackable {
  Commit
  Backtrack
}

// RUNNING PARSERS -------------------------------------------------------------

///
pub fn run(src: String, parser: Parser(a, ctx)) -> Result(a, List(DeadEnd(ctx))) {
  let graphemes =
    string.to_graphemes(src)
    |> list.index_map(fn(i, grapheme) { #(i, grapheme) })
    |> map.from_list

  let init = State(graphemes, 0, [], 1, 1)

  case runwrap(init, parser) {
    Cont(_, a, _) -> Ok(a)

    Fail(_, bag) -> Error(to_deadends(bag, []))
  }
}

fn runwrap(state: State(ctx), parser: Parser(a, ctx)) -> Step(a, ctx) {
  let Parser(parse) = parser
  parse(state)
}

fn next(state: State(ctx)) -> #(Option(String), State(ctx)) {
  case map.get(state.src, state.offset) {
    Ok("\n") -> #(
      option.Some("\n"),
      State(..state, offset: state.offset + 1, col: 1, row: state.row + 1),
    )

    Ok(g) -> #(
      option.Some(g),
      State(..state, offset: state.offset + 1, col: state.col + 1),
    )

    Error(_) -> #(option.None, state)
  }
}

// CONSTRUCTORS ----------------------------------------------------------------

///
pub fn succeed(a: a) -> Parser(a, ctx) {
  Parser(fn(state) { Cont(Backtrack, a, state) })
}

///
pub fn fail(message: String) -> Parser(a, ctx) {
  Parser(fn(state) { Fail(Backtrack, bag_from_state(state, Custom(message))) })
}

///
pub fn lazy(parser: fn() -> Parser(a, ctx)) -> Parser(a, ctx) {
  Parser(fn(state) { runwrap(state, parser()) })
}

// BACKTRACKING ----------------------------------------------------------------

///
pub fn backtrackable(parser: Parser(a, ctx)) -> Parser(a, ctx) {
  Parser(fn(state) {
    case runwrap(state, parser) {
      Cont(_, a, state) -> Cont(Backtrack, a, state)

      Fail(_, bag) -> Fail(Backtrack, bag)
    }
  })
}

///
pub fn commit(to a: a) -> Parser(a, ctx) {
  Parser(fn(state) { Cont(Commit, a, state) })
}

fn should_commit(to_x: Backtrackable, or to_y: Backtrackable) -> Backtrackable {
  case to_x, to_y {
    Commit, _ -> Commit

    _, Commit -> Commit

    _, _ -> Backtrack
  }
}

// MANIPULATING PARSERS --------------------------------------------------------

///
pub fn then(
  parser: Parser(a, ctx),
  f: fn(a) -> Parser(b, ctx),
) -> Parser(b, ctx) {
  Parser(fn(state) {
    case runwrap(state, parser) {
      Cont(to_a, a, state) ->
        case runwrap(state, f(a)) {
          Cont(to_b, b, state) -> Cont(should_commit(to_a, or: to_b), b, state)
          Fail(to_b, bag) -> Fail(should_commit(to_a, or: to_b), bag)
        }

      Fail(can_backtrack, bag) -> Fail(can_backtrack, bag)
    }
  })
}

///
pub fn map(parser: Parser(a, ctx), f: fn(a) -> b) -> Parser(b, ctx) {
  then(parser, fn(a) { succeed(f(a)) })
}

fn map2(
  parse_a: Parser(a, ctx),
  parse_b: Parser(b, ctx),
  f: fn(a, b) -> c,
) -> Parser(c, ctx) {
  then(parse_a, fn(a) { map(parse_b, fn(b) { f(a, b) }) })
}

///
pub fn replace(parser: Parser(a, ctx), with b: b) -> Parser(b, ctx) {
  map(parser, fn(_) { b })
}

// PIPE-FRIENDLY HELPERS -------------------------------------------------------

///
pub fn keep(
  parse_f: Parser(fn(a) -> b, ctx),
  parse_a: Parser(a, ctx),
) -> Parser(b, ctx) {
  map2(parse_f, parse_a, fn(f, a) { f(a) })
}

///
pub fn drop(parse_a: Parser(a, ctx), parse_x: Parser(x, ctx)) -> Parser(a, ctx) {
  map2(parse_a, parse_x, fn(a, _) { a })
}

// SIMPLE PARSERS --------------------------------------------------------------

///
pub fn any() -> Parser(String, ctx) {
  take_if(function.constant(True), "a single grapheme")
}

///
pub fn eof() -> Parser(Nil, ctx) {
  Parser(fn(state) {
    case next(state) {
      #(option.Some(str), _) ->
        Fail(Backtrack, bag_from_state(state, Unexpected(str)))

      #(option.None, _) -> Cont(Backtrack, Nil, state)
    }
  })
}

// GRAPHEMES AND STRINGS -------------------------------------------------------

///
pub fn grapheme(str: String) -> Parser(Nil, ctx) {
  take_if(fn(g) { g == str }, str)
  |> map(function.constant(Nil))
}

///
pub fn string(str: String) -> Parser(Nil, ctx) {
  let graphemes = string.to_graphemes(str)

  Parser(fn(state) {
    case graphemes {
      [] -> Fail(Backtrack, bag_from_state(state, BadParser("empty string")))

      [head, ..tail] -> {
        let parse_each =
          list.fold(
            tail,
            grapheme(head),
            fn(parse, next) {
              parse
              |> drop(grapheme(next))
            },
          )
        case runwrap(state, parse_each) {
          Cont(_, _, state) -> Cont(Commit, Nil, state)
          Fail(_, bag) -> Fail(Backtrack, bag)
        }
      }
    }
  })
}

// NUMBERS ---------------------------------------------------------------------

///
pub fn int() -> Parser(Int, ctx) {
  take_if_and_while(predicates.is_digit, "a digit")
  // We can make the following assertion because we know our parser will
  // only consume digits, and is guaranteed to have at least one.
  |> map(fn(digits) {
    let assert Ok(int) = int.parse(digits)
    int
  })
}

///
pub fn float() -> Parser(Float, ctx) {
  let make_float_string =
    function.curry2(fn(x, y) { string.concat([x, ".", y]) })

  succeed(make_float_string)
  |> keep(take_if_and_while(predicates.is_digit, "a digit"))
  |> drop(grapheme("."))
  |> keep(take_if_and_while(predicates.is_digit, "a digit"))
  // We can make the following assertion because we know our parser will
  // only consume digits, and is guaranteed to have at least one.
  |> map(fn(digits) {
    let assert Ok(float) = float.parse(digits)
    float
  })
}

// WHITESPACE ------------------------------------------------------------------

///
pub fn spaces() -> Parser(Nil, ctx) {
  take_while(fn(g) { g == " " })
  |> map(function.constant(Nil))
}

///
pub fn whitespace() -> Parser(Nil, ctx) {
  take_while(predicates.is_whitespace)
  |> map(function.constant(Nil))
}

// BRANCHING AND LOOPING -------------------------------------------------------

///
pub fn one_of(parsers: List(Parser(a, ctx))) -> Parser(a, ctx) {
  Parser(fn(state) {
    let init = Fail(Backtrack, Empty)

    list.fold_until(
      parsers,
      init,
      fn(result, next) {
        case result {
          Cont(_, _, _) -> list.Stop(result)

          Fail(Commit, _) -> list.Stop(result)

          Fail(_, bag) ->
            runwrap(state, next)
            |> add_bag_to_step(bag)
            |> list.Continue
        }
      },
    )
  })
}

///
pub fn many(
  parser: Parser(a, ctx),
  separator: Parser(x, ctx),
) -> Parser(List(a), ctx) {
  one_of([
    parser
    |> then(more(_, parser, separator)),
    succeed([]),
  ])
}

fn more(
  x: a,
  parser: Parser(a, ctx),
  separator: Parser(x, ctx),
) -> Parser(List(a), ctx) {
  loop(
    [x],
    fn(xs) {
      one_of([
        succeed(list.prepend(xs, _))
        |> drop(separator)
        |> keep(parser)
        |> map(Continue),
        succeed(xs)
        |> drop(eof())
        |> map(list.reverse)
        |> map(Break),
        succeed(xs)
        |> map(list.reverse)
        |> map(Break),
      ])
    },
  )
}

pub type Loop(a, state) {
  Continue(state)
  Break(a)
}

pub fn loop(
  init: state,
  step: fn(state) -> Parser(Loop(a, state), ctx),
) -> Parser(a, ctx) {
  Parser(fn(state) { loop_help(step, Backtrack, init, state) })
}

fn loop_help(f, commit, loop_state, state) {
  case runwrap(state, f(loop_state)) {
    Cont(can_backtrack, Continue(next_loop_state), next_state) ->
      loop_help(
        f,
        should_commit(commit, can_backtrack),
        next_loop_state,
        next_state,
      )

    Cont(can_backtrack, Break(result), next_state) ->
      Cont(should_commit(commit, can_backtrack), result, next_state)

    Fail(can_backtrack, bag) -> Fail(should_commit(commit, can_backtrack), bag)
  }
}

// PREDICATES ------------------------------------------------------------------

///
pub fn take_if(
  predicate: fn(String) -> Bool,
  expecting: String,
) -> Parser(String, ctx) {
  Parser(fn(state) {
    let #(str, next_state) = next(state)
    let should_take =
      str
      |> option.map(predicate)
      |> option.unwrap(False)
    let str = option.unwrap(str, "")

    case should_take {
      True -> Cont(Commit, str, next_state)

      False ->
        Fail(Backtrack, bag_from_state(state, Expected(expecting, got: str)))
    }
  })
}

///
pub fn take_while(predicate: fn(String) -> Bool) -> Parser(String, ctx) {
  Parser(fn(state) {
    let #(str, next_state) = next(state)
    let should_take =
      str
      |> option.map(predicate)
      |> option.unwrap(False)
    let str = option.unwrap(str, "")

    case should_take {
      True ->
        runwrap(next_state, map(take_while(predicate), string.append(str, _)))

      False -> Cont(Backtrack, "", state)
    }
  })
}

///
pub fn take_if_and_while(
  predicate: fn(String) -> Bool,
  expecting: String,
) -> Parser(String, ctx) {
  map2(take_if(predicate, expecting), take_while(predicate), string.append)
}

///
pub fn take_until(predicate: fn(String) -> Bool) -> Parser(String, ctx) {
  take_while(function.compose(predicate, bool.negate))
}

// ERRORS ----------------------------------------------------------------------

pub type Error {
  BadParser(String)
  Custom(String)
  EndOfInput
  Expected(String, got: String)
  Unexpected(String)
}

///
pub type DeadEnd(ctx) {
  DeadEnd(row: Int, col: Int, problem: Error, context: List(Located(ctx)))
}

type Bag(ctx) {
  Empty
  Cons(Bag(ctx), DeadEnd(ctx))
  Append(Bag(ctx), Bag(ctx))
}

fn bag_from_state(state: State(ctx), problem: Error) -> Bag(ctx) {
  Cons(Empty, DeadEnd(state.row, state.col, problem, state.context))
}

fn to_deadends(bag: Bag(ctx), acc: List(DeadEnd(ctx))) -> List(DeadEnd(ctx)) {
  case bag {
    Empty -> acc

    Cons(Empty, deadend) -> [deadend, ..acc]

    Cons(bag, deadend) -> to_deadends(bag, [deadend, ..acc])

    Append(left, right) -> to_deadends(left, to_deadends(right, acc))
  }
}

fn add_bag_to_step(step: Step(a, ctx), left: Bag(ctx)) -> Step(a, ctx) {
  case step {
    Cont(can_backtrack, a, state) -> Cont(can_backtrack, a, state)

    Fail(can_backtrack, right) -> Fail(can_backtrack, Append(left, right))
  }
}

// CONTEXT ---------------------------------------------------------------------

///
pub fn in(parser: Parser(a, ctx), context: ctx) -> Parser(a, ctx) {
  Parser(fn(state) {
    case runwrap(push_context(state, context), parser) {
      Cont(can_backtrack, a, state) ->
        Cont(can_backtrack, a, pop_context(state))

      Fail(can_backtrack, bag) -> Fail(can_backtrack, bag)
    }
  })
}

fn push_context(state: State(ctx), context: ctx) -> State(ctx) {
  let located = Located(state.row, state.col, context)
  State(..state, context: [located, ..state.context])
}

fn pop_context(state: State(ctx)) -> State(ctx) {
  case state.context {
    [] -> state

    [_, ..context] -> State(..state, context: context)
  }
}

/// Run the given parser and then inspect it's state. 
pub fn inspect(parser: Parser(a, ctx), message: String) -> Parser(a, ctx) {
  Parser(fn(state) {
    io.print(message)
    io.println(": ")

    runwrap(state, parser)
    |> io.debug
  })
}
