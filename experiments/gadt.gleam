// https://mads-hartmann.com/ocaml/2015/01/05/gadt-ocaml.html

// With ADTs
//
// type value =
//   | Bool of bool
//   | Int of int
//
// type expr =
//   | Value of value
//   | If of expr * expr * expr
//   | Eq of expr * expr
//   | Lt of expr * expr

enum Value =
  | VBool(Bool)
  | VInt(Int)

enum Expr =
  | Val(Value)
  | If(Expr, Expr, Expr)
  | Eq(Expr, Expr)
  | Lt(Expr, Expr)

// A safer example using GADTs
//
// type _ value =
//   | Bool : bool -> bool value
//   | Int : int -> int value
//
// type _ expr =
//   | Value : 'a value -> 'a expr
//   | If : bool expr * 'a expr * 'a expr -> 'a expr
//   | Eq : 'a expr * 'a expr -> bool expr
//   | Lt : int expr * int expr -> bool expr

enum Value(a) =
  | VBool(Bool) -> Value(Bool)
  | VInt(Int) -> Value(Int)

enum Expr(a) =
  | Val(Value(a)) -> Expr(a)
  | If(Expr(Bool), Expr(a), Expr(a)) -> Expr(a)
  | Eq(Expr(a), Expr(a)) -> Expr(Bool)
  | Lt(Expr(Int), Expr(Int)) -> Expr(Bool)
