module GleamAst
  exposing Ast(..), Meta(..), Module(..), Function(..), Test(..), Expr(..),
    Clause(..), RecordField(..), Export, Charlist

import Foreign exposing Foreign

type Meta =
  | Meta(Int)
; // Fix GitHub syntax highlighting

type alias Type =
  Foreign
;

type Ast =
  | Mod(Module)
  | Expr(Expr)
;

type Module =
  | AstModule(Charlist, List(Export), List(Function), List(Test))
;

type Function =
  | AstFunction(Meta, Charlist, List(Charlist), Expr)
;

type Test =
  | AstFunction(Meta, Charlist, Expr)
;

type Expr(type_) =
  | AstAdt(Meta, Charlist, List(Expr))
  | AstAssignment(Meta, type_, Charlist, Expr, Expr)
  | AstAtom(Meta, Charlist)
  | AstCall(Meta, String, String, List(Expr))
  | AstCase(Meta, Expr, Clause)
  | AstClosure(Meta, List(Charlist), Expr)
  | AstClosureCall(Meta, Expr, List(Expr))
  | AstCons(Meta, Expr, Expr)
  | AstFloat(Meta, Float)
  | AstInt(Meta, Int)
  | AstList(Meta, List(Expr))
  | AstLocalCall(Meta, Charlist, List(Expr))
  | AstPipe(Meta, Expr, Expr)
  | AstRaise(Meta, Expr)
  | AstRecord(Meta, List(RecordField))
  | AstRecordAccess(Meta, Expr, Charlist)
  | AstString(Meta, String)
  | AstThrow(Meta, Expr)
  | AstTuple(Meta, List(Expr))
  | AstVar(Meta, String)
;

type alias UntypedExpr =
  Expr(Unit)
;

type alias TypedExpr =
  Expr(Type)
;

type alias Charlist =
  List(Int)
;

type alias Export =
  (Charlist, Int)
;

type Clause =
  | AstClause(Meta, Expr, Expr)
;

type RecordField =
  | AstClause(Meta, Expr, Expr)
;
