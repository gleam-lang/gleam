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
  | AstAssignment(Meta, type_, Charlist, Expr(type_), Expr(type_))
  | AstAtom(Meta, Charlist)
  | AstCall(Meta, String, String, List(Expr(type_)))
  | AstCase(Meta, Expr(type_), Clause)
  | AstClosure(Meta, List(Charlist), Expr(type_))
  | AstClosureCall(Meta, Expr(type_), List(Expr(type_)))
  | AstCons(Meta, Expr(type_), Expr(type_))
  | AstFloat(Meta, Float)
  | AstInt(Meta, Int)
  | AstList(Meta, List(Expr(type_)))
  | AstLocalCall(Meta, Charlist, List(Expr(type_)))
  | AstPipe(Meta, Expr(type_), Expr(type_))
  | AstRaise(Meta, Expr(type_))
  | AstRecord(Meta, List(RecordField))
  | AstRecordAccess(Meta, Expr(type_), Charlist)
  | AstString(Meta, String)
  | AstThrow(Meta, Expr(type_))
  | AstTuple(Meta, List(Expr(type_)))
  | AstVar(Meta, String)
  | AstSeq(Meta, Expr(type_), Expr(type_))
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
