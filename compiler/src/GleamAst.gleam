module GleamAst
  exposing Ast(..), Meta(..), Module(..), Function(..), Test(..), Expr(..),
    Clause(..), RecordField(..), Export, Charlist

type Meta =
  | Meta(Int)

type Ast =
  | Mod(Module)
  | Expr(Expr)

type Module =
  | AstModule(Charlist, List(Export), List(Function), List(Test))

type Function =
  | AstFunction(Meta, Charlist, List(Charlist), Expr)

type Test =
  | AstFunction(Meta, Charlist, Expr)

type Expr =
  | AstAdt(Meta, Charlist, List(Expr))
  | AstAssignment(Meta, Charlist, Expr, Expr)
  | AstAtom(Meta, Charlist)
  | AstBool(Meta, Bool)
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

type alias Charlist
  = List(Int)

type alias Export
  = (Charlist, Int)

type Clause =
  | AstClause(Meta, Expr, Expr)

type RecordField =
  | AstClause(Meta, Expr, Expr)
