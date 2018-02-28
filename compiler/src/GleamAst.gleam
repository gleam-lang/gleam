module GleamAst

export Ast(..), Meta(..), Module(..), Function(..), Test(..), Expr(..),
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
  | AstTuple(Meta, List(Expr))
  | AstList(Meta, List(Expr))
  | AstInt(Meta, Int)
  | AstFloat(Meta, Float)
  | AstBool(Meta, Bool)
  | AstAtom(Meta, Charlist)
  | AstString(Meta, String)
  | AstVar(Meta, String)
  | AstClosure(Meta, List(Charlist), Expr)
  | AstCall(Meta, String, String, List(Expr))
  | AstCons(Meta, Expr, Expr)
  | AstClosureCall(Meta, Expr, List(Expr))
  | AstLocalCall(Meta, Charlist, List(Expr))
  | AstAssignment(Meta, Charlist, Expr, Expr)
  | AstAdt(Meta, Charlist, List(Expr))
  | AstCase(Meta, Expr, Clause)
  | AstRecord(Meta, List(RecordField))
  | AstRecordAccess(Meta, Expr, Charlist)
  | AstPipe(Meta, Expr, Expr)

type alias Charlist
  = List(Int)

type alias Export
  = (Charlist, Int)

type Clause =
  | AstClause(Meta, Expr, Expr)

type RecordField =
  | AstClause(Meta, Expr, Expr)
