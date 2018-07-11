module GleamAst
  exposing Ast(..), Meta(..), Module(..), Function(..), Test(..), Expr(..),
    Clause(..), RecordField(..), Export, Charlist

type Meta =
  | Meta(Int)

; // Fix GitHub syntax highlighting

type Ast =
  | Mod(Module)
  | Expr(Expr)

; // Fix GitHub syntax highlighting

type Module =
  | AstModule(Charlist, List(Export), List(Function), List(Test))

; // Fix GitHub syntax highlighting

type Function =
  | AstFunction(Meta, Charlist, List(Charlist), Expr)

; // Fix GitHub syntax highlighting

type Test =
  | AstFunction(Meta, Charlist, Expr)

; // Fix GitHub syntax highlighting

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

; // Fix GitHub syntax highlighting

type alias Charlist =
  List(Int)

; // Fix GitHub syntax highlighting

type alias Export =
  (Charlist, Int)

; // Fix GitHub syntax highlighting

type Clause =
  | AstClause(Meta, Expr, Expr)

; // Fix GitHub syntax highlighting

type RecordField =
  | AstClause(Meta, Expr, Expr)
