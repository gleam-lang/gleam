pub enum Meta =
  | Meta(Int)

pub enum Type(var_type) =
  | TypeConst(String)
  | TypeTuple(List(Type(var_type)))
  | TypeFunc(List(Type(var_type)), Type(var_type))
  | TypeVar(var_type)

pub type TypeRef =
  Reference

pub type ResolvedType =
  Type(Unit)

pub type UnresolvedType =
  Type(TypeRef)

pub enum Ast =
  | Mod(Module)
  | Expr(Expr)

pub enum Module =
  | AstModule(Charlist, List(Export), List(Function), List(Test))

pub enum Function =
  | AstFunction(Meta, Charlist, List(Charlist), Expr)

pub enum Test =
  | AstFunction(Meta, Charlist, Expr)

pub enum Expr(type_) =
  | AstAdt(Meta, Charlist, List(Expr))
  | AstAssignment(Meta, type_, Charlist, Expr(type_), Expr(type_))
  | AstAtom(Meta, Charlist)
  | AstCall(Meta, String, String, List(Expr(type_)))
  | AstCase(Meta, Expr(type_), Clause)
  | AstClosure(Meta, type_, List(Charlist), Expr(type_))
  | AstClosureCall(Meta, Expr(type_), List(Expr(type_)))
  | AstCons(Meta, Expr(type_), Expr(type_))
  | AstFloat(Meta, Float)
  | AstHole(Meta)
  | AstInt(Meta, Int)
  | AstLocalCall(Meta, Charlist, List(Expr(type_)))
  | AstNil(Meta)
  | AstPipe(Meta, Expr(type_), Expr(type_))
  | AstRaise(Meta, Expr(type_))
  | AstRecord(Meta, List(RecordField))
  | AstRecordAccess(Meta, Expr(type_), Charlist)
  | AstSeq(Meta, Expr(type_), Expr(type_))
  | AstString(Meta, String)
  | AstThrow(Meta, Expr(type_))
  | AstTuple(Meta, List(Expr(type_)))
  | AstVar(Meta, String)

pub type UntypedExpr =
  Expr(Unit)

pub type TypedExpr =
  Expr(Type)

pub type Charlist =
  List(Int)

pub type Export =
  (Charlist, Int)

pub enum Clause =
  | AstClause(Meta, Expr, Expr)

pub enum RecordField =
  | AstClause(Meta, Expr, Expr)
