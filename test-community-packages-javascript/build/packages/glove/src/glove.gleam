import gleam/int
import gleam/string
import gleam/option.{None, Option, Some}
import gleam/list

/// QBE Comparison Operators
pub type Comp {
  /// Less Than
  Slt
  /// Less or Equal
  Sle
  /// Greater than
  Sgt
  /// Greater or equal
  Sge
  /// Equal
  Eq
  /// Not equal
  Ne
}

/// QBE instruction
pub type Inst {
  /// Adds values 
  Add(Value, Value)
  /// Substracts value(b) from value(a)
  Sub(Value, Value)
  /// Multiplies values 
  Mul(Value, Value)
  /// Divides value(a) by value(b)
  Div(Value, Value)
  /// Returns a remaider from division
  Rem(Value, Value)
  /// Perform a Comparison
  Comp(Type, Comp, Value, Value)
  /// Bitwise AND
  And(Value, Value)
  /// Bitwise OR
  Or(Value, Value)
  /// Copies either temporary or literal value
  Copy(Value)
  /// Return from a function, optionally with a value
  Ret(Option(Value))
  /// Jumps to first label if a value is nonzero or to the second one otherwise
  Jnz(Value, String, String)
  /// Unconditionally jumps to a label
  Jmp(String)
  /// Calls a function
  Call(Value, List(#(Type, Value)))
  /// Allocates a 4-byte aligned area on the stack
  Alloc4(Int)
  /// Allocates a 8-byte aligned area on the stack
  Alloc8(Int)
  /// Allocates a 16-byte aligned area on the stack
  Alloc16(Int)
  /// Stores a value into memory pointed to by destination.
  /// (type, destination, value)
  Store(Type, Value, Value)
  /// Loads a value from memory pointed to by source
  /// (type, source)
  Load(Type, Value)
  /// (source, destination, n)
  ///
  /// Copy `n` bytes from the source address to the destination address.
  ///
  /// n must be a constant value.
  ///
  /// Minimum supported QBE version 1.1
  Blit(Value, Value, Int)
}

/// Display function for Instructions
pub fn display_inst(inst: Inst) -> String {
  case inst {
    Add(a, b) -> "add " <> display_value(a) <> ", " <> display_value(b)
    Sub(a, b) -> "sub " <> display_value(a) <> ", " <> display_value(b)
    Mul(a, b) -> "mul " <> display_value(a) <> ", " <> display_value(b)
    Div(a, b) -> "div " <> display_value(a) <> ", " <> display_value(b)
    Rem(a, b) -> "rem " <> display_value(a) <> ", " <> display_value(b)
    Comp(ty, cmp, a, b) -> {
      case ty {
        Aggregate(_) -> "Cannot Compare aggregate types"
        _ ->
          case cmp {
            Slt ->
              "c" <> "slt" <> " " <> display_type(ty) <> " " <> display_value(a) <> " " <> display_value(
                b,
              )
            Sle ->
              "c" <> "sle" <> " " <> display_type(ty) <> " " <> display_value(a) <> " " <> display_value(
                b,
              )
            Sgt ->
              "c" <> "sgt" <> " " <> display_type(ty) <> " " <> display_value(a) <> " " <> display_value(
                b,
              )
            Sge ->
              "c" <> "sge" <> " " <> display_type(ty) <> " " <> display_value(a) <> " " <> display_value(
                b,
              )
            Eq ->
              "c" <> "eq" <> " " <> display_type(ty) <> " " <> display_value(a) <> " " <> display_value(
                b,
              )
            Ne ->
              "c" <> "ne" <> " " <> display_type(ty) <> " " <> display_value(a) <> " " <> display_value(
                b,
              )
          }
      }
    }
    And(a, b) -> "and " <> display_value(a) <> ", " <> display_value(b)
    Or(a, b) -> "or " <> display_value(a) <> ", " <> display_value(b)
    Copy(val) -> "copy " <> display_value(val)
    Ret(val) -> {
      case val {
        Some(val) -> "ret " <> display_value(val) <> "\n"
        None -> "ret\n"
      }
    }
    Jnz(val, if_nonzero, if_zero) ->
      "jnz " <> display_value(val) <> ", @" <> if_nonzero <> ", @" <> if_zero
    Jmp(str) -> "jmp @" <> str
    Call(name, args) -> {
      let arg_str =
        args
        |> list.index_map(fn(_, arg) {
          case arg {
            #(ty, val) -> display_type(ty) <> " " <> display_value(val)
          }
        })
        |> string.join(", ")

      "call " <> display_value(name) <> "(" <> arg_str <> ")"
    }

    Alloc4(int) -> "alloc4 " <> int.to_string(int)
    Alloc8(int) -> "alloc8 " <> int.to_string(int)
    Alloc16(int) -> "alloc16 " <> int.to_string(int)
    Store(typ, value, dest) ->
      case typ {
        Aggregate(_) -> "Store to an aggregate type"
        _ ->
          "store" <> display_type(typ) <> " " <> display_value(value) <> " " <> display_value(
            dest,
          )
      }

    Load(typ, val) ->
      case typ {
        Aggregate(_) -> "Load aggregate type"
        _ -> "load" <> display_type(typ) <> " " <> display_value(val)
      }
    Blit(src, dest, n) ->
      "blit " <> display_value(src) <> ", " <> display_value(dest) <> ", " <> int.to_string(
        n,
      )
  }
}

/// QBE Value for instructions
pub type Value {
  /// `%`-temporary
  Temporary(name: String)
  /// `$`-global
  Global(name: String)
  /// Constant
  Const(value: Int)
}

/// Display Value function
pub fn display_value(value: Value) -> String {
  case value {
    Temporary(name) -> "%" <> name
    Global(name) -> "$" <> name
    Const(value) -> int.to_string(value)
  }
}

/// QBE Types
pub type Type {
  /// Base Types
  Word
  Long
  Single
  Double
  /// Extended Types
  Byte
  Halfword
  Aggregate(TypeDef)
}

/// Display Type function
pub fn display_type(ty: Type) -> String {
  case ty {
    Byte -> "b"
    Halfword -> "h"
    Word -> "w"
    Long -> "l"
    Single -> "s"
    Double -> "d"
    Aggregate(ty) -> display_type_def(ty)
  }
}

/// Aggregate type with a specified name
/// Returns a C ABI type. Extended types are converted to closest base
/// types
pub fn into_abi(self) -> Type {
  case self {
    Byte | Halfword -> Word
    other -> other
  }
}

/// Returns the closest base type
pub fn into_base(self) -> Type {
  case self {
    Byte | Halfword -> Word
    Aggregate(_) -> Long
    other -> other
  }
}

/// Returns byte size for values of the type
pub fn size(self) -> Int {
  case self {
    Byte -> 1
    Halfword -> 2
    Word | Single -> 4
    Long | Double -> 8
    // This not working 
    Aggregate(td) ->
      case td.items {
        [] -> 0
      }
  }
}

/// QBE data definition
pub type DataDef {
  DataDef(
    linkage: Linkage,
    name: String,
    align: Option(Int),
    items: List(#(Type, DataItem)),
  )
}

pub fn new_datadef() -> DataDef {
  DataDef(linkage: private(), name: "", align: None, items: [])
}

/// Display function for Datadef
pub fn display_data_def(def: DataDef) -> String {
  let linkage_str = display_linkage(def.linkage)
  let align_str = case def.align {
    Some(align) -> " align " <> int.to_string(align)
    None -> ""
  }

  let items_str =
    def.items
    |> list.index_map(fn(_, item) {
      case item {
        #(ty, di) -> display_type(ty) <> " " <> display_data_item(di)
      }
    })
    |> string.join(", ")

  linkage_str <> "data $" <> def.name <> " =" <> align_str <> " { " <> items_str <> " }"
}

/// QBE aggregate type definition
pub type TypeDef {
  TypeDef(name: String, align: Option(Int), items: List(#(Type, Int)))
}

/// Display function for TypeDef
pub fn display_type_def(def: TypeDef) -> String {
  let align_str = case def.align {
    Some(align) -> "align " <> int.to_string(align) <> " "
    None -> ""
  }

  let items_str =
    def.items
    |> list.index_map(fn(_, item) {
      case item {
        #(ty, count) ->
          case count > 1 {
            False -> display_type(ty)
            True -> display_type(ty) <> " " <> int.to_string(count)
          }
      }
    })
    |> string.join(", ")

  "type :" <> def.name <> " = " <> align_str <> "{ " <> items_str <> " }"
}

/// QBE Data definition item
pub type DataItem {
  /// Symbol and offset
  Symbol(String, Option(Int))
  /// String
  Str(String)
  /// Integer
  Constant(Int)
}

/// Display function for DataItem
pub fn display_data_item(item: DataItem) -> String {
  case item {
    Symbol(name, offset) -> {
      case offset {
        Some(off) -> "$" <> name <> " +" <> int.to_string(off)
        None -> "$" <> name
      }
    }
    Str(string) -> "\"" <> string <> "\""
    Constant(val) -> int.to_string(val)
  }
}

/// IR Statement
pub type Statement {
  Assign(Value, Type, Inst)
  Volatile(Inst)
}

/// Display function for Statement 
pub fn display_statement(stmt: Statement) -> String {
  case stmt {
    Assign(val, typ, inst) ->
      display_value(val) <> " =" <> display_type(typ) <> " " <> display_inst(
        inst,
      )
    Volatile(inst) -> display_inst(inst)
  }
}

/// Function block with a label
pub type Block {
  Block(label: String, statements: List(Statement))
}

/// Display function for block
pub fn display_block(block: Block) -> String {
  let label = block.label
  let statements =
    block.statements
    |> list.map(display_statement)
    |> string.join("\n")

  label <> "\n" <> statements
}

/// Adds a new instruction to the block
pub fn add_inst(block: Block, inst: Inst) -> Block {
  Block(
    label: block.label,
    statements: list.append(block.statements, [Volatile(inst)]),
  )
}

/// Adds a new instruction assigned to a temporary
pub fn assign_inst(block: Block, val: Value, typ: Type, inst: Inst) -> Block {
  Block(
    label: block.label,
    statements: list.append(block.statements, [Assign(val, typ, inst)]),
  )
}

/// Returns true if the block's last instruction is a jump
pub fn jumps(block: Block) -> Bool {
  case list.last(block.statements) {
    Ok(statement) ->
      case statement {
        Volatile(instr) ->
          case instr {
            Ret(_) -> True
            Jmp(_) -> True
            Jnz(_, _, _) -> True
            _ -> False
          }
        _ -> False
      }
    Error(_) -> False
  }
}

/// QBE Function
pub type Function {
  Function(
    linkage: Linkage,
    name: String,
    arguments: List(#(Type, Value)),
    return_ty: Option(Type),
    blocks: List(Block),
  )
}

/// Display function for functions
pub fn display_function(func: Function) -> String {
  let linkage_str = display_linkage(func.linkage)
  let name_str = func.name
  let return_str = case func.return_ty {
    Some(ty) -> " " <> display_type(ty)
    None -> ""
  }
  let args_str = display_arguments(func.arguments)
  let blocks_str = display_blocks(func.blocks)

  linkage_str <> "function" <> return_str <> " " <> "$" <> name_str <> "(" <> args_str <> ")" <> " {\n" <> blocks_str <> "}"
}

/// Display functions Arguments
pub fn display_arguments(arguments: List(#(Type, Value))) -> String {
  case arguments {
    [] -> ""
    _ ->
      arguments
      |> list.index_map(fn(_, arg) {
        case arg {
          #(ty, val) -> display_type(ty) <> " " <> display_value(val)
        }
      })
      |> string.join(", ")
  }
}

/// Display blocks
pub fn display_blocks(blocks: List(Block)) -> String {
  blocks
  |> list.map(fn(block) { display_block(block) })
  |> string.join("\n")
}

/// Instantiates an empty function and returns it
pub fn new_function() -> Function {
  Function(
    linkage: private(),
    name: "",
    arguments: [],
    return_ty: None,
    blocks: [],
  )
}

/// Adds a new empty block with a specified label and returns 
/// a reference to it
pub fn add_block(label: String) -> Block {
  Block(label: label, statements: [])
}

/// Returns a reference to the last block
pub fn last_block(blocks: List(Block)) -> Option(Block) {
  case list.last(blocks) {
    Ok(block) -> Some(block)
    Error(_) -> None
  }
}

/// Linkage of a function or data defintion (e.g. section and
/// private/public status)
pub type Linkage {
  Linkage(exported: Bool, section: Option(String), secflags: Option(String))
}

/// Display function for Linkage
pub fn display_linkage(linkage: Linkage) -> String {
  let exported_str = case linkage.exported {
    True -> "export "
    False -> ""
  }
  let section_str = case linkage.section {
    Some(section) ->
      "section \"" <> section <> "\"" <> case linkage.secflags {
        Some(secflags) -> " \"" <> secflags <> "\""
        None -> ""
      } <> " "
    None -> ""
  }
  exported_str <> section_str
}

/// Returns the default configuration for private linkage
pub fn private() -> Linkage {
  Linkage(exported: False, section: None, secflags: None)
}

/// Returns the configuration for private linkage with a provided section
pub fn private_with_section(section: String) -> Linkage {
  Linkage(exported: False, section: Some(section), secflags: None)
}

/// Returns the default configuration for public linkage
pub fn public() -> Linkage {
  Linkage(exported: True, section: None, secflags: None)
}

/// Returns the configuration for public linkage with a provided section
pub fn public_with_section(section: String) -> Linkage {
  Linkage(exported: True, section: Some(section), secflags: None)
}

/// A complete IL file 
pub type Module {
  Module(functions: List(Function), types: List(TypeDef), data: List(DataDef))
}

/// Creates a new module
pub fn new_module() -> Module {
  Module(functions: [], types: [], data: [])
}

/// Display function for Module
pub fn display_module(module: Module) -> String {
  let functions_str =
    module.functions
    |> list.map(display_function)
    |> string.join("\n")

  let types_str =
    module.types
    |> list.map(display_type_def)
    |> string.join("\n")

  let data_str =
    module.data
    |> list.map(display_data_def)
    |> string.join("\n")

  functions_str <> types_str <> "\n" <> data_str
}

/// Add function to module
pub fn add_function(module: Module, function: Function) -> Module {
  Module(
    functions: list.append(module.functions, [function]),
    types: module.types,
    data: module.data,
  )
}

/// Add type to module
pub fn add_type(module: Module, type_def: TypeDef) -> Module {
  Module(
    functions: module.functions,
    types: list.append(module.types, [type_def]),
    data: module.data,
  )
}

/// Add Data to module
pub fn add_data(module: Module, data_def: DataDef) -> Module {
  Module(
    functions: module.functions,
    types: module.types,
    data: list.append(module.data, [data_def]),
  )
}
