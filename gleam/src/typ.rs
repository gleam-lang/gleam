#![allow(dead_code)] // TODO

use crate::ast::{Expr, Meta, Pattern, Scope, TypedExpr, UntypedExpr};
use crate::grammar;
use crate::pretty::*;
use im::hashmap::HashMap;
use itertools::Itertools;
use std::cell::RefCell;
use std::rc::Rc;

const INDENT: isize = 2;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Const {
        public: bool,
        module: String,
        name: String,
    },

    App {
        public: bool,
        module: String,
        name: String,
        args: Vec<Type>,
    },

    Tuple {
        elems: Vec<Type>,
    },

    Fun {
        args: Vec<Type>,
        retrn: Box<Type>,
    },

    Record {
        row: Row,
    },

    Module {
        row: Row,
    },

    Var {
        typ: Rc<RefCell<TypeVar>>,
    },
}

impl Type {
    pub fn to_gleam_doc(&self, names: &mut HashMap<usize, String>, uid: &mut usize) -> Document {
        match self {
            Type::Const { name, .. } => name.clone().to_doc(),

            Type::App { name, args, .. } => name
                .clone()
                .to_doc()
                .append("(")
                .append(args_to_gleam_doc(args, names, uid))
                .append(")"),

            Type::Fun { args, retrn } => "fn("
                .to_doc()
                .append(args_to_gleam_doc(args, names, uid))
                .append(") -> ")
                .append(retrn.to_gleam_doc(names, uid)),

            Type::Tuple { elems, .. } => args_to_gleam_doc(elems, names, uid).surround("{", "}"),

            Type::Record { .. } => unimplemented!(),

            Type::Module { .. } => unimplemented!(),

            Type::Var { typ, .. } => typ.borrow().to_gleam_doc(names, uid),
        }
    }
}

impl TypeVar {
    pub fn to_gleam_doc(&self, names: &mut HashMap<usize, String>, uid: &mut usize) -> Document {
        match self {
            TypeVar::Link { ref typ, .. } => typ.to_gleam_doc(names, uid),

            TypeVar::Unbound { id, .. } => TypeVar::Generic { id: *id }.to_gleam_doc(names, uid),

            TypeVar::Generic { id, .. } => match names.get(&id) {
                Some(n) => n.clone().to_doc(),
                None => {
                    let n = next_letter(uid);
                    names.insert(*id, n.clone());
                    n.to_doc()
                }
            },
        }
    }
}

// TODO: Handle more than 27 letters
fn next_letter(i: &mut usize) -> String {
    let c = ((*i as u8 + 97) as char).to_string();
    *i += 1;
    c
}

#[test]
fn letter_test() {
    let mut i = 0;
    assert_eq!("a", next_letter(&mut i));
    assert_eq!("b", next_letter(&mut i));
    assert_eq!("c", next_letter(&mut i));
}

fn args_to_gleam_doc(
    args: &Vec<Type>,
    names: &mut HashMap<usize, String>,
    uid: &mut usize,
) -> Document {
    args.iter()
        .map(|t| t.to_gleam_doc(names, uid).group())
        .intersperse(break_(",", ", "))
        .collect::<Vec<_>>()
        .to_doc()
        .nest(INDENT)
        .append(break_(",", ""))
}

#[test]
fn to_gleam_doc_test() {
    let cases = [
        (
            Type::Const {
                module: "whatever".to_string(),
                name: "Int".to_string(),
                public: true,
            },
            "Int",
        ),
        (
            Type::App {
                module: "".to_string(),
                name: "Pair".to_string(),
                public: true,
                args: vec![
                    Type::Const {
                        module: "whatever".to_string(),
                        name: "Int".to_string(),
                        public: true,
                    },
                    Type::Const {
                        module: "whatever".to_string(),
                        name: "Bool".to_string(),
                        public: true,
                    },
                ],
            },
            "Pair(Int, Bool)",
        ),
        (
            Type::Fun {
                args: vec![
                    Type::Const {
                        module: "whatever".to_string(),
                        name: "Int".to_string(),
                        public: true,
                    },
                    Type::Const {
                        module: "whatever".to_string(),
                        name: "Bool".to_string(),
                        public: true,
                    },
                ],
                retrn: Box::new(Type::Const {
                    module: "whatever".to_string(),
                    name: "Bool".to_string(),
                    public: true,
                }),
            },
            "fn(Int, Bool) -> Bool",
        ),
        (
            Type::Var {
                typ: Rc::new(RefCell::new(TypeVar::Link {
                    typ: Box::new(Type::Const {
                        module: "whatever".to_string(),
                        name: "Int".to_string(),
                        public: true,
                    }),
                })),
            },
            "Int",
        ),
        (
            Type::Var {
                typ: Rc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 2231 })),
            },
            "a",
        ),
        (
            Type::Fun {
                args: vec![Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 78 })),
                }],
                retrn: Box::new(Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 2 })),
                }),
            },
            "fn(a) -> b",
        ),
        (
            Type::Fun {
                args: vec![Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Generic { id: 78 })),
                }],
                retrn: Box::new(Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Generic { id: 2 })),
                }),
            },
            "fn(a) -> b",
        ),
    ];

    for (typ, s) in cases.into_iter() {
        assert_eq!(
            s.to_string(),
            typ.to_gleam_doc(&mut hashmap! {}, &mut 0).format(80)
        );
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVar {
    Unbound { id: usize, level: usize },
    Link { typ: Box<Type> },
    Generic { id: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Row {
    Nil,

    Cons {
        label: String,
        head: Box<Type>,
        tail: Box<Row>,
    },
}

#[derive(Debug, Clone, Default)]
pub struct Env {
    uid: usize,
    variables: HashMap<String, Type>,
}

impl Env {
    fn next_uid(&mut self) -> usize {
        let i = self.uid;
        self.uid += 1;
        i
    }

    /// Create a new unbound type that is a specific type, we just don't
    /// know which one yet.
    ///
    pub fn new_unbound_var(&mut self, level: usize) -> Type {
        Type::Var {
            typ: Rc::new(RefCell::new(TypeVar::Unbound {
                id: self.next_uid(),
                level: level,
            })),
        }
    }

    /// Create a new generic type that can stand in for any type.
    ///
    pub fn new_generic_var(&mut self) -> Type {
        Type::Var {
            typ: Rc::new(RefCell::new(TypeVar::Generic {
                id: self.next_uid(),
            })),
        }
    }

    /// Record the type of a variable in the environment.
    ///
    pub fn put_variable(&mut self, name: String, typ: Type) {
        self.variables.insert(name, typ);
    }

    /// Record the type of a variable in the environment.
    ///
    pub fn get_variable(&mut self, name: &String) -> Option<&Type> {
        self.variables.get(name)
    }
}

#[derive(Debug)]
pub enum Error {
    UnknownVariable {
        meta: Meta,
        name: String,
        variables: HashMap<String, Type>,
    },
}

/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer(expr: UntypedExpr, level: usize, env: &mut Env) -> Result<TypedExpr, Error> {
    match expr {
        Expr::Int { meta, value } => Ok(Expr::Int { meta, value }),

        Expr::Seq {
            meta,
            typ: _,
            first,
            then,
        } => {
            let first = infer(*first, level, env)?;
            let then = infer(*then, level, env)?;
            Ok(Expr::Seq {
                meta,
                typ: then.typ(),
                first: Box::new(first),
                then: Box::new(then),
            })
        }

        Expr::Var {
            meta,
            scope: _,
            typ: _,
            name,
        } => {
            let typ = env.get_variable(&name).map(|t| t.clone()).ok_or_else(|| {
                Error::UnknownVariable {
                    meta: meta.clone(),
                    name: name.to_string(),
                    variables: env.variables.clone(),
                }
            })?;
            // TODO: Get real scope
            let scope = Scope::Local;
            Ok(Expr::Var {
                meta,
                scope,
                typ,
                name,
            })
        }

        Expr::Fun {
            meta,
            typ: _,
            args,
            body,
        } => {
            let args_types: Vec<_> = args.iter().map(|_| env.new_unbound_var(level)).collect();
            let ref mut fn_env = env.clone();
            args.iter()
                .zip(args_types.iter())
                .for_each(|(arg, t)| fn_env.put_variable(arg.name.to_string(), (*t).clone()));
            let body = infer(*body, level, fn_env)?;
            let typ = Type::Fun {
                args: args_types,
                retrn: Box::new(body.typ()),
            };
            Ok(Expr::Fun {
                meta,
                typ,
                args,
                body: Box::new(body),
            })
        }

        Expr::Nil { meta, typ: _ } => Ok(Expr::Nil {
            meta,
            typ: list(env.new_unbound_var(level)),
        }),

        Expr::Let {
            meta,
            typ: _,
            pattern:
                Pattern::Var {
                    name,
                    meta: pattern_meta,
                },
            value,
            then,
        } => {
            let value = infer(*value, level + 1, env)?;
            let typ = generalise(value.typ(), level);
            env.put_variable(name.to_string(), typ.clone());
            let then = infer(*then, level, env)?;
            Ok(Expr::Let {
                meta,
                typ,
                pattern: Pattern::Var {
                    name,
                    meta: pattern_meta,
                },
                value: Box::new(value),
                then: Box::new(then),
            })
        }

        // TODO: Support non var patterns by modifying the previous clause
        Expr::Let { .. } => unimplemented!(),

        Expr::Atom { meta, value } => Ok(Expr::Atom { meta, value }),

        Expr::Case { .. } => unimplemented!(),

        Expr::Cons { .. } => unimplemented!(),

        Expr::Call {
            meta,
            typ: _,
            fun,
            args,
        } => {
            let fun = infer(*fun, level, env)?;
            let (args_types, return_type) = match_fun_type(&fun.typ(), args.len())?;
            let args = args_types
                .iter()
                .zip(args)
                .map(|(typ, arg)| {
                    let arg = infer(arg, level, env)?;
                    unify(typ, &arg.typ())?;
                    Ok(arg)
                })
                .collect::<Result<_, _>>()?;
            Ok(Expr::Call {
                meta,
                typ: return_type,
                fun: Box::new(fun),
                args,
            })
        }

        Expr::Tuple {
            meta,
            elems,
            typ: _,
        } => {
            let elems = elems
                .into_iter()
                .map(|e| infer(e, level, env))
                .collect::<Result<Vec<_>, _>>()?;
            let typ = Type::Tuple {
                elems: elems.iter().map(|e| e.typ()).collect(),
            };
            Ok(Expr::Tuple { meta, elems, typ })
        }

        Expr::Float { meta, value } => Ok(Expr::Float { meta, value }),

        Expr::BinOp { .. } => unimplemented!(),

        Expr::String { meta, value } => Ok(Expr::String { meta, value }),

        Expr::RecordNil { .. } => unimplemented!(),

        Expr::RecordCons { .. } => unimplemented!(),

        Expr::Constructor { .. } => unimplemented!(),

        Expr::RecordSelect { .. } => unimplemented!(),

        Expr::ModuleSelect { .. } => unimplemented!(),
    }
}

// let instantiate level ty =
// 	let id_var_map = Hashtbl.create 10 in
// 	let rec f ty = match ty with
// 		| TConst _ -> ty
// 		| TVar {contents = Link ty} -> f ty
// 		| TVar {contents = Generic id} -> begin
// 				try
// 					Hashtbl.find id_var_map id
// 				with Not_found ->
// 					let var = new_var level in
// 					Hashtbl.add id_var_map id var ;
// 					var
// 			end
// 		| TVar {contents = Unbound _} -> ty
// 		| TApp(ty, ty_arg_list) ->
// 				TApp(f ty, List.map f ty_arg_list)
// 		| TArrow(param_ty_list, return_ty) ->
// 				TArrow(List.map f param_ty_list, f return_ty)
// 	in
// 	f ty

// let rec unify ty1 ty2 =
// 	if ty1 == ty2 then () else
// 	match (ty1, ty2) with
// 		| TConst name1, TConst name2 when name1 = name2 -> ()
// 		| TApp(ty1, ty_arg_list1), TApp(ty2, ty_arg_list2) ->
// 				unify ty1 ty2 ;
// 				List.iter2 unify ty_arg_list1 ty_arg_list2
// 		| TArrow(param_ty_list1, return_ty1), TArrow(param_ty_list2, return_ty2) ->
// 				List.iter2 unify param_ty_list1 param_ty_list2 ;
// 				unify return_ty1 return_ty2
// 		| TVar {contents = Link ty1}, ty2 | ty1, TVar {contents = Link ty2} -> unify ty1 ty2
// 		| TVar {contents = Unbound(id1, _)}, TVar {contents = Unbound(id2, _)} when id1 = id2 ->
// 				assert false (* There is only a single instance of a particular type variable. *)
// 		| TVar ({contents = Unbound(id, level)} as tvar), ty
// 		| ty, TVar ({contents = Unbound(id, level)} as tvar) ->
// 				occurs_check_adjust_levels id level ty ;
// 				tvar := Link ty
// 		| _, _ -> error ("cannot unify types " ^ string_of_ty ty1 ^ " and " ^ string_of_ty ty2)
fn unify(t1: &Type, t2: &Type) -> Result<(), Error> {
    unimplemented!()
}

// let rec match_fun_ty num_params = function
// 	| TArrow(param_ty_list, return_ty) ->
// 			if List.length param_ty_list <> num_params then
// 				error "unexpected number of arguments"
// 			else
// 				param_ty_list, return_ty
// 	| TVar {contents = Link ty} -> match_fun_ty num_params ty
// 	| TVar ({contents = Unbound(id, level)} as tvar) ->
// 			let param_ty_list =
// 				let rec f = function
// 					| 0 -> []
// 					| n -> new_var level :: f (n - 1)
// 				in
// 				f num_params
// 			in
// 			let return_ty = new_var level in
// 			tvar := Link (TArrow(param_ty_list, return_ty)) ;
// 			param_ty_list, return_ty
// 	| _ -> error "expected a function"
fn match_fun_type(typ: &Type, arity: usize) -> Result<(Vec<Type>, Type), Error> {
    unimplemented!()
}

#[test]
fn infer_test() {
    let cases = [
        ("1", "Int"),
        ("-2", "Int"),
        ("1.0", "Float"),
        ("-8.0", "Float"),
        ("'hello'", "Atom"),
        ("\"ok\"", "String"),
        ("\"ok\"", "String"),
        ("1 2.0", "Float"),
        ("x = 1 2", "Int"),
        ("x = 1 x", "Int"),
        ("x = 'ok' x", "Atom"),
        ("x = 'ok' y = x y", "Atom"),
        ("x = 'ok' y = x y", "Atom"),
        ("[]", "List(a)"),
        ("fn(x) { x }", "fn(a) -> a"),
        ("fn(x) { x }", "fn(a) -> a"),
        ("fn(x, y) { x }", "fn(a, b) -> a"),
        ("fn(x, y) { [] }", "fn(a, b) -> List(c)"),
        ("{1}", "{Int}"),
        ("{1, 2.0}", "{Int, Float}"),
        ("{1, 2.0, '3'}", "{Int, Float, Atom}"),
        ("{1, 2.0, {'ok', 1}}", "{Int, Float, {Atom, Int}}"),
        ("id = fn(x) { x } id(1)", "Int"),
    ];

    for (src, typ) in cases.into_iter() {
        let ast = grammar::ExprParser::new().parse(src).expect("syntax error");
        assert_eq!(
            typ.to_string(),
            infer(ast, 1, &mut Env::default())
                .expect("should successfully infer")
                .typ()
                .to_gleam_doc(&mut hashmap! {}, &mut 0)
                .format(80)
        );
    }
}

// let rec generalize level = function
// 	| TVar {contents = Unbound(id, other_level)} when other_level > level ->
// 			TVar (ref (Generic id))
// 	| TApp(ty, ty_arg_list) ->
// 			TApp(generalize level ty, List.map (generalize level) ty_arg_list)
// 	| TArrow(param_ty_list, return_ty) ->
// 			TArrow(List.map (generalize level) param_ty_list, generalize level return_ty)
// 	| TVar {contents = Link ty} -> generalize level ty
// 	| TVar {contents = Generic _} | TVar {contents = Unbound _} | TConst _ as ty -> ty
/// Takes a level and a type and turns all type variables within the type that have
/// level higher than the input level into generalized (polymorphic) type variables.
///
fn generalise(t: Type, level: usize) -> Type {
    match t {
        Type::Var { typ } => match (*typ).borrow() {
            // TODO: How do I pattern match on a Ref?
            TypeVar::Unbound { id, level: t_level } => t,
        },

        Type::Var { .. } => unimplemented!(),

        Type::App { .. } => unimplemented!(),

        Type::Fun { args, retrn } => {
            let args = args.into_iter().map(|t| generalise(t, level)).collect();
            let retrn = generalise(*retrn, level);
            Type::Fun {
                args,
                retrn: Box::new(retrn),
            }
        }

        Type::Tuple { .. } => unimplemented!(),

        Type::Const { .. } => t,

        Type::Record { .. } => unimplemented!(),

        Type::Module { .. } => unimplemented!(),
    }
}

pub fn int() -> Type {
    Type::Const {
        public: true,
        name: "Int".to_string(),
        module: "".to_string(),
    }
}

pub fn float() -> Type {
    Type::Const {
        public: true,
        name: "Float".to_string(),
        module: "".to_string(),
    }
}

pub fn atom() -> Type {
    Type::Const {
        public: true,
        name: "Atom".to_string(),
        module: "".to_string(),
    }
}

pub fn string() -> Type {
    Type::Const {
        public: true,
        name: "String".to_string(),
        module: "".to_string(),
    }
}

pub fn list(t: Type) -> Type {
    Type::App {
        public: true,
        name: "List".to_string(),
        module: "".to_string(),
        args: vec![t],
    }
}
