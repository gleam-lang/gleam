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

    IncorrectArity {
        meta: Meta,
        expected: usize,
        given: usize,
    },

    CouldNotUnify {
        meta: Meta,
        expected: Type,
        given: Type,
    },

    RecursiveType {
        meta: Meta,
    },
}

/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer(expr: UntypedExpr, level: usize, env: &mut Env) -> Result<TypedExpr, Error> {
    match expr {
        Expr::Int { meta, value, .. } => Ok(Expr::Int {
            meta,
            value,
            typ: int(),
        }),

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
                typ: then.typ().clone(),
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
            let typ = instantiate(typ, level, env);
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
            let vars = env.variables.clone();
            args.iter().zip(args_types.iter()).for_each(|(arg, t)| {
                env.put_variable(arg.name.to_string(), (*t).clone());
            });
            let body = infer(*body, level, env)?;
            env.variables = vars;
            let typ = Type::Fun {
                args: args_types,
                retrn: Box::new(body.typ().clone()),
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
            let value_typ = generalise(value.typ().clone(), level);
            env.put_variable(name.to_string(), value_typ.clone());
            let then = infer(*then, level, env)?;
            let typ = then.typ().clone();
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

        Expr::Atom { meta, value, .. } => Ok(Expr::Atom {
            meta,
            value,
            typ: atom(),
        }),

        Expr::Case { .. } => unimplemented!(),

        Expr::Cons { .. } => unimplemented!(),

        Expr::Call {
            meta,
            typ: _,
            fun,
            args,
        } => {
            let fun = infer(*fun, level, env)?;
            let (mut args_types, return_type) = match_fun_type(fun.typ(), args.len(), env)
                .map_err(|e| convert_not_fun_error(e, &meta))?;
            let args = args_types
                .iter_mut()
                .zip(args)
                .map(|(typ, arg): (&mut Type, _)| {
                    let arg = infer(arg, level, env)?;
                    unify(typ, arg.typ()).map_err(|e| convert_unify_error(e, &meta))?;
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
                elems: elems.iter().map(|e| e.typ().clone()).collect(),
            };
            Ok(Expr::Tuple { meta, elems, typ })
        }

        Expr::Float { meta, value, .. } => Ok(Expr::Float {
            meta,
            value,
            typ: float(),
        }),

        Expr::BinOp { .. } => unimplemented!(),

        Expr::String { meta, value, .. } => Ok(Expr::String {
            meta,
            value,
            typ: string(),
        }),

        Expr::RecordNil { .. } => unimplemented!(),

        Expr::RecordCons { .. } => unimplemented!(),

        Expr::Constructor { .. } => unimplemented!(),

        Expr::RecordSelect { .. } => unimplemented!(),

        Expr::ModuleSelect { .. } => unimplemented!(),
    }
}

fn convert_unify_error(e: UnifyError, meta: &Meta) -> Error {
    match e {
        UnifyError::CouldNotUnify { expected, given } => Error::CouldNotUnify {
            meta: meta.clone(),
            expected,
            given,
        },

        UnifyError::RecursiveType => Error::RecursiveType { meta: meta.clone() },
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
/// Instanciate converts generic variables into unbound ones.
///
fn instantiate(typ: Type, ctx_level: usize, env: &mut Env) -> Type {
    fn go(t: Type, ctx_level: usize, ids: &mut HashMap<usize, Type>, env: &mut Env) -> Type {
        match t {
            Type::Const { .. } => t,

            Type::App { .. } => unimplemented!(),

            Type::Var { typ } => match &*typ.borrow() {
                TypeVar::Link { .. } => unimplemented!(),

                TypeVar::Unbound { id, level } => Type::Var { typ: typ.clone() },

                TypeVar::Generic { id } => match ids.get(id) {
                    Some(t) => t.clone(),
                    None => {
                        let v = env.new_unbound_var(ctx_level);
                        ids.insert(*id, v.clone());
                        v
                    }
                },
            },

            Type::Tuple { .. } => unimplemented!(),

            Type::Fun { args, retrn, .. } => {
                let args = args
                    .into_iter()
                    .map(|t| go(t, ctx_level, ids, env))
                    .collect();
                let retrn = Box::new(go(*retrn, ctx_level, ids, env));
                Type::Fun { args, retrn }
            }

            Type::Record { .. } => unimplemented!(),

            Type::Module { .. } => unimplemented!(),
        }
    }

    go(typ, ctx_level, &mut hashmap![], env)
}

enum UnifyError {
    CouldNotUnify { expected: Type, given: Type },

    RecursiveType,
}

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
fn unify(t1: &Type, t2: &Type) -> Result<(), UnifyError> {
    if let Type::Var { typ } = t1 {
        let new_value = match &*typ.borrow() {
            TypeVar::Link { .. } => unimplemented!(),

            TypeVar::Unbound { id, level } => {
                update_levels(t2, *level, *id)?;
                Some(TypeVar::Link {
                    typ: Box::new((*t2).clone()),
                })
            }

            TypeVar::Generic { .. } => unimplemented!(),
        };

        if let Some(t) = new_value {
            *typ.borrow_mut() = t;
        }

        return Ok(());
    }

    if let Type::Var { typ } = t1 {
        return Ok(());
    }

    if let Type::Var { .. } = t2 {
        return unify(t2, t1).map_err(|e| flip_unify_error(e));
    }

    match (t1, t2) {
        (
            Type::Const {
                module: m1,
                name: n1,
                ..
            },
            Type::Const {
                module: m2,
                name: n2,
                ..
            },
        ) => {
            if n1 == n2 && m1 == m2 {
                Ok(())
            } else {
                unimplemented!()
            }
        }

        (Type::App { .. }, Type::App { .. }) => unimplemented!(),

        (Type::Tuple { .. }, Type::Tuple { .. }) => unimplemented!(),

        (Type::Fun { .. }, Type::Fun { .. }) => unimplemented!(),

        (Type::Record { .. }, Type::Record { .. }) => unimplemented!(),

        (Type::Module { .. }, Type::Module { .. }) => unimplemented!(),

        (_, _) => unimplemented!(),
    }
}

fn flip_unify_error(e: UnifyError) -> UnifyError {
    match e {
        UnifyError::CouldNotUnify { expected, given } => UnifyError::CouldNotUnify {
            expected: given,
            given: expected,
        },
        other => other,
    }
}

// let occurs_check_adjust_levels tvar_id tvar_level ty =
// 	let rec f = function
// 		| TVar {contents = Link ty} -> f ty
// 		| TVar {contents = Generic _} -> assert false
// 		| TVar ({contents = Unbound(other_id, other_level)} as other_tvar) ->
// 				if other_id = tvar_id then
// 					error "recursive types"
// 				else
// 					if other_level > tvar_level then
// 						other_tvar := Unbound(other_id, tvar_level)
// 					else
// 						()
// 		| TApp(ty, ty_arg_list) ->
// 				f ty ;
// 				List.iter f ty_arg_list
// 		| TArrow(param_ty_list, return_ty) ->
// 				List.iter f param_ty_list ;
// 				f return_ty
// 		| TConst _ -> ()
// 	in
// 	f ty
/// This function makes sure that the type variable being unified
/// doesn't occur within the type it is being unified with. This
/// prevents the algorithm from inferring recursive types, which
/// could cause naively-implemented type checking to diverge.
/// While traversing the type tree, this function also takes care
/// of updating the levels of the type variables appearing within
/// the type, thus ensuring the type will be correctly generalized.
///
fn update_levels(typ: &Type, own_level: usize, own_id: usize) -> Result<(), UnifyError> {
    if let Type::Var { typ } = &typ {
        let new_value = match &*typ.borrow() {
            TypeVar::Link { .. } => unimplemented!(),

            TypeVar::Unbound { id, level } => {
                if id == &own_id {
                    unimplemented!()
                } else if level > &own_level {
                    unimplemented!()
                } else {
                    return Ok(());
                }
            }

            TypeVar::Generic { .. } => unimplemented!(),
        };

        if let Some(t) = new_value {
            *typ.borrow_mut() = t;
        }
        return Ok(());
    }

    match typ {
        Type::Const { .. } => Ok(()),

        Type::App { .. } => unimplemented!(),

        Type::Tuple { .. } => unimplemented!(),

        Type::Fun { args, retrn, .. } => unimplemented!(),

        Type::Record { .. } => unimplemented!(),

        Type::Module { .. } => unimplemented!(),

        Type::Var { .. } => unreachable!(),
    }
}

struct NotFunError {
    pub expected: usize,
    pub given: usize,
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
fn match_fun_type(
    typ: &Type,
    arity: usize,
    env: &mut Env,
) -> Result<(Vec<Type>, Type), NotFunError> {
    if let Type::Var { typ } = &typ {
        let new_value = match &*typ.borrow() {
            TypeVar::Link { .. } => unimplemented!(),

            TypeVar::Unbound { id, level } => {
                let args: Vec<_> = (0..arity).map(|_| env.new_unbound_var(*level)).collect();
                let retrn = env.new_unbound_var(*level);
                Some((args, retrn))
            }

            TypeVar::Generic { .. } => None,
        };

        if let Some((args, retrn)) = new_value {
            *typ.borrow_mut() = TypeVar::Link {
                typ: Box::new(Type::Fun {
                    args: args.clone(),
                    retrn: Box::new(retrn.clone()),
                }),
            };
            return Ok((args, retrn));
        }
    }

    if let Type::Fun { args, retrn } = typ {
        return if args.len() != arity {
            Err(NotFunError {
                expected: args.len(),
                given: arity,
            })
        } else {
            Ok((args.clone(), (**retrn).clone()))
        };
    }

    unimplemented!()
}

fn convert_not_fun_error(e: NotFunError, meta: &Meta) -> Error {
    Error::IncorrectArity {
        meta: meta.clone(),
        expected: e.expected,
        given: e.given,
    }
}

#[test]
fn infer_test() {
    struct Case {
        src: &'static str,
        typ: &'static str,
    }
    let cases = [
        Case {
            src: "1",
            typ: "Int",
        },
        Case {
            src: "-2",
            typ: "Int",
        },
        Case {
            src: "1.0",
            typ: "Float",
        },
        Case {
            src: "-8.0",
            typ: "Float",
        },
        Case {
            src: "'hello'",
            typ: "Atom",
        },
        Case {
            src: "\"ok\"",
            typ: "String",
        },
        Case {
            src: "\"ok\"",
            typ: "String",
        },
        Case {
            src: "1 2.0",
            typ: "Float",
        },
        Case {
            src: "[]",
            typ: "List(a)",
        },
        /* Assignments

        */
        Case {
            src: "x = 1 2",
            typ: "Int",
        },
        Case {
            src: "x = 1 x",
            typ: "Int",
        },
        Case {
            src: "x = 'ok' x",
            typ: "Atom",
        },
        Case {
            src: "x = 'ok' y = x y",
            typ: "Atom",
        },
        Case {
            src: "x = 'ok' y = x y",
            typ: "Atom",
        },
        /* Tuples

        */
        Case {
            src: "{1}",
            typ: "{Int}",
        },
        Case {
            src: "{1, 2.0}",
            typ: "{Int, Float}",
        },
        Case {
            src: "{1, 2.0, '3'}",
            typ: "{Int, Float, Atom}",
        },
        Case {
            src: "{1, 2.0, {'ok', 1}}",
            typ: "{Int, Float, {Atom, Int}}",
        },
        /* Funs

        */
        Case {
            src: "fn(x) { x }",
            typ: "fn(a) -> a",
        },
        Case {
            src: "fn(x) { x }",
            typ: "fn(a) -> a",
        },
        Case {
            src: "fn(x, y) { x }",
            typ: "fn(a, b) -> a",
        },
        Case {
            src: "fn(x, y) { [] }",
            typ: "fn(a, b) -> List(c)",
        },
        Case {
            src: "x = 1.0 'nope'",
            typ: "Atom",
        },
        Case {
            src: "id = fn(x) { x } id(1)",
            typ: "Int",
        },
        Case {
            src: "x = fn() { 1.0 } x()",
            typ: "Float",
        },
        // TODO: FIXME
        // Case {src:"fn(x) { x }('ok')", typ: "Atom"},
        Case {
            src: "fn() { 1 }",
            typ: "fn() -> Int",
        },
        Case {
            src: "fn() { 1.1 }",
            typ: "fn() -> Float",
        },
        Case {
            src: "fn(x) { 1.1 }",
            typ: "fn(a) -> Float",
        },
        Case {
            src: "fn(x) { x }",
            typ: "fn(a) -> a",
        },
        Case {
            src: "x = fn(x) { 1.1 } x",
            typ: "fn(a) -> Float",
        },
        Case {
            src: "fn(x, y, z) { 1 }",
            typ: "fn(a, b, c) -> Int",
        },
        Case {
            src: "fn(x) { y = x y }",
            typ: "fn(a) -> a",
        },
        Case {
            src: "fn(x) { {'ok', x} }",
            typ: "fn(a) -> {Atom, a}",
        },
        Case {
            src: "id = fn(x) { x } id(1)",
            typ: "Int",
        },
        Case {
            src: "const = fn(x) { fn(y) { x } } one = const(1) one('ok')",
            typ: "Int",
        },
        Case {
            src: "fn(f) { f(1) }",
            typ: "fn(fn(Int) -> a) -> a",
        },
        Case {
            src: "fn(f, x) { f(x) }",
            typ: "fn(fn(a) -> b, a) -> b",
        },
        Case {
            src: "fn(f) { fn(x) { f(x) } }",
            typ: "fn(fn(a) -> b) -> fn(a) -> b",
        },
        Case {
            src: "fn(f) { fn(x) { fn(y) { f(x, y) } } }",
            typ: "fn(fn(a, b) -> c) -> fn(a) -> fn(b) -> c",
        },
        /*
        Case {
            src: "fn(f) { fn(x, y) { ff = f(x) ff(y) } }",
            typ: "fn(fn(a) -> fn(b) -> c) -> fn(a, b) -> c",
        },
        Case {
            src: "fn(x) { fn(y) { x } }",
            typ: "fn(a) -> fn(b) -> a",
        },
        Case {
            src: "fn(f) { f() }",
            typ: "fn(fn() -> a) -> a",
        },
        Case {
            src: "fn(f, x) { f(f(x)) }",
            typ: "fn(fn(a) -> a, a) -> a",
        },
        Case {
            src: "fn(x) { y = fn(z) { z } y(y) }",
            typ: "fn(a) -> fn(b) -> b",
        },
        Case {
            src: "fn(x, y) { {x, y} }",
            typ: "fn(a, b) -> Tuple(a, b)",
        },
        Case {
            src: "fn(x) { {x, x} }",
            typ: "fn(a) -> Tuple(a, a)",
        },
        Case {
            src: "id = fnCase {src:a) { a } fn(x) { x(id) }",
            typ: "fnCase {src:fn(fn(a) -> a) -> b) -> b",
        },
        /* Operators

        */
        Case {
        src: "1 + 1",
        typ: "Int",
        },
        Case {
        src: "1 - 1",
        typ: "Int",
        },
        Case {
        src: "1 * 1",
        typ: "Int",
        },
        Case {
        src: "1 / 1",
        typ: "Int",
        },
        Case {
        src: "1.0 +. 1.0",
        typ: "Float",
        },
        Case {
        src: "1.0 -. 1.0",
        typ: "Float",
        },
        Case {
        src: "1.0 *. 1.0",
        typ: "Float",
        },
        Case {
        src: "1.0 /. 1.0",
        typ: "Float",
        },
        Case {
        src: "fn(a, b) { a + b }",
        typ: "fn(Int, Int) -> Int",
        },
        Case {
        src: "inc = fn(a) { a + 1 } 1 |> inc |> inc",
        typ: "Int",
        },
        /* Equality

        */
        Case {
        src: "1 == 1",
        typ: "Bool",
        },
        Case {
        src: "1.0 == 2.0",
        typ: "Bool",
        },
        Case {
        src: "'ok' == 'ko'",
        typ: "Bool",
        },
        Case {
        src: "{'ok', 1} == {'ko', 2}",
        typ: "Bool",
        },
        Case {
        src: "1 != 1",
        typ: "Bool",
        },
        Case {
        src: "1.0 != 2.0",
        typ: "Bool",
        },
        Case {
        src: "'ok' != 'ko'",
        typ: "Bool",
        },
        Case {
        src: "{'ok', 1} != {'ko', 2}",
        typ: "Bool",
        },
        Case {
        src: "x = 1 x == x",
        typ: "Bool",
        },
        Case {
        src: "id = fn(x) { x } id == id",
        typ: "Bool",
        },
        Case {
        src: "id1 = fn(x) { x } id2 = fn(x) { x } id1 == id2",
        typ: "Bool",
        },
        Case {
        src: "id = fn(x) { x } inc = fn(x) { x + 1 } id == inc",
        typ: "Bool",
        },
        /* Lists

        */
        Case {
        src: "[]",
        typ: "List(a)",
        },
        Case {
        src: "[1]",
        typ: "List(Int)",
        },
        Case {
        src: "[1, 2, 3]",
        typ: "List(Int)",
        },
        Case {
        src: "[[]]",
        typ: "List(List(a))",
        },
        Case {
        src: "[[1.0, 2.0]]",
        typ: "List(List(Float))",
        },
        Case {
        src: "[fn(x) { x }]",
        typ: "List(fn(a) -> a)",
        },
        Case {
        src: "[fn(x) { x + 1 }]",
        typ: "List(fn(Int) -> Int)",
        },
        Case {
        src: "[{[], []}]",
        typ: "List(Tuple(List(a), List(b)))",
        },
        Case {
        src: "[fn(x) { x }, fn(x) { x + 1 }]",
        typ: "List(fn(Int) -> Int)",
        },
        Case {
        src: "[fn(x) { x + 1 }, fn(x) { x }]",
        typ: "List(fn(Int) -> Int)",
        },
        Case {
        src: "[[], []]",
        typ: "List(List(a))",
        },
        Case {
        src: "[[], ['ok']]",
        typ: "List(List(Atom))",
        },
        Case {
        src: "1 :: 2 :: []",
        typ: "List(Int)",
        },
        Case {
        src: "fn(x) { x } :: []",
        typ: "List(fn(a) -> a)",
        },
        Case {
        src: "f = fn(x) { x } [f, f]",
        typ: "List(fn(a) -> a)",
        },
        Case {
        src: "x = 1 :: [] 2 :: x",
        typ: "List(Int)",
        },
        /* Records

        */
        Case {
        src: "{}",
        typ: "{}",
        },
        Case {
        src: "{a = 1}",
        typ: "{a = Int}",
        },
        Case {
        src: "{a = 1, b = 2}",
        typ: "{a = Int, b = Int}",
        },
        Case {
        src: "{a = 1, b = 2.0, c = -1}",
        typ: "{a = Int, b = Float, c = Int}",
        },
        Case {
        src: "{a = {a = 'ok'}}",
        typ: "{a = {a = Atom}}",
        },
        Case {
        src: "{} == {}",
        typ: "Bool",
        },
        Case {
        src: "{a = 1} == {a = 2}",
        typ: "Bool",
        },
        Case {
        src: "{a = 1, b = 1} == {a = 1, b = 1}",
        typ: "Bool",
        },
        Case {
        src: "{a = fn(x) { x }} == {a = fn(a) { a }}",
        typ: "Bool",
        },
        Case {
        src: "{b = 1, a = 1} == {a = 1, b = 1}",
        typ: "Bool",
        },
        /* Record select

        */
        Case {
        src: "{a = 1, b = 2.0}.b",
        typ: "Float",
        },
        Case {
        src: "r = {a = 1, b = 2} r.a",
        typ: "Int",
        },
        Case {
        src: "r = {a = 1, b = 2} r.a + r.b",
        typ: "Int",
        },
        Case {
        src: "fn(x) { x.t }",
        typ: "fn({a | t = b}) -> b",
        },
        Case {
        src: "f = fn(x) { x } r = {a = f} r.a",
        typ: "fn(a) -> a",
        },
        Case {
        src: "r = {a = fn(x) { x }} r.a",
        typ: "fn(a) -> a",
        },
        Case {
        src: "r = {a = fnCase {src:x) { x }, b = fn(x) { x }} [r.a, r.b]",
        typ: "ListCase {src:fn(a) -> a)",
        },
        Case {
        src: "f = fn(x) { x.t } f({ t = 1 })",
        typ: "Int",
        },
        Case {
        src: "f = fn(x) { x.t(1) } f({t = fn(x) { x + 1 }})",
        typ: "Int",
        },
        Case {
        src: "{a = 1, b = 2.0}.a",
        typ: "Int",
        },
         */
    ];

    for Case { src, typ } in cases.into_iter() {
        let ast = grammar::ExprParser::new().parse(src).expect("syntax error");
        let result = infer(ast, 1, &mut Env::default()).expect("should successfully infer");
        assert_eq!(
            (src, typ.to_string()),
            (
                src,
                result
                    .typ()
                    .to_gleam_doc(&mut hashmap![], &mut 0)
                    .format(80)
            )
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
fn generalise(t: Type, ctx_level: usize) -> Type {
    match t {
        Type::Var { typ } => match &*typ.borrow() {
            TypeVar::Unbound { id, level } => {
                let id = *id;
                let var = if *level > ctx_level {
                    TypeVar::Generic { id }
                } else {
                    let level = *level;
                    TypeVar::Unbound { id, level }
                };

                Type::Var {
                    typ: Rc::new(RefCell::new(var)),
                }
            }

            TypeVar::Link { typ } => generalise((**typ).clone(), ctx_level),

            TypeVar::Generic { .. } => unimplemented!(),
        },

        Type::App { .. } => unimplemented!(),

        Type::Fun { args, retrn } => {
            let args = args.into_iter().map(|t| generalise(t, ctx_level)).collect();
            let retrn = generalise(*retrn, ctx_level);
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

pub fn record_nil() -> Type {
    Type::Record { row: Row::Nil }
}
