use crate::ast::{TypeAst, TypeAstConstructor, TypeAstFn, TypeAstHole, TypeAstTuple, TypeAstVar};

/// A convenience trait for walking AST, potentially mutating it.

pub trait TypeAstFolder {
    /// Visit a node and potentially replace it with another node using the
    /// `fold_*` methods. Afterwards, the `walk` method is called on the new
    /// node to continue traversing.
    ///
    /// You probably don't want to override this method.
    fn fold(&mut self, t: TypeAst) -> TypeAst {
        let t = self.update(t);
        self.walk(t)
    }

    /// You probably don't want to override this method.
    fn update(&mut self, t: TypeAst) -> TypeAst {
        match t {
            TypeAst::Constructor(c) => self.fold_constructor(c),
            TypeAst::Fn(f) => self.fold_fn(f),
            TypeAst::Var(v) => self.fold_var(v),
            TypeAst::Tuple(t) => self.fold_tuple(t),
            TypeAst::Hole(h) => self.fold_hole(h),
        }
    }

    /// You probably don't want to override this method.
    fn walk(&mut self, t: TypeAst) -> TypeAst {
        match t {
            TypeAst::Constructor(mut c) => {
                c.arguments = self.fold_all(c.arguments);
                TypeAst::Constructor(c)
            }

            TypeAst::Fn(mut f) => {
                f.arguments = self.fold_all(f.arguments);
                f.return_ = Box::new(self.fold(*f.return_));
                TypeAst::Fn(f)
            }

            TypeAst::Tuple(mut t) => {
                t.elems = self.fold_all(t.elems);
                TypeAst::Tuple(t)
            }

            TypeAst::Var(_) | TypeAst::Hole(_) => t,
        }
    }

    /// You probably don't want to override this method.
    fn fold_all(&mut self, ts: Vec<TypeAst>) -> Vec<TypeAst> {
        ts.into_iter().map(|t| self.fold(t)).collect()
    }

    fn fold_constructor(&mut self, constructor: TypeAstConstructor) -> TypeAst {
        TypeAst::Constructor(constructor)
    }

    fn fold_fn(&mut self, function: TypeAstFn) -> TypeAst {
        TypeAst::Fn(function)
    }

    fn fold_tuple(&mut self, tuple: TypeAstTuple) -> TypeAst {
        TypeAst::Tuple(tuple)
    }

    fn fold_var(&mut self, var: TypeAstVar) -> TypeAst {
        TypeAst::Var(var)
    }

    fn fold_hole(&mut self, hole: TypeAstHole) -> TypeAst {
        TypeAst::Hole(hole)
    }
}
