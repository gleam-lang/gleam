pub type Setoid(t, r) = module { r |
  fn equals(t, t) -> Bool
};

pub type Ord(t, r) = module { r |
  fn less_than_equal(t, t) -> Bool
};

pub type Semigroup(t, r) = module { r |
  fn concat(t, t) -> t
};

pub type Monoid(t, r) = Semigroup(t, module { r |
  fn empty() -> t
});

pub type Group(t, r) = Monoid(t, module { r |
  fn invert(t) -> t
});

pub type Semigroupoid(t, r) = module { r |
  fn compose(t(i, j), t(j, k)) -> t(i, k)
};

pub type Category(t, r) = Semigroupoid(t, module { r |
  fn id() -> t(i, k)
});

pub type Filterable(t, r) = module { r |
  fn filter(t(a), fn(a) -> Bool) -> t(a)
};

pub type Functor(t, r) = module { r |
  fn map(t(a), fn(a) -> b) -> t(b)
};

pub type Contravariant(t, r) = module { r |
  fn contramap(t(b), fn(a) -> b) -> t(a)
};

pub type Apply(t, r) = Functor(t, module { r |
  fn ap(t(fn(a) -> b), t(a)) -> t(b)
});

pub type Apply(t, r) = module { r |
  fn ap(t(fn(a) -> b), t(a)) -> t(b)
};

pub type Applicative(t, r) = Apply(t, module { r |
  fn of(a) -> t(a)
});

pub type Alt(t, r) = Functor(t, module { r |
  fn alt(t(a), t(a)) -> t(a)
});

pub type Plus(t, r) = Alt(t, module { r |
  fn zero() -> t(a)
});

pub type Alternative(t, r) = Applicative(t, Plus(t, r));

pub type Chain(t, r) = Apply(t, module { r |
  fn chain(t(a), fn(a) -> t(b)) -> t(b)
});

pub type Monad(t, r) = Applicative(t, Chain(t, r));

pub type Foldable(t, r) = module { r |
  fn reduce(t(b), a, fn(a) -> b) -> b
};

pub type Extend(t, r) = Functor(t, module { r |
  fn extend(t(a), fn(t(a)) -> a) -> t(b)
});

pub type Comonad(t, r) = Extend(t, module { r |
  fn extract(t(a)) -> a
});

pub type Traversable(t, r) = module { r |
  fn traverse(t(a), Applicative(u), u(b)) -> u(t(b))
};
