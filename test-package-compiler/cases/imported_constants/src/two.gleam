import one.{A, B}

pub const qualified_const_a = one.A

pub fn qualified_fn_a() {
  qualified_const_a
}

pub const qualified_const_b = one.B(one.A)

pub fn qualified_fn_b() {
  qualified_const_b
}

pub const unqualified_const_a = A

pub fn unqualified_fn_a() {
  unqualified_const_a
}

pub const unqualified_const_b = B(A)

pub fn unqualified_fn_b() {
  unqualified_const_b
}
