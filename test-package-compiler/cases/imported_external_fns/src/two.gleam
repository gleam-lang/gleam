import one.{thing, thing as xthing}
import one as xone

// Assigning a function to constants

const const_qualified = one.thing

const const_qualified_aliased = xone.thing

const const_unqualified = thing

const const_unqualified_aliased = xthing

pub fn the_consts() {
  const_qualified
  const_qualified_aliased
  const_unqualified
  const_unqualified_aliased
  const_qualified()
  const_qualified_aliased()
  const_unqualified()
  const_unqualified_aliased()
}

// Referencing in a function

pub fn fn_reference_qualified() {
  one.thing
}

pub fn fn_reference_qualified_aliased() {
  xone.thing
}

pub fn fn_reference_unqualified() {
  thing
}

pub fn fn_reference_unqualified_aliased() {
  xthing
}

// Calling the function

pub fn fn_call_qualified() {
  one.thing()
}

pub fn fn_call_qualified_aliased() {
  xone.thing()
}

pub fn fn_call_unqualified() {
  thing()
}

pub fn fn_call_unqualified_aliased() {
  xthing()
}
