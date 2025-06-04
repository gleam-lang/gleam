import one.{
  escaped_thing, escaped_thing as xescaped_thing, thing, thing as xthing,
}
import three as aliased

// Assigning a function to constants

const const_qualified = one.thing

const const_qualified_aliased = aliased.thing

const const_unqualified = thing

const const_unqualified_aliased = xthing

const escaped_const_qualified = one.escaped_thing

const escaped_const_qualified_aliased = aliased.escaped_thing

const escaped_const_unqualified = escaped_thing

const escaped_const_unqualified_aliased = xescaped_thing

pub fn the_consts() {
  let _ = const_qualified
  let _ = const_qualified_aliased
  let _ = const_unqualified
  let _ = const_unqualified_aliased
  let _ = escaped_const_qualified
  let _ = escaped_const_qualified_aliased
  let _ = escaped_const_unqualified
  let _ = escaped_const_unqualified_aliased
  const_qualified()
  const_qualified_aliased()
  const_unqualified()
  const_unqualified_aliased()
  escaped_const_qualified()
  escaped_const_qualified_aliased()
  escaped_const_unqualified()
  escaped_const_unqualified_aliased()
}

// Referencing in a function

pub fn fn_reference_qualified() {
  one.thing
}

pub fn fn_reference_qualified_aliased() {
  aliased.thing
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
  aliased.thing()
}

pub fn fn_call_unqualified() {
  thing()
}

pub fn fn_call_unqualified_aliased() {
  xthing()
}

// Referencing a function as an argument

pub fn argument_reference_qualified() {
  x(one.escaped_thing)
}

pub fn argument_reference_qualified_aliased() {
  x(aliased.escaped_thing)
}

pub fn argument_reference_unqualified() {
  x(escaped_thing)
}

pub fn argument_reference_unqualified_aliased() {
  x(xescaped_thing)
}

fn x(_) {
  Nil
}
