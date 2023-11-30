import { CustomType as $CustomType, makeError, isEqual } from "../gleam.mjs";

export class Lt extends $CustomType {}

export class Eq extends $CustomType {}

export class Gt extends $CustomType {}

export function negate(order) {
  if (order instanceof Lt) {
    return new Gt();
  } else if (order instanceof Eq) {
    return new Eq();
  } else if (order instanceof Gt) {
    return new Lt();
  } else {
    throw makeError(
      "case_no_match",
      "gleam/order",
      36,
      "negate",
      "No case clause matched",
      { values: [order] }
    )
  }
}

export function to_int(order) {
  if (order instanceof Lt) {
    return -1;
  } else if (order instanceof Eq) {
    return 0;
  } else if (order instanceof Gt) {
    return 1;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/order",
      63,
      "to_int",
      "No case clause matched",
      { values: [order] }
    )
  }
}

export function compare(a, b) {
  if (isEqual(a, b)) {
    let x = a;
    let y = b;
    return new Eq();
  } else if (a instanceof Lt) {
    return new Lt();
  } else if (a instanceof Eq && b instanceof Gt) {
    return new Lt();
  } else {
    return new Gt();
  }
}

export function max(a, b) {
  if (a instanceof Gt) {
    return new Gt();
  } else if (a instanceof Eq && b instanceof Lt) {
    return new Eq();
  } else {
    return b;
  }
}

export function min(a, b) {
  if (a instanceof Lt) {
    return new Lt();
  } else if (a instanceof Eq && b instanceof Gt) {
    return new Eq();
  } else {
    return b;
  }
}

export function reverse(orderer) {
  return (a, b) => { return orderer(b, a); };
}
