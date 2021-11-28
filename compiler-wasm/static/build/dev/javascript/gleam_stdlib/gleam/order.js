import { CustomType, throwError, isEqual } from "../gleam.js";

export class Lt extends CustomType {}

export class Eq extends CustomType {}

export class Gt extends CustomType {}

export function reverse(order) {
  if (order instanceof Lt) {
    return new Gt();
  } else if (order instanceof Eq) {
    return new Eq();
  } else if (order instanceof Gt) {
    return new Lt();
  } else {
    throwError(
      "case_no_match",
      "gleam/order",
      30,
      "reverse",
      "No case clause matched",
      { values: [order] }
    );
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
    throwError(
      "case_no_match",
      "gleam/order",
      51,
      "to_int",
      "No case clause matched",
      { values: [order] }
    );
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
