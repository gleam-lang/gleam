import { throwError } from "../gleam.js";
import * as $order from "../gleam/order.js";

export function negate(bool) {
  if (bool) {
    return false;
  } else if (!bool) {
    return true;
  } else {
    throwError(
      "case_no_match",
      "gleam/bool",
      24,
      "negate",
      "No case clause matched",
      { values: [bool] }
    );
  }
}

export function nor(a, b) {
  if (!a && !b) {
    return true;
  } else if (!a && b) {
    return false;
  } else if (a && !b) {
    return false;
  } else if (a && b) {
    return false;
  } else {
    throwError(
      "case_no_match",
      "gleam/bool",
      47,
      "nor",
      "No case clause matched",
      { values: [a, b] }
    );
  }
}

export function nand(a, b) {
  if (!a && !b) {
    return true;
  } else if (!a && b) {
    return true;
  } else if (a && !b) {
    return true;
  } else if (a && b) {
    return false;
  } else {
    throwError(
      "case_no_match",
      "gleam/bool",
      72,
      "nand",
      "No case clause matched",
      { values: [a, b] }
    );
  }
}

export function exclusive_or(a, b) {
  if (!a && !b) {
    return false;
  } else if (!a && b) {
    return true;
  } else if (a && !b) {
    return true;
  } else if (a && b) {
    return false;
  } else {
    throwError(
      "case_no_match",
      "gleam/bool",
      97,
      "exclusive_or",
      "No case clause matched",
      { values: [a, b] }
    );
  }
}

export function exclusive_nor(a, b) {
  if (!a && !b) {
    return true;
  } else if (!a && b) {
    return false;
  } else if (a && !b) {
    return false;
  } else if (a && b) {
    return true;
  } else {
    throwError(
      "case_no_match",
      "gleam/bool",
      122,
      "exclusive_nor",
      "No case clause matched",
      { values: [a, b] }
    );
  }
}

export function compare(a, b) {
  if (a && b) {
    return new $order.Eq();
  } else if (a && !b) {
    return new $order.Gt();
  } else if (!a && !b) {
    return new $order.Eq();
  } else if (!a && b) {
    return new $order.Lt();
  } else {
    throwError(
      "case_no_match",
      "gleam/bool",
      139,
      "compare",
      "No case clause matched",
      { values: [a, b] }
    );
  }
}

export function max(a, b) {
  if (a) {
    return true;
  } else if (!a) {
    return b;
  } else {
    throwError(
      "case_no_match",
      "gleam/bool",
      161,
      "max",
      "No case clause matched",
      { values: [a] }
    );
  }
}

export function min(a, b) {
  if (!a) {
    return false;
  } else if (a) {
    return b;
  } else {
    throwError(
      "case_no_match",
      "gleam/bool",
      181,
      "min",
      "No case clause matched",
      { values: [a] }
    );
  }
}

export function to_int(bool) {
  if (!bool) {
    return 0;
  } else if (bool) {
    return 1;
  } else {
    throwError(
      "case_no_match",
      "gleam/bool",
      198,
      "to_int",
      "No case clause matched",
      { values: [bool] }
    );
  }
}
