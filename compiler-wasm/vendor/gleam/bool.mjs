import { makeError } from "../gleam.mjs";
import * as $order from "../gleam/order.mjs";

export function and(a, b) {
  return a && b;
}

export function or(a, b) {
  return a || b;
}

export function negate(bool) {
  if (bool) {
    return false;
  } else if (!bool) {
    return true;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/bool",
      80,
      "negate",
      "No case clause matched",
      { values: [bool] }
    )
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
    throw makeError(
      "case_no_match",
      "gleam/bool",
      111,
      "nor",
      "No case clause matched",
      { values: [a, b] }
    )
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
    throw makeError(
      "case_no_match",
      "gleam/bool",
      144,
      "nand",
      "No case clause matched",
      { values: [a, b] }
    )
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
    throw makeError(
      "case_no_match",
      "gleam/bool",
      177,
      "exclusive_or",
      "No case clause matched",
      { values: [a, b] }
    )
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
    throw makeError(
      "case_no_match",
      "gleam/bool",
      210,
      "exclusive_nor",
      "No case clause matched",
      { values: [a, b] }
    )
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
    throw makeError(
      "case_no_match",
      "gleam/bool",
      229,
      "compare",
      "No case clause matched",
      { values: [a, b] }
    )
  }
}

export function max(a, b) {
  if (a) {
    return true;
  } else if (!a) {
    return b;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/bool",
      257,
      "max",
      "No case clause matched",
      { values: [a] }
    )
  }
}

export function min(a, b) {
  if (!a) {
    return false;
  } else if (a) {
    return b;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/bool",
      281,
      "min",
      "No case clause matched",
      { values: [a] }
    )
  }
}

export function to_int(bool) {
  if (!bool) {
    return 0;
  } else if (bool) {
    return 1;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/bool",
      300,
      "to_int",
      "No case clause matched",
      { values: [bool] }
    )
  }
}

export function to_string(bool) {
  if (!bool) {
    return "False";
  } else if (bool) {
    return "True";
  } else {
    throw makeError(
      "case_no_match",
      "gleam/bool",
      321,
      "to_string",
      "No case clause matched",
      { values: [bool] }
    )
  }
}

export function guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence;
  } else if (!requirement) {
    return alternative();
  } else {
    throw makeError(
      "case_no_match",
      "gleam/bool",
      384,
      "guard",
      "No case clause matched",
      { values: [requirement] }
    )
  }
}

export function lazy_guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence();
  } else if (!requirement) {
    return alternative();
  } else {
    throw makeError(
      "case_no_match",
      "gleam/bool",
      424,
      "lazy_guard",
      "No case clause matched",
      { values: [requirement] }
    )
  }
}
