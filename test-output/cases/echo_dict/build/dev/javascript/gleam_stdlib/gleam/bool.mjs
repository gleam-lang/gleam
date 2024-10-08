import * as $order from "../gleam/order.mjs";

export function and(a, b) {
  return a && b;
}

export function or(a, b) {
  return a || b;
}

export function negate(bool) {
  return !bool;
}

export function nor(a, b) {
  return !(a || b);
}

export function nand(a, b) {
  return !(a && b);
}

export function exclusive_or(a, b) {
  return a !== b;
}

export function exclusive_nor(a, b) {
  return a === b;
}

export function compare(a, b) {
  if (a && b) {
    return new $order.Eq();
  } else if (a && !b) {
    return new $order.Gt();
  } else if (!a && !b) {
    return new $order.Eq();
  } else {
    return new $order.Lt();
  }
}

export function to_int(bool) {
  if (!bool) {
    return 0;
  } else {
    return 1;
  }
}

export function to_string(bool) {
  if (!bool) {
    return "False";
  } else {
    return "True";
  }
}

export function guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence;
  } else {
    return alternative();
  }
}

export function lazy_guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence();
  } else {
    return alternative();
  }
}
