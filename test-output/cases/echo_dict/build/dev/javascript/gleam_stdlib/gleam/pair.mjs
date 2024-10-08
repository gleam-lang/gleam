export function first(pair) {
  let a = pair[0];
  return a;
}

export function second(pair) {
  let a = pair[1];
  return a;
}

export function swap(pair) {
  let a = pair[0];
  let b = pair[1];
  return [b, a];
}

export function map_first(pair, fun) {
  let a = pair[0];
  let b = pair[1];
  return [fun(a), b];
}

export function map_second(pair, fun) {
  let a = pair[0];
  let b = pair[1];
  return [a, fun(b)];
}

export function new$(first, second) {
  return [first, second];
}
