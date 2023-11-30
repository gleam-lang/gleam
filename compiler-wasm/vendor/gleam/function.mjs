export function compose(fun1, fun2) {
  return (a) => { return fun2(fun1(a)); };
}

export function curry2(fun) {
  return (a) => { return (b) => { return fun(a, b); }; };
}

export function curry3(fun) {
  return (a) => { return (b) => { return (c) => { return fun(a, b, c); }; }; };
}

export function curry4(fun) {
  return (a) => {
    return (b) => {
      return (c) => { return (d) => { return fun(a, b, c, d); }; };
    };
  };
}

export function curry5(fun) {
  return (a) => {
    return (b) => {
      return (c) => {
        return (d) => { return (e) => { return fun(a, b, c, d, e); }; };
      };
    };
  };
}

export function curry6(fun) {
  return (a) => {
    return (b) => {
      return (c) => {
        return (d) => {
          return (e) => { return (f) => { return fun(a, b, c, d, e, f); }; };
        };
      };
    };
  };
}

export function flip(fun) {
  return (b, a) => { return fun(a, b); };
}

export function identity(x) {
  return x;
}

export function constant(value) {
  return (_) => { return value; };
}

export function tap(arg, effect) {
  effect(arg);
  return arg;
}

export function apply1(fun, arg1) {
  return fun(arg1);
}

export function apply2(fun, arg1, arg2) {
  return fun(arg1, arg2);
}

export function apply3(fun, arg1, arg2, arg3) {
  return fun(arg1, arg2, arg3);
}
