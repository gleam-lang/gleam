import other

pub fn when() {
  {
    a1 = fn(x) { fn(x) { fn(x) { fn(x) { x } } } },
    a2 = fn(x) {
      {
        a1 = fn(x) { fn(x) { fn(x) { fn(x) { x } } } },
        a2 = fn(x) { x },
        a3 = fn(x) { x },
        a4 = fn(x) { x },
        a5 = fn(x) { x },
        a6 = fn(x) { x },
        a7 = fn(x) { x },
        a9 = fn(x) { x },
        a8 = fn(x) { x },
      }
    },
    a3 = fn(x) { x },
    a4 = fn(x) { x },
    a5 = fn(x) { x },
    a6 = fn(x) { x },
    a7 = fn(x) { x },
    a9 = fn(x) { x },
    a8 = fn(x) { x },
  } + 1
  other + 1
}
