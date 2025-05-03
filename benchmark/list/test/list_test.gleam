import benchmarks.{print_results}
import gleam/list
import gleamy/bench

pub fn main() {
  print_results([bench_odd_nums_between(), bench_count_ones(), bench_slice()])
}

fn bench_odd_nums_between() -> bench.BenchResults {
  let odd_nums_between = fn(args) {
    let #(start, end) = args
    odd_nums_between(start, end, [])
  }

  bench.run(
    [
      bench.Input("[0, 100)", #(0, 100)),
      bench.Input("[0, 1000)", #(0, 1000)),
      bench.Input("[0, 10_000)", #(0, 10_000)),
      bench.Input("[0, 100_000)", #(0, 100_000)),
      bench.Input("[0, 1_000_000)", #(0, 1_000_000)),
    ],
    [bench.Function("odd_nums_between", odd_nums_between)],
    [bench.Duration(1000), bench.Warmup(100)],
  )
}

/// Returns a list of numbers from `start` up to, but not including, `end`.
fn odd_nums_between(start: Int, end: Int, acc: List(Int)) -> List(Int) {
  case start {
    start if start >= end -> list.reverse(acc)
    _ -> {
      let acc = case start {
        n if n % 2 == 1 -> [n, ..acc]
        _ -> acc
      }
      odd_nums_between(start + 1, end, acc)
    }
  }
}

fn bench_count_ones() -> bench.BenchResults {
  let create_list = fn(size) {
    list.range(0, size)
    |> list.map(fn(n) {
      case n {
        _ if n % 3 == 0 -> 1
        _ -> 0
      }
    })
  }

  let count_ones = fn(list) { count_ones(list, 0) }

  bench.run(
    [
      bench.Input("100 numbers", create_list(100)),
      bench.Input("1000 numbers", create_list(1000)),
      bench.Input("10_000 numbers", create_list(10_000)),
      bench.Input("100_000 numbers", create_list(100_000)),
      bench.Input("1_000_000 numbers", create_list(1_000_000)),
    ],
    [bench.Function("count_ones", count_ones)],
    [bench.Duration(1000), bench.Warmup(100)],
  )
}

/// Counts the number of ones in a list.
fn count_ones(list: List(Int), count: Int) -> Int {
  case list {
    [] -> count
    [1, ..tail] -> count_ones(tail, count + 1)
    [_, ..tail] -> count_ones(tail, count)
  }
}

fn bench_slice() -> bench.BenchResults {
  let list = list.range(0, 1_000_000)

  let slice = fn(args) {
    let #(list, start, end) = args
    slice(list, start, end, [])
  }

  bench.run(
    [
      bench.Input("[0, 1000)", #(list, 0, 1000)),
      bench.Input("[0, 10_000)", #(list, 0, 10_000)),
      bench.Input("[0, 100_000)", #(list, 0, 100_000)),
      bench.Input("[999_000, 1_000_000)", #(list, 999_000, 1_000_000)),
      bench.Input("[990_000, 1_000_000)", #(list, 990_000, 1_000_000)),
      bench.Input("[900_000, 1_000_000)", #(list, 900_000, 1_000_000)),
      bench.Input("[0, 1_000_000)", #(list, 0, 1_000_000)),
    ],
    [bench.Function("slice", slice)],
    [bench.Duration(1000), bench.Warmup(100)],
  )
}

/// Slices a list.
fn slice(list: List(Int), start: Int, end: Int, acc: List(Int)) -> List(Int) {
  case list {
    // If the first element is before the slice, skip it.
    [_, ..tail] if start > 0 -> {
      slice(tail, start - 1, end - 1, acc)
    }
    // If the first element is within the slice, add it to the accumulator.
    [head, ..tail] if end > 0 -> {
      slice(tail, start - 1, end - 1, [head, ..acc])
    }
    // Otherwise, return the accumulator.
    _ -> list.reverse(acc)
  }
}
