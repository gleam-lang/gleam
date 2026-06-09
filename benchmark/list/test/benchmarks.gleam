// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

import gleamy/bench

import gleam/io
import gleam/list

pub fn print_results(results: List(bench.BenchResults)) {
  results
  |> list.map(fn(result) {
    result
    |> bench.table([bench.IPS, bench.Min, bench.P(99)])
    |> io.println()
  })
}
