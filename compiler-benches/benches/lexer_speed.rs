// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

mod common;

use std::time::Duration;

use criterion::{Criterion, criterion_group, criterion_main};

fn bench_lexer(c: &mut Criterion) {
    for package in common::PACKAGES {
        let modules = common::package_modules(package);
        let _ = c.bench_function(package, |b| b.iter(|| common::lex_all(&modules)));
    }
}

criterion_group! {
    name = benches;
    config = Criterion::default().measurement_time(Duration::from_secs(10));
    targets = bench_lexer
}
criterion_main!(benches);
