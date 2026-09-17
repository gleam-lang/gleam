// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

mod common;

#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

fn report(label: &str, modules: &[String]) {
    let profiler = dhat::Profiler::builder().testing().build();
    common::lex_all(modules);
    let stats = dhat::HeapStats::get();
    drop(profiler);

    let source_bytes: usize = modules.iter().map(String::len).sum();
    println!("[{label}] source bytes:          {source_bytes}");
    println!("[{label}] total allocations:     {}", stats.total_blocks);
    println!("[{label}] total bytes allocated: {}", stats.total_bytes);
    println!("[{label}] peak bytes live:       {}", stats.max_bytes);
    println!();
}

fn main() {
    for package in common::PACKAGES {
        report(package, &common::package_modules(package));
    }
}
