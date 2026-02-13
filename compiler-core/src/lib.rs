#![warn(
    clippy::all,
    clippy::dbg_macro,
    clippy::todo,
    clippy::mem_forget,
    // TODO: enable once the false positive bug is solved
    // clippy::use_self,
    clippy::filter_map_next,
    clippy::needless_continue,
    clippy::needless_borrow,
    clippy::match_wildcard_for_single_variants,
    clippy::imprecise_flops,
    clippy::suboptimal_flops,
    clippy::lossy_float_literal,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::fn_params_excessive_bools,
    clippy::inefficient_to_string,
    clippy::linkedlist,
    clippy::macro_use_imports,
    clippy::option_option,
    clippy::verbose_file_reads,
    clippy::unnested_or_patterns,
    rust_2018_idioms,
    missing_debug_implementations,
    missing_copy_implementations,
    trivial_casts,
    trivial_numeric_casts,
    nonstandard_style,
    unexpected_cfgs,
    unused_import_braces,
    unused_qualifications,
    clippy::wildcard_enum_match_arm
)]
#![deny(
    clippy::await_holding_lock,
    clippy::disallowed_methods,
    clippy::if_let_mutex,
    clippy::indexing_slicing,
    clippy::mem_forget,
    clippy::ok_expect,
    clippy::unimplemented,
    clippy::unwrap_used,
    unsafe_code,
    unstable_features,
    unused_results
)]
#![allow(
    clippy::assign_op_pattern,
    clippy::to_string_trait_impl,
    clippy::match_single_binding,
    clippy::match_like_matches_macro,
    clippy::inconsistent_struct_constructor,
    clippy::len_without_is_empty,
    // TODO: fix
    clippy::arc_with_non_send_sync,
)]

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

pub mod analyse;
pub mod ast;
pub mod bit_array;
pub mod build;
pub mod codegen;
pub mod config;
pub mod dependency;
pub mod diagnostic;
pub mod docs;
pub mod encryption;
pub mod erlang;
pub mod error;
pub mod fix;
pub mod format;
pub mod hex;
pub mod io;
pub mod javascript;
pub mod line_numbers;
pub mod manifest;
pub mod metadata;
pub mod package_interface;
pub mod parse;
pub mod paths;
pub mod pretty;
pub mod requirement;
pub mod strings;
pub mod type_;
pub mod uid;
pub mod version;
pub mod warning;

pub(crate) mod ast_folder;
mod call_graph;
mod dep_tree;
pub(crate) mod derivation_tree;
pub mod exhaustiveness;
pub(crate) mod graph;
pub(crate) mod inline;
pub mod reference;

pub use error::{Error, Result};
pub use warning::Warning;

const GLEAM_CORE_PACKAGE_NAME: &str = "";
pub const STDLIB_PACKAGE_NAME: &str = "gleam_stdlib";
