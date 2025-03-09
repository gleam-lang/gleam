#![allow(clippy::enum_variant_names)]

pub mod package {
    include!(concat!(env!("OUT_DIR"), "/package.rs"));
}

pub mod signed {
    include!(concat!(env!("OUT_DIR"), "/signed.rs"));
}

pub mod versions {
    include!(concat!(env!("OUT_DIR"), "/versions.rs"));
}
