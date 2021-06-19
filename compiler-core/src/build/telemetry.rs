use std::fmt::Debug;

pub trait Telemetry: Debug {
    fn compiling_package(&self, name: &str);
}
