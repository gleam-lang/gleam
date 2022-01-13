use std::fmt::Debug;

use crate::Warning;

pub trait Telemetry: Debug {
    fn compiling_package(&self, name: &str);
    fn checking_package(&self, name: &str);
    fn warning(&self, warning: &Warning);
}
