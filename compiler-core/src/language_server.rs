mod compiler;
mod feedback;
mod files;
mod progress;

use std::any::Any;

pub use compiler::LspProjectCompiler;
pub use feedback::{Feedback, FeedbackBookKeeper};
pub use files::FileSystemProxy;
pub use progress::ProgressReporter;

#[derive(Debug)]
pub struct LockGuard(pub Box<dyn Any>);

pub trait Locker {
    fn lock_for_build(&self) -> LockGuard;
}
