/// A wrapper structure that contains an error with some form of data.
/// This will be useful to transmit errors through the pipeline while also providing
/// with some data, e.g for LSP implementations.
///
/// Since most of the time the compiler will want to report multiple errors instead of just one,
/// it uses a Vec to store the errors.
#[derive(Debug)]
pub struct FilledResult<T, E> {
    data: T,
    errors: Vec<E>,
}
impl<T, E> FilledResult<T, E> {
    pub const fn ok(data: T) -> Self {
        Self {
            data,
            errors: Vec::new(),
        }
    }

    pub fn err(error: E, data: T) -> Self {
        Self {
            data,
            errors: vec![error],
        }
    }

    pub fn with_check(mut self, res: Result<(), E>) -> Self {
        if let Err(e) = res {
            self.errors.push(e);
        }
        self
    }

    pub fn map<U>(self, map_fn: impl FnOnce(T) -> U) -> FilledResult<U, E> {
        FilledResult {
            data: map_fn(self.data),
            errors: self.errors,
        }
    }

    pub fn collapse_into_result(self) -> Result<T, Vec<E>> {
        if self.errors.is_empty() {
            Ok(self.data)
        } else {
            Err(self.errors)
        }
    }

    /// Useful function for retrieving a context from this result that already has the errors from
    /// this result loaded.
    pub fn into_context(self) -> (FilledResultContext<E>, T) {
        let ctx = FilledResultContext {
            errors: self.errors,
        };
        (ctx, self.data)
    }

    /// A bind-like operator for chaining operations directly with FilledResults. This method is
    /// meant to be used sparsely, the 'imperative' methods with `FilledResultContext` are preferred.
    pub fn join_with<U>(self, f: impl FnOnce(T) -> FilledResult<U, E>) -> FilledResult<U, E> {
        let mut res = f(self.data);
        // make sure that errors are forwarded in the correct order.
        let errors = self
            .errors
            .into_iter()
            .chain(std::mem::take(&mut res.errors))
            .collect();
        res.errors = errors;
        res
    }
}

/// The context to manage `FilledResult`s. It can convert to and from them, and absorb their
/// errors.
#[derive(Debug)]
pub struct FilledResultContext<E> {
    errors: Vec<E>,
}

impl<E> FilledResultContext<E> {
    pub const fn new() -> Self {
        Self { errors: Vec::new() }
    }

    /// Absorbs the errors from another `FilledResult`.
    pub fn slurp_filled<U>(&mut self, res: FilledResult<U, E>) -> U {
        self.errors.extend(res.errors);
        res.data
    }

    /// Absorbs the errors from another `FilledResult`, using the mapping function to convert
    /// between iterators.
    pub fn slurp_filled_with<U, E2, F, I>(&mut self, res: FilledResult<U, E2>, iterator_fn: F) -> U
    where
        I: IntoIterator<Item = E>,
        F: FnOnce(<Vec<E2> as IntoIterator>::IntoIter) -> I,
    {
        self.errors.extend(iterator_fn(res.errors.into_iter()));
        res.data
    }

    /// Absorbs the error from a `Result`, while ignoring the value
    pub fn just_slurp_result(&mut self, res: Result<(), E>) {
        if let Err(e) = res {
            self.errors.push(e);
        }
    }

    /// Absorbs the error from a Result, turning it into an Option.
    pub fn slurp_result<U>(&mut self, res: Result<U, E>) -> Option<U> {
        match res {
            Ok(v) => Some(v),
            Err(e) => {
                self.errors.push(e);
                None
            }
        }
    }

    pub fn register_error(&mut self, error: E) {
        self.errors.push(error);
    }

    pub fn finish<T>(self, data: T) -> FilledResult<T, E> {
        FilledResult {
            data,
            errors: self.errors,
        }
    }
}
