/// An error that occurred while reading a `.env` file.
pub type Error {
  /// An IO error.
  Io(message: String)
}
