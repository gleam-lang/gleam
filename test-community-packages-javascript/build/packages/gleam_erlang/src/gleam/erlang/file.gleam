//// Working with files on the filesystem.
////
//// The functions included in this module are for high-level concepts such as
//// reading and writing.

import gleam/bit_string
import gleam/result

/// Reason represents all of the reasons that Erlang surfaces of why a file
/// system operation could fail. Most of these reasons are POSIX errors, which
/// come from the operating system and start with `E`. Others have been added to
/// represent other issues that may arise.
pub type Reason {
  /// Permission denied.
  Eacces
  /// Resource temporarily unavailable.
  Eagain
  /// Bad file number
  Ebadf
  /// Bad message.
  Ebadmsg
  /// File busy.
  Ebusy
  /// Resource deadlock avoided.
  Edeadlk
  /// On most architectures, same as `Edeadlk`. On some architectures, it
  /// means "File locking deadlock error."
  Edeadlock
  /// Disk quota exceeded.
  Edquot
  /// File already exists.
  Eexist
  /// Bad address in system call argument.
  Efault
  /// File too large.
  Efbig
  /// Inappropriate file type or format. Usually caused by trying to set the
  /// "sticky bit" on a regular file (not a directory).
  Eftype
  /// Interrupted system call.
  Eintr
  /// Invalid argument.
  Einval
  /// I/O error.
  Eio
  /// Illegal operation on a directory.
  Eisdir
  /// Too many levels of symbolic links.
  Eloop
  /// Too many open files.
  Emfile
  /// Too many links.
  Emlink
  /// Multihop attempted.
  Emultihop
  /// Filename too long
  Enametoolong
  /// File table overflow
  Enfile
  /// No buffer space available.
  Enobufs
  /// No such device.
  Enodev
  /// No locks available.
  Enolck
  /// Link has been severed.
  Enolink
  /// No such file or directory.
  Enoent
  /// Not enough memory.
  Enomem
  /// No space left on device.
  Enospc
  /// No STREAM resources.
  Enosr
  /// Not a STREAM.
  Enostr
  /// Function not implemented.
  Enosys
  /// Block device required.
  Enotblk
  /// Not a directory.
  Enotdir
  /// Operation not supported.
  Enotsup
  /// No such device or address.
  Enxio
  /// Operation not supported on socket.
  Eopnotsupp
  /// Value too large to be stored in data type.
  Eoverflow
  /// Not owner.
  Eperm
  /// Broken pipe.
  Epipe
  /// Result too large.
  Erange
  /// Read-only file system.
  Erofs
  /// Invalid seek.
  Espipe
  /// No such process.
  Esrch
  /// Stale remote file handle.
  Estale
  /// Text file busy.
  Etxtbsy
  /// Cross-domain link.
  Exdev
  /// File was requested to be read as UTF-8, but is not UTF-8 encoded.
  NotUtf8
}

/// The type of file found by `file_info` or `link_info`.
///
pub type FileType {
  Device
  Directory
  Other
  Regular
  Symlink
}

/// The read/write permissions a user can have for a file.
///
pub type Access {
  NoAccess
  Read
  ReadWrite
  Write
}

/// Meta information for a file.
///
/// Timestamps are in seconds before or after the Unix time epoch,
/// `1970-01-01 00:00:00 UTC`.
///
pub type FileInfo {
  FileInfo(
    /// File size in bytes.
    ///
    size: Int,
    /// `Regular`, `Directory`, `Symlink`, `Device`, or `Other`.
    ///
    file_type: FileType,
    /// `ReadWrite`, `Read`, `Write`, or `NoAccess`.
    ///
    access: Access,
    /// Timestamp of most recent access.
    ///
    atime: Int,
    /// Timestamp of most recent modification.
    ///
    mtime: Int,
    /// Timestamp of most recent change (or file creation, depending on
    /// operating system).
    ///
    ctime: Int,
    /// File permissions encoded as a sum of bit values, including but not
    /// limited to:
    ///
    /// Owner read, write, execute.
    ///
    /// `0o400`, `0o200`, `0o100`
    ///
    /// Group read, write, execute.
    ///
    /// `0o40`, `0o20`, `0o10`
    ///
    /// Other read, write, execute.
    ///
    /// `0o4`, `0o2`, `0o1`
    ///
    /// Set user ID, group ID on execution.
    ///
    /// `0x800`, `0x400`
    ///
    mode: Int,
    /// Total links to a file (always `1` for file systems without links).
    ///
    links: Int,
    /// The file system where a file is located (`0` for drive `A:` on Windows,
    /// `1` for `B:`, etc.).
    ///
    major_device: Int,
    /// Character device (or `0` on non-Unix systems).
    ///
    minor_device: Int,
    /// The `inode` number for a file (always `0` on non-Unix file systems).
    ///
    inode: Int,
    /// The owner of a file (always `0` on non-Unix file systems).
    ///
    user_id: Int,
    /// The group id of a file (always `0` on non-Unix file systems).
    ///
    group_id: Int,
  )
}

/// Results in `FileInfo` about the given `path` on success, otherwise a
/// `Reason` for failure.
///
/// When `path` refers to a symlink, the result pertains to the link's target.
/// To get `FileInfo` about a symlink itself, use `link_info`.
///
/// ## Examples
///
/// ```gleam
/// > file_info("gleam.toml")
/// Ok(FileInfo(
///   size: 430,
///   file_type: Regular,
///   access: ReadWrite,
///   atime: 1680580321,
///   mtime: 1680580272,
///   ctime: 1680580272,
///   mode: 33188,
///   links: 1,
///   major_device: 64,
///   minor_device: 0,
///   inode: 469028,
///   user_id: 1000,
///   group_id: 1000,
/// ))
///
/// > file_info("/root")
/// Ok(FileInfo(
///   size: 16,
///   file_type: Directory,
///   access: Read,
///   atime: 1677789967,
///   mtime: 1664561240,
///   ctime: 1664561240,
///   mode: 16877,
///   links: 11,
///   major_device: 54,
///   minor_device: 0,
///   inode: 34,
///   user_id: 0,
///   group_id: 0,
/// ))
///
/// > file_info("./build/dev/erlang/rad/priv")
/// Ok(FileInfo(
///   size: 140,
///   file_type: Directory,
///   access: ReadWrite,
///   atime: 1680580321,
///   mtime: 1680580272,
///   ctime: 1680580272,
///   mode: 33188,
///   links: 1,
///   major_device: 64,
///   minor_device: 0,
///   inode: 469028,
///   user_id: 1000,
///   group_id: 1000,
/// ))
///
/// > file_info("/does_not_exist")
/// Error(Enoent)
///
/// > file_info("/root/.local/maybe_exists")
/// Error(Eacces)
/// ```
///
pub external fn file_info(String) -> Result(FileInfo, Reason) =
  "gleam_erlang_ffi" "file_info"

/// Results in `FileInfo` about the given `path` on success, otherwise a
/// `Reason` for failure.
///
/// When `path` refers to a symlink, the result pertains to the link itself.
/// To get `FileInfo` about a symlink's target, use `file_info`.
///
/// ## Examples
///
/// ```gleam
/// > link_info("gleam.toml")
/// Ok(FileInfo(
///   size: 430,
///   file_type: Regular,
///   access: ReadWrite,
///   atime: 1680580321,
///   mtime: 1680580272,
///   ctime: 1680580272,
///   mode: 33188,
///   links: 1,
///   major_device: 64,
///   minor_device: 0,
///   inode: 469028,
///   user_id: 1000,
///   group_id: 1000,
/// ))
///
/// > link_info("/root")
/// Ok(FileInfo(
///   size: 16,
///   file_type: Directory,
///   access: Read,
///   atime: 1677789967,
///   mtime: 1664561240,
///   ctime: 1664561240,
///   mode: 16877,
///   links: 11,
///   major_device: 54,
///   minor_device: 0,
///   inode: 34,
///   user_id: 0,
///   group_id: 0,
/// ))
///
/// > link_info("./build/dev/erlang/rad/priv")
/// Ok(FileInfo(
///   size: 41,
///   file_type: Symlink,
///   access: ReadWrite,
///   atime: 1680581150,
///   mtime: 1680581150,
///   ctime: 1680581150,
///   mode: 41471,
///   links: 1,
///   major_device: 64,
///   minor_device: 0,
///   inode: 471587,
///   user_id: 1000,
///   group_id: 1000,
/// ))
///
/// > link_info("/does_not_exist")
/// Error(Enoent)
///
/// > link_info("/root/.local/maybe_exists")
/// Error(Eacces)
/// ```
///
pub external fn link_info(String) -> Result(FileInfo, Reason) =
  "gleam_erlang_ffi" "link_info"

/// Results in a `Bool` on success that indicates whether the given `path` has
/// a `Directory` `FileType`, otherwise a `Reason` for failure.
///
/// When `path` refers to a symlink, the result pertains to the link's target.
///
/// ## Examples
///
/// ```gleam
/// > is_directory("/tmp")
/// Ok(True)
///
/// > is_directory("resume.pdf")
/// Ok(False)
///
/// > is_directory("/does_not_exist")
/// Error(Enoent)
/// ```
///
pub fn is_directory(path: String) -> Result(Bool, Reason) {
  use FileInfo(file_type: file_type, ..) <- result.map(over: file_info(path))
  file_type == Directory
}

/// Results in a `Bool` on success that indicates whether the given `path` has
/// a `Regular` `FileType`, otherwise a `Reason` for failure.
///
/// When `path` refers to a symlink, the result pertains to the link's target.
///
/// ## Examples
///
/// ```gleam
/// > is_regular("resume.pdf")
/// Ok(True)
///
/// > is_regular("/tmp")
/// Ok(False)
///
/// > is_regular("/does_not_exist.txt")
/// Error(Enoent)
/// ```
///
pub fn is_regular(path: String) -> Result(Bool, Reason) {
  use FileInfo(file_type: file_type, ..) <- result.map(over: file_info(path))
  file_type == Regular
}

/// Results in a `Bool` on success that indicates whether the given `path`
/// exists, otherwise a `Reason` for failure.
///
/// When `path` refers to a symlink, the result pertains to the link's target.
/// To find whether a symlink itself exists, use `link_exists`.
///
/// ## Examples
///
/// ```gleam
/// > file_exists("resume.pdf")
/// Ok(True)
///
/// > file_exists("/tmp")
/// Ok(True)
///
/// > file_exists("/does_not_exist")
/// Ok(False)
///
/// > file_exists("/root/.local/maybe_exists")
/// Error(Eacces)
/// ```
///
pub fn file_exists(path: String) -> Result(Bool, Reason) {
  let result =
    path
    |> file_info
    |> result.replace(True)
  case result {
    Error(Enoent) -> Ok(False)
    _ -> result
  }
}

/// Results in a `Bool` on success that indicates whether the given `path`
/// exists, otherwise a `Reason` for failure.
///
/// When `path` refers to a symlink, the result pertains to the link itself.
/// To find whether a symlink's target exists, use `file_exists`.
///
/// ## Examples
///
/// ```gleam
/// > link_exists("resume.pdf")
/// Ok(True)
///
/// > link_exists("/tmp")
/// Ok(True)
///
/// > link_exists("/does_not_exist")
/// Ok(False)
///
/// > link_exists("/root/.local/maybe_exists")
/// Error(Eacces)
/// ```
///
pub fn link_exists(path: String) -> Result(Bool, Reason) {
  let result =
    path
    |> link_info
    |> result.replace(True)
  case result {
    Error(Enoent) -> Ok(False)
    _ -> result
  }
}

/// Tries to create a directory. Missing parent directories are not created.
///
/// Returns a Result of nil if the directory is created or Reason if the
/// operation failed.
///
/// ## Examples
///
/// ```gleam
/// > make_directory("/tmp/foo")
/// Ok(Nil)
///
/// > make_directory("relative_directory")
/// Ok(Nil)
///
/// > make_directory("/tmp/missing_intermediate_directory/foo")
/// Error(Enoent)
/// ```
///
pub external fn make_directory(String) -> Result(Nil, Reason) =
  "gleam_erlang_ffi" "make_directory"

/// Lists all files in a directory, except files with
/// [raw filenames](https://www.erlang.org/doc/apps/stdlib/unicode_usage.html#notes-about-raw-filenames).
///
/// Returns a Result containing the list of filenames in the directory, or Reason
/// if the operation failed.
///
/// ## Examples
///
/// ```gleam
/// > list_directory("/tmp")
/// Ok(["FB01293B-8597-4359-80D5-130140A0C0DE","AlTest2.out"])
///
/// > list_directory("resume.docx")
/// Error(Enotdir)
/// ```
///
pub external fn list_directory(String) -> Result(List(String), Reason) =
  "gleam_erlang_ffi" "list_directory"

/// Deletes a directory.
///
/// The directory must be empty before it can be deleted. Returns a nil Success
/// or Reason if the operation failed.
///
/// ## Examples
///
/// ```gleam
/// > delete_directory("foo")
/// Ok(Nil)
///
/// > delete_directory("does_not_exist/")
/// Error(Enoent)
/// ```
///
pub external fn delete_directory(String) -> Result(Nil, Reason) =
  "gleam_erlang_ffi" "delete_directory"

/// Deletes a file or directory recursively.
///
/// Returns a nil Success or Reason if the operation failed.
///
/// ## Examples
///
/// ```gleam
/// > recursive_delete("foo")
/// Ok(Nil)
///
/// > recursive_delete("/bar")
/// Ok(Nil)
///
/// > recursive_delete("does_not_exist/")
/// Error(Enoent)
/// ```
///
pub external fn recursive_delete(String) -> Result(Nil, Reason) =
  "gleam_erlang_ffi" "recursive_delete"

/// Read the contents of the given file as a String
///
/// Assumes the file is UTF-8 encoded. Returns a Result containing the file's
/// contents as a String if the operation was successful, or Reason if the file
/// operation failed. If the file is not UTF-8 encoded, the `NotUTF8` variant
/// will be returned.
///
/// ## Examples
///
/// ```gleam
/// > read("example.txt")
/// Ok("Hello, World!")
///
/// > read(from: "example.txt")
/// Ok("Hello, World!")
///
/// > read("does_not_exist.txt")
/// Error(Enoent)
///
/// > read("cat.gif")
/// Error(NotUTF8)
/// ```
///
pub fn read(from path: String) -> Result(String, Reason) {
  path
  |> do_read_bits()
  |> result.then(fn(content) {
    case bit_string.to_string(content) {
      Ok(string) -> Ok(string)
      Error(Nil) -> Error(NotUtf8)
    }
  })
}

/// Read the contents of the given file as a BitString
///
/// Returns a Result containing the file's contents as a BitString if the
/// operation was successful, or Reason if the operation failed.
///
/// ## Examples
///
/// ```gleam
/// > read_bits("example.txt")
/// Ok(<<"Hello, World!">>)
///
/// > read_bits(from: "cat.gif")
/// Ok(<<71,73,70,56,57,97,1,0,1,0,0,0,0,59>>)
///
/// > read_bits("does_not_exist.txt")
/// Error(Enoent)
/// ```
///
pub fn read_bits(from path: String) -> Result(BitString, Reason) {
  do_read_bits(path)
}

external fn do_read_bits(path) -> Result(BitString, Reason) =
  "gleam_erlang_ffi" "read_file"

/// Write the given String contents to a file of the given name.
///
/// Returns a Result with Nil if the operation was successful or a Reason
/// otherwise.
///
/// ## Examples
///
/// ```gleam
/// > write("Hello, World!", "file.txt")
/// Ok(Nil)
///
/// > write(to: "file.txt", contents: "Hello, World!")
/// Ok(Nil)
///
/// > write("Hello, World!", "does_not_exist/file.txt")
/// Error(Enoent)
/// ```
///
pub fn write(contents contents: String, to path: String) -> Result(Nil, Reason) {
  contents
  |> bit_string.from_string
  |> do_write_bits(path)
}

/// Write the given BitString contents to a file of the given name.
///
/// Returns a Result with Nil if the operation was successful or a Reason
/// otherwise.
///
/// ## Examples
///
/// ```gleam
/// > write_bits(<<71,73,70,56,57,97,1,0,1,0,0,0,0,59>>, "cat.gif")
/// Ok(Nil)
///
/// > write_bits(to: "cat.gif", contents: <<71,73,70,56,57,97,1,0,1,0,0,0,0,59>>)
/// Ok(Nil)
///
/// > write_bits(<<71,73,70,56,57,97,1,0,1,0,0,0,0,59>>, "does_not_exist/cat.gif")
/// Error(Enoent)
/// ```
///
pub fn write_bits(
  contents contents: BitString,
  to path: String,
) -> Result(Nil, Reason) {
  do_write_bits(contents, path)
}

external fn do_write_bits(BitString, String) -> Result(Nil, Reason) =
  "gleam_erlang_ffi" "write_file"

/// Append the given String contents to a file of the given name.
///
/// Returns a Result with Nil if the operation was successful or a Reason
/// otherwise.
///
/// ## Examples
///
/// ```gleam
/// > append("Hello, World!", "file.txt")
/// Ok(Nil)
///
/// > append(to: "file.txt", contents: "Hello, World!")
/// Ok(Nil)
///
/// > append("Hello, World!", "does_not_exist/file.txt")
/// Error(Enoent)
/// ```
///
pub fn append(contents contents: String, to path: String) -> Result(Nil, Reason) {
  contents
  |> bit_string.from_string
  |> do_append_bits(path)
}

/// Append the given BitString contents to a file of the given name.
///
/// Returns a Result with Nil if the operation was successful or a Reason
/// otherwise.
///
/// ## Examples
///
/// ```gleam
/// > append_bits(<<71,73,70,56,57,97,1,0,1,0,0,0,0,59>>, "cat.gif")
/// Ok(Nil)
///
/// > append_bits(to: "cat.gif", contents: <<71,73,70,56,57,97,1,0,1,0,0,0,0,59>>)
/// Ok(Nil)
///
/// > append_bits(<<71,73,70,56,57,97,1,0,1,0,0,0,0,59>>, "does_not_exist/cat.gif")
/// Error(Enoent)
/// ```
///
pub fn append_bits(
  contents contents: BitString,
  to path: String,
) -> Result(Nil, Reason) {
  do_append_bits(contents, path)
}

external fn do_append_bits(
  contents: BitString,
  path: String,
) -> Result(Nil, Reason) =
  "gleam_erlang_ffi" "append_file"

/// Delete the given file.
///
/// Returns a Result with Nil if the operation was successful or a Reason
/// otherwise.
///
/// ## Examples
///
/// ```gleam
/// > delete("file.txt")
/// Ok(Nil)
///
/// > delete("does_not_exist.txt")
/// Error(Enoent)
/// ```
///
pub external fn delete(String) -> Result(Nil, Reason) =
  "gleam_erlang_ffi" "delete_file"
