Description of behaviour
Given some project foo that has a path dependency bar, if bar adds a dependency baz after foo has already been built, attempting to rebuild foo will fail complaining that module baz cannot be found.

Steps to reproduce
The following steps but as a video

Given the following project setup:

foo/
  gleam.toml
  src/
    foo.gleam
bar/
  gleam.toml
  src/
    bar.gleam
# foo/gleam.toml

[dependencies]
bar = { path = "../bar" }
// foo/src/foo.gleam

import bar

pub fn main() {
  bar.say_hello()
}
# bar/gleam.toml

[dependencies]
gleam_stdlib = "~> 0.29"
// bar/src/bar.gleam

import gleam/io

pub fn say_hello() {
  io.println("Hello from bar!")
}
If you then take the following commands:

~/foo $ gleam build
~/foo $ cd ../bar
~/bar $ gleam add simplifile
And then update bar/src/bar.gleam like following

// bar/src/bar.gleam

import simplifile
import gleam/io

pub fn say_hello() {
  let assert Ok(_) = simplifile.make_directory("hello-from-bar")

  io.println("Hello from bar!")
}
And then finally attempt to build foo again

~/foo $ gleam build
It will fail with the following error message:

  Compiling gleam_stdlib
  Compiling bar
error: Unknown module
  ┌─ /Users/dan/Repositories/gleam-path-deps-bug/bar/src/bar.gleam:1:8
  │
1 │ import simplifile
  │        ^^^^^^^^^^

No module has been found with the name `simplifile`.
