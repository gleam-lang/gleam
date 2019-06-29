# Installing Gleam

## Precompiled for Linux or macOS

The easiest way to install Gleam on Linux and Apple macOS is to download a
prebuilt version of the compiler from the [GitHub release
page](https://github.com/lpil/gleam/releases).

## asdf version manager

[asdf](https://github.com/asdf-vm/asdf) is a tool for installing and managing
multiple version of programming languages at the same time. Install the
[asdf-gleam plugin](https://github.com/vic/asdf-gleam) to manage Gleam with
asdf.

## Arch Linux

Gleam is available through the [Arch User Repository](https://wiki.archlinux.org/index.php/Arch_User_Repository)
as package `gleam`. You can use your prefered [helper](https://wiki.archlinux.org/index.php/AUR_helpers)
to install it or clone it for manual build from [https://aur.archlinux.org/gleam.git](https://aur.archlinux.org/gleam.git).

## Build from source

The compiler is written in the Rust programming language and so if you wish to
build Gleam from source you will need to [install the Rust
compiler](https://www.rust-lang.org/tools/install).

```sh
# Download the Gleam source code git repository
cd /tmp
git clone https://github.com/lpil/gleam.git --branch v0.2.0
cd gleam

# Build the Gleam compiler. This will take some time!
make install

# Verify the compiler is installed
# Prints "gleam $VERSION"
gleam --version
```
