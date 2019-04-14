# Installation

A Gleam installation is made up of two parts: The Gleam compiler, and the
Erlang runtime.

## Installing the compiler

The compiler is written in the Rust programming language and must be build
from source. [Install the Rust compiler](https://www.rust-lang.org/tools/install)
before proceeding.

```sh
# Download the Gleam source code git repository
cd /tmp
git clone https://github.com/lpil/gleam.git
cd gleam

# Build the Gleam compiler. This will take some time!
make install

# Verify the compiler is installed
# Prints "gleam $VERSION"
gleam --version
```

## Installing the Erlang runtime

Gleam compiles to Erlang code, so Erlang needs to be installed to run Gleam
code.

Precompiled builds for many popular operating systems can be downloaded from
the [Erlang solutions website](https://www.erlang-solutions.com/resources/download.html),

Guides for installing Erlang on specific operating systems can be found below,
as well as information on installing multiple versions of Erlang at once using
version manager tools.

Once Erlang has been installed you can check it is working by typing `erl
-version` in your computer's terminal. You will see version information like
this if all is well:

```
$ erl -version
Erlang (SMP,ASYNC_THREADS,HIPE) (BEAM) emulator version 10.1
```

### Linux

#### Debian Linux

```sh
sudo apt-get update
sudo apt-get install erlang
```

#### Ubuntu Linux

```sh
sudo apt-get update
sudo apt-get install erlang
```


### Mac OS X

#### Using Homebrew

With [Homebrew](https://brew.sh) installed run the following:

```sh
brew update
brew install erlang
```


### Windows

#### Using Chocolatey

With [Chocolatey](https://chocolatey.org/) installed on your computer run the
following:

```
choco install erlang
```

### Using version managers

#### asdf

The asdf version manager has a plugin for installing Erlang. Installation and
usage instructions can be found here:

- [https://github.com/asdf-vm/asdf](https://github.com/asdf-vm/asdf)
- [https://github.com/asdf-vm/asdf-erlang](https://github.com/asdf-vm/asdf-erlang)
