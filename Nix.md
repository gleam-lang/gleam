# Reproducible builds for Gleam.

[nix](https://nixos.org/) is a purely functional package manager with
fully-cacheable, to-the-byte reproducible builds. 

This guide documents how people using the Nix package manager or 
NixOS systems can easily build every version of Gleam with just
a single command.

##### Requirements

For using this guide you'll need `nix` version [2.4](https://discourse.nixos.org/t/nix-2-4-released/15822)
or latter which must have the [`flakes`](https://nixos.wiki/wiki/Flakes) feature enabled.

See [nix quick-install](https://nixos.org/download.html) or the [install-nix tutorial](https://nix.dev/tutorials/install-nix)
for more in depth instructions.

## Running Gleam nightly. (or any branch/commit)

The following command runs `gleam --help` on the build from
the latest commit on `main` branch.

```shell
nix shell github:gleam-lang/gleam -c gleam --help
```

Gleam maintainers can also use this to try PR experimental features
from other contributors just by adapting the previous command 
repository/branch name.

```shell
# The latest commit from main branch. (can be a commit hash, tag or branch name)
nix shell github:gleam-lang/gleam/main -c gleam --help

# running gleam to try other people branches:
nix shell github:<someone>/gleam/<cool-feature> -c gleam --help
```

## Developing Gleam with a Nix environment.

Also, for Gleam developers, using Nix ensures we get the same
development environment in an instant, all you have to do is
checkout the Gleam repo and run:

```shell
nix develop # -c fish # you might use your preferred shell
# open your editor and hack hack hack..
cargo build # build dependencies are loaded in your shell
# or 
nix run . -- --help # runs your local `gleam --help`
```

## flake.nix

[Nix flakes] are the secret sauce for nix reproducible builds.
Since all build dependencies get hashed, even the source code.
Every external dependency such external repos (e.g. nixpkgs), 
external utilities (e.g. cargo llvm make), and any Cargo.toml
workspace dependency (read from `Cargo.nix`) gets hashed so that
nix only builds what has actually changed.

If you edit the `flake.nix` file, for example to change the rust
toolchain or the nixpkgs revision, run `nix flake udpate` afterwards
to regenerate the lock file.

## Regenerating `Cargo.nix`

Whenever we add or change dependencies on our Cargo workspace, we should
regenerate the `Cargo.nix` file by using [cargo2nix](https://github.com/cargo2nix/cargo2nix).

The following command should do it:

```shell
nix shell github:cargo2nix/cargo2nix/master -c cargo2nix -f
```
