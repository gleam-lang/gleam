# Reproducible builds for Gleam.

[nix](https://nixos.org/) is a purely functional package manager with
fully-cacheable, to-the-byte reproducible builds. 

This guide documents how people using the Nix package manager or 
NixOS systems can easily build every version of Gleam with just
a single command.

## Running Gleam nightly. (or any branch/commit)


## Developing Gleam with a Nix environment.

Also, for Gleam developers, using Nix ensures we get the same
development environment.


## flake.nix

The best approach is using our provided `flake.nix` file which pins
all dependencies, thus ensuring that Gleam builds will be exactly
the same given we build from the same commit.


## Regenerating `Cargo.nix`

Use [cargo2nix](https://github.com/cargo2nix/cargo2nix) to regenerate it.

```shell
nix shell github:cargo2nix/cargo2nix/master -c cargo2nix -f
```
