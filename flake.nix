{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=release-21.05";

    cargo2nix.url = "github:cargo2nix/cargo2nix";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, cargo2nix, flake-utils, rust-overlay, ... }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (import "${cargo2nix}/overlay")
            rust-overlay.overlay
          ];
        };

        rustPkgs = pkgs.rustBuilder.makePackageSet' {
          rustChannel = "1.57.0";
          packageFun = import ./Cargo.nix;
        };
      in rec {
        packages = {
          gleam = (rustPkgs.workspace.gleam {}).bin;
        };

        defaultPackage = packages.gleam;
      }
    );
}
