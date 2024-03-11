{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nci = {
      url = "github:yusdacra/nix-cargo-integration";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        parts.follows = "parts";
      };
    };
    parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ parts, nci, ... }:
    parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      imports = [
        parts.flakeModules.easyOverlay
        nci.flakeModule
        ./crates.nix
      ];

      perSystem = { config, ... }:
        let crateOutputs = config.nci.outputs.gleam; in
        {
          overlayAttrs.gleam = config.packages.gleam;
          packages = rec{
            gleam = crateOutputs.packages.release;
            default = gleam;
          };
          devShells.default = crateOutputs.devShell;
        };
    };
}

