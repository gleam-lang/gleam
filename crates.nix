{
  perSystem = { pkgs, lib, config, ... }:
    let inherit (pkgs.darwin.apple_sdk) frameworks; in
    {
      nci = {
        toolchainConfig = {
          channel = "stable";
          components = [ "rust-analyzer" "clippy" "rustfmt" "rust-src" ];
        };

        projects.gleam = rec {
          path = ./.;
          depsDrvConfig = drvConfig;
          drvConfig.mkDerivation = {
            nativeBuildInputs = [ pkgs.git pkgs.pkg-config ];
            buildInputs = [ pkgs.erlang pkgs.openssl ]
              ++ lib.optionals pkgs.stdenv.isDarwin [
              frameworks.SystemConfiguration
              frameworks.Security
            ];
          };
        };
      };
    };
}
