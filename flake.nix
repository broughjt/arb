{
  description = "A personal ripoff of forester for scientific Zettelkasten notes in Typst";

  # TODO: Run `nix flake update` once we match the build oracle
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # A Typst package directory laid out the way Typst looks up `@local`
        # packages, `local/<name>/<version>`, with the name and version taken
        # from its typst.toml.
        localTypstPackage = source:
          let manifest = (pkgs.lib.importTOML (source + "/typst.toml")).package;
          in pkgs.runCommand "typst-local-${manifest.name}" { } ''
            mkdir -p $out/local/${manifest.name}
            cp -r ${source} $out/local/${manifest.name}/${manifest.version}
          '';

        arbCore = localTypstPackage ./typst/arb-core;
      in
      {
        devShells.default = pkgs.mkShellNoCC {
          packages = with pkgs; [
            racket
            typst
          ];

          # Typst reads the packages from the store, so editing one takes
          # effect only after re-entering the dev shell.
          TYPST_PACKAGE_PATH = arbCore;
        };
      });
}
