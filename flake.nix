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
      in
      {
        devShells.default = pkgs.mkShellNoCC {
          packages = with pkgs; [
            racket
            typst
          ];
        };
      });
}
