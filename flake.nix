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

        # Typst Universe packages, laid out as Typst's package cache,
        # `preview/<name>/<version>`, so that Typst never downloads them.
        # fletcher depends on cetz, which depends on oxifmt.
        previewPackages = pkgs.linkFarm "typst-preview-packages"
          (map
            ({ name, version, hash }: {
              name = "preview/${name}/${version}";
              path = pkgs.fetchzip {
                url = "https://packages.typst.org/preview/${name}-${version}.tar.gz";
                stripRoot = false;
                inherit hash;
              };
            })
            [
              {
                name = "fletcher";
                version = "0.5.8";
                hash = "sha256-kiqiNzFU4imhPnwzwV1bGgGfS3mZOo96JsKboA5yjtU=";
              }
              {
                name = "cetz";
                version = "0.3.4";
                hash = "sha256-UvHEY4klR5Wi0Pk8DYVcfUmLsOnxEGOcjNu6B9/Nr9s=";
              }
              {
                name = "oxifmt";
                version = "0.2.1";
                hash = "sha256-Cl6Y34d3fi6aB07HIk4zsyy0ucHgWCId0yMVxE4DnWw=";
              }
            ]);
      in
      {
        devShells.default = pkgs.mkShellNoCC {
          packages = with pkgs; [
            racket
            typst
          ];

          # If you edit a Typst package here, you need to re-enter the dev shell
          # for the change to take effect inside the environment.
          TYPST_PACKAGE_PATH = arbCore;
          TYPST_PACKAGE_CACHE_PATH = previewPackages;
        };
      });
}
