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
        localTypstPackage = source: files:
          let
            manifest = (pkgs.lib.importTOML (source + "/typst.toml")).package;
            directory = "$out/local/${manifest.name}/${manifest.version}";
          in
          pkgs.runCommand "typst-local-${manifest.name}" { } ''
            mkdir -p $out/local/${manifest.name}
            cp -r ${source} ${directory}
            chmod u+w ${directory}
            ${pkgs.lib.concatLines (pkgs.lib.mapAttrsToList
              (name: path: "cp ${path} ${directory}/${name}")
              files)}
          '';

        chicagoAuthorDateCsl = pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/citation-style-language/styles/3220666a2b6918cccfeca3d86f6f7d9ea9661236/chicago-author-date.csl";
          hash = "sha256-AC+t541+T+nUKTaha0OoBmsJcBP2JV30Cxv7pmMe/5s=";
        };

        # Typst takes a single package path, so the local packages need to share
        # a single `local/` tree.
        localPackages = pkgs.symlinkJoin {
          name = "typst-local-packages";
          paths = [
            (localTypstPackage ./typst/arb-core { })
            (localTypstPackage ./typst/prelude { })
            (localTypstPackage ./typst/template {
              "chicago-author-date.csl" = chicagoAuthorDateCsl;
            })
          ];
        };

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
                name = "citegeist";
                version = "0.3.1";
                hash = "sha256-MBiAHNLDRvt8WSsYFv9f7LCyqJCSCEenGDMP48J1vPg=";
              }
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

        agda-annotate = pkgs.haskell.lib.justStaticExecutables
          (pkgs.haskellPackages.callCabal2nix "agda-annotate" ./agda-annotate { });

        # A stopgap build of the notes site until arb's build command exists.
        # Note: expects the dev shell's Typst package paths.
        arb-build = pkgs.writeShellApplication {
          name = "arb-build";
          runtimeInputs = with pkgs; [ jq typst ];
          text = ''
            styles=${./typst/template/assets/styles}
          '' + builtins.readFile ./build.sh;
        };
      in
      {
        packages = {
          inherit agda-annotate arb-build;
        };

        devShells.default = pkgs.mkShellNoCC {
          packages = with pkgs; [
            arb-build
            racket
            typst
          ];

          # If you edit a Typst package here, you need to re-enter the dev shell
          # for the change to take effect inside the environment.
          TYPST_PACKAGE_PATH = localPackages;
          TYPST_PACKAGE_CACHE_PATH = previewPackages;
        };
      });
}
