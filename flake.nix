{
  description = "A personal ripoff of forester for scientific Zettelkasten notes in Typst";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    agda = {
      url = "github:agda/agda";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, agda }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        localTypstPackage = source: files:
          let
            manifest = (pkgs.lib.importTOML (source + "/typst.toml")).package;
            directory = "$out/local/${manifest.name}/${manifest.version}";
          in
          pkgs.runCommand "typst-local-${manifest.name}" { } ''
            mkdir -p $out/local/${manifest.name}
            cp -r ${source} ${directory}
            chmod -R u+w ${directory}
            ${pkgs.lib.concatLines (pkgs.lib.mapAttrsToList
              (name: path: "cp '${path}' ${directory}/${name}")
              files)}
          '';

        chicagoAuthorDateCsl = pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/citation-style-language/styles/3220666a2b6918cccfeca3d86f6f7d9ea9661236/chicago-author-date.csl";
          hash = "sha256-AC+t541+T+nUKTaha0OoBmsJcBP2JV30Cxv7pmMe/5s=";
        };

        script = pkgs.runCommand "arb-script" { nativeBuildInputs = [ pkgs.esbuild ]; } ''
          mkdir $out
          cd $out
          cp ${./typst/template/assets}/*.ts .
          esbuild main.ts --bundle '--external:/pagefind/*' --format=esm \
            --minify --sourcemap --outfile=main.js
          rm *.ts
        '';

        fonts = {
          "EBGaramond-Variable.ttf" = "${pkgs.google-fonts}/share/fonts/truetype/EBGaramond[wght].ttf";
          "EBGaramond-Italic-Variable.ttf" = "${pkgs.google-fonts}/share/fonts/truetype/EBGaramond-Italic[wght].ttf";
          "SourceSans3-Variable.ttf" = "${pkgs.google-fonts}/share/fonts/truetype/SourceSans3[wght].ttf";
          "JuliaMono-Regular.ttf" = "${pkgs.julia-mono}/share/fonts/truetype/JuliaMono-Regular.ttf";
          "JuliaMono-Bold.ttf" = "${pkgs.julia-mono}/share/fonts/truetype/JuliaMono-Bold.ttf";
          "LatinModernMath-Regular.otf" = "${pkgs.lmodern}/share/fonts/opentype/public/lm-math/latinmodern-math.otf";
        };

        localPackages = pkgs.symlinkJoin {
          name = "typst-local-packages";
          paths = [
            (localTypstPackage ./typst/arb-core { })
            (localTypstPackage ./typst/prelude { })
            (localTypstPackage ./typst/template ({
              "chicago-author-date.csl" = chicagoAuthorDateCsl;
              "assets/main.js" = "${script}/main.js";
              "assets/main.js.map" = "${script}/main.js.map";
            } // pkgs.lib.mapAttrs' (name: pkgs.lib.nameValuePair "assets/${name}") fonts))
          ];
        };

        typstUniversePackages = pkgs.linkFarm "typst-preview-packages"
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

        # Agda's master branch, with the changes nixpkgs makes to its own Agda.
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            Agda = pkgs.lib.pipe (self.callCabal2nix "Agda" agda { }) (with pkgs.haskell.lib.compose; [
              (enableCabalFlag "optimise-heavily")
              (enableCabalFlag "debug")
              (overrideCabal (drv: {
                mainProgram = "agda";
                postInstall = drv.postInstall or "" + ''
                  agdaExe=''${bin:-$out}/bin/agda
                  (cd "$("$agdaExe" --print-agda-data-dir)/lib/prim" && "$agdaExe" --build-library)
                '';
              }))
              enableSeparateBinOutput
              # Nothing here uses the profiling libraries or the documentation,
              # and building them would more than double the build time.
              disableLibraryProfiling
              dontHaddock
            ]);
          };
        };

        agda-annotate = pkgs.haskell.lib.justStaticExecutables
          (haskellPackages.callCabal2nix "agda-annotate" ./agda-annotate { });

        racketDependencies = pkgs.callPackage ./nix/racket-dependencies.nix { };

        arb = pkgs.stdenvNoCC.mkDerivation {
          name = "arb";
          src = ./wrapper;
          nativeBuildInputs = with pkgs; [ racket makeWrapper removeReferencesTo ];

          buildPhase = ''
            runHook preBuild
            export HOME=$TMPDIR PLTADDONDIR=${racketDependencies}
            raco exe -o arb main.rkt
            raco distribute dist arb
            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall
            cp -r dist/. $out
            wrapProgram $out/bin/arb \
              --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.typst agda-annotate pkgs.pagefind ]} \
              --set TYPST_PACKAGE_PATH ${localPackages} \
              --set TYPST_PACKAGE_CACHE_PATH ${typstUniversePackages}
            runHook postInstall
          '';

          # The `raco distribute` command makes the executable self-contained,
          # but compiled code keeps source paths into Racket and its add-on
          # directory. Nothing reads them at run time, so remove them to keep
          # the full distribution, GTK included, out of the closure.
          postFixup = ''
            find $out -type f -exec remove-references-to -t ${pkgs.racket} -t ${racketDependencies} {} +
          '';
          disallowedReferences = [ pkgs.racket racketDependencies ];

          doInstallCheck = true;
          installCheckPhase = ''
            $out/bin/arb --help | grep -q 'build  build the site'
          '';
        };

        # The site built by `arb build` from `src`, a source tree with the
        # marker file at its root.
        buildSite = { src }: pkgs.stdenvNoCC.mkDerivation {
          name = "arb-site";
          inherit src;
          nativeBuildInputs = [ arb ];
          buildPhase = ''
            runHook preBuild
            export HOME=$TMPDIR
            arb build
            runHook postBuild
          '';
          installPhase = ''
            runHook preInstall
            cp -r build $out
            runHook postInstall
          '';
        };
      in
      {
        lib = {
          inherit buildSite;
        };

        packages = {
          inherit agda-annotate arb;
          agda = haskellPackages.Agda.bin;
        };

        devShells.default = pkgs.mkShellNoCC {
          packages = with pkgs; [
            arb
            self.packages.${system}.agda
            racket
            typst
          ];

          # If you edit a Typst package here, you need to re-enter the dev shell
          # for the change to take effect inside the environment.
          TYPST_PACKAGE_PATH = localPackages;
          TYPST_PACKAGE_CACHE_PATH = typstUniversePackages;
          PLTADDONDIR = racketDependencies;
        };
      });
}
