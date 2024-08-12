{
  description = "A flake for scheme-langserver";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    chez-exe.url = "github:rschardt/chez-exe";
  };

  outputs = {
    self,
    nixpkgs,
    utils,
    ...
    } @ inputs:
    utils.lib.eachSystem ["x86_64-linux"] (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        chez = pkgs.chez-racket;
        chez-exe = inputs.chez-exe.packages.${system}.default;
      in
        rec {
          packages = rec {

            default = scheme-langserver;

            scheme-langserver = pkgs.stdenv.mkDerivation {
              name = "scheme-langserver";
              version = "0.0.1";
              src = ./.;
              buildInputs = [
                chez
                chez-exe
                pkgs.akku
                pkgs.libuuid
              ];

              # variables taken from .akku/bin/activate
              buildPhase = ''
                AKKU_CHEZ_PATH="$R6RS_PATH";
                AKKU_R6RS_PATH="$R6RS_PATH";
                AKKU_R7RS_PATH="$R7RS_PATH";
                AKKU_CHEZ_PATH=$PWD/.akku/lib::$PWD/.akku/libobj''${AKKU_CHEZ_PATH:+:}$AKKU_CHEZ_PATH;
                AKKU_R6RS_PATH=$PWD/.akku/lib''${AKKU_R6RS_PATH:+:}$AKKU_R6RS_PATH;
                AKKU_R7RS_PATH=$PWD/.akku/lib''${AKKU_R7RS_PATH:+:}$AKKU_R7RS_PATH;
                export CHEZSCHEMELIBDIRS="$AKKU_CHEZ_PATH";
                unset CHEZSCHEMELIBEXTS;
                export GUILE_LOAD_PATH="$AKKU_R6RS_PATH";
                export GUILE_LOAD_COMPILED_PATH="$PWD/.akku/libobj";
                export IKARUS_LIBRARY_PATH="$AKKU_R6RS_PATH";
                export MOSH_LOADPATH="$AKKU_R6RS_PATH";
                export PLTCOLLECTS=":$AKKU_R6RS_PATH";
                export SAGITTARIUS_LOADPATH="$AKKU_R6RS_PATH";
                export VICARE_SOURCE_PATH="$AKKU_R6RS_PATH";
                export YPSILON_SITELIB="$AKKU_R6RS_PATH";
                export LARCENY_LIBPATH="$AKKU_R6RS_PATH";
                export IRONSCHEME_LIBRARY_PATH="$AKKU_R6RS_PATH";
                export LOKO_LIBRARY_PATH="$AKKU_R6RS_PATH";
                export DIGAMMA_SITELIB="$AKKU_R6RS_PATH";
                export CHIBI_MODULE_PATH="$AKKU_R7RS_PATH";
                export GAUCHE_LOAD_PATH="$AKKU_R7RS_PATH";
                export PATH=$PWD/.akku/bin''${PATH:+:}$PATH;
                export LD_LIBRARY_PATH=$PWD/.akku/ffi''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH;
                export DYLD_LIBRARY_PATH=$PWD/.akku/ffi''${DYLD_LIBRARY_PATH:+:}$DYLD_LIBRARY_PATH;

                compile-chez-program --optimize-level 2 run.ss
              '';

              installPhase = ''
                mkdir -p $out/bin
                chmod +x run
                mv run $out/bin/scheme-langserver
              '';
            };

          devShells."${system}".default = pkgs.mkShell {
            packages = with pkgs; [
              akku
            ];
          };

          apps.default = { type = "app"; program = "${packages.default}/bin/scheme-langserver"; };
        };
      }
    );
}
