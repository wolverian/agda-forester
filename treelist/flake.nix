{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    opam-repository.url = "github:ocaml/opam-repository";
    opam-repository.flake = false;
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.opam-repository.follows = "opam-repository";
    };
    # forester.url = "sourcehut:~jonsterling/ocaml-forester?rev=58af4f74e009655999302e63c9c58f9640c262ad";
  };
  outputs =
    {
      self,
      flake-utils,
      opam-nix,
      nixpkgs,
      ...
    }@inputs:
    # Don't forget to put the package name instead of `throw':
    let
      package = "treelist";
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        devPackagesQuery = {
          # You can add "development" packages here. They will get added to the devShell automatically.
          ocaml-lsp-server = "*";
          ocamlformat = "*";
        };
        query = devPackagesQuery // {
          ## You can force versions of certain packages here, e.g:
          ## - force the ocaml compiler to be taken from opam-repository:
          ocaml-system = "*";
          ## - or force the compiler to be taken from nixpkgs and be a certain version:
          # ocaml-system = "4.14.0";
          ## - or force ocamlfind to be a certain version:
          # ocamlfind = "1.9.2";
        };
        scope = on.buildOpamProject' { } ./. query;
        overlay = final: prev: {
          # You can add overrides here
          ${package} = prev.${package}.overrideAttrs (_: {
            # Prevent the ocaml dependencies from leaking into dependent environments
            doNixSupport = false;
          });

          dune = prev.dune.overrideAttrs (o: {
            version = "3.17.2";
            src = o.src;
            # builtins.fetchurl {
            #   url = "https://github.com/ocaml/dune/releases/download/3.17.2/dune-3.17.2.tbz";
            #   sha256 = "0r7al83jwkdfk6qvb53vrlzzfr08gwcydn1ccigfdsfg1vnzxslx";
            # };
            nativeBuildInputs = o.nativeBuildInputs ++ [ pkgs.makeWrapper ];
            postFixup =
              if pkgs.stdenv.isDarwin then ''
                wrapProgram $out/bin/dune \
                --suffix PATH : "${pkgs.darwin.sigtool}/bin"
              '' else "";
          });

          forester = prev.forester.overrideAttrs (_: {
            DUNE_CACHE = "enabled";
            DUNE_CACHE_TRANSPORT = "direct";
            XDG_CACHE_HOME = "/tmp/dune-cache";
          });
        };
        scope' = scope.overrideScope overlay;
        # The main package containing the executable
        main = scope'.${package};
        # Packages from devPackagesQuery
        devPackages = builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope');
      in
      {
        legacyPackages = scope';

        packages.default = main;

        devShells.default = pkgs.mkShell {
          inputsFrom = [ main ];
          buildInputs = devPackages ++ [
            # You can add packages from nixpkgs here
          ];
        };
      }
    );
}
