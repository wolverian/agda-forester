{
system ? builtins.currentSystem
}:
let
  pkgs = import ./nix/nixpkgs.nix { inherit system; };
  drv = import ./default.nix { inherit system; };
in 
pkgs.mkShell {
    name = "agda-forester-shell";

    inputsFrom = [ drv.env ];

    buildInputs = drv.buildInputs or [];

    shellHook = ''
    echo "Welcome to the dev shell!"
    echo "Agda version: $(agda --version || echo not found)"
    echo "Forester version: $(forester --version || echo not found)"
    '';
}
