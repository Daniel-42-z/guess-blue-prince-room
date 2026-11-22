{
  nixConfig = {
    extra-substituters = [
      "https://haskell-miso-cachix.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-miso-cachix.cachix.org-1:m8hN1cvFMJtYib4tj+06xkKt5ABMSGfe8W7s40x1kQ0="
    ];
  };

  inputs = {
    miso.url = "github:dmjio/miso";
    mielo-css = {
      url = "github:mielo-ui/mielo.css";
      flake = false;
    };
  };

  outputs = inputs:
    inputs.miso.inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = inputs.miso.inputs.nixpkgs.legacyPackages.${system};
        wasmShell = inputs.miso.outputs.devShells.${system}.wasm;
      in
      {
        devShells.default = inputs.miso.outputs.devShells.${system}.default;
        devShells.hls = inputs.miso.outputs.devShells.${system}.hls;
        devShells.wasm = wasmShell.overrideAttrs (old: {
          nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.python3 ];
          MIELO_CSS_DIR = "${inputs.mielo-css}/css";
        });
      });
}
