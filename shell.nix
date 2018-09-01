let
  pkgs = import <nixpkgs> {};
  buildInputs = with pkgs; [
    vulkan-headers
    vulkan-loader
    vulkan-tools
    vulkan-validation-layers
  ];
in
  with pkgs;

  stdenv.mkDerivation {
    name = "nix-shell";
    inherit buildInputs;
    shellHook = ''
      export LD_LIBRARY_PATH="${stdenv.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH";
      export VK_LAYER_PATH=${vulkan-validation-layers}/share/vulkan/explicit_layer.d/
    '';
  }
