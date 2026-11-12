{
  description = "Project with LLVM 21";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        llvm = pkgs.llvmPackages_21;
      in
      {
        devShells.programming-language = pkgs.mkShell {
          name = "programming-language";

          buildInputs = with pkgs; [
            llvm.llvm
            llvm.llvm.dev
            llvm.libclang
            llvm.libllvm
            pkg-config
            libxml2
            libffi
            gcc
          ];

          shellHook = ''
            export LLVM_SYS_211_PREFIX="${llvm.llvm.dev}"
            export LLVM_CONFIG_PATH="${llvm.llvm}/bin/llvm-config"
          '';
        };
      }
    );
}
