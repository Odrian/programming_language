{ pkgs ? import <nixpkgs> {} }:

let
  llvm = pkgs.llvmPackages_21;
in
pkgs.mkShell {
  buildInputs = [
    llvm.llvm
    llvm.llvm.dev
    llvm.libclang
    llvm.libllvm
    pkgs.pkg-config
    pkgs.libxml2
    pkgs.libffi
  ];

  LLVM_SYS_211_PREFIX = "${llvm.llvm.dev}";
  LLVM_CONFIG_PATH    = "${llvm.llvm}/bin/llvm-config";
}
