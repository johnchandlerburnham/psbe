{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = with pkgs; [
      nodePackages.npm
      nodejs-8_x
      purescript
      psc-package
    ];
}
