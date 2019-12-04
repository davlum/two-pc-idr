{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation { 
  name = "two-pc-idr";

  buildInputs = [ (idrisPackages.with-packages (with idrisPackages; [ contrib pruviloj ])) gmp postgresql]; 

  src = ./.;

  installPhase = ''
    idris --build two-pc-idr.ipkg
    mkdir -p $out/bin
    cp twopc $out/bin/
  '';
} 
