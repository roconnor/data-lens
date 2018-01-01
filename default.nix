{ pkgs ? import <nixpkgs> {}
, hp ? pkgs.haskellPackages
}:

hp.callPackage
 ({ mkDerivation, comonad, semigroupoids }:
  mkDerivation {
   pname = "data-lens";
   version = "2.11.2";
   src = pkgs.lib.sourceFilesBySuffices ./. [".hs" ".cabal" "CHANGELOG" "LICENSE"];
   buildDepends = [ comonad semigroupoids ];
   license = pkgs.lib.licenses.bsd3;
 }) {}
