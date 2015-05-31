{ pkgs ? import <nixpkgs> {}
, hp ? pkgs.haskellPackages
}:

hp.cabal.mkDerivation (self: {
  pname = "data-lens";
  version = "2.10.6";
  src = pkgs.lib.sourceFilesBySuffices ./. [".hs" ".cabal" "CHANGELOG" "LICENSE"];
  buildDepends = [ hp.comonad hp.semigroupoids ];
})
