{ pkgs ? import <nixpkgs> {}
, hp ? pkgs.haskellPackages
, src ? ./.
}:

hp.cabal.mkDerivation (self: {
  pname = "data-lens";
  version = "2.10.5";
  inherit src;
  buildDepends = [ hp.comonad hp.semigroupoids hp.transformers ];
})
