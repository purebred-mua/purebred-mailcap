{ nixpkgs ? null }:

with (import .nix/nixpkgs.nix { inherit nixpkgs; });

let
  env = [];
  nativeBuildTools = with pkgs.haskellPackages; [
    cabal-install
    cabal2nix
    ghcid
    hlint
    haskell-language-server
    ormolu
    hie-bios
  ];
in
if pkgs.lib.inNixShell
then haskellPackages.shellFor {
  withHoogle = true;
  packages = haskellPackages: [ haskellPackages.purebred-mailcap ];
  nativeBuildInputs = haskellPackages.purebred-mailcap.env.nativeBuildInputs ++ nativeBuildTools;
}
else {
  purebred-mailcap = pkgs.stdenv.mkDerivation {
    name = "purebred-mailcap";
    preferLocalBuild = true;
    allowSubstitutes = false;
  };
}
