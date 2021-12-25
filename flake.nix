{
  description = "Purebred mailcap";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=266dc8c3d052f549826ba246d06787a219533b8f";
  };

  outputs = { self, nixpkgs }: {

    overlays = self: super: with super.haskell.lib; {
      haskellPackages = super.haskell.packages.ghc884.override {
        overrides = hself: hsuper: {
          purebred-mailcap = hsuper.callPackage ./.nix/purebred-mailcap.nix { };
        };
      };
    };

    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays ]; };
      let
        nativeBuildTools = with haskellPackages; [
          cabal-install
          cabal2nix
          ghcid
          hlint
          haskell-language-server
          ormolu
          hie-bios
        ];
      in haskellPackages.shellFor {
        withHoogle = true;
        packages = haskellPackages: [ haskellPackages.purebred-mailcap ];
        nativeBuildInputs = haskellPackages.purebred-mailcap.env.nativeBuildInputs ++ nativeBuildTools;
      };

      defaultPackage.x86_64-linux =
        with import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays ]; };
        haskellPackages.purebred-mailcap;

  };
}
