{ mkDerivation, attoparsec, base, lib }:
mkDerivation {
  pname = "purebred-mailcap";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ attoparsec base ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
