{ mkDerivation, attoparsec, base, lib, tasty, tasty-hunit, text
, time
}:
mkDerivation {
  pname = "purebred-mailcap";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ attoparsec base text ];
  executableHaskellDepends = [ base text ];
  testHaskellDepends = [
    attoparsec base tasty tasty-hunit text time
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
