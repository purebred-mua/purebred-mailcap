{ mkDerivation, attoparsec, base, bytestring, case-insensitive, lib
, tasty, tasty-hunit, text, time
}:
mkDerivation {
  pname = "purebred-mailcap";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring case-insensitive text
  ];
  executableHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    attoparsec base tasty tasty-hunit text time
  ];
  description = "A parser for parsing mailcap (RFC1524) files";
  license = lib.licenses.agpl3Plus;
}
