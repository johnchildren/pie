{ mkDerivation, base, containers, hedgehog, megaparsec
, prettyprinter, recursion-schemes, repline, stdenv, tasty
, tasty-discover, tasty-hedgehog, tasty-hspec, text
}:
mkDerivation {
  pname = "pie";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers megaparsec prettyprinter recursion-schemes text
  ];
  executableHaskellDepends = [ base repline text ];
  testHaskellDepends = [
    base hedgehog tasty tasty-discover tasty-hedgehog tasty-hspec text
  ];
  testToolDepends = [ tasty-discover ];
  description = "Pie interpreter";
  license = stdenv.lib.licenses.bsd3;
}
