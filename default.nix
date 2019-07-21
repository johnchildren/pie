{ mkDerivation, base, containers, fused-effects, haskeline
, hedgehog, megaparsec, prettyprinter, recursion-schemes, repline
, stdenv, tasty, tasty-hedgehog, tasty-hunit, text, transformers
}:
mkDerivation {
  pname = "pie";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers fused-effects megaparsec prettyprinter
    recursion-schemes text transformers
  ];
  executableHaskellDepends = [
    base fused-effects haskeline repline text transformers
  ];
  testHaskellDepends = [
    base fused-effects hedgehog tasty tasty-hedgehog tasty-hunit text
    transformers
  ];
  description = "Pie interpreter";
  license = stdenv.lib.licenses.bsd3;
}
