{ mkDerivation, aeson, base, bytestring, deepseq, exceptions
, filepath, genvalidity, genvalidity-hspec, hashable, hspec, lib
, QuickCheck, template-haskell, text, validity-bytestring
}:
mkDerivation {
  pname = "path";
  version = "0.9.6";
  src = ./.;
  libraryHaskellDepends = [
    aeson base deepseq exceptions filepath hashable template-haskell
    text
  ];
  testHaskellDepends = [
    aeson base bytestring exceptions filepath genvalidity
    genvalidity-hspec hspec QuickCheck template-haskell
    validity-bytestring
  ];
  doHaddock = false;
  description = "Support for well-typed paths";
  license = lib.licenses.bsd3;
}
