{ mkDerivation, aeson, ansi-terminal, array, base, binary
, bytestring, Cabal-syntax, containers, deepseq, Diff, directory
, file-embed, filepath, ghc-lib-parser, hspec, hspec-discover
, hspec-megaparsec, lib, megaparsec, MemoTrie, mtl
, optparse-applicative, path, path-io, pretty, process, QuickCheck
, scientific, syb, temporary, text, th-env, yaml
}:
mkDerivation {
  pname = "fourmolu";
  version = "0.13.0.0";
  sha256 = "5102c41c159f41901cde28728d8f4e9feb443f3c4c8798a5c21ba78872fda257";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-terminal array base binary bytestring Cabal-syntax
    containers deepseq Diff directory file-embed filepath
    ghc-lib-parser megaparsec MemoTrie mtl scientific syb text yaml
  ];
  executableHaskellDepends = [
    base Cabal-syntax containers directory filepath ghc-lib-parser
    optparse-applicative text th-env yaml
  ];
  testHaskellDepends = [
    base Cabal-syntax containers Diff directory filepath ghc-lib-parser
    hspec hspec-megaparsec megaparsec path path-io pretty process
    QuickCheck temporary text
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/fourmolu/fourmolu";
  description = "A formatter for Haskell source code";
  license = lib.licenses.bsd3;
  mainProgram = "fourmolu";
}
