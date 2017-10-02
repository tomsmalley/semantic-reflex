{ mkDerivation, array, base, base-orphans, bifunctors, bytestring
, Cabal, cabal-doctest, comonad, containers, contravariant
, criterion, deepseq, directory, distributive, doctest, exceptions
, filepath, free, generic-deriving, ghc-prim, hashable, hlint
, HUnit, kan-extensions, mtl, nats, parallel, profunctors
, QuickCheck, reflection, semigroupoids, semigroups, simple-reflect
, stdenv, tagged, template-haskell, test-framework
, test-framework-hunit, test-framework-quickcheck2
, test-framework-th, text, th-abstraction, transformers
, transformers-compat, unordered-containers, vector, void
}:
mkDerivation {
  pname = "lens";
  version = "4.15.3";
  sha256 = "0znd63nkpdndpdgpvcwnqm31v4w2d1ipkj8lnnbsabbrhywknqd2";
  setupHaskellDepends = [ base Cabal cabal-doctest filepath ];
  libraryHaskellDepends = [
    array base base-orphans bifunctors bytestring comonad containers
    contravariant distributive exceptions filepath free ghc-prim
    hashable kan-extensions mtl parallel profunctors reflection
    semigroupoids semigroups tagged template-haskell text
    th-abstraction transformers transformers-compat
    unordered-containers vector void
  ];
  testHaskellDepends = [
    base bytestring containers deepseq directory doctest filepath
    generic-deriving hlint HUnit mtl nats parallel QuickCheck
    semigroups simple-reflect test-framework test-framework-hunit
    test-framework-quickcheck2 test-framework-th text transformers
    unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    base bytestring comonad containers criterion deepseq
    generic-deriving transformers unordered-containers vector
  ];
  homepage = "http://github.com/ekmett/lens/";
  description = "Lenses, Folds and Traversals";
  license = stdenv.lib.licenses.bsd2;
}
