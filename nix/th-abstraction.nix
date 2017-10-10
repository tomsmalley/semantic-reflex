{ mkDerivation, base, containers, ghc-prim, stdenv
, template-haskell
}:
mkDerivation {
  pname = "th-abstraction";
  version = "0.2.6.0";
  sha256 = "0g42h6wnj2awc5ryhbvx009wd8w75pn66bjzsq1z4s3xajd2hbp5";
  libraryHaskellDepends = [
    base containers ghc-prim template-haskell
  ];
  testHaskellDepends = [ base containers template-haskell ];
  homepage = "https://github.com/glguy/th-abstraction";
  description = "Nicer interface for reified information about data types";
  license = stdenv.lib.licenses.isc;
}
