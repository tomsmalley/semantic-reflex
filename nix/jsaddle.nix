{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, bytestring, containers, deepseq, filepath, ghc-prim, http-types
, lens, primitive, process, random, ref-tf, scientific, stdenv, stm
, text, time, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "jsaddle";
  version = "0.9.4.0";
  sha256 = "0lk4cbvl2n3zcc709hjcnxw3wm1vd49dqlm12cwy9im4aif1zbq1";
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring bytestring containers
    deepseq filepath ghc-prim http-types lens primitive process random
    ref-tf scientific stm text time transformers unordered-containers
    vector
  ];
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
