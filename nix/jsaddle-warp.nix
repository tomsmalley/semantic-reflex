{ mkDerivation, aeson, base, bytestring, containers, deepseq
, doctest, filepath, foreign-store, ghc-prim, http-types, jsaddle
, lens, network, primitive, process, QuickCheck, ref-tf, stdenv
, stm, text, time, transformers, wai, wai-websockets, warp
, webdriver, websockets
}:
mkDerivation {
  pname = "jsaddle-warp";
  version = "0.9.5.0";
  sha256 = "18rvs0m8407piavqvv95dp4bfcgn73c22xjcb75fax0bhf0s6aak";
  libraryHaskellDepends = [
    aeson base bytestring containers foreign-store http-types jsaddle
    stm text time transformers wai wai-websockets warp websockets
  ];
  testHaskellDepends = [
    aeson base bytestring containers deepseq doctest filepath ghc-prim
    http-types jsaddle lens network primitive process QuickCheck ref-tf
    stm text time transformers wai wai-websockets warp webdriver
    websockets
  ];
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
