{ mkDerivation
, ghc
, stdenv

, base
, bytestring
, containers
, data-default
, file-embed
, ghcjs-dom
, hspec
, jsaddle
, lens
, mtl
, QuickCheck
, reflex
, reflex-dom-core
, ref-tf
, text
, th-abstraction
, these
, time
, unbounded-delays

, jsaddle-warp
, wai-app-static
, warp
, websockets
}:
let ghcjs = ghc.isGhcjs or false;
in mkDerivation {
  pname = "semantic-reflex";
  version = "0.1";
  src = ./.;
  buildDepends = [
    base
    bytestring
    containers
    data-default
    file-embed
    ghcjs-dom
    hspec
    jsaddle
    lens
    mtl
    QuickCheck
    reflex
    reflex-dom-core
    ref-tf
    text
    th-abstraction
    these
    time
    unbounded-delays
  ] ++ (if ghcjs then [
  ] else [
    jsaddle-warp
    wai-app-static
    warp
    websockets
  ]);
  description = "A reflex-dom API for semantic-ui components";
  license = stdenv.lib.licenses.bsd3;
}
