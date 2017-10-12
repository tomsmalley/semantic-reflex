{ mkDerivation, base, bytestring, containers, data-default
, file-embed, ghcjs-dom, jsaddle, lens, mtl, reflex
, QuickCheck, hspec
, reflex-dom-core, ref-tf, stdenv, template-haskell, text, th-abstraction, these, time, unbounded-delays, jsaddle-warp, wai-app-static, warp
, websockets, ghcjs
}:
mkDerivation {
  pname = "semantic-reflex";
  version = "0.1";
  src = ./.;
  buildDepends = [
    base bytestring containers data-default file-embed ghcjs-dom
    hspec jsaddle lens mtl QuickCheck reflex reflex-dom-core ref-tf template-haskell text th-abstraction these
    time unbounded-delays
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
