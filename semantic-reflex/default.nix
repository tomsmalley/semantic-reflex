{ mkDerivation, base, bytestring, containers, data-default
, file-embed, ghcjs-dom, jsaddle, lens, mtl, reflex
, reflex-dom-core, stdenv, text, these, jsaddle-warp, wai-app-static, warp,
websockets, ghcjs
}:
mkDerivation {
  pname = "semantic-reflex";
  version = "0.1";
  src = ./.;
  buildDepends = [
    base bytestring containers data-default file-embed ghcjs-dom
    jsaddle lens mtl reflex reflex-dom-core text these
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
