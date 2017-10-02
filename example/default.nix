{ mkDerivation, base, bytestring, containers, data-default
, file-embed, ghc-prim, ghcjs-dom, haskell-src-exts
, haskell-src-meta, hscolour, jsaddle, jsaddle-warp, lens, mtl
, reflex, reflex-dom-core, semantic-reflex, stdenv, template-haskell, text, these
, wai, wai-app-static, warp, websockets, ghcjs
, closurecompiler, runCC
}:
mkDerivation rec {
  pname = "example";
  version = "0.1";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path)
    [ "makedocs.sh" "dist" "dist-newstyle" ]
  )) ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base
    bytestring
    containers
    data-default
    file-embed
    ghcjs-dom
    haskell-src-exts
    haskell-src-meta
    hscolour
    jsaddle
    lens
    mtl
    reflex
    reflex-dom-core
    semantic-reflex
    template-haskell
    text
  ] ++ (if ghcjs then [
  ] else [
    jsaddle-warp
    wai-app-static
    warp
    websockets
  ]);
  description = "A reflex-dom API for semantic-ui components";
  license = stdenv.lib.licenses.bsd3;
  buildTools = [ closurecompiler ];
  postInstall = stdenv.lib.optionalString ghcjs (''
    mkdir $out/dist;
    mkdir $out/dist/js;
    cp -r ${semantic-reflex}/share/*/*/lib/dist/* $out/dist;
  '' + (if runCC then (''
    echo Running closure compiler...;
  '' + builtins.replaceStrings ["\n"] [" "] ''
      closure-compiler
      --js_output_file $out/dist/js/all.js
      --externs=$out/bin/${pname}.jsexe/all.js.externs
      --externs=${semantic-reflex}/share/*/*/lib/jquery.js.externs
      $out/bin/${pname}.jsexe/all.js
      -O ADVANCED;
  '') else (''
    cp $out/bin/${pname}.jsexe/all.js $out/dist/js/all.js
  '')) + ''
  echo GHCJS build output in result/dist;
  '');
}
