(import ./reflex-platform {}).project ({ pkgs, hackGet, ... }: {
  packages = {
    semantic-reflex = ./semantic-reflex;
    semantic-reflex-example = ./semantic-reflex-example;
    reflex-dom-nested-routing = hackGet ./deps/reflex-dom-nested-routing;
    reflex-dom-contrib = hackGet ./deps/reflex-dom-contrib;
  };

  overrides = self: super: with pkgs.haskell.lib; {
    # Haddock is broken in GHC 8.4's version:
    # https://github.com/haskell/haddock/issues/775
    semantic-reflex = dontHaddock super.semantic-reflex;

    # https://github.com/reflex-frp/reflex-dom-contrib/pull/68
    reflex-dom-contrib = doJailbreak super.reflex-dom-contrib;
  };

  shells =
    let mkShell = c: { name = c; value = ["semantic-reflex" "semantic-reflex-example"]; };
    in builtins.listToAttrs (map mkShell [
      "ghc"
      "ghc8_6"

      "ghcjs"
      "ghcjs8_6"
    ]);
})
