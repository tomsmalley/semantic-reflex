(import ./obelisk {}).reflex-platform.project ({ pkgs, ... }: {
  packages = {
    semantic-reflex = ./semantic-reflex;
    semantic-reflex-example = ./semantic-reflex-example;
  };

  overrides = self: super: {

    # Haddock is broken in GHC 8.4's version:
    # https://github.com/haskell/haddock/issues/775
    semantic-reflex = pkgs.haskell.lib.dontHaddock super.semantic-reflex;

  };

  shells =
    let mkShell = c: { name = c; value = ["semantic-reflex" "semantic-reflex-example"]; };
    in builtins.listToAttrs (map mkShell [
      "ghc"
      "ghc8_4"
      "ghc8_2"
      "ghc8_0"
      "ghc7"

      "ghcjs"
      "ghcjs8_4"
      "ghcjs8_2"
      "ghcjs8_0"
    ]);
})
