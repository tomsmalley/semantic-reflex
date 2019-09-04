(import ./reflex-platform {}).project ({ pkgs, hackGet, ... }: {
  packages = {
    semantic-reflex = ./semantic-reflex;
    semantic-reflex-example = ./semantic-reflex-example;
    reflex-dom-nested-routing = hackGet ./deps/reflex-dom-nested-routing;
  };

  overrides = self: super: {

    # Haddock is broken in GHC 8.4's version:
    # https://github.com/haskell/haddock/issues/775
    semantic-reflex = pkgs.haskell.lib.dontHaddock super.semantic-reflex;

    reflex-dom-contrib = self.callCabal2nix
      "reflex-dom-contrib"
      (pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-dom-contrib";
        rev = "1265f7bba7a4c03cdb79c0798d03db18e200cec0";
        sha256 = "04xbliw43h2fmyhvpm7yhvx0bd8fnm5mxwrz4l0znmj7sfk21w3q";
      }) {};

  };

  shells =
    let mkShell = c: { name = c; value = ["semantic-reflex" "semantic-reflex-example"]; };
    in builtins.listToAttrs (map mkShell [
      "ghc"
      "ghc8_4"

      "ghcjs"
      "ghcjs8_4"
    ]);
})
