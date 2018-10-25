(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    semantic-reflex = ./semantic-reflex;
    semantic-reflex-example = ./semantic-reflex-example;
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
        rev = "9900f2d433240a3f93cdae930a6ffbb73c50bb86";
        sha256 = "1z8cnnhibsiap08pq2iw1r5zqvbla6hci7dhrz9mhfr0nqyryk65";
      }) {};

    reflex-dom-nested-routing = self.callCabal2nix
      "reflex-dom-nested-routing"
      (pkgs.fetchFromGitHub {
        owner = "3noch";
        repo = "reflex-dom-nested-routing";
        rev = "c49c75c693de8516d1b19314be500482bea9426c";
        sha256 = "00bmakqm9893h8l3w7l1r1fjkpyffifcaicqmj2q5wwlfvm96hbf";
      }) {};

  };

  shells = {
    ghc = ["semantic-reflex" "semantic-reflex-example"];
    ghcjs = ["semantic-reflex" "semantic-reflex-example"];
  };
})
