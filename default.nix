{ runCC ? true }:

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    semantic-reflex = ./semantic-reflex;
    example = ./example;
  };

  shells = {
    ghc = ["semantic-reflex" "example"];
    ghcjs = ["semantic-reflex" "example"];
  };
})
