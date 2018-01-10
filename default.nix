{ runCC ? true }:

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
#    semantic-reflex = ./semantic-reflex;
#    example = ./example/default.nix;
  };

  overrides = self: super: {
    semantic-reflex = self.callPackage ./semantic-reflex/default.nix { };
    semantic-reflex-example = self.callPackage ./semantic-reflex-example/default.nix { inherit runCC; };
  };

  shells = {
    ghc = ["semantic-reflex" "semantic-reflex-example"];
    ghcjs = ["semantic-reflex" "semantic-reflex-example"];
  };
})
