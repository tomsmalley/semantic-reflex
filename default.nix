{ runCC ? true }:

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
#    semantic-reflex = ./semantic-reflex;
#    example = ./example/default.nix;
  };

  overrides = self: super: {
    semantic-reflex = self.callPackage ./semantic-reflex/default.nix { };
    example = self.callPackage ./example/default.nix { inherit runCC; };
  };

  shells = {
    ghc = ["semantic-reflex" "example"];
    ghcjs = ["semantic-reflex" "example"];
  };
})
