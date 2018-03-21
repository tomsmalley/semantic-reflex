(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    semantic-reflex = ./semantic-reflex;
    semantic-reflex-example = ./semantic-reflex-example;
  };

  overrides = self: super: {

    reflex-dom-contrib = self.callCabal2nix
      "reflex-dom-contrib"
      (pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-dom-contrib";
        rev = "88bfbf5df196c2207e50e88e78ae9b43af4be44b";
        sha256 = "19hdyijjwcwnqs8sw6gn5kdb92pyb6k9q2qbrk0cjm3nd93x60wm";
      }) {};

    reflex-dom-nested-routing = self.callCabal2nix
      "reflex-dom-nested-routing"
      (pkgs.fetchFromGitHub {
        owner = "3noch";
        repo = "reflex-dom-nested-routing";
        rev = "fb85e65fe89bc4fd62a2d0570ac27149e1c9cb86";
        sha256 = "135l988y3jvfhvds76mm58blfj9fr7i7qqd733j8vjywblblqjxi";
      }) {};

  };

  shells = {
    ghc = ["semantic-reflex"];
    ghcjs = ["semantic-reflex" "semantic-reflex-example"];
  };
})
