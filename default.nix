{ reflex-platform ? ../reflex-platform/. , ghcjs ? false, runCC ? true }:
let

  dontCheckPackages = [
    "lens"
    "jsaddle-warp"
  ];

  pkgs = import <nixpkgs> { };

  makeOverrides = function: names: self: super:
    let toPackage = name: {
          inherit name;
          value = function super.${name};
        };
    in builtins.listToAttrs (map toPackage names);

  manualOverrides = self: super: rec {
  };

  composeExtensionsList = pkgs.lib.fold pkgs.lib.composeExtensions (_: _: { });
  doOverrides = haskellPackages: haskellPackages.override {
    overrides = composeExtensionsList [
      generatedOverrides
      (makeOverrides pkgs.haskell.lib.dontCheck dontCheckPackages)
      manualOverrides
    ];
  };
  generatedOverrides = self: super:
    let toPackage = file: _: {
          name = builtins.replaceStrings [ ".nix" ] [ "" ] file;
          value = self.callPackage (./. + "/nix/${file}") { };
        };
    in pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix);

  platform = import reflex-platform { };
  compiler = doOverrides (if ghcjs then platform.ghcjs else platform.ghc);

in rec {
  semantic-reflex = compiler.callPackage ./semantic-reflex { inherit ghcjs; };
  example = compiler.callPackage ./example { inherit ghcjs runCC semantic-reflex; };
}

