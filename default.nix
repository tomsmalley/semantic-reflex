{ reflex-platform ? ../reflex-platform/. , ghcjs ? false, runCC ? true }:
let

  dontCheckPackages = [
    "lens"
  ];

  pkgs = import <nixpkgs> { };
  platform = import reflex-platform { };

  makeOverrides = function: names: self: super:
    let toPackage = name: {
          inherit name;
          value = function super.${name};
        };
    in builtins.listToAttrs (map toPackage names);

  manualOverrides = self: super: rec {
    semantic-reflex = self.callPackage ./semantic-reflex { inherit ghcjs; };
    example = self.callPackage ./example { inherit semantic-reflex; inherit ghcjs; };
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

in if ghcjs
    then (doOverrides platform.ghcjs).callPackage ./example { inherit ghcjs runCC; }
    else (doOverrides platform.ghc).callPackage ./example { inherit ghcjs runCC; }
