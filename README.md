# Introduction

This library aims to provide a type safe Haskell wrapper around Semantic UI components, to allow easy construction of nice looking web applications in GHCJS. It is currently in early development and started as a fork of the reflex-dom-semui library.

## Building

To work on the library use nix-shell and cabal, or nix-build. For GHC:

    $ nix-shell --attr env
    [nix-shell]$ cabal new-configure --ghc
    [nix-shell]$ cabal new-build

    $ nix-build

And for GHCJS:

    $ nix-shell --attr env --arg ghcjs true
    [nix-shell]$ cabal new-configure --ghcjs
    [nix-shell]$ cabal new-build

    $ nix-build --arg ghcjs true

reflex-platform is expected to be in `../reflex-platform`, if it is elsewhere
you can pass `--arg reflex-platform /path/to/reflex-platform`. All commands are
expected to be executed from the projects root directory (where this readme is located).

---

A compiled version of the code in the `example` folder is available in `docs/index.html` or online at https://tomsmalley.github.io/semantic-reflex/.

The example app can be run by GHC or GHCJS. For GHC it uses jsaddle-warp to run
a warp server, this is useful for development:

    $ nix-shell --attr env
    [nix-shell]$ cabal new-configure --ghc
    [nix-shell]$ ghcid "--command=stack ghci --system-ghc example" "--test=Main.debug" --warnings

We use stack ghci to load multiple targets, so we don't have to reload ghci when
working on the library and examples at the same time.
This will print the url of the warp server where the app is running.

For compiling for GHCJS just run `make`, and the result will be copied into the
`docs` folder.

---

If you have changed the example and are submitting a pull request you should
update the `docs` folder with the updated javascript. Run `make` before
committing!

---
