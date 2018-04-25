# Semantic UI for Reflex-DOM

This library aims to provide a type safe Haskell reimplementation of Semantic UI components, to allow easy construction of nice looking web applications in GHCJS. It started as a fork of the reflex-dom-semui library.

There are no dependencies on upstream JavaScript (either Semantic-UI or jQuery).

Currently only version 2.2 of the Semantic-UI CSS is supported.

## Building

The library uses the `project` implementation of `reflex-platform`.

    $ nix-shell -A shells.ghc
    [nix-shell]$ cabal new-configure --ghc
    [nix-shell]$ cabal new-build

---

## Examples

A compiled version of the code in the `example` folder is available in `docs/index.html` or online at https://tomsmalley.github.io/semantic-reflex/.

The example app can be run by GHC or GHCJS. For GHC it uses jsaddle-warp to run
a warp server, this is useful for development:

    $ nix-shell --attr env
    [nix-shell]$ ghcid -c 'stack ghci semantic-reflex semantic-reflex-example' -W -T Main.main

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
