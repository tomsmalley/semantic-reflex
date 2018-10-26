# Semantic UI for Reflex-DOM [![Build Status](https://travis-ci.com/tomsmalley/semantic-reflex.svg?branch=master)](https://travis-ci.com/tomsmalley/semantic-reflex)

This library aims to provide a type safe Haskell reimplementation of Semantic UI components, to allow easy construction of nice looking web applications in GHCJS. It started as a fork of the reflex-dom-semui library.

There are no dependencies on upstream JavaScript (either Semantic-UI or jQuery).

Semantic-UI CSS support:

 | Version | Supported | Notes                                   |
 | ------- | --------- | --------------------------------------- |
 | 2.2     | Yes       | Zoom/Glow transitions will not work     |
 | 2.3     | Yes       | [Changes](https://semantic-ui.com/introduction/new.html#/twothree) |

## Building

The library uses the `project` implementation of `reflex-platform`.

    $ nix-shell -A shells.ghc
    [nix-shell]$ cabal new-configure --ghc
    [nix-shell]$ cabal new-build

---

## Examples

A compiled version of the code in the `semantic-reflex-example` folder is available online at https://tomsmalley.github.io/semantic-reflex/.

The example app can be run by GHC or GHCJS. For GHC it uses jsaddle-warp to run
a warp server, this is useful for development:

    $ ./repl

This script will reload when either the library or examples change. Provided everything compiles, you should be able to point your browser at `localhost:3911` and see the examples.

For compiling for GHCJS just run `make`, and the resulting `all.js` will be copied into the
`docs` folder.
