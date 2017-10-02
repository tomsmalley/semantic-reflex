[![Build Status](https://travis-ci.org/reflex-frp/reflex-dom-semui.svg?branch=master)](https://travis-ci.org/reflex-frp/reflex-dom-semui)

This package provides a Reflex wrapper around the Semantic UI components.  It is very
incomplete and was derived from [code written for
hsnippet](https://github.com/mightybyte/hsnippet/blob/64cc17d2bf2bcce219f3ab8e96b7fd6071d5b56b/frontend/src/SemanticUI.hs).
This is also intended to serve as an example of how to structure FFI packages
that rely on external JS packages.

To build this library locally use reflex-platform's work-on script as follows:

    ~/reflex-platform/work-on ghcjs ./reflex-dom-semui.nix

This puts you into a nix shell that has GHCJS and the environment it needs.
Then you can build with:

    cabal configure --ghcjs
    cabal build

Alternatively you can use default.nix to get into a nix-shell or build the
project:

    $ nix-shell --attr ghc.env
    $ nix-shell --attr ghcjs.env

    $ nix-build --attr ghc
    $ nix-build --attr ghcjs

reflex-platform is expected to be in `../reflex-platform`, if it is elsewhere
you can pass `--arg reflex-platform /path/to/reflex-platform`.

---

A compiled version of the code in the `example` folder is available in `docs/index.html` or online at https://reflex-frp.github.io/reflex-dom-semui/.

The example app can be run by ghc or ghcjs. For ghc:

    $ nix-shell --attr ghc.env
    $ cabal configure
    $ cabal run

This will print the url of the warp server where the app is running. For ghcjs:

    $ nix-build --attr ghcjs

Navigate to `./result/bin/example.jsexe/index.html` to see the app.

---

If you have changed the example and are submitting a pull request you should
update the `docs` folder with the updated javascript. Build the example app with
ghcjs and run `./makedocs.sh` to do this.

---

# Rapid development

Once in a ghc nix-shell:
```
ghcid "--command=stack ghci --system-ghc example" "--test=Main.debug" --warnings
```

