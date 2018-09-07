{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Example
#ifdef ghcjs_HOST_OS
import Reflex.Dom
#else
import Language.Javascript.JSaddle.Warp (run)
#endif

main :: IO ()
main =
#ifndef ghcjs_HOST_OS
  run 3911 $
#endif
    Example.main
