{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Example

#ifndef ghcjs_HOST_OS

import Reflex.Dom.SemanticUI.Warp
import Data.ByteString
import Data.FileEmbed

#endif

main :: IO ()

#ifdef ghcjs_HOST_OS

main = Example.main

#else

port :: Int
port = 8080

css :: ByteString
css = $(embedFile =<< makeRelativeToProject "resources/styling.css")

static :: Maybe FilePath
static = Just $(strToExp =<< makeRelativeToProject "resources")

-- | Start the warp server
main = server port css Example.main static

-- | Restart the warp server and tell any connected clients to refresh
debug :: IO ()
debug = daemon port css Example.main static

#endif
