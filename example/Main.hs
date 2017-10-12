{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Example

#ifndef ghcjs_HOST_OS

import Reflex.Dom.SemanticUI.Warp
import Data.ByteString
import Data.FileEmbed

#endif

main :: IO ()

#ifdef ghcjs_HOST_OS

main = example

#else

port :: Int
port = 3708

css :: ByteString
css = $(embedFile =<< makeRelativeToProject "resources/styling.css")

static :: Maybe FilePath
static = Just $(strToExp =<< makeRelativeToProject "resources")

-- | Start the warp server
main = server port css example static

-- | Restart the warp server and tell any connected clients to refresh
debug :: IO ()
debug = daemon port css example static

#endif
