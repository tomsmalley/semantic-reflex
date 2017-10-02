{-# LANGUAGE TemplateHaskell #-}

-- | This module is to help with fast development cycles. Use 'server' or
-- 'daemon' to start a warp server on the given port where you can see your
-- app.
module Reflex.Dom.SemanticUI.Warp where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Control.Lens ((^.))
import Control.Monad (void)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.WebSockets
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.Wai
import Network.WebSockets

import Data.FileEmbed

-- | Start the warp server
server :: Int -> ByteString -> JSM () -> IO ()
server port css app = do
  putStrLn $ "starting warp server: http://localhost:" ++ show port
  runApp css port app id $ return ()

-- | Restart the warp server and tell any connected clients to refresh
daemon :: Int -> ByteString -> JSM () -> IO ()
daemon port css app = do
  debugWrapper (runApp css port app)
  putStrLn $ "restarting warp server: http://localhost:" ++ show port

-- | We serve the standard jsaddle-warp app but with a static server for the
-- javascript, css, and theme folder.
runApp :: ByteString -> Int -> JSM () -> Middleware -> JSM ()-> IO ()
runApp css port mainApp middleware preApp = do
  jsaddle <- jsaddleWithAppOr defaultConnectionOptions
    (preApp >> app) (middleware static)
  runSettings settings jsaddle
  where
    settings = setPort port $ setTimeout 3600 $ defaultSettings
    static = staticApp $ defaultFileServerSettings dir
    app = makeHead css >> mainApp >> syncPoint
    dir = $(strToExp =<< makeRelativeToProject "lib/dist")

-- Needed for non js targets, since the js-sources in the cabal file are not
-- linked
makeHead :: ByteString -> JSM ()
makeHead css = do

  document <- jsg "document"

  -- Push the css into a style tag
  style <- document ^. js1 "createElement" "style"
  style ^. jss "innerText" (unpack css)
  void $ document ^. js "head" ^. js1 "appendChild" style

  semanticCSS <- document ^. js1 "createElement" "link"
  semanticCSS ^. jss "rel" "stylesheet"
  semanticCSS ^. jss "type" "text/css"
  semanticCSS ^. jss "href" "semantic.min.css"
  void $ document ^. js "head" ^. js1 "appendChild" semanticCSS

  jquery <- document ^. js1 "createElement" "script"
  jquery ^. jss "src" "js/jquery.min.js"
  void $ document ^. js "head" ^. js1 "appendChild" jquery

  semantic <- document ^. js1 "createElement" "script"
  semantic ^. jss "src" "js/semantic.min.js"
  void $ document ^. js "head" ^. js1 "appendChild" semantic
