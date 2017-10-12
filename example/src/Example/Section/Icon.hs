{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Icon where

import Control.Lens
import Control.Monad ((<=<), void, when, join)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

icons :: MonadWidget t m => Section m
icons = LinkedSection "Icon" blank $ do

  hscode $ $(printDefinition id stripParens ''Icon)
  hscode $ $(printDefinition id stripParens ''IconConfig)
  hscode $ $(printDefinition id stripParens ''IconsConfig)

  ui $ Header H3 (ui $ Text "Groups") def

  divClass "ui equal width stackable grid" $ do

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Icons" "Several icons can be used together as a group" [mkExample|
        ui $ Icons
          [ Icon "circle" $ def & size |?~ Big & color |?~ Blue
          , Icon "car" $ def & inverted |~ True
          ] $ def & size |?~ Huge
        ui $ Icons
          [ Icon "thin circle" $ def & size |?~ Big
          , Icon "user" def
          ] $ def & size |?~ Huge
        ui $ Icons
          [ Icon "certificate" $ def
              & size |?~ Big & loading |~ True
              & color |?~ Grey & inverted |~ True
          , Icon "cloud download" def
          ] $ def & size |?~ Huge
        |]

      divClass "column" $ do
        exampleCard "Corner Icon" "A group of icons can display a smaller corner icon"
          [mkExample|
        ui_ $ Header H2 (ui $ Text "Add on Twitter") $ def
          & icon .~ AlwaysRender (Icons
              [ Icon "twitter" def
              , Icon "corner add" $ def & inverted |~ True
              ] (def & size |?~ Large) )
        |]

