{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Flag where

import Control.Lens
import Control.Monad ((<=<), void, when, join)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common
import Example.CountryEnum

flags :: MonadWidget t m => Section m
flags = LinkedSection "Flag" (ui $ Text "A flag is used to represent a political state") $ do

  ui $ Paragraph $ do
    ui $ Text "For available flag types, see "
    ui $ Anchor (ui $ Text "the Semantic UI docs") $ def
      & href |?~ "https://semantic-ui.com/elements/flag.html"
    ui $ Text "."

  hscode $ $(printDefinition id id ''Flag)

  exampleCard "Flag" "" [mkExample|
  mapM_ (ui . flip Flag def . Static . T.toLower . T.pack . show) [minBound .. maxBound :: CountryEnum]
  |]

