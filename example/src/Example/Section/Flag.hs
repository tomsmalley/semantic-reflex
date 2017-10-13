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
flags = LinkedSection "Flag" (text "A flag is used to represent a political state") $ do

  ui $ Paragraph $ do
    text "For available flag types, see "
    ui $ Anchor (text "the Semantic UI docs") $ def
      & href |?~ "https://semantic-ui.com/elements/flag.html"
    text "."

  hscode $ $(printDefinition id id ''Flag)

  exampleCard "Flag" "" [mkExample|
  mapM_ (ui . flip Flag def . Static . T.toLower . T.pack . show) [minBound .. maxBound :: CountryEnum]
  |]

