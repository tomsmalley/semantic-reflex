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
import Data.Foldable (for_)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)

import Example.QQ
import Example.Common
import Example.CountryEnum

flags :: MonadWidget t m => Section t m
flags = Section "Flag" (text "A flag is used to represent a political state") $ do

  paragraph $ do
    text "For available flag types, see "
    let url = "https://semantic-ui.com/elements/flag.html"
    hyperlink url $ text "the Semantic UI docs"
    text "."

  mkExample "Flag" (def
    & subtitle ?~ text "A flag can use the two digit country code, the full name, or a common alias")
    [example|
  for_ [minBound .. maxBound :: CountryEnum] $
    flip flag def . pure . T.toLower . T.pack . show
  |]

  return ()

