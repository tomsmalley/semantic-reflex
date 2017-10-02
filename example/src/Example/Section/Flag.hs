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
flags = LinkedSection "Flag" "A flag is used to represent a political state" $ do

  el "p" $ do
    text "For available flag types, see "
    elAttr "a" ("href" =: "https://semantic-ui.com/elements/flag.html")
      $ text "the Semantic UI docs"
    text "."

  $(printDefinition id ''Flag)

  exampleCard "Flag" "" [mkExample|
  mapM_ (ui . Flag . Static . T.toLower . T.pack . show) [minBound .. maxBound :: CountryEnum]
  |]

