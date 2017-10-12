{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.RadioGroup where

import Control.Lens
import Control.Monad ((<=<), void, when, join)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

radioGroups :: forall t m. MonadWidget t m => Section m
radioGroups = LinkedSection "Radio Group" blank $ do

  {-

  hscode $ $(printDefinition id stripParens ''RadioGroup)
  hscode $ $(printDefinition id stripParens ''RadioGroupConfig)
  hscode $ $(printDefinition id stripParens ''RadioItem)
  hscode $ $(printDefinition id stripParens ''RadioItemConfig)

  divClass "ui two column stackable grid" $ do
    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn dynCode "Radio group" "" [mkExample|
        \resetEvent -> do
          let mkRadioItem x = RadioItem x (showFreq x) def
              freqencies = map mkRadioItem [minBound..maxBound]
          divClass "ui form" $ ui $ RadioGroup "frequency" freqencies $
            def & setValue .~ (Nothing <$ resetEvent)
        |]

      divClass "column" $ do
        exampleCardDyn dynCode "Slider group" "" [mkExample|
        \resetEvent -> do
          let mkRadioItem x = RadioItem x (showThroughput x) def
              throughputs = mkRadioItem <$> [Metered 20, Metered 10, Metered 5, Unmetered]
          divClass "ui form" $ ui $ RadioGroup "throughput" throughputs $
            def & initialValue ?~ Unmetered
                & setValue .~ (Just Unmetered <$ resetEvent)
                & altType ?~ Slider
        |]
  -}

  return ()

