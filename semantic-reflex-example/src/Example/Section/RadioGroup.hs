{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}

module Example.Section.RadioGroup where

import Control.Lens
import Control.Monad ((<=<), void, when, join)
import Data.Text (Text)
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core

import qualified Data.Map as M
import qualified Data.Text as T

import Example.QQ
import Example.CountryEnum
import Example.Common

radioGroups :: forall t m. MonadWidget t m => Section t m
radioGroups = Section "Radio Group" blank $ do
{-
  divClass "ui two column stackable grid" $ do
    divClass "row" $ do

      divClass "column" $ do
        mkExample "Radio group" (def
          & dynamic ?~ dynCode
          & subtitle ?~ text "")
          [resetExample|
        \resetEvent -> do
          let countries = [minBound .. maxBound] :: [CountryEnum]
              render = pure $ M.fromList
                [ (c, flag (pure $ T.toLower $ tshow c) def >> text (countryText c))
                | c <- countries ]
          radioGroup "test" (def :: RadioGroupConfig t Maybe CountryEnum) render
        |]

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

