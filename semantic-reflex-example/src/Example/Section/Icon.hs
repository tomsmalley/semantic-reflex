{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Icon where

import Control.Lens hiding (flipped)
import Data.Foldable (for_)
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)

import Example.QQ
import Example.Common

iconSection :: MonadWidget t m => Section t m
iconSection = LinkedSection "Icon" blank $ do

  hscode $ $(printDefinition id stripParens ''IconConfig)
  hscode $ $(printDefinition id stripParens ''IconsConfig)

  pageHeader H3 def $ text "Icons"

  mkExample "Disabled" (def
    & subtitle ?~ text "An icon can show that it is disabled")
    [example|
  icon "users" $ def & iconDisabled |~ True
  |]

  mkExample "Loading" (def
    & subtitle ?~ text "An icon can be used as a simple loader")
    [example|
  icon "spinner" $ def & iconLoading |~ True
  icon "notched circle" $ def & iconLoading |~ True
  icon "asterisk" $ def & iconLoading |~ True
  |]

  mkExample "Fitted" (def
    & subtitle ?~ text "An icon can have no whitespace around it")
    [example|
  text "Fitted"
  icon "help" $ def & iconFitted |~ True
  text "Icon"
  |]

  mkExample "Size" (def
    & subtitle ?~ text "An icon can vary in size")
    [example|
  for_ [minBound .. maxBound] $ \s -> icon "home" $ def & iconSize |?~ s
  |]

  mkExample "Link" (def
    & subtitle ?~ text "An icon can be formatted as a link")
    [example|
  icon "close" $ def & iconLink |~ True
  icon "help" $ def & iconLink |~ True
  |]

  mkExample "Flipped" (def
    & subtitle ?~ text "An icon can be flipped")
    [example|
  icon "thumbs outline up" def
  icon "thumbs outline up" $ def & iconFlipped |?~ HorizontallyFlipped
  icon "thumbs outline up" $ def & iconFlipped |?~ VerticallyFlipped
  |]

  mkExample "Rotated" (def
    & subtitle ?~ text "An icon can be rotated")
    [example|
  icon "smile" def
  icon "smile" $ def & iconRotated |?~ Clockwise
  icon "smile" $ def & iconRotated |?~ Anticlockwise
  |]

  mkExample "Circular" (def
    & subtitle ?~ text "An icon can be formatted to appear circular")
    [example|
  icon "users" $ def & iconCircular |~ True
  icon "users" $ def & iconCircular |~ True & iconColor |?~ Teal
  icon "users" $ def & iconCircular |~ True & iconInverted |~ True
  icon "users" $ def
    & iconCircular |~ True & iconColor |?~ Teal & iconInverted |~ True
  |]

  mkExample "Bordered" (def
    & subtitle ?~ text "An icon can be formatted to appear bordered")
    [example|
  icon "users" $ def & iconBordered |~ True
  icon "users" $ def & iconBordered |~ True & iconColor |?~ Teal
  icon "users" $ def & iconBordered |~ True & iconInverted |~ True
  icon "users" $ def
    & iconBordered |~ True & iconColor |?~ Teal & iconInverted |~ True
  |]

  mkExample "Colored" (def
    & subtitle ?~ text "An icon can be formatted with different colors")
    [example|
  for_ [minBound .. maxBound] $ \c -> icon "users" $ def & iconColor |?~ c
  |]

  mkExample "Inverted" (def
    & subtitle ?~ text "An icon can have its colors inverted for contrast")
    [example|
  segment (def & segmentCompact |~ True & segmentInverted |~ True) $ do
    for_ [minBound .. maxBound] $ \c -> icon "users" $ def
      & iconColor |?~ c & iconInverted |~ True
  |]

  pageHeader H3 def $ text "Groups"

  mkExample "Icons" (def
    & inbetween ?~ upstreamIssue 5861 "There is an upstream issue causing icon groups to be off-centre."
    & subtitle ?~ text "Several icons can be used together as a group")
    [example|
  icons (def & iconsSize |?~ Huge) $ do
    icon "circle" $ def & iconColor |?~ Blue
    icon "announcement" $ def & iconInverted |~ True & iconSize |?~ Tiny
  icons (def & iconsSize |?~ Huge) $ do
    icon "thin circle" def
    icon "user" $ def & iconSize |?~ Tiny
  icons (def & iconsSize |?~ Huge) $ do
    icon "certificate" $ def
      & iconLoading |~ True & iconColor |?~ Grey & iconInverted |~ True
    icon "cloud download" $ def & iconSize |?~ Tiny
  |]

  mkExample "Corner Icon" (def
    & subtitle ?~ text "A group of icons can display a smaller corner icon")
    [example|
  icons (def & iconsSize |?~ Huge) $ do
    icon "line chart" def
    icon "dollar" $ def & iconCorner |?~ TopLeft & iconColor |?~ Green
  icons (def & iconsSize |?~ Huge) $ do
    icon "heart" $ def & iconColor |?~ Pink
    icon "plus" $ def & iconCorner |?~ BottomRight
  |]

  return ()

