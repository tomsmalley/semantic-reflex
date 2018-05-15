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
iconSection = Section "Icon" blank $ do

  hscode $ $(printDefinition id stripParens ''IconConfig)
  hscode $ $(printDefinition id stripParens ''IconsConfig)

  pageHeader H3 def $ text "Icons"

  mkExample "Disabled" (def
    & subtitle ?~ text "An icon can show that it is disabled")
    [example|
  icon "users" $ def & iconConfig_disabled |~ True
  |]

  mkExample "Loading" (def
    & subtitle ?~ text "An icon can be used as a simple loader")
    [example|
  icon "spinner" $ def & iconConfig_loading |~ True
  icon "notched circle" $ def & iconConfig_loading |~ True
  icon "asterisk" $ def & iconConfig_loading |~ True
  |]

  mkExample "Fitted" (def
    & subtitle ?~ text "An icon can have no whitespace around it")
    [example|
  text "Fitted"
  icon "help" $ def & iconConfig_fitted |~ True
  text "Icon"
  |]

  mkExample "Size" (def
    & subtitle ?~ text "An icon can vary in size")
    [example|
  for_ [minBound .. maxBound] $ \s -> icon "home" $ def & iconConfig_size |?~ s
  |]

  mkExample "Link" (def
    & subtitle ?~ text "An icon can be formatted as a link")
    [example|
  icon "close" $ def & iconConfig_link |~ True
  icon "help" $ def & iconConfig_link |~ True
  |]

  mkExample "Flipped" (def
    & subtitle ?~ text "An icon can be flipped")
    [example|
  icon "thumbs outline up" def
  icon "thumbs outline up" $ def & iconConfig_flipped |?~ HorizontallyFlipped
  icon "thumbs outline up" $ def & iconConfig_flipped |?~ VerticallyFlipped
  |]

  mkExample "Rotated" (def
    & subtitle ?~ text "An icon can be rotated")
    [example|
  icon "smile" def
  icon "smile" $ def & iconConfig_rotated |?~ Clockwise
  icon "smile" $ def & iconConfig_rotated |?~ Anticlockwise
  |]

  mkExample "Circular" (def
    & subtitle ?~ text "An icon can be formatted to appear circular")
    [example|
  icon "users" $ def & iconConfig_circular |~ True
  icon "users" $ def & iconConfig_circular |~ True & iconConfig_color |?~ Teal
  icon "users" $ def & iconConfig_circular |~ True & iconConfig_inverted |~ True
  icon "users" $ def
    & iconConfig_circular |~ True & iconConfig_color |?~ Teal & iconConfig_inverted |~ True
  |]

  mkExample "Bordered" (def
    & subtitle ?~ text "An icon can be formatted to appear bordered")
    [example|
  icon "users" $ def & iconConfig_bordered |~ True
  icon "users" $ def & iconConfig_bordered |~ True & iconConfig_color |?~ Teal
  icon "users" $ def & iconConfig_bordered |~ True & iconConfig_inverted |~ True
  icon "users" $ def
    & iconConfig_bordered |~ True & iconConfig_color |?~ Teal & iconConfig_inverted |~ True
  |]

  mkExample "Colored" (def
    & subtitle ?~ text "An icon can be formatted with different colors")
    [example|
  for_ [minBound .. maxBound] $ \c -> icon "users" $ def & iconConfig_color |?~ c
  |]

  mkExample "Inverted" (def
    & subtitle ?~ text "An icon can have its colors inverted for contrast")
    [example|
  segment (def & segmentConfig_compact |~ True & segmentConfig_inverted |~ True) $ do
    for_ [minBound .. maxBound] $ \c -> icon "users" $ def
      & iconConfig_color |?~ c & iconConfig_inverted |~ True
  |]

  pageHeader H3 def $ text "Groups"

  mkExample "Icons" (def
    & inbetween ?~ upstreamIssue 5861 "There is an upstream issue causing icon groups to be off-centre."
    & subtitle ?~ text "Several icons can be used together as a group")
    [example|
  icons (def & iconsConfig_size |?~ Huge) $ do
    icon "circle" $ def & iconConfig_color |?~ Blue
    icon "announcement" $ def & iconConfig_inverted |~ True & iconConfig_size |?~ Tiny
  icons (def & iconsConfig_size |?~ Huge) $ do
    icon "thin circle" def
    icon "user" $ def & iconConfig_size |?~ Tiny
  icons (def & iconsConfig_size |?~ Huge) $ do
    icon "certificate" $ def
      & iconConfig_loading |~ True & iconConfig_color |?~ Grey & iconConfig_inverted |~ True
    icon "cloud download" $ def & iconConfig_size |?~ Tiny
  |]

  mkExample "Corner Icon" (def
    & subtitle ?~ text "A group of icons can display a smaller corner icon")
    [example|
  icons (def & iconsConfig_size |?~ Huge) $ do
    icon "line chart" def
    icon "dollar" $ def & iconConfig_corner |?~ TopLeft & iconConfig_color |?~ Green
  icons (def & iconsConfig_size |?~ Huge) $ do
    icon "heart" $ def & iconConfig_color |?~ Pink
    icon "plus" $ def & iconConfig_corner |?~ BottomRight
  |]

  return ()

