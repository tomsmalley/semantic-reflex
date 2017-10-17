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

import Example.QQ
import Example.Common

icons :: MonadWidget t m => Section t m
icons = LinkedSection "Icon" blank $ do

  hscode $ $(printDefinition id stripParens ''Icon)
  hscode $ $(printDefinition id stripParens ''IconConfig)
  hscode $ $(printDefinition id stripParens ''IconsConfig)

  ui $ PageHeader H3 def $ text "Icons"

  ui $ Example "Disabled" (def
    & subtitle ?~ text "An icon can show that it is disabled")
    [example|
  ui $ Icon "users" $ def & disabled |~ True
  |]

  ui $ Example "Loading" (def
    & subtitle ?~ text "An icon can be used as a simple loader")
    [example|
  ui $ Icon "spinner" $ def & loading |~ True
  ui $ Icon "notched circle" $ def & loading |~ True
  ui $ Icon "asterisk" $ def & loading |~ True
  |]

  ui $ Example "Fitted" (def
    & subtitle ?~ text "An icon can have no whitespace around it")
    [example|
  text "Fitted"
  ui $ Icon "help" $ def & fitted |~ True
  text "Icon"
  |]

  ui $ Example "Size" (def
    & subtitle ?~ text "An icon can vary in size")
    [example|
  for_ [minBound .. maxBound] $ \s -> ui $ Icon "home" $ def & size |?~ s
  |]

  ui $ Example "Link" (def
    & subtitle ?~ text "An icon can be formatted as a link")
    [example|
  ui $ Icon "close" $ def & link |~ True
  ui $ Icon "help" $ def & link |~ True
  |]

  ui $ Example "Flipped" (def
    & subtitle ?~ text "An icon can be flipped")
    [example|
  ui $ Icon "thumbs outline up" def
  ui $ Icon "thumbs outline up" $ def & flipped |?~ HorizontallyFlipped
  ui $ Icon "thumbs outline up" $ def & flipped |?~ VerticallyFlipped
  |]

  ui $ Example "Rotated" (def
    & subtitle ?~ text "An icon can be rotated")
    [example|
  ui $ Icon "smile" def
  ui $ Icon "smile" $ def & rotated |?~ Clockwise
  ui $ Icon "smile" $ def & rotated |?~ Anticlockwise
  |]

  ui $ Example "Circular" (def
    & subtitle ?~ text "An icon can be formatted to appear circular")
    [example|
  ui $ Icon "users" $ def & circular |~ True
  ui $ Icon "users" $ def & circular |~ True & color |?~ Teal
  ui $ Icon "users" $ def & circular |~ True & inverted |~ True
  ui $ Icon "users" $ def & circular |~ True & color |?~ Teal & inverted |~ True
  |]

  ui $ Example "Bordered" (def
    & subtitle ?~ text "An icon can be formatted to appear bordered")
    [example|
  ui $ Icon "users" $ def & bordered |~ True
  ui $ Icon "users" $ def & bordered |~ True & color |?~ Teal
  ui $ Icon "users" $ def & bordered |~ True & inverted |~ True
  ui $ Icon "users" $ def & bordered |~ True & color |?~ Teal & inverted |~ True
  |]

  ui $ Example "Colored" (def
    & subtitle ?~ text "An icon can be formatted with different colors")
    [example|
  for_ [minBound .. maxBound] $ \c -> ui $ Icon "users" $ def & color |?~ c
  |]

  ui $ Example "Inverted" (def
    & subtitle ?~ text "An icon can have its colors inverted for contrast")
    [example|
  ui $ Segment (def & compact |~ True & inverted |~ True) $ do
    for_ [minBound .. maxBound] $ \c -> ui $ Icon "users" $ def
      & color |?~ c & inverted |~ True
  |]

  ui $ PageHeader H3 def $ text "Groups"

  ui $ Example "Icons" (def
    & inbetween ?~ upstreamIssue 5861 "There is an upstream issue causing icon groups to be off-centre."
    & subtitle ?~ text "Several icons can be used together as a group")
    [example|
  ui $ Icons (def & size |?~ Huge) $ do
    ui $ Icon "circle" $ def & color |?~ Blue
    ui $ Icon "announcement" $ def & inverted |~ True & size |?~ Tiny
  ui $ Icons (def & size |?~ Huge) $ do
    ui $ Icon "thin circle" def
    ui $ Icon "user" $ def & size |?~ Tiny
  ui $ Icons (def & size |?~ Huge) $ do
    ui $ Icon "certificate" $ def
      & loading |~ True & color |?~ Grey & inverted |~ True
    ui $ Icon "cloud download" $ def & size |?~ Tiny
  |]

  ui $ Example "Corner Icon" (def
    & subtitle ?~ text "A group of icons can display a smaller corner icon")
    [example|
  ui $ Icons (def & size |?~ Huge) $ do
    ui $ Icon "line chart" def
    ui $ Icon "dollar" $ def & corner |?~ TopLeft & color |?~ Green
  ui $ Icons (def & size |?~ Huge) $ do
    ui $ Icon "heart" $ def & color |?~ Pink
    ui $ Icon "plus" $ def & corner |?~ BottomRight
  |]
