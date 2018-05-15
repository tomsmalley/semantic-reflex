{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Input where

import Control.Lens
import Data.Foldable (for_)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text, keypress)

import Example.QQ
import Example.Common

inputs :: forall t m. MonadWidget t m => Section t m
inputs = Section "Input" (text "An input is a field used to elicit a response from a user " >> simpleLink "https://semantic-ui.com/elements/input.html") $ do

  hscode $ $(printDefinition id stripParens ''InputConfig)
  hscode $ $(printDefinition id stripParens ''TextInputConfig)

  pageHeader H3 def $ text "Types"

  mkExample "Input" (def
    & subtitle ?~ text "A standard input" & dynamic ?~ dynShowCode)
    [example|
  input def $ textInput $ def & textInputConfig_placeholder |~ "Search..."
  |]

  mkExample "Loading" (def
    & subtitle ?~ text "An icon input field can show that it is currently loading data")
    [example|
  isLoading <- buttons def $ do
    stop <- button def $ text "Stop"
    start <- button def $ text "Start"
    holdDyn True $ leftmost [ False <$ stop, True <$ start ]

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_loading .~ Dyn isLoading
             & inputConfig_icon |?~ LeftIcon) $ do
    icon "search" def
    textInput $ def & textInputConfig_placeholder |~ "Search..."

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_loading .~ Dyn isLoading
             & inputConfig_icon |?~ RightIcon) $ do
    icon "search" def
    textInput $ def & textInputConfig_placeholder |~ "Search..."
  |]

  mkExample "Disabled" (def
    & subtitle ?~ text "An input field can show that it is disabled")
    [example|
  input (def & inputConfig_disabled |~ True) $ do
    textInput $ def & textInputConfig_placeholder |~ "Search..."

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_disabled |~ True & inputConfig_icon |?~ LeftIcon) $ do
    icon "search" def
    textInput $ def & textInputConfig_placeholder |~ "Search..."
  |]

  mkExample "Error" (def
    & subtitle ?~ text "An input field can show that it contains errors")
    [example|
  input (def & inputConfig_error |~ True) $ do
    textInput $ def & textInputConfig_placeholder |~ "Search..."
  |]

  mkExample "Icon" (def
    & subtitle ?~ text "An input field can be formatted with an icon")
    [example|
  input (def & inputConfig_icon |?~ RightIcon) $ do
    icon "search" def
    textInput $ def & textInputConfig_placeholder |~ "Search..."

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_icon |?~ LeftIcon) $ do
    icon "users" def
    textInput $ def & textInputConfig_placeholder |~ "Search users..."
  |]

  mkExample "Link Icon" (def
    & subtitle ?~ text "The icon can be used as a submit button" & dynamic ?~ dynCode)
    [example|
  input (def & inputConfig_icon |?~ RightIcon) $ do
    e <- icon' "search" $ def & iconConfig_link |~ True & iconConfig_circular |~ True
    ti <- textInput $ def & textInputConfig_placeholder |~ "Search..."
    holdDyn "" $ tagPromptlyDyn (ti ^. textInput_value) $ leftmost
      [ domEvent Click e, keyIs Enter $ ti ^. textInput_keypress ]
  |]

  mkExample "Labeled" (def
    & subtitle ?~ text "An input can be formatted with a label")
    [example|
  input (def & inputConfig_labeled |?~ LeftLabeled) $ do
    label def $ text "http://"
    textInput $ def & textInputConfig_placeholder |~ "mysite.com"

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_labeled |?~ RightLabeled) $ do
    textInput $ def & textInputConfig_placeholder |~ "Search users..."
    text "TODO: Menu goes here"
    --menuDropdown (mkDropdownConfig (Just "com") & as |?~ DropdownLabel) $ do
    --  menuItem "com" def $ text ".com"
    --  menuItem "net" def $ text ".net"
    --  menuItem "org" def $ text ".org"

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_labeled |?~ RightLabeled) $ do
    textInput $ def & textInputConfig_placeholder |~ "Enter weight.."
    label (def & labelConfig_basic |~ True) $ text "kg"

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_labeled |?~ RightLabeled) $ do
    label def $ text "$"
    textInput $ def & textInputConfig_placeholder |~ "Amount"
    label (def & labelConfig_basic |~ True) $ text ".00"

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_labeled |?~ RightLabeled & inputConfig_icon |?~ LeftIcon) $ do
    icon "tags" def
    textInput $ def & textInputConfig_placeholder |~ "Enter tags"
    label (def & labelConfig_tag |~ True & labelConfig_link .~ True) $ text "Add Tag"

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_labeled |?~ LeftLabeled & inputConfig_icon |?~ LeftIcon) $ do
    textInput $ def & textInputConfig_placeholder |~ "Search..."
    label (def & labelConfig_corner |?~ LeftCorner) $ icon "asterisk" def

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_labeled |?~ RightLabeled & inputConfig_icon |?~ RightIcon) $ do
    textInput $ def & textInputConfig_placeholder |~ "Search..."
    label (def & labelConfig_corner |?~ RightCorner) $ icon "asterisk" def
  |]

  mkExample "Action" (def
    & subtitle ?~ text "An input can be formatted to alert the user to an action they may perform")
    [example|
  input (def & inputConfig_action |?~ RightAction) $ do
    textInput $ def & textInputConfig_placeholder |~ "Search..."
    button def $ text "Search"

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_action |?~ LeftAction) $ do
    button (def & buttonConfig_color |?~ Teal & buttonConfig_labeledIcon |?~ LeftLabeled) $ do
      icon "cart" def
      text "Checkout"
    textInput $ def & textInputConfig_placeholder |~ "Search..."
                    & textInputConfig_value . initial .~ "£52.03"

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_action |?~ RightAction & inputConfig_icon |?~ LeftIcon) $ do
    icon "search" def
    textInput $ def & textInputConfig_placeholder |~ "Search..."
    text "TODO: Menu goes here"
    --menuDropdown (mkDropdownConfig (Just "page") & as |?~ DropdownButton) $ do
    --  menuItem "page" def $ text "This Page"
    --  menuItem "org" def $ text "This Organisation"
    --  menuItem "site" def $ text "Entire Site"

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_action |?~ RightAction) $ do
    textInput $ def & textInputConfig_placeholder |~ "Search..."
    text "TODO: Menu goes here"
    --menuDropdown (mkDropdownConfig (Just "all") & compact |~ True & selection |~ True) $ do
    --  menuItem "all" def $ text "All"
    --  menuItem "articles" def $ text "Articles"
    --  menuItem "products" def $ text "Products"
    button def $ text "Search"

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_action |?~ RightAction) $ do
    textInput $ def & textInputConfig_value . initial .~ "http://ww.short.url/c0opd"
    button (def & buttonConfig_color |?~ Teal
                & buttonConfig_labeledIcon |?~ RightLabeled) $ do
      icon "copy" def
      text "Copy"

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_action |?~ RightAction) $ do
    textInput $ def & textInputConfig_placeholder |~ "Search..."
    button (def & buttonConfig_icon |~ True) $ do
      icon "search" def
  |]

  mkExample "Transparent" (def
    & subtitle ?~ text "A transparent input has no background")
    [example|
  input (def & inputConfig_transparent |~ True) $ do
    textInput $ def & textInputConfig_placeholder |~ "Search..."

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_icon |?~ RightIcon & inputConfig_transparent |~ True) $ do
    icon "search" def
    textInput $ def & textInputConfig_placeholder |~ "Search..."

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_icon |?~ LeftIcon & inputConfig_transparent |~ True) $ do
    icon "search" def
    textInput $ def & textInputConfig_placeholder |~ "Search..."
  |]

  mkExample "Inverted" (def
    & subtitle ?~ text "An input can be formatted to appear on dark backgrounds")
    [example|
  segment (def & segmentConfig_inverted |~ True) $ do

    input (def & inputConfig_inverted |~ True) $ do
      textInput $ def & textInputConfig_placeholder |~ "Search..."

    divider $ def & dividerConfig_inverted |~ True

    input (def & inputConfig_icon |?~ RightIcon & inputConfig_inverted |~ True) $ do
      icon "search" def
      textInput $ def & textInputConfig_placeholder |~ "Search..."

    divider $ def & dividerConfig_inverted |~ True

    input (def & inputConfig_icon |?~ RightIcon & inputConfig_inverted |~ True
               & inputConfig_transparent |~ True) $ do
      icon "search" def
      textInput $ def & textInputConfig_placeholder |~ "Search..."
  |]

  mkExample "Fluid" (def
    & subtitle ?~ text "An input can take the size of its container")
    [example|
  input (def & inputConfig_fluid |~ True & inputConfig_icon |?~ RightIcon) $ do
    icon "search" def
    textInput $ def & textInputConfig_placeholder |~ "Search a very wide input..."

  divider $ def & dividerConfig_hidden |~ True

  input (def & inputConfig_fluid |~ True & inputConfig_action |?~ RightAction) $ do
    textInput $ def & textInputConfig_placeholder |~ "Search..."
    button def $ text "Search"
  |]

  mkExample "Size" (def
    & subtitle ?~ text "An input can vary in size")
    [example|
  for_ [minBound .. maxBound] $ \s -> do
    input (def & inputConfig_size |?~ s & inputConfig_icon |?~ RightIcon) $ do
      icon "search" def
      textInput $ def & textInputConfig_placeholder |~ ("Search " <> tshow s <> "...")
    divider $ def & dividerConfig_hidden |~ True
  |]

  return ()
