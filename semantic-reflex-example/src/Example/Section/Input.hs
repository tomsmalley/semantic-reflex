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
  input def $ textInput $ def & textInputPlaceholder |~ "Search..."
  |]

  mkExample "Loading" (def
    & subtitle ?~ text "An icon input field can show that it is currently loading data")
    [example|
  isLoading <- buttons def $ do
    stop <- button def $ text "Stop"
    start <- button def $ text "Start"
    holdDyn True $ leftmost [ False <$ stop, True <$ start ]

  divider $ def & dividerHidden |~ True

  input (def & inputLoading .~ Dyn isLoading
             & inputIcon |?~ LeftIcon) $ do
    icon "search" def
    textInput $ def & textInputPlaceholder |~ "Search..."

  divider $ def & dividerHidden |~ True

  input (def & inputLoading .~ Dyn isLoading
             & inputIcon |?~ RightIcon) $ do
    icon "search" def
    textInput $ def & textInputPlaceholder |~ "Search..."
  |]

  mkExample "Disabled" (def
    & subtitle ?~ text "An input field can show that it is disabled")
    [example|
  input (def & inputDisabled |~ True) $ do
    textInput $ def & textInputPlaceholder |~ "Search..."

  divider $ def & dividerHidden |~ True

  input (def & inputDisabled |~ True & inputIcon |?~ LeftIcon) $ do
    icon "search" def
    textInput $ def & textInputPlaceholder |~ "Search..."
  |]

  mkExample "Error" (def
    & subtitle ?~ text "An input field can show that it contains errors")
    [example|
  input (def & inputError |~ True) $ do
    textInput $ def & textInputPlaceholder |~ "Search..."
  |]

  mkExample "Icon" (def
    & subtitle ?~ text "An input field can be formatted with an icon")
    [example|
  input (def & inputIcon |?~ RightIcon) $ do
    icon "search" def
    textInput $ def & textInputPlaceholder |~ "Search..."

  divider $ def & dividerHidden |~ True

  input (def & inputIcon |?~ LeftIcon) $ do
    icon "users" def
    textInput $ def & textInputPlaceholder |~ "Search users..."
  |]

  mkExample "Link Icon" (def
    & subtitle ?~ text "The icon can be used as a submit button" & dynamic ?~ dynCode)
    [example|
  input (def & inputIcon |?~ RightIcon) $ do
    e <- icon' "search" $ def & iconLink |~ True & iconCircular |~ True
    ti <- textInput $ def & textInputPlaceholder |~ "Search..."
    holdDyn "" $ tagPromptlyDyn (ti ^. textInput_value) $ leftmost
      [ domEvent Click e, keyIs Enter $ ti ^. textInput_keypress ]
  |]

  mkExample "Labeled" (def
    & subtitle ?~ text "An input can be formatted with a label")
    [example|
  input (def & inputLabeled |?~ LeftLabeled) $ do
    label def $ text "http://"
    textInput $ def & textInputPlaceholder |~ "mysite.com"

  divider $ def & dividerHidden |~ True

  input (def & inputLabeled |?~ RightLabeled) $ do
    textInput $ def & textInputPlaceholder |~ "Search users..."
    text "TODO: Menu goes here"
    --menuDropdown (mkDropdownConfig (Just "com") & as |?~ DropdownLabel) $ do
    --  menuItem "com" def $ text ".com"
    --  menuItem "net" def $ text ".net"
    --  menuItem "org" def $ text ".org"

  divider $ def & dividerHidden |~ True

  input (def & inputLabeled |?~ RightLabeled) $ do
    textInput $ def & textInputPlaceholder |~ "Enter weight.."
    label (def & labelBasic |~ True) $ text "kg"

  divider $ def & dividerHidden |~ True

  input (def & inputLabeled |?~ RightLabeled) $ do
    label def $ text "$"
    textInput $ def & textInputPlaceholder |~ "Amount"
    label (def & labelBasic |~ True) $ text ".00"

  divider $ def & dividerHidden |~ True

  input (def & inputLabeled |?~ RightLabeled & inputIcon |?~ LeftIcon) $ do
    icon "tags" def
    textInput $ def & textInputPlaceholder |~ "Enter tags"
    label (def & labelTag |~ True & labelLink .~ True) $ text "Add Tag"

  divider $ def & dividerHidden |~ True

  input (def & inputLabeled |?~ LeftLabeled & inputIcon |?~ LeftIcon) $ do
    textInput $ def & textInputPlaceholder |~ "Search..."
    label (def & labelCorner |?~ LeftCorner) $ icon "asterisk" def

  divider $ def & dividerHidden |~ True

  input (def & inputLabeled |?~ RightLabeled & inputIcon |?~ RightIcon) $ do
    textInput $ def & textInputPlaceholder |~ "Search..."
    label (def & labelCorner |?~ RightCorner) $ icon "asterisk" def
  |]

  mkExample "Action" (def
    & subtitle ?~ text "An input can be formatted to alert the user to an action they may perform")
    [example|
  input (def & inputAction |?~ RightAction) $ do
    textInput $ def & textInputPlaceholder |~ "Search..."
    button def $ text "Search"

  divider $ def & dividerHidden |~ True

  input (def & inputAction |?~ LeftAction) $ do
    button (def & buttonColor |?~ Teal & buttonLabeledIcon |?~ LeftLabeled) $ do
      icon "cart" def
      text "Checkout"
    textInput $ def & textInputPlaceholder |~ "Search..."
                    & textInputValue . initial .~ "£52.03"

  divider $ def & dividerHidden |~ True

  input (def & inputAction |?~ RightAction & inputIcon |?~ LeftIcon) $ do
    icon "search" def
    textInput $ def & textInputPlaceholder |~ "Search..."
    text "TODO: Menu goes here"
    --menuDropdown (mkDropdownConfig (Just "page") & as |?~ DropdownButton) $ do
    --  menuItem "page" def $ text "This Page"
    --  menuItem "org" def $ text "This Organisation"
    --  menuItem "site" def $ text "Entire Site"

  divider $ def & dividerHidden |~ True

  input (def & inputAction |?~ RightAction) $ do
    textInput $ def & textInputPlaceholder |~ "Search..."
    text "TODO: Menu goes here"
    --menuDropdown (mkDropdownConfig (Just "all") & compact |~ True & selection |~ True) $ do
    --  menuItem "all" def $ text "All"
    --  menuItem "articles" def $ text "Articles"
    --  menuItem "products" def $ text "Products"
    button def $ text "Search"

  divider $ def & dividerHidden |~ True

  input (def & inputAction |?~ RightAction) $ do
    textInput $ def & textInputValue . initial .~ "http://ww.short.url/c0opd"
    button (def & buttonColor |?~ Teal
                & buttonLabeledIcon |?~ RightLabeled) $ do
      icon "copy" def
      text "Copy"

  divider $ def & dividerHidden |~ True

  input (def & inputAction |?~ RightAction) $ do
    textInput $ def & textInputPlaceholder |~ "Search..."
    button (def & buttonIcon |~ True) $ do
      icon "search" def
  |]

  mkExample "Transparent" (def
    & subtitle ?~ text "A transparent input has no background")
    [example|
  input (def & inputTransparent |~ True) $ do
    textInput $ def & textInputPlaceholder |~ "Search..."

  divider $ def & dividerHidden |~ True

  input (def & inputIcon |?~ RightIcon & inputTransparent |~ True) $ do
    icon "search" def
    textInput $ def & textInputPlaceholder |~ "Search..."

  divider $ def & dividerHidden |~ True

  input (def & inputIcon |?~ LeftIcon & inputTransparent |~ True) $ do
    icon "search" def
    textInput $ def & textInputPlaceholder |~ "Search..."
  |]

  mkExample "Inverted" (def
    & subtitle ?~ text "An input can be formatted to appear on dark backgrounds")
    [example|
  segment (def & segmentInverted |~ True) $ do

    input (def & inputInverted |~ True) $ do
      textInput $ def & textInputPlaceholder |~ "Search..."

    divider $ def & dividerInverted |~ True

    input (def & inputIcon |?~ RightIcon & inputInverted |~ True) $ do
      icon "search" def
      textInput $ def & textInputPlaceholder |~ "Search..."

    divider $ def & dividerInverted |~ True

    input (def & inputIcon |?~ RightIcon & inputInverted |~ True
               & inputTransparent |~ True) $ do
      icon "search" def
      textInput $ def & textInputPlaceholder |~ "Search..."
  |]

  mkExample "Fluid" (def
    & subtitle ?~ text "An input can take the size of its container")
    [example|
  input (def & inputFluid |~ True & inputIcon |?~ RightIcon) $ do
    icon "search" def
    textInput $ def & textInputPlaceholder |~ "Search a very wide input..."

  divider $ def & dividerHidden |~ True

  input (def & inputFluid |~ True & inputAction |?~ RightAction) $ do
    textInput $ def & textInputPlaceholder |~ "Search..."
    button def $ text "Search"
  |]

  mkExample "Size" (def
    & subtitle ?~ text "An input can vary in size")
    [example|
  for_ [minBound .. maxBound] $ \s -> do
    input (def & inputSize |?~ s & inputIcon |?~ RightIcon) $ do
      icon "search" def
      textInput $ def & textInputPlaceholder |~ ("Search " <> tshow s <> "...")
    divider $ def & dividerHidden |~ True
  |]

  return ()
