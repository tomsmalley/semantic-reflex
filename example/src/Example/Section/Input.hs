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

import Example.QQ
import Example.Common

inputs :: forall t m. MonadWidget t m => Section t m
inputs = LinkedSection "Input" (text "An input is a field used to elicit a response from a user " >> simpleLink "https://semantic-ui.com/elements/input.html") $ do

  hscode $ $(printDefinition id stripParens ''Input)
  hscode $ $(printDefinition id stripParens ''InputConfig)
  hscode $ $(printDefinition id stripParens ''TextInputConfig)

  ui_ $ PageHeader H3 def $ text "Types"

  ui_ $ Example "Input" (def
    & subtitle ?~ text "A standard input" & dynamic ?~ dynShowCode)
    [example|
  ui $ Input def $ textInput $ def
    & placeholder |~ "Search..."
  |]

  ui_ $ Example "Loading" (def
    & subtitle ?~ text "An icon input field can show that it is currently loading data")
    [example|
  isLoading <- ui $ Buttons def $ do
    stop <- ui $ Button def $ text "Stop"
    start <- ui $ Button def $ text "Start"
    holdDyn True $ leftmost [ False <$ stop, True <$ start ]

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & loading .~ Dynamic isLoading & icon |?~ LeftIcon) $ do
    ui $ Icon "search" def
    textInput $ def & placeholder |~ "Search..."

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & loading .~ Dynamic isLoading & icon |?~ RightIcon) $ do
    ui $ Icon "search" def
    textInput $ def & placeholder |~ "Search..."
  |]

  ui_ $ Example "Disabled" (def
    & subtitle ?~ text "An input field can show that it is disabled")
    [example|
  ui $ Input (def & disabled |~ True) $ do
    textInput $ def & placeholder |~ "Search..."

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & disabled |~ True & icon |?~ LeftIcon) $ do
    ui $ Icon "search" def
    textInput $ def & placeholder |~ "Search..."
  |]

  ui_ $ Example "Error" (def
    & subtitle ?~ text "An input field can show that it contains errors")
    [example|
  ui $ Input (def & hasError |~ True) $ do
    textInput $ def & placeholder |~ "Search..."
  |]

  ui_ $ Example "Icon" (def
    & subtitle ?~ text "An input field can be formatted with an icon")
    [example|
  ui $ Input (def & icon |?~ RightIcon) $ do
    ui $ Icon "search" def
    textInput $ def & placeholder |~ "Search..."

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & icon |?~ LeftIcon) $ do
    ui $ Icon "users" def
    textInput $ def & placeholder |~ "Search users..."
  |]

  ui_ $ Example "Link Icon" (def
    & subtitle ?~ text "The icon can be used as a submit button" & dynamic ?~ dynCode)
    [example|
  ui $ Input (def & icon |?~ RightIcon) $ do
    (e, _) <- ui' $ Icon "search" $ def & link |~ True & circular |~ True
    ti <- textInput $ def & placeholder |~ "Search..."
    holdDyn "" $ tagPromptlyDyn (ti ^. value) $ leftmost
      [ domEvent Click e, keyIs Enter $ ti ^. keypress ]
  |]

  ui_ $ Example "Labeled" (def
    & subtitle ?~ text "An input can be formatted with a label")
    [example|
  ui $ Input (def & labeled |?~ LeftLabeled) $ do
    ui $ Label def $ text "http://"
    textInput $ def & placeholder |~ "mysite.com"

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & labeled |?~ RightLabeled) $ do
    textInput $ def & placeholder |~ "Search users..."
    ui $ MenuDropdown (mkDropdownConfig (Just "com") & as |?~ DropdownLabel) $ do
      ui $ MenuItem "com" def $ text ".com"
      ui $ MenuItem "net" def $ text ".net"
      ui $ MenuItem "org" def $ text ".org"

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & labeled |?~ RightLabeled) $ do
    textInput $ def & placeholder |~ "Enter weight.."
    ui $ Label (def & basic |~ True) $ text "kg"

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & labeled |?~ RightLabeled) $ do
    ui $ Label def $ text "$"
    textInput $ def & placeholder |~ "Amount"
    ui $ Label (def & basic |~ True) $ text ".00"

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & labeled |?~ RightLabeled & icon |?~ LeftIcon) $ do
    ui $ Icon "tags" def
    textInput $ def & placeholder |~ "Enter tags"
    ui $ Label (def & tag |~ True & link .~ True) $ text "Add Tag"

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & labeled |?~ LeftLabeled & icon |?~ LeftIcon) $ do
    textInput $ def & placeholder |~ "Search..."
    ui $ Label (def & corner |?~ LeftCorner) $ ui $ Icon "asterisk" def

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & labeled |?~ RightLabeled & icon |?~ RightIcon) $ do
    textInput $ def & placeholder |~ "Search..."
    ui $ Label (def & corner |?~ RightCorner) $ ui $ Icon "asterisk" def
  |]

  ui_ $ Example "Action" (def
    & subtitle ?~ text "An input can be formatted to alert the user to an action they may perform")
    [example|
  ui $ Input (def & action |?~ RightAction) $ do
    textInput $ def & placeholder |~ "Search..."
    ui $ Button def $ text "Search"

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & action |?~ LeftAction) $ do
    ui $ Button (def & color |?~ Teal & labeledIcon |?~ LeftLabeled) $ do
      ui $ Icon "cart" def
      text "Checkout"
    textInput $ def & placeholder |~ "Search..."
                    & value . initial .~ "£52.03"

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & action |?~ RightAction & icon |?~ LeftIcon) $ do
    ui $ Icon "search" def
    textInput $ def & placeholder |~ "Search..."
    ui $ MenuDropdown (mkDropdownConfig (Just "page") & as |?~ DropdownButton) $ do
      ui $ MenuItem "page" def $ text "This Page"
      ui $ MenuItem "org" def $ text "This Organisation"
      ui $ MenuItem "site" def $ text "Entire Site"

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & action |?~ RightAction) $ do
    textInput $ def & placeholder |~ "Search..."
    ui $ MenuDropdown (mkDropdownConfig (Just "all") & compact |~ True & selection |~ True) $ do
      ui $ MenuItem "all" def $ text "All"
      ui $ MenuItem "articles" def $ text "Articles"
      ui $ MenuItem "products" def $ text "Products"
    ui $ Button def $ text "Search"

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & action |?~ RightAction) $ do
    textInput $ def & value . initial .~ "http://ww.short.url/c0opd"
    ui $ Button (def & color |?~ Teal & labeledIcon |?~ RightLabeled) $ do
      ui $ Icon "copy" def
      text "Copy"

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & action |?~ RightAction) $ do
    textInput $ def & placeholder |~ "Search..."
    ui $ Button (def & icon |~ True) $ do
      ui $ Icon "search" def
  |]

  ui_ $ Example "Transparent" (def
    & subtitle ?~ text "A transparent input has no background")
    [example|
  ui $ Input (def & transparent |~ True) $ do
    textInput $ def & placeholder |~ "Search..."

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & icon |?~ RightIcon & transparent |~ True) $ do
    ui $ Icon "search" def
    textInput $ def & placeholder |~ "Search..."

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & icon |?~ LeftIcon & transparent |~ True) $ do
    ui $ Icon "search" def
    textInput $ def & placeholder |~ "Search..."
  |]

  ui_ $ Example "Inverted" (def
    & subtitle ?~ text "An input can be formatted to appear on dark backgrounds")
    [example|
  ui $ Segment (def & inverted |~ True) $ do

    ui $ Input (def & inverted |~ True) $ do
      textInput $ def & placeholder |~ "Search..."

    ui $ Divider $ def & inverted |~ True

    ui $ Input (def & icon |?~ RightIcon & inverted |~ True) $ do
      ui $ Icon "search" def
      textInput $ def & placeholder |~ "Search..."

    ui $ Divider $ def & inverted |~ True

    ui $ Input (def & icon |?~ RightIcon & inverted |~ True & transparent |~ True) $ do
      ui $ Icon "search" def
      textInput $ def & placeholder |~ "Search..."
  |]

  ui_ $ Example "Fluid" (def
    & subtitle ?~ text "An input can take the size of its container")
    [example|
  ui $ Input (def & fluid |~ True & icon |?~ RightIcon) $ do
    ui $ Icon "search" def
    textInput $ def & placeholder |~ "Search a very wide input..."

  ui $ Divider $ def & hidden |~ True

  ui $ Input (def & fluid |~ True & action |?~ RightAction) $ do
    textInput $ def & placeholder |~ "Search..."
    ui $ Button def $ text "Search"
  |]

  ui_ $ Example "Size" (def
    & subtitle ?~ text "An input can vary in size")
    [example|
  for_ [minBound .. maxBound] $ \s -> do
    ui_ $ Input (def & size |?~ s & icon |?~ RightIcon) $ do
      ui $ Icon "search" def
      textInput $ def & placeholder |~ ("Search " <> T.pack (show s) <> "...")
    ui_ $ Divider $ def & hidden |~ True
  |]

  return ()
