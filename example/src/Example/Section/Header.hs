{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Header where

import Control.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

succMaybe :: forall a. (Eq a, Bounded a, Enum a) => a -> Maybe a
succMaybe a
  | a == (maxBound :: a) = Nothing
  | otherwise = Just $ succ a

predMaybe :: forall a. (Eq a, Bounded a, Enum a) => a -> Maybe a
predMaybe a
  | a == (minBound :: a) = Nothing
  | otherwise = Just $ pred a

headers :: MonadWidget t m => Section t m
headers = LinkedSection "Header" (simpleLink "https://semantic-ui.com/elements/header.html") $ do

  hscode $(printDefinition id stripParens ''Header)
  hscode $(printDefinition id stripParens ''HeaderConfig)

--  header -- (def :: HeaderConfig t) (text "Simple header" :: Component Header m ()) :: Component None m ()

  ui $ Example "Page Headers" (def
    & subtitle ?~ text "Headers may be oriented to give the hierarchy of a section in the context of the page"
    & inbetween ?~ (ui_ $ Message (def & messageType |?~ InfoMessage) $ do
      paragraph $ text "Page headings are sized using 'rem' and are not affected by surrounding content size."))
    [example|
  ui $ PageHeader H1 def $ text "First Header"
  ui $ PageHeader H2 def $ text "Second Header"
  ui $ PageHeader H3 def $ text "Third Header"
  ui $ PageHeader H4 def $ text "Fourth Header"
  ui $ PageHeader H5 def $ text "Fifth Header"
  |]

  ui $ Example "Content Headers" (def
    & subtitle ?~ text "Headers may be oriented to give the importance of a section in the context of the content that surrounds it"
    & inbetween ?~ (ui_ $ Message (def & messageType |?~ InfoMessage) $ do
      paragraph $ text "Content headings are sized using 'em' and are based on the font-size of their container."))
    [resetExample|
  \resetEvent -> do
    dSize <- ui $ Buttons def $ do
      plus <- ui $ Button (def & icon |~ True) $ ui $ Icon "plus" def
      minus <- ui $ Button (def & icon |~ True) $ ui $ Icon "minus" def
      foldDyn (\f a -> fromMaybe a $ f a) Medium $
        leftmost [ succMaybe <$ plus, predMaybe <$ minus, const (Just Medium) <$ resetEvent ]

    ui $ Segment (def & size .~ Dynamic (Just <$> dSize)) $ do
      ui $ Header (def & size |?~ H1) $ text "First Header"
      ui $ Header (def & size |?~ H2) $ text "Second Header"
      ui $ Header (def & size |?~ H3) $ text "Third Header"
      ui $ Header (def & size |?~ H4) $ text "Fourth Header"
      ui $ Header (def & size |?~ H5) $ text "Fifth Header"
  |]

  ui $ Example "Icon Headers" (def
    & subtitle ?~ text "A header can be formatted to emphasise an icon")
    [example|
  ui $ PageHeader H2 (def
    & icon ?~ Icon "settings" def
    & iconHeader |~ True
    & aligned |?~ CenterAligned) $ do
    text "Account Settings"
    ui $ SubHeader $ text "Manage your account settings and set e-mail preferences."

  ui $ Header (def
    & iconHeader |~ True
    & icon ?~ Icon "users" (def & circular |~ True & inverted |~ True)
    & aligned |?~ CenterAligned) $ do
    text "Friends"
  |]

  ui $ Example "Sub Headers" (def
    & subtitle ?~ text "A header may be formatted to label smaller or de-emphasised content")
    [example|
  divClass "ui horizontal list" $ do
    divClass "item" $ do
      ui $ Image "images/animals/bat.png" $ def
        & size |?~ Mini
        & shape |?~ Circular
      divClass "content" $ do
        ui $ Header (def & sub |~ True) $ text "Benjamin"
        text "Bat"

    divClass "item" $ do
      ui $ Image "images/animals/horse.png" $ def
        & size |?~ Mini
        & shape |?~ Circular
      divClass "content" $ do
        ui $ Header (def & sub |~ True) $ text "Harriet"
        text "Horse"

    divClass "item" $ do
      ui $ Image "images/animals/monkey.png" $ def
        & size |?~ Mini
        & shape |?~ Circular
      divClass "content" $ do
        ui $ Header (def & sub |~ True) $ text "Molly"
        text "Monkey"
  |]

  ui $ Example "Image" (def
    & subtitle ?~ text "A header may contain an image before the content")
    [example|
  let url = "images/animals/penguin.png"
  ui $ Header (def & image ?~ Image url (def & shape |?~ Circular)) $ do
    text "Penelope"
    ui $ SubHeader $ text "Penguin"
  |]

  ui $ Example "Icon" (def
    & subtitle ?~ text "A header may contain an icon before the content")
    [example|
  ui $ PageHeader H2 (def & icon ?~ Icon "plug" def) $ text "Uptime Guarantee"

  ui $ PageHeader H2 (def & icon ?~ Icon "settings" def) $ do
    text "Account Settings"
    ui $ SubHeader $ text "Manage your preferences"
  |]

  ui $ Example "Icon" (def
    & subtitle ?~ text "A header may contain an icon inside the content")
    [example|
  ui_ $ PageHeader H2 def $ do
    text "Add calendar event"
    ui $ Icon "add to calendar" $ def
      & link |~ True & color |?~ Teal
      & style |~ Style ("margin-left" =: "0.5em")
  |]

  ui $ Example "Subheader" (def
    & subtitle ?~ text "A header may contain a subheader")
    [example|
  ui $ PageHeader H1 def $ do
    text "First Header"
    ui $ SubHeader $ text "Sub Header"
  ui $ PageHeader H2 def $ do
    text "Second Header"
    ui $ SubHeader $ text "Sub Header"
  ui $ PageHeader H3 def $ do
    text "Third Header"
    ui $ SubHeader $ text "Sub Header"
  ui $ PageHeader H4 def $ do
    text "Fourth Header"
    ui $ SubHeader $ text "Sub Header"
  ui $ PageHeader H5 def $ do
    text "Fifth Header"
    ui $ SubHeader $ text "Sub Header"
  |]

  ui $ Example "Disabled" (def
    & subtitle ?~ text "A header may show that it is inactive")
    [example|
  ui $ Header (def & disabled |~ True) $ text "Disabled Header"
  |]

  ui $ Example "Block" (def
    & subtitle ?~ text "A header can be formatted to appear inside a content block")
    [example|
  ui $ PageHeader H3 (def & block |~ True) $ text "Block Header"
  |]

  ui $ Example "Dividing" (def
    & subtitle ?~ text "A header can be formatted to divide itself from the content below it")
    [example|
  paragraph $ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam at diam mauris. Cras quis purus fringilla, maximus ex volutpat, tristique velit. In in pulvinar tellus, a ultrices erat. Quisque sit amet gravida lectus, vel viverra mauris."
  ui $ PageHeader H3 (def & dividing |~ True) $ text "Dividing Header"
  paragraph $ text "Nam diam neque, euismod nec maximus ut, lacinia egestas ex. Etiam condimentum finibus venenatis. Sed commodo lobortis dolor nec molestie. Maecenas commodo metus diam, nec accumsan est consectetur quis. Nunc sed est nunc. Duis turpis ipsum, vulputate non mi placerat, vehicula dignissim est. Integer venenatis tortor nec massa dapibus, sed laoreet eros lacinia."
  |]

  ui $ Example "Attached" (def
    & subtitle ?~ text "A header can be attached to other content such as a segment")
    [example|
  ui $ Header (def & attached |?~ TopAttached) $ text "Top Attached"
  ui $ Segment (def & attached |?~ Attached) $ paragraph $ text "Nullam tincidunt, elit et placerat convallis, lacus erat convallis turpis, id tincidunt eros leo ac felis. Nam tincidunt eget ligula vel cursus. Nam eu vehicula lacus. Sed quis tellus nec massa semper condimentum sit amet non libero."
  ui $ Header (def & attached |?~ Attached) $ text "Attached"
  ui $ Segment (def & attached |?~ Attached) $ paragraph $ text "Aliquam tincidunt libero nec turpis porta consectetur. Nam eget ex auctor, sagittis diam at, pharetra leo. Phasellus venenatis iaculis sem id posuere."
  ui $ Header (def & attached |?~ BottomAttached) $ text "Bottom Attached"
  |]

  ui $ Example "Floating" (def
    & subtitle ?~ text "A header can sit to the left or right of other content")
    [example|
  ui $ Header (def & floated |?~ LeftFloated) $
    text "Go Back"
  ui $ Header (def & floated |?~ RightFloated) $
    text "Go Forward"
  |]

  ui $ Example "Text Alignment" (def
    & subtitle ?~ text "A header can have its text aligned to a side")
    [example|
  ui $ Header (def & aligned |?~ RightAligned) $ text "Right"
  ui $ Header (def & aligned |?~ LeftAligned) $ text "Left"
  ui $ Header (def & aligned |?~ Justified) $
    text "This should take up the full width even if only one line"
  ui $ Header (def & aligned |?~ CenterAligned) $ text "Center"
  |]

  ui $ Example "Colored" (def
    & subtitle ?~ text "A header can be formatted with different colors")
    [example|
  let makeHeader c = Header (def & color |?~ c) $
        text $ Static $ T.pack $ show c
  mapM_ (ui_ . makeHeader) [Red .. Grey]
  |]

  ui $ Example "Inverted" (def
    & subtitle ?~ text "A header can have its colors inverted for contrast")
    [example|
  ui $ Segment (def & inverted |~ True) $ do
    let makeHeader c = Header (def & color |?~ c & inverted |~ True) $
          text $ Static $ T.pack $ show c
    mapM_ (ui_ . makeHeader) [Red .. Grey]
  |]
