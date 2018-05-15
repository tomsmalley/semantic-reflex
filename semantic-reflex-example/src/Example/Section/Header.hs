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
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)

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
headers = Section "Header" (simpleLink "https://semantic-ui.com/elements/header.html") $ do

  hscode $(printDefinition id stripParens ''HeaderConfig)

--  header -- (def :: HeaderConfig t) (text "Simple header" :: Component Header m ()) :: Component None m ()

  mkExample "Page Headers" (def
    & subtitle ?~ text "Headers may be oriented to give the hierarchy of a section in the context of the page"
    & inbetween ?~ (message (def & messageConfig_type |?~ InfoMessage) $ do
      paragraph $ text "Page headings are sized using 'rem' and are not affected by surrounding content size."))
    [example|
  pageHeader H1 def $ text "First Header"
  pageHeader H2 def $ text "Second Header"
  pageHeader H3 def $ text "Third Header"
  pageHeader H4 def $ text "Fourth Header"
  pageHeader H5 def $ text "Fifth Header"
  |]

  mkExample "Content Headers" (def
    & subtitle ?~ text "Headers may be oriented to give the importance of a section in the context of the content that surrounds it"
    & inbetween ?~ (message (def & messageConfig_type |?~ InfoMessage) $ do
      paragraph $ text "Content headings are sized using 'em' and are based on the font-size of their container."))
    [resetExample|
  \resetEvent -> do
    dSize <- buttons def $ do
      plus <- button (def & buttonConfig_icon |~ True) $ icon "plus" def
      minus <- button (def & buttonConfig_icon |~ True) $ icon "minus" def
      foldDyn (\f a -> fromMaybe a $ f a) Medium $
        leftmost [ succMaybe <$ plus, predMaybe <$ minus, const (Just Medium) <$ resetEvent ]

    segment (def & segmentConfig_size .~ Dyn (Just <$> dSize)) $ do
      header (def & headerConfig_size |?~ H1) $ text "First Header"
      header (def & headerConfig_size |?~ H2) $ text "Second Header"
      header (def & headerConfig_size |?~ H3) $ text "Third Header"
      header (def & headerConfig_size |?~ H4) $ text "Fourth Header"
      header (def & headerConfig_size |?~ H5) $ text "Fifth Header"
  |]

  mkExample "Icon Headers" (def
    & subtitle ?~ text "A header can be formatted to emphasise an icon")
    [example|
  pageHeader H2 (def
    & headerConfig_preContent ?~ icon "settings" def
    & headerConfig_largeIcon |~ True
    & headerConfig_aligned |?~ CenterAligned) $ do
    text "Account Settings"
    subHeader $ text "Manage your account settings and set e-mail preferences."

  header (def
    & headerConfig_largeIcon |~ True
    & headerConfig_preContent ?~ icon "users" (def & iconConfig_circular |~ True & iconConfig_inverted |~ True)
    & headerConfig_aligned |?~ CenterAligned) $ do
    text "Friends"
  |]

  mkExample "Sub Headers" (def
    & subtitle ?~ text "A header may be formatted to label smaller or de-emphasised content")
    [example|
  list (def & listConfig_horizontal |~ True) $ do
    let animal name = image (def & imageConfig_size |?~ Mini & imageConfig_shape |?~ Circular) $
          Left $ Img (pure $ "images/animals/" <> name <> ".png") def
    listItem (def & listItemConfig_preContent ?~ animal "bat") $ do
      header (def & headerConfig_sub |~ True) $ text "Benjamin"
      text "Bat"

    listItem (def & listItemConfig_preContent ?~ animal "horse") $ do
      header (def & headerConfig_sub |~ True) $ text "Harriet"
      text "Horse"

    listItem (def & listItemConfig_preContent ?~ animal "monkey") $ do
      header (def & headerConfig_sub |~ True) $ text "Molly"
      text "Monkey"
  |]

  mkExample "Image" (def
    & subtitle ?~ text "A header may contain an image before the content")
    [example|
  let url = "images/animals/penguin.png"
  header (def & headerConfig_preContent ?~ image (def & imageConfig_shape |?~ Circular) (Left $ Img url def)) $ do
    text "Penelope"
    subHeader $ text "Penguin"
  |]

  mkExample "Icon" (def
    & subtitle ?~ text "A header may contain an icon before the content")
    [example|
  pageHeader H2 (def & headerConfig_preContent ?~ icon "plug" def) $ text "Uptime Guarantee"

  pageHeader H2 (def & headerConfig_preContent ?~ icon "settings" def) $ do
    text "Account Settings"
    subHeader $ text "Manage your preferences"
  |]

  mkExample "Icon" (def
    & subtitle ?~ text "A header may contain an icon inside the content")
    [example|
  pageHeader H2 def $ do
    text "Add calendar event"
    icon "add to calendar" $ def
      & iconConfig_link |~ True & iconConfig_color |?~ Teal
      & style |~ Style "margin-left: 0.5em"
  |]

  mkExample "Subheader" (def
    & subtitle ?~ text "A header may contain a subheader")
    [example|
  pageHeader H1 def $ do
    text "First Header"
    subHeader $ text "Sub Header"
  pageHeader H2 def $ do
    text "Second Header"
    subHeader $ text "Sub Header"
  pageHeader H3 def $ do
    text "Third Header"
    subHeader $ text "Sub Header"
  pageHeader H4 def $ do
    text "Fourth Header"
    subHeader $ text "Sub Header"
  pageHeader H5 def $ do
    text "Fifth Header"
    subHeader $ text "Sub Header"
  |]

  mkExample "Disabled" (def
    & subtitle ?~ text "A header may show that it is inactive")
    [example|
  header (def & headerConfig_disabled |~ True) $ text "Disabled Header"
  |]

  mkExample "Block" (def
    & subtitle ?~ text "A header can be formatted to appear inside a content block")
    [example|
  pageHeader H3 (def & headerConfig_block |~ True) $ text "Block Header"
  |]

  mkExample "Dividing" (def
    & subtitle ?~ text "A header can be formatted to divide itself from the content below it")
    [example|
  paragraph $ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam at diam mauris. Cras quis purus fringilla, maximus ex volutpat, tristique velit. In in pulvinar tellus, a ultrices erat. Quisque sit amet gravida lectus, vel viverra mauris."
  pageHeader H3 (def & headerConfig_dividing |~ True) $ text "Dividing Header"
  paragraph $ text "Nam diam neque, euismod nec maximus ut, lacinia egestas ex. Etiam condimentum finibus venenatis. Sed commodo lobortis dolor nec molestie. Maecenas commodo metus diam, nec accumsan est consectetur quis. Nunc sed est nunc. Duis turpis ipsum, vulputate non mi placerat, vehicula dignissim est. Integer venenatis tortor nec massa dapibus, sed laoreet eros lacinia."
  |]

  mkExample "Attached" (def
    & subtitle ?~ text "A header can be attached to other content such as a segment")
    [example|
  header (def & headerConfig_attached |?~ TopAttached) $ text "Top Attached"
  segment (def & segmentConfig_attached |?~ Attached) $ paragraph $ text "Nullam tincidunt, elit et placerat convallis, lacus erat convallis turpis, id tincidunt eros leo ac felis. Nam tincidunt eget ligula vel cursus. Nam eu vehicula lacus. Sed quis tellus nec massa semper condimentum sit amet non libero."
  header (def & headerConfig_attached |?~ Attached) $ text "Attached"
  segment (def & segmentConfig_attached |?~ Attached) $ paragraph $ text "Aliquam tincidunt libero nec turpis porta consectetur. Nam eget ex auctor, sagittis diam at, pharetra leo. Phasellus venenatis iaculis sem id posuere."
  header (def & headerConfig_attached |?~ BottomAttached) $ text "Bottom Attached"
  |]

  mkExample "Floating" (def
    & subtitle ?~ text "A header can sit to the left or right of other content")
    [example|
  header (def & headerConfig_floated |?~ LeftFloated) $
    text "Go Back"
  header (def & headerConfig_floated |?~ RightFloated) $
    text "Go Forward"
  |]

  mkExample "Text Alignment" (def
    & subtitle ?~ text "A header can have its text aligned to a side")
    [example|
  header (def & headerConfig_aligned |?~ RightAligned) $ text "Right"
  header (def & headerConfig_aligned |?~ LeftAligned) $ text "Left"
  header (def & headerConfig_aligned |?~ Justified) $
    text "This should take up the full width even if only one line"
  header (def & headerConfig_aligned |?~ CenterAligned) $ text "Center"
  |]

  mkExample "Colored" (def
    & subtitle ?~ text "A header can be formatted with different colors")
    [example|
  let makeHeader c = header (def & headerConfig_color |?~ c) $ text $ tshow c
  traverse_ makeHeader [Red .. Grey]
  |]

  mkExample "Inverted" (def
    & subtitle ?~ text "A header can have its colors inverted for contrast")
    [example|
  segment (def & segmentConfig_inverted |~ True) $ do
    let mkHeader c = header (def & headerConfig_color |?~ c & headerConfig_inverted |~ True) $
          text $ tshow c
    traverse_ mkHeader [Red .. Grey]
  |]

  return ()

