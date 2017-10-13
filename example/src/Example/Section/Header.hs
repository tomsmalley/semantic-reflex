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
import Control.Monad ((<=<), void, when, join)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

headers :: MonadWidget t m => Section m
headers = LinkedSection "Header" blank $ do

  hscode $ $(printDefinition id stripParens ''Header)
  hscode $ $(printDefinition id stripParens ''HeaderConfig)

  divClass "ui equal width stackable grid" $ do

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Page Headers" "" [mkExample|
        ui $ PageHeader H1 def $ ui $ Content def $ text "First Header"
        ui $ PageHeader H2 def $ ui $ Content def $ text "Second Header"
        ui $ PageHeader H3 def $ ui $ Content def $ text "Third Header"
        ui $ PageHeader H4 def $ ui $ Content def $ text "Fourth Header"
        ui $ PageHeader H5 def $ ui $ Content def $ text "Fifth Header"
        |]

      divClass "column" $ do
        exampleCard "Content Headers" "" [mkExample|
        ui $ ContentHeader (def & size |?~ H1) $ ui $ Content def $ text "First Header"
        ui $ ContentHeader (def & size |?~ H2) $ ui $ Content def $ text "Second Header"
        ui $ ContentHeader (def & size |?~ H3) $ ui $ Content def $ text "Third Header"
        ui $ ContentHeader (def & size |?~ H4) $ ui $ Content def $ text "Fourth Header"
        ui $ ContentHeader (def & size |?~ H5) $ ui $ Content def $ text "Fifth Header"
        |]

{-
    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Icon Headers" "A header can be formatted to emphasize an icon"
          [mkExample|
        ui $ Header H2 (text "Account Settings") $ def
          & subHeader ?~ ui (Text "Manage your account settings and set e-mail preferences.")
          & icon .~ AlwaysRender (Icon "settings" def)
          & iconHeader |~ True
          & aligned |?~ CenterAligned

        ui $ Header H2 (text "Friends") $ def
          & icon .~ AlwaysRender (Icon "users" $ def & circular |~ True)
          & iconHeader |~ True
          & aligned |?~ CenterAligned
        |]
-}


    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Sub Headers" "A header may be formatted to label smaller or de-emphasized content." [mkExample|
        ui $ PageHeader H2 (def & sub |~ True) $ ui $ Content def $ text "Price"
        el "span" `mapRestrict` text "£10.99"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Sub Headers" "A header may be formatted to label smaller or de-emphasized content." [mkExample|
        divClass "ui horizontal list" $ do
          divClass "item" $ do
            ui $ Image "images/animals/bat.png" $ def
              & size |?~ Mini
              & shape |?~ Circular
            divClass "content" $ do
              ui $ ContentHeader (def & sub |~ True) $ ui $ Content def $ text "Benjamin"
              text "Bat"

          divClass "item" $ do
            ui $ Image "images/animals/horse.png" $ def
              & size |?~ Mini
              & shape |?~ Circular
            divClass "content" $ do
              ui $ ContentHeader (def & sub |~ True) $ ui $ Content def $ text "Harriet"
              text "Horse"

          divClass "item" $ do
            ui $ Image "images/animals/monkey.png" $ def
              & size |?~ Mini
              & shape |?~ Circular
            divClass "content" $ do
              ui $ ContentHeader (def & sub |~ True) $ ui $ Content def $ text "Molly"
              text "Monkey"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Image" "A header may contain an image before the content"
          [mkExample|
        let url = "images/animals/penguin.png"
        ui $ ContentHeader def $ do
          ui $ Image url $ def & shape |?~ Circular
          ui $ Content def $ do
            text "Penelope"
            ui $ SubHeader $ text "Penguin"
        |]

{-
    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Image" "Desired behaviour: suck up the first image or icon and ignore the rest, placing *other* valid elements into the content div. Allow image to be placed anywhere in the do block."
          [mkExample|
        let url = "images/animals/penguin.png"
        ui $ ContentHeader def $ do
          text "Penelope"
          ui $ Image url $ def & shape |~ Circular
          ui $ Icon "error" def
        |]
      -}

      divClass "column" $ do
        exampleCard "Icon" "A header may contain an icon before the content"
          [mkExample|
        ui $ PageHeader H2 def $ do
          ui $ Icon "plug" def
          ui $ Content def $ text "Uptime Guarantee"

        ui $ PageHeader H2 def $ do
          ui $ Icon "settings" def
          ui $ Content def $ do
            text "Account Settings"
            ui $ SubHeader $ text "Manage your preferences"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Icon" "A header may contain an icon inside the content" [mkExample|
        ui_ $ PageHeader H2 def $ ui $ Content def $ do
          text "Add calendar event"
          ui $ Icon "add to calendar" $ def
            & link |~ True & color |?~ Teal
            & style |~ Style ("margin-left" =: "0.5em")
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Subheader" "Headers may contain subheaders"
          [mkExample|
        ui $ PageHeader H1 def $ ui $ Content def $ do
          text "First Header"
          ui $ SubHeader $ text "Sub Header"
        ui $ PageHeader H2 def $ ui $ Content def $ do
          text "Second Header"
          ui $ SubHeader $ text "Sub Header"
        ui $ PageHeader H3 def $ ui $ Content def $ do
          text "Third Header"
          ui $ SubHeader $ text "Sub Header"
        ui $ PageHeader H4 def $ ui $ Content def $ do
          text "Fourth Header"
          ui $ SubHeader $ text "Sub Header"
        ui $ PageHeader H5 def $ ui $ Content def $ do
          text "Fifth Header"
          ui $ SubHeader $ text "Sub Header"
        |]

      divClass "column" $ do
        divClass "ui grid" $ do
          divClass "row" $ do
            divClass "column" $ do
              exampleCard "Disabled" "A header can show that it is inactive"
                [mkExample|
              ui $ ContentHeader (def & disabled |~ True) $ ui $ Content def $ text "Disabled Header"
              |]

          divClass "row" $ do
            divClass "column" $ do
              exampleCard "Block" "A header can be formatted to appear inside a content block"
                [mkExample|
              ui $ PageHeader H3 (def & block |~ True) $ ui $ Content def $ text "Block Header"
              |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Dividing" "A header can be formatted to divide itself from the content below it"
          [mkExample|
        ui $ Paragraph $ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam at diam mauris. Cras quis purus fringilla, maximus ex volutpat, tristique velit. In in pulvinar tellus, a ultrices erat. Quisque sit amet gravida lectus, vel viverra mauris."
        ui $ PageHeader H3 (def & dividing |~ True) $ ui $ Content def $ text "Dividing Header"
        ui $ Paragraph $ text "Nam diam neque, euismod nec maximus ut, lacinia egestas ex. Etiam condimentum finibus venenatis. Sed commodo lobortis dolor nec molestie. Maecenas commodo metus diam, nec accumsan est consectetur quis. Nunc sed est nunc. Duis turpis ipsum, vulputate non mi placerat, vehicula dignissim est. Integer venenatis tortor nec massa dapibus, sed laoreet eros lacinia."
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Attached" "A header can be attached to other content, like a segment"
          [mkExample|
        ui $ ContentHeader (def & attached |?~ TopAttached) $ ui $ Content def $ text "Top Attached"
        ui $ Segment (def & attached |?~ Attached) $ ui $ Paragraph $ text "Nullam tincidunt, elit et placerat convallis, lacus erat convallis turpis, id tincidunt eros leo ac felis. Nam tincidunt eget ligula vel cursus. Nam eu vehicula lacus. Sed quis tellus nec massa semper condimentum sit amet non libero."
        ui $ ContentHeader (def & attached |?~ Attached) $ ui $ Content def $ text "Attached"
        ui $ Segment (def & attached |?~ Attached) $ ui $ Paragraph $ text "Aliquam tincidunt libero nec turpis porta consectetur. Nam eget ex auctor, sagittis diam at, pharetra leo. Phasellus venenatis iaculis sem id posuere."
        ui $ ContentHeader (def & attached |?~ BottomAttached) $ ui $ Content def $ text "Bottom Attached"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Floating" "A header can sit to the left or right of other content"
          [mkExample|
        ui $ Segment (def & clearing |~ True) $ do
          ui $ ContentHeader (def & floated |?~ LeftFloated) $
            ui $ Content def $ text "Go Back"
          ui $ ContentHeader (def & floated |?~ RightFloated) $
            ui $ Content def $ text "Go Forward"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Text Alignment" "A header can have its text aligned to a side"
          [mkExample|
        ui $ Segment def $ do
          ui $ ContentHeader (def & aligned |?~ RightAligned) $ ui $ Content def $ text "Right"
          ui $ ContentHeader (def & aligned |?~ LeftAligned) $ ui $ Content def $ text "Left"
          ui $ ContentHeader (def & aligned |?~ Justified) $
            ui $ Content def $ text "This should take up the full width even if only one line"
          ui $ ContentHeader (def & aligned |?~ CenterAligned) $ ui $ Content def $ text "Center"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Colored" "A header can be formatted with different colors"
          [mkExample|
        ui $ Segment def $ do
          let makeHeader c = ContentHeader (def & color |?~ c) $
                ui $ Content def $ text $ Static $ T.pack $ show c
          mapM_ (ui_ . makeHeader) [Red .. Grey]
        |]

      divClass "column" $ do
        exampleCard "Inverted" "A header can have its colors inverted for contrast"
          [mkExample|
        ui $ Segment (def & inverted |~ True) $ do
          let makeHeader c = ContentHeader (def & color |?~ c & inverted |~ True) $
                ui $ Content def $ text $ Static $ T.pack $ show c
          mapM_ (ui_ . makeHeader) [Red .. Grey]
        |]
