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
        ui $ Header H1 (ui $ Text "First Header") def
        ui $ Header H2 (ui $ Text "Second Header") def
        ui $ Header H3 (ui $ Text "Third Header") def
        ui $ Header H4 (ui $ Text "Fourth Header") def
        ui $ Header H5 (ui $ Text "Fifth Header") def
        |]

      divClass "column" $ do
        exampleCard "Content Headers" "" [mkExample|
        ui $ Header H1 (ui $ Text "First Header") $ def & header .~ ContentHeader
        ui $ Header H2 (ui $ Text "Second Header") $ def & header .~ ContentHeader
        ui $ Header H3 (ui $ Text "Third Header") $ def & header .~ ContentHeader
        ui $ Header H4 (ui $ Text "Fourth Header") $ def & header .~ ContentHeader
        ui $ Header H5 (ui $ Text "Fifth Header") $ def & header .~ ContentHeader
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Icon Headers" "A header can be formatted to emphasize an icon"
          [mkExample|
        ui $ Header H2 (ui $ Text "Account Settings") $ def
          & subHeader ?~ ui (Text "Manage your account settings and set e-mail preferences.")
          & icon .~ AlwaysRender (Icon "settings" def)
          & iconHeader |~ True
          & aligned |?~ CenterAligned

        ui $ Header H2 (ui $ Text "Friends") $ def
          & icon .~ AlwaysRender (Icon "users" $ def & circular |~ True)
          & iconHeader |~ True
          & aligned |?~ CenterAligned
        |]


    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Sub Headers" "A header may be formatted to label smaller or de-emphasized content."
          [mkExample|
        ui $ Header H2 (ui $ Text "Price") $ def & sub |~ True
        el "span" `mapRestrict` ui (Text "£10.99")
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Sub Headers" "A header may be formatted to label smaller or de-emphasized content."
          [mkExample|
        divClass "ui horizontal list" $ do
          divClass "item" $ do
            ui $ Image "https://semantic-ui.com/images/avatar2/small/molly.png" $ def
              & size |?~ Mini
              & shape |~ Circular
            divClass "content" $ do
              ui $ Header H3 (ui $ Text "Molly") $ def & sub |~ True
              ui $ Text "Coordinator"

          divClass "item" $ do
            ui $ Image "https://semantic-ui.com/images/avatar2/small/elyse.png" $ def
              & size |?~ Mini
              & shape |~ Circular
            divClass "content" $ do
              ui $ Header H3 (ui $ Text "Elyse") $ def & sub |~ True
              ui $ Text "Developer"

          divClass "item" $ do
            ui $ Image "https://semantic-ui.com/images/avatar2/small/eve.png" $ def
              & size |?~ Mini
              & shape |~ Circular
            divClass "content" $ do
              ui $ Header H3 (ui $ Text "Eve") $ def & sub |~ True
              ui $ Text "Project Manager"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Image" "A header may contain an image"
          [mkExample|
        let url = "https://semantic-ui.com/images/avatar2/large/patrick.png"
        ui $ Header H2 (ui $ Text "Patrick") $ def
          & image .~ AlwaysRender (Image url $ def & shape |~ Circular)
        |]

      divClass "column" $ do
        exampleCard "Icon" "A header may contain an icon"
          [mkExample|
        ui $ Header H2 (ui $ Text "Uptime Guarantee") $ def
          & icon .~ AlwaysRender (Icon "plug" def)

        ui $ Header H2 (ui $ Text "Account Settings") $ def
          & icon .~ AlwaysRender (Icon "settings" def)
          & subHeader ?~ ui (Text "Manage your preferences")
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Subheader" "Headers may contain subheaders"
          [mkExample|
        ui $ Header H1 (ui $ Text "First Header") $ def
          & subHeader ?~ ui (Text "Sub Header")
        ui $ Header H2 (ui $ Text "Second Header") $ def
          & subHeader ?~ ui (Text "Sub Header")
        ui $ Header H3 (ui $ Text "Third Header") $ def
          & subHeader ?~ ui (Text "Sub Header")
        ui $ Header H4 (ui $ Text "Fourth Header") $ def
          & subHeader ?~ ui (Text "Sub Header")
        ui $ Header H5 (ui $ Text "Fifth Header") $ def
          & subHeader ?~ ui (Text "Sub Header")
        |]

      divClass "column" $ do
        exampleCard "Disabled" "A header can show that it is inactive"
          [mkExample|
        ui $ Header H3 (ui $ Text "Disabled Header") $ def
          & disabled |~ True
          & header .~ ContentHeader
        |]

        exampleCard "Block" "A header can be formatted to appear inside a content block"
          [mkExample|
        ui $ Header H3 (ui $ Text "Block Header") $ def
          & block |~ True
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Dividing" "A header can be formatted to divide itself from the content below it"
          [mkExample|
        ui $ Paragraph $ ui $ Text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam at diam mauris. Cras quis purus fringilla, maximus ex volutpat, tristique velit. In in pulvinar tellus, a ultrices erat. Quisque sit amet gravida lectus, vel viverra mauris."
        ui $ Header H3 (ui $ Text "Dividing Header") $ def
          & dividing |~ True
        ui $ Paragraph $ ui $ Text "Nam diam neque, euismod nec maximus ut, lacinia egestas ex. Etiam condimentum finibus venenatis. Sed commodo lobortis dolor nec molestie. Maecenas commodo metus diam, nec accumsan est consectetur quis. Nunc sed est nunc. Duis turpis ipsum, vulputate non mi placerat, vehicula dignissim est. Integer venenatis tortor nec massa dapibus, sed laoreet eros lacinia."
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Attached" "A header can be attached to other content, like a segment"
          [mkExample|
        ui $ Header H3 (ui $ Text "Top Attached") $ def
          & attachedSide |?~ TopAttached
          & attached |~ True
        ui $ Segment (def & attached |?~ Attached) $ ui $ Paragraph $ ui $ Text "Nullam tincidunt, elit et placerat convallis, lacus erat convallis turpis, id tincidunt eros leo ac felis. Nam tincidunt eget ligula vel cursus. Nam eu vehicula lacus. Sed quis tellus nec massa semper condimentum sit amet non libero."
        ui $ Header H3 (ui $ Text "Attached") $ def
          & attached |~ True
        ui $ Segment (def & attached |?~ Attached) $ ui $ Paragraph $ ui $ Text "Aliquam tincidunt libero nec turpis porta consectetur. Nam eget ex auctor, sagittis diam at, pharetra leo. Phasellus venenatis iaculis sem id posuere."
        ui $ Header H3 (ui $ Text "Bottom Attached") $ def
          & attachedSide |?~ BottomAttached
          & attached |~ True
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Floating" "A header can sit to the left or right of other content"
          [mkExample|
        ui $ Segment (def & clearing |~ True) $ do
          ui $ Header H3 (ui $ Text "Go Back") $ def
            & floated |?~ LeftFloated
          ui $ Header H3 (ui $ Text "Go Forward") $ def
            & floated |?~ RightFloated
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Text Alignment" "A header can have its text aligned to a side"
          [mkExample|
        ui $ Segment def $ do
          ui $ Header H3 (ui $ Text "Right") $ def
            & aligned |?~ RightAligned
          ui $ Header H3 (ui $ Text "Left") $ def
            & aligned |?~ LeftAligned
          ui $ Header H3 (ui $ Text "This should take up the full width even if only one line") $ def
            & aligned |?~ Justified
          ui $ Header H3 (ui $ Text "Center") $ def
            & aligned |?~ CenterAligned
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Colored" "A header can be formatted with different colors"
          [mkExample|
        ui $ Segment def $ do
          let makeHeader c = Header H3 (ui $ Text $ Static $ T.pack $ show c) $ def
                & color |?~ c
          mapM_ (ui_ . makeHeader) [Red .. Grey]
        |]

      divClass "column" $ do
        exampleCard "Inverted" "A header can have its colors inverted for contrast"
          [mkExample|
        ui $ Segment (def & inverted |~ True) $ do
          let makeHeader c = Header H3 (ui $ Text $ Static $ T.pack $ show c) $ def
                & color |?~ c & inverted |~ True
          mapM_ (ui_ . makeHeader) [Red .. Grey]
        |]
