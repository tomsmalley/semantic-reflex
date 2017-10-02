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
headers = LinkedSection "Header" "" $ do

  $(printDefinition stripParens ''Header)
  $(printDefinition stripParens ''HeaderConfig)

  divClass "ui equal width stackable grid" $ do

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Page Headers" "" [mkExample|
        ui $ Header H1 (text "First Header") def
        ui $ Header H2 (text "Second Header") def
        ui $ Header H3 (text "Third Header") def
        ui $ Header H4 (text "Fourth Header") def
        ui $ Header H5 (text "Fifth Header") def
        |]

      divClass "column" $ do
        exampleCard "Content Headers" "" [mkExample|
        ui $ Header H1 (text "First Header") $ def & header .~ ContentHeader
        ui $ Header H2 (text "Second Header") $ def & header .~ ContentHeader
        ui $ Header H3 (text "Third Header") $ def & header .~ ContentHeader
        ui $ Header H4 (text "Fourth Header") $ def & header .~ ContentHeader
        ui $ Header H5 (text "Fifth Header") $ def & header .~ ContentHeader
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Icon Headers" "A header can be formatted to emphasize an icon"
          [mkExample|
        ui $ Header H2 (text "Account Settings") $ def
          & subHeader |?~ "Manage your account settings and set e-mail preferences."
          & icon .~ AlwaysRender (Icon "settings" def)
          & iconHeader |~ True
          & aligned |?~ CenterAligned

        ui $ Header H2 (text "Friends") $ def
          & icon .~ AlwaysRender (Icon "users" $ def & circular |~ True)
          & iconHeader |~ True
          & aligned |?~ CenterAligned
        |]


    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Sub Headers" "A header may be formatted to label smaller or de-emphasized content."
          [mkExample|
        ui $ Header H2 (text "Price") $ def & sub |~ True
        el "span" $ text "£10.99"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Sub Headers" "A header may be formatted to label smaller or de-emphasized content."
          [mkExample|
        divClass "ui horizontal list" $ do
          divClass "item" $ do
            ui $ Image "https://semantic-ui.com/images/avatar2/small/molly.png" $ def
              & size |?~ Mini
              & rounded |?~ Circular
            divClass "content" $ do
              ui $ Header H3 (text "Molly") $ def & sub |~ True
              text "Coordinator"

          divClass "item" $ do
            ui $ Image "https://semantic-ui.com/images/avatar2/small/elyse.png" $ def
              & size |?~ Mini
              & rounded |?~ Circular
            divClass "content" $ do
              ui $ Header H3 (text "Elyse") $ def & sub |~ True
              text "Developer"

          divClass "item" $ do
            ui $ Image "https://semantic-ui.com/images/avatar2/small/eve.png" $ def
              & size |?~ Mini
              & rounded |?~ Circular
            divClass "content" $ do
              ui $ Header H3 (text "Eve") $ def & sub |~ True
              text "Project Manager"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Image" "A header may contain an image"
          [mkExample|
        let url = "https://semantic-ui.com/images/avatar2/large/patrick.png"
        ui $ Header H2 (text "Patrick") $ def
          & image .~ AlwaysRender (Image url $ def & rounded |?~ Circular)
        |]

      divClass "column" $ do
        exampleCard "Icon" "A header may contain an icon"
          [mkExample|
        ui $ Header H2 (text "Uptime Guarantee") $ def
          & icon .~ AlwaysRender (Icon "plug" def)

        ui $ Header H2 (text "Account Settings") $ def
          & icon .~ AlwaysRender (Icon "settings" def)
          & subHeader |?~ "Manage your preferences"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Subheader" "Headers may contain subheaders"
          [mkExample|
        ui $ Header H1 (text "First Header") $ def
          & subHeader |?~ "Sub Header"
        ui $ Header H2 (text "Second Header") $ def
          & subHeader |?~ "Sub Header"
        ui $ Header H3 (text "Third Header") $ def
          & subHeader |?~ "Sub Header"
        ui $ Header H4 (text "Fourth Header") $ def
          & subHeader |?~ "Sub Header"
        ui $ Header H5 (text "Fifth Header") $ def
          & subHeader |?~ "Sub Header"
        |]

      divClass "column" $ do
        exampleCard "Disabled" "A header can show that it is inactive"
          [mkExample|
        ui $ Header H3 (text "Disabled Header") $ def
          & disabled |~ True
          & header .~ ContentHeader
        |]

        exampleCard "Block" "A header can be formatted to appear inside a content block"
          [mkExample|
        ui $ Header H3 (text "Block Header") $ def
          & block |~ True
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Dividing" "A header can be formatted to divide itself from the content below it"
          [mkExample|
        el "p" $ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam at diam mauris. Cras quis purus fringilla, maximus ex volutpat, tristique velit. In in pulvinar tellus, a ultrices erat. Quisque sit amet gravida lectus, vel viverra mauris."
        ui $ Header H3 (text "Dividing Header") $ def
          & dividing |~ True
        el "p" $ text "Nam diam neque, euismod nec maximus ut, lacinia egestas ex. Etiam condimentum finibus venenatis. Sed commodo lobortis dolor nec molestie. Maecenas commodo metus diam, nec accumsan est consectetur quis. Nunc sed est nunc. Duis turpis ipsum, vulputate non mi placerat, vehicula dignissim est. Integer venenatis tortor nec massa dapibus, sed laoreet eros lacinia."
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Attached" "A header can be attached to other content, like a segment"
          [mkExample|
        ui $ Header H3 (text "Top Attached") $ def
          & attachedSide |?~ TopAttached
          & attached |~ True
        divClass "ui attached segment" $ el "p" $ text "Nullam tincidunt, elit et placerat convallis, lacus erat convallis turpis, id tincidunt eros leo ac felis. Nam tincidunt eget ligula vel cursus. Nam eu vehicula lacus. Sed quis tellus nec massa semper condimentum sit amet non libero."
        ui $ Header H3 (text "Attached") $ def
          & attached |~ True
        divClass "ui attached segment" $ el "p" $ text "Aliquam tincidunt libero nec turpis porta consectetur. Nam eget ex auctor, sagittis diam at, pharetra leo. Phasellus venenatis iaculis sem id posuere."
        ui $ Header H3 (text "Bottom Attached") $ def
          & attachedSide |?~ BottomAttached
          & attached |~ True
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Floating" "A header can sit to the left or right of other content"
          [mkExample|
        divClass "ui clearing segment" $ do
          ui $ Header H3 (text "Go Back") $ def
            & floated |?~ LeftFloated
          ui $ Header H3 (text "Go Forward") $ def
            & floated |?~ RightFloated
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Text Alignment" "A header can have its text aligned to a side"
          [mkExample|
        divClass "ui segment" $ do
          ui $ Header H3 (text "Right") $ def
            & aligned |?~ RightAligned
          ui $ Header H3 (text "Left") $ def
            & aligned |?~ LeftAligned
          ui $ Header H3 (text "This should take up the full width even if only one line") $ def
            & aligned |?~ Justified
          ui $ Header H3 (text "Center") $ def
            & aligned |?~ CenterAligned
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Colored" "A header can be formatted with different colors"
          [mkExample|
        divClass "ui segment" $ do
          let makeHeader c = ui $ Header H3 (text $ T.pack $ show c) $ def
                & color |?~ c
          mapM_ makeHeader [Red .. Grey]
        |]

      divClass "column" $ do
        exampleCard "Inverted" "A header can have its colors inverted for contrast"
          [mkExample|
        divClass "ui inverted segment" $ do
          let makeHeader c = ui $ Header H3 (text $ T.pack $ show c) $ def
                & color |?~ c & inverted |~ True
          mapM_ makeHeader [Red .. Grey]
        |]
