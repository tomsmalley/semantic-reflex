{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Divider where

import Control.Lens
import Control.Monad ((<=<), void, when, join)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

dividers :: forall t m. MonadWidget t m => Section m
dividers = LinkedSection "Divider" (text "A divider visually segments content into groups") $ do

  let conf = flip (set message) def $ Just $ do
        ui $ Icon "warning" def
        text "Vertical dividers are not exposed here due to them being broken upstream. See Semantic UI issue "
        ui $ Anchor (text "#4342") $ def
          & href |?~ "https://github.com/Semantic-Org/Semantic-UI/issues/4342"
        text "."
  ui $ Message $ conf & messageType |?~ WarningMessage

  hscode $ $(printDefinition id stripParens ''Divider)
  hscode $ $(printDefinition id stripParens ''ContentDivider)
  hscode $ $(printDefinition id stripParens ''DividerConfig)

  ui $ PageHeader H3 def $ ui $ Content def $ text "Types"

  divClass "ui equal width stackable grid" $ do

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Divider" "A standard divider" [mkExample|
        ui $ Paragraph $ text "Donec tristique velit ut lacus facilisis, id interdum elit eleifend. Nulla id vulputate ipsum. Nunc efficitur ex at tempus pulvinar. Morbi gravida viverra gravida. Sed dapibus, nulla vitae sodales faucibus, sem massa mollis lacus, et lobortis dui ex sit amet ante. Praesent auctor gravida elit. Curabitur sit amet sollicitudin nisl."
        ui $ Divider def
        ui $ Paragraph $ text "Maecenas hendrerit nisl at metus vestibulum, sit amet ultrices mauris iaculis. Duis tincidunt nibh eu est tincidunt semper. Nullam eu purus id dui efficitur sagittis vel quis libero. Etiam mollis diam quis tortor tincidunt, non interdum libero tincidunt. Nunc posuere elit id sem tempor, non lacinia metus lobortis. Proin lacinia."
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Content Divider" "A horizontal divider with content inside it" [mkExample|
        ui_ $ Segment (def & aligned |?~ CenterAligned & basic |~ True) $ do
          ui $ Input $ def & placeholder |?~ "Search..."
          ui $ ContentDivider def $ text "Or"
          ui $ Button "Create New Order" $ def
            & color |?~ Teal
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Header Divider" "A content divider can include a header" [mkExample|
        ui $ ContentDivider def $ ui $ PageHeader H4 def $ ui $ Content def $ do
          ui $ Icon "tag" def
          text "Description"
        ui $ Paragraph $ text "Quisque ac efficitur dolor. Vestibulum ut elit id eros congue dapibus. Pellentesque sollicitudin a erat bibendum placerat. Curabitur in tellus sollicitudin, dapibus eros sit amet, eleifend risus. Vestibulum tempor erat a tellus gravida venenatis. Nulla auctor metus quis leo posuere, eget dignissim leo condimentum. In hac habitasse platea dictumst. Donec."
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Inverted" "A divider can can have its colors inverted" [mkExample|
        ui $ Segment (def & inverted |~ True) $ do
          ui $ Paragraph $ text "Vestibulum orci nisl, ultrices at consequat commodo, pretium venenatis risus. Vivamus imperdiet massa sed posuere scelerisque. Duis egestas felis sed est fringilla, eget tempor nibh vestibulum. Fusce tempus enim non dolor ultrices fringilla. Pellentesque sagittis consectetur ante eu condimentum."
          ui $ Divider $ def & inverted |~ True
          ui $ Paragraph $ text "Nulla lacinia velit sapien, nec maximus turpis ornare a. Donec libero sapien, dignissim a tortor sed, iaculis feugiat libero. Donec dignissim sapien eget eros malesuada, vitae blandit leo sodales. Donec ante felis, porta at lectus vitae, condimentum lobortis libero. Donec a pretium massa. Vivamus eget malesuada leo, quis consequat metus."
          ui $ ContentDivider (def & inverted |~ True) $ text "Horizontal"
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Fitted" "A divider can can be closely fitted" [mkExample|
        ui $ Segment def $ do
          text "Duis lectus magna, egestas euismod dolor non, ornare ornare leo. Mauris et dolor quis purus ornare laoreet. Pellentesque euismod mi tellus, eget lacinia sapien malesuada dapibus. Duis ac efficitur ante."
          ui $ Divider $ def & fitted |~ True
          text "Sed in odio vel lectus mattis molestie a fringilla nibh. Aliquam et rhoncus augue. Ut tempor est eu est bibendum, imperdiet semper lectus vehicula. Proin ultrices, turpis in ullamcorper accumsan, mauris magna malesuada massa, gravida malesuada purus metus sed ante."
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Hidden" "A hidden divider divides content without a visible line" [mkExample|
        text "Maecenas maximus sapien sit amet neque vulputate venenatis. Sed accumsan egestas pellentesque. Nam ullamcorper urna orci, a egestas felis placerat quis. Vivamus aliquet risus vel nunc pretium tincidunt sed eu nunc."
        ui $ Divider $ def & hidden |~ True
        text "Donec mauris nulla, placerat et venenatis eu, eleifend sit amet risus. Suspendisse ornare varius accumsan. Donec quis massa augue. Nam ac urna enim."
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Section" "A divider can have greater margins for dividing sections" [mkExample|
        ui $ ContentHeader def $ ui $ Content def $ text "Section One"
        ui $ Paragraph $ text "Nam ac sapien rutrum, tempor purus vel, porttitor neque. Aliquam molestie tellus in sem laoreet bibendum. In accumsan metus a diam consectetur, ut sagittis libero dapibus. Duis at elementum urna. Proin rhoncus odio eget sem auctor pharetra. Donec sit amet lobortis ante. Etiam placerat nibh at blandit euismod. Maecenas at ex quam."
        ui $ Divider $ def & section |~ True
        ui $ ContentHeader def $ ui $ Content def $ text "Section Two"
        ui $ Paragraph $ text "Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Donec semper ut tellus nec tincidunt. Curabitur in turpis pulvinar dui sodales placerat quis pulvinar felis. Integer auctor velit quis nunc placerat dignissim."
        |]

    divClass "row" $ do
      divClass "column" $ do
        exampleCard "Clearing" "A divider can clear floated content" [mkExample|
        ui $ ContentHeader (def & floated |?~ RightFloated) $ ui $ Content def $ text "Floated Content"
        ui $ Divider $ def & clearing |~ True
        ui $ Paragraph $ text "Phasellus aliquet, orci vitae tempor gravida, magna felis aliquam sapien, sit amet feugiat mi lorem at risus. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos."
        |]
