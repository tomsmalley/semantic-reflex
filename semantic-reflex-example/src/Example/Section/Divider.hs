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
import Reflex.Dom.SemanticUI
import Reflex.Dom.Core (text)

import Example.QQ
import Example.Common

dividers :: forall t m. MonadWidget t m => Section t m
dividers = Section "Divider" (text "A divider visually segments content into groups") $ do

  upstreamIssue 4342 "Vertical dividers are not exposed here due to them being broken upstream."

  hscode $ $(printDefinition id stripParens ''DividerConfig)

  pageHeader H3 def $ text "Types"

  mkExample "Divider" (def
    & subtitle ?~ text "A standard divider")
    [example|
  paragraph $ text "Donec tristique velit ut lacus facilisis, id interdum elit eleifend. Nulla id vulputate ipsum. Nunc efficitur ex at tempus pulvinar. Morbi gravida viverra gravida. Sed dapibus, nulla vitae sodales faucibus, sem massa mollis lacus, et lobortis dui ex sit amet ante. Praesent auctor gravida elit. Curabitur sit amet sollicitudin nisl."
  divider def
  paragraph $ text "Maecenas hendrerit nisl at metus vestibulum, sit amet ultrices mauris iaculis. Duis tincidunt nibh eu est tincidunt semper. Nullam eu purus id dui efficitur sagittis vel quis libero. Etiam mollis diam quis tortor tincidunt, non interdum libero tincidunt. Nunc posuere elit id sem tempor, non lacinia metus lobortis. Proin lacinia."
  |]

  mkExample "Content Divider" (def
    & subtitle ?~ text "A horizontal divider with content inside it")
    [example|
  segment (def & segmentConfig_aligned |?~ CenterAligned & segmentConfig_basic |~ True) $ do
    input def $ do
      textInput $ def & textInputConfig_placeholder |~ "Search..."
    contentDivider def $ text "Or"
    button (def & buttonConfig_color |?~ Teal) $ text "Create New Order"
  |]

  mkExample "Header Divider" (def
    & subtitle ?~ text "A content divider can include a header")
    [example|
  contentDivider def $
    pageHeader H4 (def & headerConfig_preContent ?~ icon "tag" def) $ text "Description"
  paragraph $ text "Quisque ac efficitur dolor. Vestibulum ut elit id eros congue dapibus. Pellentesque sollicitudin a erat bibendum placerat. Curabitur in tellus sollicitudin, dapibus eros sit amet, eleifend risus. Vestibulum tempor erat a tellus gravida venenatis. Nulla auctor metus quis leo posuere, eget dignissim leo condimentum. In hac habitasse platea dictumst. Donec."
  |]

  mkExample "Inverted" (def
    & subtitle ?~ text "A divider can have its colors inverted")
    [example|
  segment (def & segmentConfig_inverted |~ True) $ do
    paragraph $ text "Vestibulum orci nisl, ultrices at consequat commodo, pretium venenatis risus. Vivamus imperdiet massa sed posuere scelerisque. Duis egestas felis sed est fringilla, eget tempor nibh vestibulum. Fusce tempus enim non dolor ultrices fringilla. Pellentesque sagittis consectetur ante eu condimentum."
    divider $ def & dividerConfig_inverted |~ True
    paragraph $ text "Nulla lacinia velit sapien, nec maximus turpis ornare a. Donec libero sapien, dignissim a tortor sed, iaculis feugiat libero. Donec dignissim sapien eget eros malesuada, vitae blandit leo sodales. Donec ante felis, porta at lectus vitae, condimentum lobortis libero. Donec a pretium massa. Vivamus eget malesuada leo, quis consequat metus."
    contentDivider (def & dividerConfig_inverted |~ True) $ text "Horizontal"
  |]

  mkExample "Fitted" (def
    & subtitle ?~ text "A divider can be closely fitted")
    [example|
  segment def $ do
    text "Duis lectus magna, egestas euismod dolor non, ornare ornare leo. Mauris et dolor quis purus ornare laoreet. Pellentesque euismod mi tellus, eget lacinia sapien malesuada dapibus. Duis ac efficitur ante."
    divider $ def & dividerConfig_fitted |~ True
    text "Sed in odio vel lectus mattis molestie a fringilla nibh. Aliquam et rhoncus augue. Ut tempor est eu est bibendum, imperdiet semper lectus vehicula. Proin ultrices, turpis in ullamcorper accumsan, mauris magna malesuada massa, gravida malesuada purus metus sed ante."
  |]

  mkExample "Hidden" (def
    & subtitle ?~ text "A hidden divider divides content without a visible line")
    [example|
  text "Maecenas maximus sapien sit amet neque vulputate venenatis. Sed accumsan egestas pellentesque. Nam ullamcorper urna orci, a egestas felis placerat quis. Vivamus aliquet risus vel nunc pretium tincidunt sed eu nunc."
  divider $ def & dividerConfig_hidden |~ True
  text "Donec mauris nulla, placerat et venenatis eu, eleifend sit amet risus. Suspendisse ornare varius accumsan. Donec quis massa augue. Nam ac urna enim."
  |]

  mkExample "Section" (def
    & subtitle ?~ text "A divider can have greater margins for dividing sections")
    [example|
  header def $ text "Section One"
  paragraph $ text "Nam ac sapien rutrum, tempor purus vel, porttitor neque. Aliquam molestie tellus in sem laoreet bibendum. In accumsan metus a diam consectetur, ut sagittis libero dapibus. Duis at elementum urna. Proin rhoncus odio eget sem auctor pharetra. Donec sit amet lobortis ante. Etiam placerat nibh at blandit euismod. Maecenas at ex quam."
  divider $ def & dividerConfig_section |~ True
  header def $ text "Section Two"
  paragraph $ text "Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Donec semper ut tellus nec tincidunt. Curabitur in turpis pulvinar dui sodales placerat quis pulvinar felis. Integer auctor velit quis nunc placerat dignissim."
  |]

  mkExample "Clearing" (def
    & subtitle ?~ text "A divider can clear floated content")
    [example|
  header (def & headerConfig_floated |?~ RightFloated) $ text "Floated Content"
  divider $ def & dividerConfig_clearing |~ True
  paragraph $ text "Phasellus aliquet, orci vitae tempor gravida, magna felis aliquam sapien, sit amet feugiat mi lorem at risus. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos."
  |]

  return ()
