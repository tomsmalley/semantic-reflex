{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Example.Section.Checkbox where

import Control.Lens
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

checkboxes :: forall t m. MonadWidget t m => Section t m
checkboxes = LinkedSection "Checkbox" (simpleLink "https://semantic-ui.com/modules/checkbox.html") $ do

  ui $ Message (def & messageType |?~ InfoMessage) $ paragraph $ do
    ui $ Icon "announcement" def
    text "The implementation of the Checkbox module does not depend on the Semantic UI or jQuery Javascript libraries."

  paragraph $ text "Checkboxes consist of a label and a configuration."

  hscode $(printDefinition id stripParens ''Checkbox)

  paragraph $ text "The configuration allows you to set the value, indeterminate state, Semantic UI checkbox type, fitted property, and disabled state. The value and indeterminate states are separated into 'initial' and 'set' in order to disconnect them from the resultant dynamic values."

  hscode $(printDefinition id stripParens ''CheckboxConfig)
  hscode $(printDefinition oneline id ''CheckboxType)

  paragraph $ do
    text "Running the checkbox gives access to the dynamic value, the change event (only changes caused by the user), the current indeterminate state and whether the checkbox has focus."

  hscode $(printDefinition id stripParens ''CheckboxResult)

  ui $ PageHeader H3 def $ text "Examples"

  ui $ Example "Checkbox" (def
    & dynamic ?~ dynCode
    & subtitle ?~ text "Standard checkbox styles")
    [resetExample|
  \resetEvent -> do
    normal <- ui $ Checkbox "Normal checkbox" $ def
      & setValue .~ (False <$ resetEvent)

    ui $ Divider $ def & hidden |~ True

    toggle <- ui $ Checkbox "Toggle checkbox (checked by default)" $ def
      & altType |?~ Toggle
      & setValue .~ (True <$ resetEvent)
      & initialValue .~ True

    ui $ Divider $ def & hidden |~ True

    slider <- ui $ Checkbox "Slider checkbox" $ def
      & altType |?~ Slider
      & setValue .~ (False <$ resetEvent)

    return $ traverse (view value) [normal, toggle, slider]
  |]

  ui $ Example "Disabled" (def
    & dynamic ?~ dynCode
    & subtitle ?~ text "Checkboxes can be enabled or disabled")
    [resetExample|
  \resetEvent -> do
    enabled <- ui $ Buttons def $ do
      enable <- ui $ Button def $ text "Enable"
      disable <- ui $ Button def $ text "Disable"
      holdDyn False $ leftmost
        [ True <$ enable, False <$ disable, False <$ resetEvent ]

    ui $ Divider $ def & hidden |~ True

    normal <- ui $ Checkbox "Initially disabled" $ def
      & setValue .~ (False <$ resetEvent)
      & disabled .~ Dynamic (fmap not enabled)

    ui $ Divider $ def & hidden |~ True

    toggle <- ui $ Checkbox "Initially disabled (checked by default)" $ def
      & altType |?~ Toggle
      & setValue .~ (True <$ resetEvent)
      & initialValue .~ True
      & disabled .~ Dynamic (fmap not enabled)

    return $ traverse (view value) [normal, toggle]
  |]

  ui $ Example "Indeterminate" (def
    & dynamic ?~ dynShowCode
    & subtitle ?~ text "Checkboxes can be indeterminate")
    [resetExample|
  \resetEvent -> do
    eIndeterminate <- ui $ Buttons def $ do
      enable <- ui $ Button def $ text "Indeterminate"
      disable <- ui $ Button def $ text "Determinate"
      return $ leftmost
        [ True <$ enable, False <$ disable, True <$ resetEvent ]

    ui $ Divider $ def & hidden |~ True

    ui $ Checkbox "Indeterminate" $ def
      & setValue .~ (True <$ resetEvent)
      & setIndeterminate .~ eIndeterminate
      & initialIndeterminate .~ True
      & initialValue .~ True
  |]

  ui $ Example "Fitted" (def
    & dynamic ?~ dynCode
    & subtitle ?~ text "A fitted checkbox does not leave padding for a label")
    [resetExample|
  \resetEvent -> do
    normal <- ui $ Segment (def & compact |~ True)
      $ ui $ Checkbox "" $ def
        & fitted |~ True
        & setValue .~ (False <$ resetEvent)

    slider <- ui $ Segment (def & compact |~ True)
      $ ui $ Checkbox "" $ def
        & fitted |~ True
        & altType |?~ Toggle
        & setValue .~ (False <$ resetEvent)

    toggle <- ui $ Segment (def & compact |~ True)
      $ ui $ Checkbox "" $ def
        & fitted |~ True
        & altType |?~ Slider
        & setValue .~ (False <$ resetEvent)

    return $ traverse (view value) [normal, slider, toggle]
  |]

  return ()

