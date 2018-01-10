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
import Reflex.Dom.Core hiding (Checkbox, CheckboxConfig, checkbox, button)

import Example.QQ
import Example.Common

checkboxes :: forall t m. MonadWidget t m => Section t m
checkboxes = LinkedSection "Checkbox" (simpleLink "https://semantic-ui.com/modules/checkbox.html") $ do

  message (def & messageType |?~ InfoMessage) $ paragraph $ do
    icon "announcement" def
    text "The implementation of the Checkbox module does not depend on the Semantic UI or jQuery Javascript libraries."

  paragraph $ text "Checkboxes consist of a label and a configuration."

  hscode $(printDefinition id stripParens ''Checkbox)

  paragraph $ text "The configuration allows you to set the checkboxValue, indeterminate state, Semantic UI checkbox type, fitted property, and disabled state. The value and indeterminate states are separated into 'initial' and 'set' in order to disconnect them from the resultant dynamic values."

  hscode $(printDefinition id stripParens ''CheckboxConfig)
  hscode $(printDefinition oneline id ''CheckboxType)

  paragraph $ do
    text "Running the checkbox gives access to the dynamic value, the change event (only changes caused by the user), the current indeterminate state and whether the checkbox has focus."

  hscode $(printDefinition id stripParens ''Checkbox)

  pageHeader H3 def $ text "Examples"

  mkExample "Checkbox" (def
    & dynamic ?~ dynCode
    & subtitle ?~ text "Standard checkbox styles")
    [resetExample|
  \resetEvent -> do
    normal <- checkbox "Normal checkbox" $ def
      & checkboxSetValue . event ?~ (False <$ resetEvent)

    divider $ def & dividerHidden |~ True

    toggle <- checkbox "Toggle checkbox (checked by default)" $ def
      & checkboxType |?~ Toggle
      & checkboxSetValue . event ?~ (True <$ resetEvent)
      & checkboxSetValue . initial .~ True

    divider $ def & dividerHidden |~ True

    slider <- checkbox "Slider checkbox" $ def
      & checkboxType |?~ Slider
      & checkboxSetValue . event ?~ (False <$ resetEvent)

    return $ traverse (view checkboxValue) [normal, toggle, slider]
  |]

  mkExample "Disabled" (def
    & dynamic ?~ dynCode
    & subtitle ?~ text "Checkboxes can be enabled or disabled")
    [resetExample|
  \resetEvent -> do
    enabled <- buttons def $ do
      enable <- button def $ text "Enable"
      disable <- button def $ text "Disable"
      holdDyn False $ leftmost
        [ True <$ enable, False <$ disable, False <$ resetEvent ]

    divider $ def & dividerHidden |~ True

    normal <- checkbox "Initially disabled" $ def
      & checkboxSetValue . event ?~ (False <$ resetEvent)
      & checkboxDisabled .~ Dynamic (fmap not enabled)

    divider $ def & dividerHidden |~ True

    toggle <- checkbox "Initially disabled (checked by default)" $ def
      & checkboxType |?~ Toggle
      & checkboxSetValue . event ?~ (True <$ resetEvent)
      & checkboxSetValue . initial .~ True
      & checkboxDisabled .~ Dynamic (fmap not enabled)

    return $ traverse (view checkboxValue) [normal, toggle]
  |]

  mkExample "Indeterminate" (def
    & dynamic ?~ dynShowCode
    & subtitle ?~ text "Checkboxes can be indeterminate")
    [resetExample|
  \resetEvent -> do
    eIndeterminate <- buttons def $ do
      enable <- button def $ text "Indeterminate"
      disable <- button def $ text "Determinate"
      return $ leftmost
        [ True <$ enable, False <$ disable, True <$ resetEvent ]

    divider $ def & dividerHidden |~ True

    checkbox "Indeterminate" $ def
      & checkboxSetValue . event ?~ (True <$ resetEvent)
      & checkboxSetIndeterminate . event ?~ eIndeterminate
      & checkboxSetIndeterminate . initial .~ True
      & checkboxSetValue . initial .~ True
  |]

  mkExample "Fitted" (def
    & dynamic ?~ dynCode
    & subtitle ?~ text "A fitted checkbox does not leave padding for a label")
    [resetExample|
  \resetEvent -> do
    normal <- segment (def & segmentCompact |~ True)
      $ checkbox "" $ def
        & checkboxFitted |~ True
        & checkboxSetValue . event ?~ (False <$ resetEvent)

    slider <- segment (def & segmentCompact |~ True)
      $ checkbox "" $ def
        & checkboxFitted |~ True
        & checkboxType |?~ Toggle
        & checkboxSetValue . event ?~ (False <$ resetEvent)

    toggle <- segment (def & segmentCompact |~ True)
      $ checkbox "" $ def
        & checkboxFitted |~ True
        & checkboxType |?~ Slider
        & checkboxSetValue . event ?~ (False <$ resetEvent)

    return $ traverse (view checkboxValue) [normal, slider, toggle]
  |]

  return ()

