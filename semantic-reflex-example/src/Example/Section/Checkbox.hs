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
import Reflex.Dom.Core hiding (Checkbox, CheckboxConfig, checkbox, button, checkboxConfig_setValue, checkbox_value)

import Example.QQ
import Example.Common

checkboxes :: forall t m. MonadWidget t m => Section t m
checkboxes = Section "Checkbox" (simpleLink "https://semantic-ui.com/modules/checkbox.html") $ do

  message (def & messageConfig_type |?~ InfoMessage) $ paragraph $ do
    icon "announcement" def
    text "The implementation of the Checkbox module does not depend on the Semantic UI or jQuery Javascript libraries."

  paragraph $ text "Checkboxes consist of a label and a configuration."

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
    normal <- checkbox (text "Normal checkbox") $ def
      & checkboxConfig_setValue . event ?~ (False <$ resetEvent)

    divider $ def & dividerConfig_hidden |~ True

    toggle <- checkbox (text "Toggle checkbox (checked by default)") $ def
      & checkboxConfig_type |?~ Toggle
      & checkboxConfig_setValue . event ?~ (True <$ resetEvent)
      & checkboxConfig_setValue . initial .~ True

    divider $ def & dividerConfig_hidden |~ True

    slider <- checkbox (text "Slider checkbox") $ def
      & checkboxConfig_type |?~ Slider
      & checkboxConfig_setValue . event ?~ (False <$ resetEvent)

    return $ traverse (view checkbox_value) [normal, toggle, slider]
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

    divider $ def & dividerConfig_hidden |~ True

    normal <- checkbox (text "Initially disabled") $ def
      & checkboxConfig_setValue . event ?~ (False <$ resetEvent)
      & checkboxConfig_disabled .~ Dyn (fmap not enabled)

    divider $ def & dividerConfig_hidden |~ True

    toggle <- checkbox (text "Initially disabled (checked by default)") $ def
      & checkboxConfig_type |?~ Toggle
      & checkboxConfig_setValue . event ?~ (True <$ resetEvent)
      & checkboxConfig_setValue . initial .~ True
      & checkboxConfig_disabled .~ Dyn (fmap not enabled)

    return $ traverse (view checkbox_value) [normal, toggle]
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

    divider $ def & dividerConfig_hidden |~ True

    checkbox (text "Indeterminate") $ def
      & checkboxConfig_setValue . event ?~ (True <$ resetEvent)
      & checkboxConfig_setIndeterminate . event ?~ eIndeterminate
      & checkboxConfig_setIndeterminate . initial .~ True
      & checkboxConfig_setValue . initial .~ True
  |]

  mkExample "Fitted" (def
    & dynamic ?~ dynCode
    & subtitle ?~ text "A fitted checkbox does not leave padding for a label")
    [resetExample|
  \resetEvent -> do
    normal <- segment (def & segmentConfig_compact |~ True)
      $ checkbox blank $ def
        & checkboxConfig_fitted |~ True
        & checkboxConfig_setValue . event ?~ (False <$ resetEvent)

    slider <- segment (def & segmentConfig_compact |~ True)
      $ checkbox blank $ def
        & checkboxConfig_fitted |~ True
        & checkboxConfig_type |?~ Toggle
        & checkboxConfig_setValue . event ?~ (False <$ resetEvent)

    toggle <- segment (def & segmentConfig_compact |~ True)
      $ checkbox blank $ def
        & checkboxConfig_fitted |~ True
        & checkboxConfig_type |?~ Slider
        & checkboxConfig_setValue . event ?~ (False <$ resetEvent)

    return $ traverse (view checkbox_value) [normal, slider, toggle]
  |]

  return ()

