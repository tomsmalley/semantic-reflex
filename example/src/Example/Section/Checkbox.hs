{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Example.Section.Checkbox where

import GHC.Tuple -- TH requires this for (,)
import Control.Lens
import Control.Monad ((<=<), void, when, join)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.SemanticUI

import Example.QQ
import Example.Common

checkboxes :: forall t m. MonadWidget t m => Section m
checkboxes = LinkedSection "Checkbox" "" $ do

  $(printDefinition stripParens ''Checkbox)
  $(printDefinition stripParens ''CheckboxConfig)

  divClass "ui two column stackable grid" $ do
    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Checkbox" "Standard checkbox styles" $ [mkExample|
        \resetEvent -> do
          normal <- divClass "ui compact segment"
            $ ui $ Checkbox "Normal checkbox"
            $ def & setValue .~ (Unchecked <$ resetEvent)
          toggle <- divClass "ui compact segment"
            $ ui $ Checkbox "Toggle checkbox (checked by default)"
            $ def & types .~ [Toggle]
                  & setValue .~ (Checked <$ resetEvent)
                  & initialValue .~ Checked
          slider <- divClass "ui compact segment"
            $ ui $ Checkbox "Slider checkbox"
            $ def & types .~ [Slider]
                  & setValue .~ (Unchecked <$ resetEvent)
          return $ traverse (view value) [normal, toggle, slider]
        |]

      divClass "column" $ do
        exampleCardDyn id "Disabled checkbox" "Checkboxes can be enabled or disabled" [mkExample|
        \resetEvent -> do
          enable <- ui $ Button "Enable" $ def
            & attached |?~ Horizontally LeftAttached
          disable <- ui $ Button "Disable" $ def
            & attached |?~ Horizontally RightAttached
          let enableEvent = leftmost [Enabled <$ enable, Disabled <$ disable]
          normal <- divClass "ui segment"
            $ ui $ Checkbox "Initially disabled"
            $ def & setValue .~ (Unchecked <$ resetEvent)
                  & initialEnabled .~ Disabled
                  & setEnabled .~ leftmost [enableEvent, Disabled <$ resetEvent]
          toggle <- divClass "ui segment"
            $ ui $ Checkbox "Initially disabled (checked by default)"
            $ def & types .~ [Toggle]
                  & setValue .~ (Checked <$ resetEvent)
                  & initialEnabled .~ Disabled
                  & initialValue .~ Checked
                  & setEnabled .~ leftmost [enableEvent, Disabled <$ resetEvent]
          return $ traverse (\cb -> (,) <$> view value cb <*> view enabled cb) [normal, toggle]
        |]

    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Checkbox states" "Checkboxes can be indeterminate" [mkExample|
        \resetEvent -> do
          indeterminateButton <- ui $ Button "Indeterminate" $ def
            & attached |?~ Horizontally LeftAttached
          determinateButton <- ui $ Button "Determinate" $ def
            & attached |?~ Horizontally RightAttached
          let indeterminateEvent = leftmost [Indeterminate <$ indeterminateButton, Determinate <$ determinateButton]
          cb <- divClass "ui compact segment"
            $ ui $ Checkbox "Indeterminate"
            $ def & types .~ []
                  & setValue .~ (Checked <$ resetEvent)
                  & initialIndeterminate .~ Indeterminate
                  & initialValue .~ Checked
                  & setIndeterminate .~ leftmost [indeterminateEvent, Indeterminate <$ resetEvent]
          return $ (,) <$> view value cb <*> view indeterminate cb
        |]

      divClass "column" $ do
        exampleCardDyn id "Fitted checkbox" "A fitted checkbox does not leave padding for a label" [mkExample|
        \resetEvent -> do
          normal <- divClass "ui compact segment"
            $ ui $ Checkbox ""
            $ def & types .~ [Fitted]
                  & setValue .~ (Unchecked <$ resetEvent)
          slider <- divClass "ui compact segment"
            $ ui $ Checkbox ""
            $ def & types .~ [Fitted, Toggle]
                  & setValue .~ (Unchecked <$ resetEvent)
          toggle <- divClass "ui compact segment"
            $ ui $ Checkbox ""
            $ def & types .~ [Fitted, Slider]
                  & setValue .~ (Unchecked <$ resetEvent)
          return $ traverse (view value) [normal, slider, toggle]
        |]

    return ()

