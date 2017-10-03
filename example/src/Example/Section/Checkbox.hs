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
import Data.Semigroup ((<>))
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
            $ def & setValue .~ (False <$ resetEvent)
          toggle <- divClass "ui compact segment"
            $ ui $ Checkbox "Toggle checkbox (checked by default)"
            $ def & altType |?~ Toggle
                  & setValue .~ (True <$ resetEvent)
                  & initialValue .~ True
          slider <- divClass "ui compact segment"
            $ ui $ Checkbox "Slider checkbox"
            $ def & altType |?~ Slider
                  & setValue .~ (False <$ resetEvent)
          return $ traverse (view value) [normal, toggle, slider]
        |]

      divClass "column" $ do
        exampleCardDyn id "Disabled checkbox" "Checkboxes can be enabled or disabled" [mkExample|
        \resetEvent -> do
          enable <- ui $ Button "Enable" $ def
            & attached |?~ Horizontally LeftAttached
          disable <- ui $ Button "Disable" $ def
            & attached |?~ Horizontally RightAttached
          enabled <- holdDyn False $ leftmost [True <$ enable, False <$ disable, False <$ resetEvent]
          normal <- divClass "ui segment"
            $ ui $ Checkbox "Initially disabled"
            $ def & setValue .~ (False <$ resetEvent)
                  & disabled .~ Dynamic (fmap not enabled)
          toggle <- divClass "ui segment"
            $ ui $ Checkbox "Initially disabled (checked by default)"
            $ def & altType |?~ Toggle
                  & setValue .~ (True <$ resetEvent)
                  & initialValue .~ True
                  & disabled .~ Dynamic (fmap not enabled)
          display enabled
          return $ traverse (view value) [normal, toggle]
        |]

    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Checkbox states" "Checkboxes can be indeterminate" [mkExample|
        \resetEvent -> do
          indeterminateButton <- ui $ Button "Indeterminate" $ def
            & attached |?~ Horizontally LeftAttached
          determinateButton <- ui $ Button "Determinate" $ def
            & attached |?~ Horizontally RightAttached
          isIndeterminate <- holdDyn True $ leftmost [True <$ indeterminateButton, False <$ determinateButton, True <$ resetEvent]
          cb <- divClass "ui compact segment"
            $ ui $ Checkbox "Indeterminate"
            $ def & setValue .~ (True <$ resetEvent)
                  & setIndeterminate .~ leftmost [True <$ indeterminateButton, False <$ determinateButton, True <$ resetEvent]
                  & initialIndeterminate .~ True
                  & initialValue .~ True
          return $ (,) <$> view value cb <*> view indeterminate cb
        |]

      divClass "column" $ do
        exampleCardDyn id "Fitted checkbox" "A fitted checkbox does not leave padding for a label" [mkExample|
        \resetEvent -> do
          let inlineSegment = elAttr "div" $ "class" =: "ui compact segment"
                                          <> "style" =: "display: table;"
          normal <- inlineSegment $ ui $ Checkbox ""
            $ def & fitted |~ True
                  & setValue .~ (False <$ resetEvent)
          slider <- inlineSegment $ ui $ Checkbox ""
            $ def & fitted |~ True
                  & altType |?~ Toggle
                  & setValue .~ (False <$ resetEvent)
          toggle <- inlineSegment $ ui $ Checkbox ""
            $ def & fitted |~ True
                  & altType |?~ Slider
                  & setValue .~ (False <$ resetEvent)
          return $ traverse (view value) [normal, slider, toggle]
        |]

    return ()

