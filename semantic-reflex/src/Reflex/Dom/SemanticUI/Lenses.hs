{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Dom.SemanticUI.Lenses where

import Control.Lens.TH
import Control.Lens

import Data.Map (Map)
import Data.Text (Text)

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common (Style, Classes)

import Reflex.Dom.SemanticUI.Button
import Reflex.Dom.SemanticUI.Checkbox
import Reflex.Dom.SemanticUI.Dimmer
import Reflex.Dom.SemanticUI.Divider
import Reflex.Dom.SemanticUI.Dropdown
import Reflex.Dom.SemanticUI.Header
import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Input
import Reflex.Dom.SemanticUI.Image
import Reflex.Dom.SemanticUI.Label
import Reflex.Dom.SemanticUI.Menu
import Reflex.Dom.SemanticUI.Message
import Reflex.Dom.SemanticUI.Paragraph
import Reflex.Dom.SemanticUI.Segment
import Reflex.Dom.SemanticUI.Transition

$(makeFieldsNoPrefix ''Button)
$(makeFieldsNoPrefix ''AnimatedButton)
$(makeFieldsNoPrefix ''ButtonConfig)
$(makeFieldsNoPrefix ''LabeledButtonConfig)
$(makeFieldsNoPrefix ''ButtonsConfig)
$(makeFieldsNoPrefix ''ConditionalConfig)

$(makeFieldsNoPrefix ''Input)
$(makeFieldsNoPrefix ''InputConfig)
$(makeFieldsNoPrefix ''TextInputConfig)
$(makeFieldsNoPrefix ''TextInputResult)

$(makeFieldsNoPrefix ''Checkbox)
$(makeFieldsNoPrefix ''CheckboxResult)
$(makeFieldsNoPrefix ''CheckboxConfig)

$(makeFieldsNoPrefix ''DropdownConfig)
{-
$(makeFieldsNoPrefix ''Dropdown)

$(makeFieldsNoPrefix ''DropdownItem)
$(makeFieldsNoPrefix ''DropdownItemConfig)

$(makeFieldsNoPrefix ''RadioItem)
$(makeFieldsNoPrefix ''RadioItemConfig)
$(makeFieldsNoPrefix ''RadioGroupConfig)
-}

$(makeFieldsNoPrefix ''MenuConfig)
$(makeFieldsNoPrefix ''MenuItemConfig)

$(makeFieldsNoPrefix ''DimmerConfig)

$(makeFieldsNoPrefix ''DividerConfig)

$(makeFieldsNoPrefix ''LabelConfig)
$(makeFieldsNoPrefix ''LabelAttached)

$(makeFieldsNoPrefix ''ImageConfig)
$(makeFieldsNoPrefix ''IconConfig)
$(makeFieldsNoPrefix ''IconsConfig)
$(makeFieldsNoPrefix ''HeaderConfig)
$(makeFieldsNoPrefix ''AnchorConfig)

$(makeFieldsNoPrefix ''SegmentConfig)

$(makeFieldsNoPrefix ''Message)
-- $(makeFieldsNoPrefix ''MessageResult)
$(makeFieldsNoPrefix ''MessageConfig)

$(makeFieldsNoPrefix ''SetValue')
$(makeFieldsNoPrefix ''TransitionConfig)
$(makeFieldsNoPrefix ''TransConfig)
-- $(makeFieldsNoPrefix ''Transition)
-- $(makeFieldsNoPrefix ''ActiveElConfig)
-- $(makeFieldsNoPrefix ''Animation)

class HasTransition t a where
  transition :: Lens' a (Maybe (TransConfig t))

class HasAttributes t a where
  attributes :: Lens' a (Active t (Map Text Text))

class HasStyle t a where
  style :: Lens' a (Active t Style)

class HasClasses t a where
  classes :: Lens' a (Active t Classes)

instance HasConfig a (ActiveElConfig t) => HasAttributes t a where
  attributes = config . elConfigAttributes

instance HasConfig a (ActiveElConfig t) => HasStyle t a where
  style = config . elConfigStyle

instance HasConfig a (ActiveElConfig t) => HasClasses t a where
  classes = config . elConfigClasses

instance HasConfig a (ActiveElConfig t) => HasTransition t a where
  transition = config . elConfigTransition
