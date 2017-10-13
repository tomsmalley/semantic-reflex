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
import Reflex.Dom.SemanticUI.Button
import Reflex.Dom.SemanticUI.Checkbox
import Reflex.Dom.SemanticUI.Divider
import Reflex.Dom.SemanticUI.Dropdown
import Reflex.Dom.SemanticUI.Header
import Reflex.Dom.SemanticUI.RadioGroup
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
$(makeFieldsNoPrefix ''ButtonConfig)
$(makeFieldsNoPrefix ''ButtonsConfig)
$(makeFieldsNoPrefix ''ConditionalConfig)

$(makeFieldsNoPrefix ''Input)
$(makeFieldsNoPrefix ''InputConfig)
$(makeFieldsNoPrefix ''InputResult)

$(makeFieldsNoPrefix ''Checkbox)
$(makeFieldsNoPrefix ''CheckboxResult)
$(makeFieldsNoPrefix ''CheckboxConfig)

{-
$(makeFieldsNoPrefix ''Dropdown)
$(makeFieldsNoPrefix ''DropdownConfig)

$(makeFieldsNoPrefix ''DropdownItem)
$(makeFieldsNoPrefix ''DropdownItemConfig)

$(makeFieldsNoPrefix ''RadioItem)
$(makeFieldsNoPrefix ''RadioItemConfig)
$(makeFieldsNoPrefix ''RadioGroupConfig)
-}

$(makeFieldsNoPrefix ''MenuConfig)
$(makeFieldsNoPrefix ''MenuItemConfig)

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
-- $(makeFieldsNoPrefix ''Transition)
$(makeFieldsNoPrefix ''AnimationConfig)
-- $(makeFieldsNoPrefix ''ActiveElConfig)
-- $(makeFieldsNoPrefix ''Animation)

instance HasConfig a (ActiveElConfig t) => HasAttributes t a where
  attributes = config . elConfigAttributes

instance HasConfig a (ActiveElConfig t) => HasStyle t a where
  style = config . elConfigStyle

instance HasConfig a (ActiveElConfig t) => HasClasses t a where
  classes = config . elConfigClasses

instance HasConfig a (ActiveElConfig t) => HasTransition t a where
  transition = config . elConfigTransition
