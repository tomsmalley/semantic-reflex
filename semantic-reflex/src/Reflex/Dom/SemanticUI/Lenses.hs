{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE UndecidableInstances #-}

module Reflex.Dom.SemanticUI.Lenses where

import Control.Lens.TH
import Reflex.Dom.SemanticUI.Button
import Reflex.Dom.SemanticUI.Checkbox
import Reflex.Dom.SemanticUI.Dropdown
import Reflex.Dom.SemanticUI.Header
import Reflex.Dom.SemanticUI.RadioGroup
import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Input
import Reflex.Dom.SemanticUI.Image
import Reflex.Dom.SemanticUI.Label
import Reflex.Dom.SemanticUI.Menu
import Reflex.Dom.SemanticUI.Message

$(makeFieldsNoPrefix ''Button)
$(makeFieldsNoPrefix ''ButtonConfig)

$(makeFieldsNoPrefix ''Input)
$(makeFieldsNoPrefix ''InputConfig)
$(makeFieldsNoPrefix ''InputResult)

$(makeFieldsNoPrefix ''Checkbox)
$(makeFieldsNoPrefix ''CheckboxResult)
$(makeFieldsNoPrefix ''CheckboxConfig)

$(makeFieldsNoPrefix ''Dropdown)
$(makeFieldsNoPrefix ''DropdownConfig)

$(makeFieldsNoPrefix ''DropdownItem)
$(makeFieldsNoPrefix ''DropdownItemConfig)

$(makeFieldsNoPrefix ''RadioItem)
$(makeFieldsNoPrefix ''RadioItemConfig)
$(makeFieldsNoPrefix ''RadioGroupConfig)

$(makeFieldsNoPrefix ''LabelConfig)

$(makeFieldsNoPrefix ''ImageConfig)
$(makeFieldsNoPrefix ''IconConfig)
$(makeFieldsNoPrefix ''IconsConfig)
$(makeFieldsNoPrefix ''HeaderConfig)

$(makeFieldsNoPrefix ''MenuConfig)
$(makeFieldsNoPrefix ''MenuItemConfig)

$(makeFieldsNoPrefix ''Message)
-- $(makeFieldsNoPrefix ''MessageResult)
$(makeFieldsNoPrefix ''MessageConfig)

