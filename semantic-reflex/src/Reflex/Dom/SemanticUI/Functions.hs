{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Reflex.Dom.SemanticUI.Functions where

import Reflex.Dom.SemanticUI.Button
import Reflex.Dom.SemanticUI.Checkbox
import Reflex.Dom.SemanticUI.Header
import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Image
import Reflex.Dom.SemanticUI.Input
import Reflex.Dom.SemanticUI.Label
import Reflex.Dom.SemanticUI.Message
import Reflex.Dom.SemanticUI.Segment

import Reflex.Dom.SemanticUI.TH

$(makeFunctions ''Button)
$(makeFunctions ''Checkbox)
-- $(makeFunctions ''ContentHeader)
-- $(makeFunctions ''PageHeader)
$(makeFunctions ''Icon)
$(makeFunctions ''Image)
$(makeFunctions ''Input)
$(makeFunctions ''Label)
$(makeFunctions ''Message)
$(makeFunctions ''Segment)
