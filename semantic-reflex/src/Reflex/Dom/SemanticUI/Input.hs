{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeApplications         #-}

module Reflex.Dom.SemanticUI.Input where

import Data.Default
import Data.Text (Text)
import Reflex.Dom.Core hiding (Input)

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data Input t = Input
  { _config :: InputConfig t
  }

data InputConfig t = InputConfig
  { _disabled :: Active t Bool
--  , _icon :: RenderWhen t (Icon t)
  , _placeholder :: Active t (Maybe Text)
  , _fluid :: Active t Bool
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (InputConfig t) where
  def = InputConfig
    { _disabled = pure False
--    , _icon = NeverRender
    , _placeholder = pure Nothing
    , _fluid = pure False
    , _config = def
    }

inputConfigClasses :: Reflex t => InputConfig t -> Active t Classes
inputConfigClasses InputConfig {..} = activeClasses
  [ Static $ Just "ui input"
  , boolClass "disabled" _disabled
--  , if isNeverRender _icon then mempty else Just <$> "icon"
  , boolClass "fluid" _fluid
  ]

data InputResult t = InputResult
  { _value :: Dynamic t Text }
