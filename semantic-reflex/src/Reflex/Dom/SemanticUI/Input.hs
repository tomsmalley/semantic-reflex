{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeApplications         #-}

module Reflex.Dom.SemanticUI.Input where

import Data.Default
import Data.Monoid
import Data.Text (Text)
import Reflex.Dom.Core hiding (Input, fromJSString)

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition
import Reflex.Dom.SemanticUI.Icon

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

instance Default (InputConfig t) where
  def = InputConfig
    { _disabled = Static False
--    , _icon = NeverRender
    , _placeholder = Static Nothing
    , _fluid = Static False
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
