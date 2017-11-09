{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Semantic UI forms.
-- https://semantic-ui.com/collections/form.html
module Reflex.Dom.SemanticUI.Form where

import Data.Default
import Reflex

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data FormConfig t = FormConfig
  { _config :: ActiveElConfig t
  -- ^ Config
  }

instance Reflex t => Default (FormConfig t) where
  def = FormConfig
    { _config = def
    }

-- | Make the form div classes from the configuration
formConfigClasses :: Reflex t => FormConfig t -> Active t Classes
formConfigClasses FormConfig {..} = activeClasses
  [ Static $ Just "ui form"
  ]

-- | Form UI Element.
data Form t m a = Form
  { _config :: FormConfig t
  , _content :: Component Form m a
  }
