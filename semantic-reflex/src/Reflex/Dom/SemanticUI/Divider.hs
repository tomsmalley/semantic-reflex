{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeApplications         #-}

module Reflex.Dom.SemanticUI.Divider where

import Data.Default
import Reflex.Dom.Core

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data Divider t = Divider
  { _config :: DividerConfig t
  }

-- | In semantic-ui terms, this is a horizontal divider. Vertical dividers are
-- not implemented due to them being broken:
-- https://github.com/Semantic-Org/Semantic-UI/issues/4342
data ContentDivider t m a = ContentDivider
  { _config :: DividerConfig t
  , _content :: Component ContentDivider m a
  }

data DividerConfig t = DividerConfig
  { _inverted :: Active t Bool
  , _fitted :: Active t Bool
  , _hidden :: Active t Bool
  , _section :: Active t Bool
  , _clearing :: Active t Bool
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (DividerConfig t) where
  def = DividerConfig
    { _inverted = pure False
    , _fitted = pure False
    , _hidden = pure False
    , _section = pure False
    , _clearing = pure False
    , _config = def
    }

dividerConfigClasses :: Reflex t => DividerConfig t -> Active t Classes
dividerConfigClasses DividerConfig {..} = activeClasses
  [ Static $ Just "ui divider"
  , boolClass "inverted" _inverted
  , boolClass "fitted" _fitted
  , boolClass "hidden" _hidden
  , boolClass "section" _section
  , boolClass "clearing" _clearing
  ]

