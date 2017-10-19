{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}

module Reflex.Dom.SemanticUI.Sticky where

import Data.Default
import Data.Semigroup hiding (First)
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data Sticky t m a = Sticky (StickyConfig t) (Component None m a)

data StickyConfig t = StickyConfig
  { _pushing :: Bool
  , _config :: ActiveElConfig t
  }

instance Reflex t => Default (StickyConfig t) where
  def = StickyConfig
    { _pushing = False
    , _config = def
    }

stickyConfigClasses :: Reflex t => StickyConfig t -> Active t Classes
stickyConfigClasses StickyConfig {..} = activeClasses
  [ Static $ Just "ui sticky"
  ]

