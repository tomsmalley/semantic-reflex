{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Semantic UI dimmers. Pure reflex implementation is provided.
-- https://semantic-ui.com/modules/dimmers.html
module Reflex.Dom.SemanticUI.Dimmer where

import Data.Default
import Reflex

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data DimmerConfig t = DimmerConfig
  { _inverted :: Active t Bool
  -- ^ Dimmers can be a different size
  , _page :: Bool
  -- ^ Dimmers can dim the whole page
  , _closeOnClick :: Active t Bool
  -- ^ User can click out of a dimmer
  , _config :: ActiveElConfig t
  -- ^ Config
  }

instance Reflex t => Default (DimmerConfig t) where
  def = DimmerConfig
    { _inverted = pure False
    , _page = False
    , _closeOnClick = pure True
    , _config = def
    }

-- | Make the dimmer div classes from the configuration
dimmerConfigClasses :: Reflex t => DimmerConfig t -> Active t Classes
dimmerConfigClasses DimmerConfig {..} = activeClasses
  [ Static $ Just "ui active dimmer"
  , boolClass "inverted" _inverted
  , Static $ if _page then Just "page" else Nothing
  ]

-- | Dimmer UI Element.
data Dimmer t m a = Dimmer
  { _config :: DimmerConfig t
  , _content :: Component None m a
  }
