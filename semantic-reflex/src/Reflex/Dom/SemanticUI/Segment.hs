{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Semantic UI segments. Pure reflex implementation is provided.
-- https://semantic-ui.com/collections/segments.html
module Reflex.Dom.SemanticUI.Segment where

import Data.Default
import Reflex

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data Stacked = Stacked | TallStacked | Piled
  deriving (Eq, Show)

instance ToClassText Stacked where
  toClassText Stacked = "stacked"
  toClassText TallStacked = "tall stacked"
  toClassText Piled = "piled"

data SegmentConfig t = SegmentConfig
  { _raised :: Active t Bool
  -- ^ Segments can be raised
  , _stacked :: Active t (Maybe Stacked)
  -- ^ Segments can be stacked
  , _vertical :: Active t Bool
  -- ^ Segments can be formatted as part of a vertical group
  , _inverted :: Active t Bool
  -- ^ Segments can have inverted colors
  , _attached :: Active t (Maybe VerticalAttached)
  -- ^ Segments can be attached vertically
  , _padded :: Active t Bool
  -- ^ Segments can have extra padding
  , _color :: Active t (Maybe Color)
  -- ^ Segment color
  , _compact :: Active t Bool
  -- ^ If the segment should be compact
  , _emphasis :: Active t Emphasis
  -- ^ Segmants can have different emphasis
  , _circular :: Active t Bool
  -- ^ Segments can be circular
  , _clearing :: Active t Bool
  -- ^ Segments can clear floated content
  , _floated :: Active t (Maybe Floated)
  -- ^ Segments can be floated
  , _aligned :: Active t (Maybe Aligned)
  -- ^ Segments can have aligned text
  , _basic :: Active t Bool
  -- ^ A basic segment has no special formatting
  , _size :: Active t (Maybe Size)
  -- ^ Segments can be a different size
  , _config :: ActiveElConfig t
  -- ^ Config
  }

instance Reflex t => Default (SegmentConfig t) where
  def = SegmentConfig
    { _raised = Static False
    , _stacked = Static Nothing
    , _vertical = Static False
    , _inverted = Static False
    , _attached = Static Nothing
    , _padded = Static False
    , _color = Static Nothing
    , _compact = Static False
    , _emphasis = Static Primary
    , _circular = Static False
    , _clearing = Static False
    , _floated = Static Nothing
    , _aligned = Static Nothing
    , _basic = Static False
    , _size = Static Nothing
    , _config = def
    }

-- | Make the segment div classes from the configuration
segmentConfigClasses :: Reflex t => SegmentConfig t -> Active t Classes
segmentConfigClasses SegmentConfig {..} = activeClasses
  [ Static $ Just "ui segment"
  , boolClass "raised" _raised
  , fmap toClassText <$> _stacked
  , boolClass "vertical" _vertical
  , boolClass "inverted" _inverted
  , fmap toClassText <$> _attached
  , boolClass "padded" _padded
  , fmap toClassText <$> _color
  , boolClass "compact" _compact
  , Just . toClassText <$> _emphasis
  , boolClass "circular" _circular
  , boolClass "clearing" _clearing
  , fmap toClassText <$> _floated
  , fmap toClassText <$> _aligned
  , boolClass "basic" _basic
  , fmap toClassText <$> _size
  ]

-- | Segment UI Element.
data Segment t m a = Segment
  { _config :: SegmentConfig t
  , _content :: UI None m a
  }
